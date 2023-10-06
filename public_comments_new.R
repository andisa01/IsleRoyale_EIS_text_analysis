
###
# Text analaysis of Isle Royale EIS public comments via AI tools
# Purpose: The purpose of this analysis is to synthesize free-text public comments submitted in response to the National Park Service's Environmental Impact Statement concerning replacing wolves in the Park Wilderness. The secondary purpose is to explore using AI tools for complex text analysis
# Author: A. Z. Andis Arietta
# Initiated 2023 08
###

# Load libraries
library(pdftools) # We will use 'pdftools' to  convert the pdf to plain text
library(tidyverse)
library(stringr)
library(RColorBrewer)
source("https://raw.githubusercontent.com/andisa01/andis_utils/main/00_HelperFunctions.R") # This is just a handful of helper function and presets for myself.

# Set up the directory structure:
make_new_dir("./data/")
make_new_dir("./figs/")

# Get the comments from the NPS website
download.file(
  url = "https://parkplanning.nps.gov/showFile.cfm?projectID=59316&MIMEType=application%252Fpdf&filename=ISRO%5FMWVPlan%5FAllCorrespondence%5FPEPC%2Epdf&sfid=232552",
  destfile = "./data/ISRO_MWVPlan_AllCorrespondence_PEPC.pdf",
  mode = "wb"
)

# Read in the text document with PDFtools
pdf.text <- pdftools::pdf_text("./data/ISRO_MWVPlan_AllCorrespondence_PEPC.pdf")

## We can take a look at the first few pages of the document
pdf.text %>% head() %>% cat()

## PDFtools outputs each page as an individual list element. Our comments spill over pages, so we actually want to collapse everything into a single string for now.
pdf.string <- paste(pdf.text, collapse = " ")

## Now we need to clean up the text string to remove the formatting
pdf.df.raw <- 
  pdf.string %>%
  # Remove the "Page XX of 1117" that separates pages
  gsub("Page [1-9][0-9]{0,2} of 1117", "", .) %>%
  # Remove the line breaks
  gsub("\n", " ", .) %>%
  # Collapse blank spaces down to single space
  gsub("\\s+", " ", .)

## Now we need to separate each comment into it's own row. Every comment starts with "Correspondence ID:". So, a clever trick is to add a flag "END" before every instance of "Correspondence ID:" so that we can specify that everything between "Correspondence ID:" and "END" is a unique comment.
pdf.df.raw <- 
  pdf.df.raw %>%
  gsub("Correspondence ID:", "END Correspondence ID:", .) %>%
  str_extract_all("Correspondence ID:.*?END") %>% 
  # Convert the object to a dataframe
  as.data.frame(col.names = c("text"))

## Let's take a look at the first and last comments.
pdf.df.raw[1,1] %>% cat()

pdf.df.raw[2776,1] %>% cat()

## Now we want to break apart the test for each row into it's component parts. We'll start by extracting the 'Correspondence ID', 'Received' date time, the 'Correspondence' type, and finally the Content of the message.
pdf.df <- 
  pdf.df.raw %>%
  # Extract the 'Correspondence ID'
  mutate(ID = str_extract(text, "Correspondence ID: (.+?) Moose-Wolf-Vegetation")) %>%
  mutate(ID = gsub("Correspondence ID: ", "", ID),
         ID = gsub(" Moose-Wolf-Vegetation", "", ID)) %>%
  # Extract the 'Received' column
  mutate(Received = str_extract(text, "Received: [A-Za-z]+,[0-9]+,[0-9]+ [0-9]+:[0-9]+:[0-9]+")) %>%
  mutate(Received = as.POSIXct(Received, format = "Received: %b,%d,%Y %H:%M:%S")) %>%
  # Extract the 'Correspondence type' column
  mutate(Correspondence = str_extract(text, "Correspondence Type: [A-Za-z ]+")) %>%
  mutate(Correspondence = gsub("Correspondence Type: ", "", Correspondence)) %>%
  mutate(Correspondence = ifelse(Correspondence == "E", "E-mail", Correspondence)) %>%
  # Extract the content of the correspondence
  mutate(Content = str_extract(text, "Correspondence: (.+?) END")) %>%
  # We need to remove the Question and Comments headings, the 'Correspondence:' lead, and the 'END' flag
  mutate(Content = gsub("Topic Question [1-9]: ", "", Content),
         Content = gsub("Comments: ", "", Content),
         Content = gsub("Correspondence: ", "", Content),
         Content = gsub(" END", "", Content)) %>%
  # Now, we can get rid of the raw text
  select(-text)

# We have a few IDs that did not parse correctly.
pdf.df %>%
  filter(str_detect(ID, "[^0-9]")) %>%
  .$ID

pdf.df %>%
  filter(is.na(ID)) %>% nrow() # There is one response without any input for ID. I matched comment text in the PDF to discover that this is comment #28 and the error was cause by formatting error in the PDF.
  
# We can fix those explicitly
pdf.df <-
  pdf.df %>%
  mutate(
    ID = case_when(
    ID == "2526 Page 1014 of 1117" ~ 2526,
    ID == "2528 Page 1015 of 1117" ~ 2528,
    ID == "2753 Private: Y" ~ 2753,
    ID == "2755 Private: Y" ~ 2755,
    is.na(ID) ~ 28,
    TRUE ~ as.numeric(ID)
  )
  )

## There is a lot of variation in the comments. For example, comment 1 shown above follows the six topic question format suggested in the scoping document, where as comment 2776 (also show above) is just a free form text entry. Comment 68 looks like someone copy and pasted their term paper while comment 341 is just four words in length. Let's take a look at the distribution of comment lengths.

pdf.df %>%
  mutate(nchar = nchar(Content)) %>%
  ggplot(aes(x = nchar)) +
  geom_histogram(bins = 100) 
## Most responses fall in under around 6000 characters, but there are a few really long comments. For example, comment #68 is nearly 58,000 characters. 
## Excessively long comments will pose a problem because the token limit is 4000 for GPT. OpenAI says that 1 token ~ 4 characters so we need to cut all of the comments to a total of 3000 tokens (because we need to reserve ~1000 tokens for the prompt) or about 12000 characters.

pdf.df <-
  pdf.df %>%
  # Keep the raw version of the comment text
  mutate(Content_raw = Content) %>%
  # Truncate the content text to 12000 characters
  mutate(Content = str_trunc(Content_raw, 12000, "right"))

EIS_comments <- pdf.df %>%
  select(-Content_raw)

# Write out the comments to short-cut processing in the future.
EIS_comments %>%
  write.csv("./data/EIS_comments.csv", row.names = FALSE)

# Read in the preprocessed comments.
EIS_comments <- read.csv("./data/EIS_comments.csv")

EIS_comments %>% glimpse()

## At this point, we have a cleaned data frame with the content of each correspondence as it's own row.
## Out of curiosity, let's take a look at the timeline for when comments were submitted.
# The comment period was technically only suppose to be 30 days after the last public hearing, but in reality the comment period ran from July 10, 2015 through August 29, 2015. Some of the comments are dated outside of this range.
EIS_comments %>%
  mutate(Date = date(Received)) %>%
  group_by(Date) %>%
  tally() %>%
  filter(Date <= "2015-08-31") %>%
  ggplot(aes(x = Date, y = n)) +
  geom_line() +
  labs(x = "", y = "")
ggsave("./figs/Recieved_line.jpg", w = 6, h = 3, dpi = 200)

EIS_comments %>%
  group_by(Correspondence) %>%
  tally() %>%
  arrange(desc(n))
## The vast majority of comments were recieved via web form. Less that 10% were letters mailed to the park. 51 were forms given to visitors.

EIS_comments %>%
  filter(Correspondence %in% c("Web Form Correspondence", "Letter Correspondence", "Park Form Correspondence")) %>%
  mutate(Date = date(Received)) %>%
  group_by(Date, Correspondence) %>%
  tally() %>%
  filter(Date <= "2015-08-31") %>%
  ggplot(aes(x = Date, y = n)) +
  geom_line() +
  facet_wrap(vars(Correspondence), ncol = 1, scales = "free_y")
## Most comments were recieved just before the end of the scoping period. There are two additoinal spikes in Web Form responses which might be interesting to look at. Perhaps these were driven by advocacy campaigns?

EIS_comments %>%
  filter(Correspondence %in% c("Web Form Correspondence", "Letter Correspondence", "Park Form Correspondence")) %>%
  mutate(Date = date(Received)) %>%
  group_by(Date, Correspondence) %>%
  tally() %>%
  # Exclude comments received after the deadline.
  filter(Date <= "2015-08-31") %>%
  group_by(Correspondence) %>%
  # Artificially create some empty records for the last day for the two correspondence types without records that day.
  bind_rows(
    data.frame(
      Date = as_date("2015-08-31"),
      Correspondence = c("Web Form Correspondence", "Park Form Correspondence"),
      n = 0
    )
  ) %>%
  mutate(csum = cumsum(n)) %>%
  ungroup() %>%
  # mutate(Correspondence = fct_relevel(Correspondence, c("Park Form Correspondence", "Letter Correspondence", "Web Form Correspondence"))) %>%
  ggplot(aes(x = Date, y = csum, fill = fct_reorder(Correspondence, n, sum, .desc = TRUE))) +
  geom_area() +
  scale_fill_manual(values= c(brewer.pal(6, "YlOrBr")[4:6])) +
  theme(legend.position = c(.3, .8)) +
  labs(fill = "Correspondence type",
       y = "Cumulative comments",
       x = "")
ggsave("./figs/Cumulative_comments.jpg", w = 6, h = 3, dpi = 200)

## Duplicates
EIS_comments %>%
  filter(duplicated(Content)) %>%
  group_by(Content) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = nrow(.))
# We can see that there are some very similar letters. Probably a web template or a letter writing campaign.

EIS_comments %>%
  filter(grepl("I care about the wildlife at our national parks, including the wolves and moose at Isle Royale. Right now there are only three", Content)) %>%
  group_by(Content) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  mutate(total = sum(n)) # 733 answers are all Web Form correspondences with the same language in favor of Alternative B.

EIS_comments %>%
  filter(grepl("I care about the wildlife at our national parks, including the wolves and moose at Isle Royale. Right now there are only three", Content)) %>%
  group_by(Content) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(row_number() == 1) %>%
  .$Content %>% 
  cat()
# Theses form letters are in support of Alternative B.

EIS_comments %>%
  filter(grepl("Isle Royale's wilderness designation requires that we protect the area's unmanipulated, untrammeled wilderness character. Wild", Content)) %>%
  group_by(Content) %>%
  tally() %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(row_number() == 1) %>%
  .$Content %>% 
  cat()
# There are 11 duplicates of a form letter in support of Alternative A.

EIS_comments %>%
  # Flag the web form duplicates
  mutate(form_duplicate = ifelse(grepl("I care about the wildlife at our national parks, including the wolves and moose at Isle Royale. Right now there are only three", Content), "for Alt B", NA)) %>%
  mutate(form_duplicate = ifelse(grepl("Isle Royale's wilderness designation requires that we protect the area's unmanipulated, untrammeled wilderness character. Wild", Content), "for Alt A", form_duplicate)) %>%
  mutate(Content_dup = ifelse(is.na(form_duplicate), Content, form_duplicate)) %>%
  group_by(Content_dup) %>%
  tally() %>% 
  arrange(desc(n))

EIS_comments_deduplicated <- 
  EIS_comments %>%
  # Remove comments with no content
  filter(!is.na(Content)) %>%
  # Flag the web form duplicates
  mutate(form_duplicate = ifelse(grepl("I care about the wildlife at our national parks, including the wolves and moose at Isle Royale. Right now there are only three", Content), "for Alt B", NA)) %>%
  mutate(form_duplicate = ifelse(grepl("Isle Royale's wilderness designation requires that we protect the area's unmanipulated, untrammeled wilderness character. Wild", Content), "for Alt A", form_duplicate)) %>%
  # Form duplicates are not exact matches
  mutate(Content_dup = ifelse(is.na(form_duplicate), Content, form_duplicate)) %>%
  group_by(Content_dup) %>%
  slice_sample(n = 1)
# 1970 remaining comments.

EIS_comments %>%
  mutate(Date = date(Received)) %>%
  group_by(Date) %>%
  tally() %>%
  filter(Date <= "2015-08-31") %>%
  mutate(csum = cumsum(n)) %>%
  ggplot(aes(x = Date, y = csum)) +
  geom_area(fill = "firebrick") +
  geom_area(
    data = EIS_comments_deduplicated %>%
      mutate(Date = date(Received)) %>%
      group_by(Date) %>%
      tally() %>%
      filter(Date <= "2015-08-31") %>%
      mutate(csum = cumsum(n)),
    aes(x = Date, y = csum),
    fill = "grey30"
  )
# There is still a significant spike in late August, but we can see that reducing the 

## Now we can start analyzing the content. There are many ways that we could do this, depending on the question we want to answer. For instance, maybe we want to see with questions naturally group together to see if we can find common themes? A simply way to do this would be to use an approach like a Latent-Dirchelt Topic Score analysis that groups comments by tf-idf values of the stems of words contained in the comment. But, one problem with this approach is that the context of words is lost.
# If we want to capture the context of the text, we might try using word embeddings from a large-laguage model like GPT. Embeddings allow each word to be described by many dimmensions. For instance, the word "train" in these sentences: "I train a model", "I train for a marathon", "I rode the train", "I'm on the Soul Train" could bedescribed in to dimmensions of more or less metaphoical and noun/verb. Then, we could use the embeddings to cluster similar comments.
# Or, maybe we just want to know how many comments support the policy versus those that don't. It would be hard to answer that from the embeddings ourselves, but we could treat GPT as an agent who could read and classify each comment as "for" or "against" the policy.

### ChatGPT response ====
library(jsonlite)
library(httr)

# First, we'll craft a prompt to submit to chatGPT
# Example input prompt:
paste0("You are a federal employee tasked with reading the following comment submitted by a member of the public in response to the The Isle Royale National Park Moose-Wolf-Vegetation Management Plan/EIS. The Plan/EIS is a document that evaluates management alternatives for the moose and wolf populations on the island National Park land.
Management alternatives include:

- Alternative A: No Action. Continue the current management of letting nature take its course, without any intervention or manipulation of the moose or wolf populations or their habitats.
- Alternative B: Immediate Wolf Introduction. Introduce 20-30 wolves over a three-year period, starting as soon as possible to reduce the moose population and its impacts on vegetation.
- Alternative C: Wolf Introduction after Thresholds are Met. Introduce wolves if certain thresholds are met, such as the extirpation of wolves, the overabundance of moose, or the degradation of vegetation. The number and timing of wolf introductions would depend on the conditions at the time.
- Alternative D: Moose Reduction and Wolf Assessment. Reduce the moose population by lethal and non-lethal means, such as hunting, contraception, or relocation. The goal would be to lower the moose density to a level that would allow vegetation recovery and assessing introducing wolves to the island in the future.

Here is the text of the public comment: '[INSERT COMMENT TEXT]'.

State which alternative the commenter is most likely to favor (A, B, C, D).
State if the comment is 'For', 'Against', or 'Neutral' on wolf introductions.
State if the strength of the commenter's opinon on a scale from 'Extremely strong', 'Very strong', 'Strong', 'Somewhat strong', or 'Mild'.\n
Produce the output in json format like this:\n{\n\"favored_alternative\": \"\",\n\"wolf_opinion\": \"\",\n\"opinion_strength\": \"\"\n}") %>% cat()

# ChatGPT 3.5 costs 0.002$ per 1000 tokens. We can use the OpenAI tokenizer (https://platform.openai.com/tokenizer) to estimate the number of tokens constituting our input prompt. Our input is 420 tokens. The output should be less than 50 tokens. So we can round to assume 500 tokens per query. 

# In the old days, you could pass a list of inputs into ChatGPT 'completions' model all at once. This is no longer possible. Using the 'chat/completions' API requires looping through each of the inputs and making individual requests. Unfortunately, the API often fails or takes too long to respond. So, we need to be smart about error handling with this larger loop. The structure of this loop is to define the prompt, then run a tryCatch block to test if the API call fails, and if so, it skips to the next record and logs the recrods that the error occurred on.

# Later, I want to validate the reponses, so I need to randomly select 500 comments to re-analyze.
set.seed(7097)

# Randomly select 500 records to resample
IDs_to_resample <- sample(unique(EIS_comments_deduplicated$ID), 500, replace = FALSE)
ID_list <- c(unique(EIS_comments_deduplicated$ID), IDs_to_resample)

# Create a vector to store failed IDs
failed_ids <- c()

for (i in 1:length(ID_list)) {
  ID_number = ID_list[i]
  # Define the prompt
  prompt_content <-
    paste0(
      "Here is the text of the public comment: '",
      EIS_comments_deduplicated %>%
        filter(ID == ID_number) %>%
        .$Content,
      "'.
    State which alternative the commenter is most likely to favor (A, B, C, D).
State if the comment is 'For', 'Against', or 'Neutral' on wolf introductions.
State if the strength of the commenter's opinon on a scale from 'Extremely strong', 'Very strong', 'Strong', 'Somewhat strong', or 'Mild'.
Produce the output in json format like this:\n{\n\"favored_alternative\": \"\",\n\"wolf_opinion\": \"\",\n\"opinion_strength\": \"\"\n}"
    )
  
  # Initialize gpt_response
  gpt_response <- NULL
  
  # With my account, I can make 3 requests per minute. To avoid denied API calls, I add a 18 second pause in each loop.
  Sys.sleep(18)
  
  tryCatch({
    # Call GPT for a response
    gpt_response <- 
      POST(
        url = "https://api.openai.com/v1/chat/completions", 
        add_headers(Authorization = paste0("Bearer ", read_lines("../credentials/openai.key"))),
        content_type_json(),
        encode = "json",
        body = list(
          model = "gpt-3.5-turbo",
          messages = list(
            list(
              "role" = "system",
              "content" = "You are a federal employee tasked with reading the following comment submitted by a member of the public in response to the The Isle Royale National Park Moose-Wolf-Vegetation Management Plan/EIS. The Plan/EIS is a document that evaluates management alternatives for the moose and wolf populations on the island National Park land.
Management alternatives include:
- Alternative A: No Action. Continue the current management of letting nature take its course, without any intervention or manipulation of the moose or wolf populations or their habitats.
- Alternative B: Immediate Wolf Introduction. Introduce 20-30 wolves over a three-year period, starting as soon as possible to reduce the moose population and its impacts on vegetation.
- Alternative C: Wolf Introduction after Thresholds are Met. Introduce wolves if certain thresholds are met, such as the extirpation of wolves, the overabundance of moose, or the degradation of vegetation. The number and timing of wolf introductions would depend on the conditions at the time.
- Alternative D: Moose Reduction and Wolf Assessment. Reduce the moose population by lethal and non-lethal means, such as hunting, contraception, or relocation. The goal would be to lower the moose density to a level that would allow vegetation recovery and assessing introducing wolves to the island in the future."
            ),
            list(
              "role" = "user",
              "content" = prompt_content
            )
          )
        )
      )
    print(paste0("API call successful for ID: ", ID_number, ", index: ", i))
  }, error = function(e) {
    # Handle API call errors
    cat("API call failed for ID: ", ID_number, ", index: ", i, "\n")
    failed_ids <- c(failed_ids, i)
  })
  
  # If the API call was successful, proceed with data wrangling and output
  if (!is.null(gpt_response)) {
    # parse the response object as JSON
    content <- content(gpt_response, as = "parsed")
    
    # Assign the ID to the GPT response
    gpt_response_df <- data.frame(
      response_id = ID_number,
      gpt_response = content$choices[[1]]$message$content
    )
    
    # Convert the JSON to a dataframe and join to the record data
    output <- bind_cols(
      EIS_comments_deduplicated %>%
        filter(ID == ID_number),
      fromJSON(gpt_response_df$gpt_response) %>% 
        as.data.frame()
    ) %>%
      mutate(response_created_time = Sys.time())
    
    # Append the data to the extant records and write the output to a file. (This is a bit less memory efficient to do this within the loop, but I )
    if (!file.exists("./data/EIS_GPT_responses.csv")) {
      write.csv(output, "./data/EIS_GPT_responses.csv", row.names = FALSE)
    } else {
      read.csv("./data/EIS_GPT_responses.csv") %>%
        mutate(across(everything(), as.character)) %>%
        bind_rows(output %>%
                    mutate(across(everything(), as.character))
        ) %>%
        write.csv("./data/EIS_GPT_responses.csv", row.names = FALSE)
    }
    
    print(paste0("Completed response ", i))
  }
}

# Log the failed IDs to a file
if (length(failed_ids) > 0) {
  write.csv(data.frame(ID = failed_ids), "./failed_ids.csv", row.names = FALSE)
  cat("Failed IDs logged to 'failed_ids.csv'\n")
}

replace_empty_with_unknown <- function(x) {ifelse(is.na(x) | x == "", "Unknown", x)}

GPT_output <- read.csv("./data/EIS_GPT_responses.csv") %>%
  mutate(
    across(
      c(
        "favored_alternative",
        "wolf_opinion",
        "opinion_strength"
        ),
      replace_empty_with_unknown
      )
    )

# A couple of interesting things to note here. First, I apparently was not specific enough in my instructions for classifying the favored alternative because chatGPT sometimes returns "Alternative B" instead of just "B". This is one of the struggles with using chatGPT, a non-deterministic model, in this way. We could either atempt to clarify our prompt or just clean it up on the backend. Second, an interesting feature of our samples is that comments 1107. 1108, 1109, 1110, and 1111 are identical. This is a common occurence with EIS comments when advocacy gruops provide stock responses for folks to send in. Insterestingly, despite containing exactly the same language, chatGPT thought that commenter 1111's opinion was stronger than the others. Again, the model is non-deterministic, just like humans. And just like human evaluators, you won't always get the same answer twice. This is a common issue in dealing with human evaluators, each reviewer has their own intra-rater reliability. We could easily assess chatGPT's IRR by passing the same questions multiple times.

# Since chatGPT is nondeterministic, it will sometimes return unexpected results. Drawing from the analogy of chatGPT as a human assistant, we might expect that human reviewers might stray from instructions when inputting free-text results. For example, common responses to the question, "How are you feeling on a scale from 1 to 10 with 1 being bad and 10 being good?" might be "I'm good" or "Okay" or "nine" or "0". None of those answers fit the instructions, so we have to clean them up.
# In the case of chatGPT, we might be able to reduce these errors with more specific prompt engineering. Or, we might simply omit those spurious responses and rerun those records.
# The bottom line is that working with chatGPT is less like working with a model and more like working with human raters and all of the reliability tasks that entails.
# For the purpose of this tutorial, we'll clean the data and rerun where necessary.

# The first issue is that, despite providing an example response, the model decided to rename the varaibles for a handful of responses (i.e "Favored_alternative" instead of "favored_alternative").
GPT_output %>% colnames()

GPT_output <-
  GPT_output %>%
  mutate(
    favored_alternative = ifelse(is.na(favored_alternative), Favored_alternative, favored_alternative),
         wolf_opinion = ifelse(is.na(wolf_opinion), Wolf_opinion, wolf_opinion),
         opinion_strength = ifelse(is.na(opinion_strength), Opinion_strength, opinion_strength)
    ) %>%
  select(
    -Wolf_opinion,
    -Favored_alternative,
    -Opinion_strength
  )

# Even though we asked chatGPT to return favored_alternative as c("A", "B", "C", or "D") it got creative with some of its responses.
GPT_output %>%
  group_by(favored_alternative) %>%
  tally() %>%
  arrange(desc(n)) %>%
  print(n = nrow(.))

# There are probably more elegant ways to write generalized rules to classify these reponses, but this does the trick
GPT_output <-
  GPT_output %>%
  mutate(
    favored_alternative_edit = case_when(
      (grepl(" and ", favored_alternative) | grepl(" or ", favored_alternative) | grepl("/", favored_alternative) | grepl("&", favored_alternative) | favored_alternative == "B, D") & !grepl(" and Wolf ", favored_alternative) & !grepl("N/A", favored_alternative) ~ "Multiple",
      grepl("\\bAlternative A\\b", favored_alternative) | favored_alternative %in% c("A", "No Action (A)") ~ "A",
      grepl("\\bAlternative B\\b", favored_alternative) | favored_alternative == "B" ~ "B",
      grepl("\\bAlternative C\\b", favored_alternative) | favored_alternative %in% c("C", "Concept C") ~ "C",
      grepl("\\bAlternative D\\b", favored_alternative) | favored_alternative == "D" ~ "D",
      TRUE ~ "Other"
    )
  )

GPT_output %>%
  group_by(favored_alternative_edit, favored_alternative) %>%
  tally() %>%
  print(n = nrow(.)) 

GPT_output %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  ggplot(aes(x = fct_rev(favored_alternative_edit))) +
  geom_bar() +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "Favored alternative (assessed via chatGPT)"
  )
ggsave("./figs/Bar_FavAlt.jpg", w = 6, h = 3, dpi = 200)

# What does the distribution of favored alternatives look like if we included the duplicated responses?
# 733 b
# 11 a
GPT_output %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  group_by(favored_alternative_edit) %>%
  tally() %>%
  mutate(type = "unique") %>%
  bind_rows(
    data.frame(
      favored_alternative_edit = c("A", "B"),
      n = c(10, 732),
      type = "duplicate"
    )
  ) %>%
  ggplot(aes(x = fct_rev(favored_alternative_edit), y= n, fill = type)) +
  geom_bar(
    stat = "identity"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick", "grey30")) +
  labs(
    x = "",
    y = "",
    title = "Favored alternative (assessed via chatGPT)",
    fill = ""
  ) +
  theme(legend.position = c(.85, .3))
ggsave("./figs/Bar_FavAlt_dups.jpg", w = 6, h = 3, dpi = 200)

# We have a similar problem with opinion_strength. We asked for the responses to be one of 'Extremely strong', 'Very strong', 'Strong', 'Somewhat strong', or 'Mild'. But chatGPT also classified some responses as "moderate" and "neutral"
GPT_output %>%
  group_by(tolower(opinion_strength)) %>%
  tally() %>%
  arrange(desc(n))

GPT_output <-
  GPT_output %>%
  mutate(opinion_strength = tolower(opinion_strength)) %>%
  mutate(
    opinion_strength_edit = case_when(
      opinion_strength %in% c("strong", "very strong", "mild", "somewhat strong", "extremely strong") ~ opinion_strength,
      TRUE ~ "other"
    )
  ) 

GPT_output %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  group_by(opinion_strength_edit, opinion_strength) %>%
  tally()

GPT_output %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  ggplot(aes(x = fct_relevel(opinion_strength_edit, c("other", "mild", "somewhat strong", "strong", "very strong", "extremely strong")))) +
  geom_bar() +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "Opinion strength (assessed via chatGPT)"
  )
ggsave("./figs/Bar_OpinionStrength.jpg", w = 6, h = 3, dpi = 200)

GPT_output %>%
  filter(favored_alternative_edit %in% c("A", "B", "C", "D")) %>%
  mutate(favored_alternative_edit = paste("Alternative ", favored_alternative_edit)) %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  ggplot(aes(x = fct_relevel(opinion_strength_edit, c("other", "mild", "somewhat strong", "strong", "very strong", "extremely strong")),
             fill = favored_alternative_edit)) +
  geom_bar() +
  facet_wrap(vars(favored_alternative_edit), scales = "free_x", ncol = 1) +
  theme(legend.position = "none") +
  coord_flip() +
  labs(
    x = "",
    y = "",
    title = "Opinion strength (assessed via chatGPT)",
    subtitle = "Duplicates removed"
  ) +
  scale_fill_brewer(palette = "Set1")
ggsave("./figs/Bar_OpinionStrength_byAlt.jpg", w = 6, h = 6, dpi = 200)


# We also need to clean the wolf_opinion variable
GPT_output %>%
  group_by(wolf_opinion) %>%
  tally()

GPT_output <-
  GPT_output %>%
  mutate(wolf_opinion = tolower(wolf_opinion)) %>%
  mutate(
    wolf_opinion_edit = case_when(
      wolf_opinion %in% c("for", "against", "neutral") ~ wolf_opinion,
      TRUE ~ "other"
    )
  )

GPT_output %>%
  group_by(ID) %>%
  filter(response_created_time == min(response_created_time)) %>%
  group_by(wolf_opinion_edit, wolf_opinion) %>%
  tally()

# Compare IRR
# Get only the responses that were double scored
IRR_comparisons <- 
  GPT_output %>%
  group_by(ID) %>%
  arrange(response_created_time) %>%
  mutate(ID_row_count = row_number()) %>%
  filter(ID_row_count <= 2) %>%
  mutate(n = n()) %>%
  filter(n > 1) %>%
  ungroup()

IRR_comparisons %>%
  select(ID, favored_alternative_edit, ID_row_count) %>%
  pivot_wider(id_cols = "ID", names_from = "ID_row_count", values_from = "favored_alternative_edit", names_prefix = "val") %>%
  group_by(val1 == val2) %>%
  tally() %>%
  mutate(
    total = sum(n),
    prop = n/total
    )

# Lets look at some examples that disagree
IRR_comparisons %>%
  select(ID, favored_alternative_edit, ID_row_count) %>%
  pivot_wider(id_cols = "ID", names_from = "ID_row_count", values_from = "favored_alternative_edit", names_prefix = "val") %>%
  filter(val1 != val2)

EIS_comments_deduplicated %>%
  filter(ID == 288) %>%
  .$Content %>%
  cat()

EIS_comments_deduplicated %>%
  filter(ID == 1160) %>%
  .$Content %>%
  cat()

IRR_comparisons %>%
  select(ID, wolf_opinion_edit, ID_row_count) %>%
  pivot_wider(id_cols = "ID", names_from = "ID_row_count", values_from = "wolf_opinion_edit", names_prefix = "val") %>%
  group_by(val1 == val2) %>%
  tally() %>%
  mutate(total = sum(n),
         prop = n/total) # <1% of the time chatGPT disagreed with itself about the wolf opinion

IRR_comparisons %>%
  select(ID, opinion_strength_edit, ID_row_count) %>%
  pivot_wider(id_cols = "ID", names_from = "ID_row_count", values_from = "opinion_strength_edit", names_prefix = "val") %>%
  group_by(val1 == val2) %>%
  tally() %>%
  mutate(total = sum(n),
         prop = n/total)
# Only 1% of the time the model disagrees with itself.
# Let's see how many change more than 1 level up or down.

IRR_comparisons %>%
  group_by(opinion_strength_edit) %>%
  tally()

IRR_comparisons %>%
  mutate(opinion_strength_rank =
           case_when(
             opinion_strength_edit == "mild" ~ 1,
             opinion_strength_edit == "somewhat strong" ~ 2,
             opinion_strength_edit == "strong" ~ 3,
             opinion_strength_edit == "very strong" ~ 4,
             opinion_strength_edit == "extremely strong" ~ 5,
             opinion_strength_edit == "other" ~ 10,
             TRUE ~ NA_real_
           )) %>%
  select(ID, opinion_strength_rank, ID_row_count) %>%
  pivot_wider(id_cols = "ID", names_from = "ID_row_count", values_from = "opinion_strength_rank", names_prefix = "val") %>%
  # remove any comparisons where the strength started or changed to "other"
  filter(val1 != 10 & val2 != 10) %>%
  mutate(dif = abs(val1 - val2)) %>%
  group_by(dif) %>%
  tally() %>%
  ungroup() %>%
  mutate(total = sum(n),
         prop = n/total) # In all cases, the model only disagreed by onw level. 

# Using Krippendorff’s Alpha.
library(irr)

# The irr package needs the dataset in wide format matrix with one row per reviewer and each record as a column. For this analysis, we'll consider the first and second responses from chatGPT as individual reviewers.
IRR_comparisons %>%
  mutate(opinion_strength_edit = fct_relevel(
    opinion_strength_edit,
    c(
      "other",
      "mild",
      "somewhat strong",
      "strong",
      "very strong",
      "extremely strong"
    )
  )) %>%
  select(ID,
         opinion_strength_edit,
         ID_row_count) %>%
  pivot_wider(
    id_cols = "ID_row_count",
    names_from = "ID",
    values_from = "opinion_strength_edit",
    names_prefix = "ID_"
  ) %>%
  select(-ID_row_count) %>%
  as.matrix() %>%
  kripp.alpha(method = "ordinal")

levels(as.factor(IRR_comparisons$opinion_strength_edit))

### Embeddings ====

# Clean up the text to remove non-alpha numeric characters
input_to_embed <- 
  EIS_comments_deduplicated %>%
  mutate(Content_cleaned = str_replace_all(Content, "[^[:alnum:]]", " "))

# Call open ai for the embeddings
embeddings_return <-
  POST(
    "https://api.openai.com/v1/embeddings",
    add_headers(Authorization = paste0(
      "Bearer ", read_lines("../credentials/openai.key")
    )),
    body = list(model = "text-embedding-ada-002",
                input = input_to_embed$Content_cleaned),
    encode = "json"
  )

# Save the output for later
saveRDS(embeddings_return, paste0("./data/embeddings_return_", Sys.Date(), ".rds"))

# Extract the embeddings from the API return
embeddings_list <-
  embeddings_return %>%
  content(as = "text", encoding = "UTF-8") %>%
  fromJSON(flatten = TRUE) %>%
  pluck("data", "embedding")

# Combine the embeddings with the original data
EIS_GPT_embeddings <- 
  EIS_comments_deduplicated %>%
  as_tibble() %>%
  mutate(
    embeddings = embeddings_list,
    ID = as.character(ID)
  ) %>%
  left_join(
    # We need to get only the first instance of the GPT response data, which also included the repeated reliability test responses, to know which alternative the comment favors
    GPT_output %>%
        group_by(ID) %>%
        arrange(response_created_time) %>%
        mutate(ID_row_count = row_number()) %>%
        filter(ID_row_count == 1) %>%
        ungroup() %>%
      select(
        ID,
        favored_alternative_edit,
        opinion_strength_edit
      )
  )

# Load Rtsne
library(Rtsne)

# Rtsne requires the embeddings to be in matrix form, so we extract the lists of emdeddings from the dataframe and convert them to matrix form.
openai_embeddings_mat <- matrix(
  unlist(EIS_GPT_embeddings  %>%
         .$embeddings), 
  ncol = 1536, byrow = TRUE
)

# Estimate tsne coordinates
set.seed(7267158)
tsne_embeddings <- 
  Rtsne(
    openai_embeddings_mat, 
    pca = TRUE, 
    theta = 0.5, 
    perplexity = 50, 
    dims = 2, 
    max_iter = 10000, 
    check_duplicates = FALSE
    )

# Extract the tSNE coordinates and add them to the main dataset
EIS_GPT_embeddings <- 
  EIS_GPT_embeddings %>%
  mutate(
    tsne_dim1 = tsne_embeddings$Y[,1],
    tsne_dim2 = tsne_embeddings$Y[,2]
  )

EIS_GPT_embeddings %>%
  ggplot(aes(x = tsne_dim1, y = tsne_dim2)) +
  geom_point(alpha = 0.5,
             pch = 16)
ggsave("./figs/tsne_raw.jpg", w = 6, h = 6, dpi = 200)

# tSNE plot of the openai embeddings
# The tSNE plot uncovers some weak groupings, but there are no extremely clear delineation between most comments. This is likely a symptom of low diversity in comments and the fact that most of our comments are very short, so there is less signal in the content.

library(tidygraph)
library(ggraph)

tsne_embedding_clusters <- hclust(dist(tsne_embeddings$Y), method = "average")

# Define clusters
EIS_embeddings_clustered <-
  EIS_GPT_embeddings %>%
  mutate(
    cluster = cutree(tsne_embedding_clusters, 7)
  )

as_tbl_graph(tsne_embedding_clusters) %>%
  left_join(
    EIS_embeddings_clustered %>%
      rownames_to_column() %>%
      mutate(cluster = as.character(cluster)) %>%
      select(label = rowname,
             ID,
             cluster),
    by = "label"
  ) %>%
  activate(nodes) %>%
  filter(cluster == 2) %>%
  length() # Use this bit of code to determine how many observations are in a given cluster. Then, use those values to define the color bounding boxes on the tree figure.

# EIS_embeddings_clustered %>%
#   saveRDS(paste0("./embedding_clusters_", Sys.Date(), ".rds"))

plot_grid(
  EIS_embeddings_clustered %>%
    ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = as.factor(cluster))) +
    geom_point(alpha = 0.5,
               pch = 16) +
    labs(col = "Cluster") +
    scale_color_brewer(palette = c("Dark2")) +
    theme_void() +
    theme(legend.position = "none"),
  
  as_tbl_graph(tsne_embedding_clusters) %>%
    left_join(
      EIS_embeddings_clustered %>%
        rownames_to_column() %>%
        mutate(cluster = as.character(cluster)) %>%
        select(label = rowname,
               ID,
               cluster),
      by = "label"
    ) %>%
    ggraph(layout = 'dendrogram', height = height) +
    # There must be a better way to make these highlights without specifying via annotations.
    annotate("rect",
             xmin = 0,
             xmax = 0 + 49,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[7]) +
    annotate("rect",
             xmin = 0 + 49,
             xmax = 0 + 49 + 297,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[3]) +
    annotate("rect",
             xmin = 0 + 49 + 297,
             xmax = 49 + 297 + 145,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[6]) +
    annotate("rect",
             xmin = 0 + 49 + 297 + 145,
             xmax = 0 + 49 + 297 + 145 + 533,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[1]) +
    annotate("rect",
             xmin = 0 + 49 + 297 + 145 + 533,
             xmax = 0 + 49 + 297 + 145 + 533 + 295,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[4]) +
    annotate("rect",
             xmin = 0 + 49 + 297 + 145 + 533 + 295,
             xmax = 0 + 49 + 297 + 145 + 533 + 295 + 173,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[5]) +
    annotate("rect",
             xmin = 0 + 49 + 297 + 145 + 533 + 295 + 173,
             xmax = 0 + 49 + 297 + 145 + 533 + 295 + 173 + 478,
             ymin = 0,
             ymax = 15,
             alpha = 0.4,
             fill = brewer.pal(8, "Dark2")[2]) +
    geom_edge_elbow(colour = "grey40") +
    geom_hline(aes(yintercept = 15), lty = 2, col = "grey20") +
    geom_node_point(data = . %>%
                      filter(label != ""),
                    aes(col = cluster)) +
    theme_void() +
    theme(legend.position = "top") +
    scale_color_brewer(palette = c("Dark2")),
  
  nrow = 1
)
ggsave("./figs/tsne_tree_clustering2.jpg", w = 10, h = 6, dpi = 200)

library(tidytext)

EIS_words <-
  EIS_embeddings_clustered %>%
  unnest_tokens(word, Content) %>%
  anti_join(get_stopwords()) %>%
  filter(
    !word %in% c("www.lifewatchgroup.org", "xxx")
  )

EIS_words %>%
  group_by(word) %>%
  tally() %>%
  arrange(desc(n))
# No big surprise on the most common words here.

EIS_words %>%
  group_by(cluster, word) %>%
  tally() %>%
  top_n(10, n) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, n, cluster)) %>%
  ggplot(aes(x = word, y = n, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(cluster), scales = "free", nrow = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  labs(
    title = "Most common words per cluster",
    x = "",
    y = "word count"
  )
# This is somewhat illuminating. That little brown cluster 7 that seemed to have representation from all favor alternatives turns out to be be heavy on management/procedural words but is separate from the wilderness topic. But wolves, moose, isle, park are top words in almost all of these clusters. We need to differentiate them.
ggsave("./figs/most_common_words_clusters02.jpg", w = 8, h = 4, dpi = 200)

EIS_words %>%
  group_by(cluster, word) %>%
  tally() %>%
  arrange(desc(n)) %>%
  bind_tf_idf(word, cluster, n)
# Common words have low tf-idf.

EIS_words %>%
  group_by(cluster, word) %>%
  tally() %>%
  bind_tf_idf(word, cluster, n) %>%
  arrange(desc(tf_idf))

# top_n doesn't work with numeric variables, so we have to rank them to get the top 20.
EIS_words %>%
  group_by(cluster, word) %>%
  tally() %>%
  bind_tf_idf(word, cluster, n) %>%
  mutate(rank = rank(-tf_idf, ties.method = "first")) %>%
  filter(rank <= 10) %>%
  ungroup() %>%
  mutate(word = reorder_within(word, -rank, cluster)) %>%
  ggplot(aes(x = word, y = tf_idf*1000, fill = as.factor(cluster))) +
  geom_bar(stat = "identity") +
  facet_wrap(vars(cluster), scales = "free", nrow = 2) +
  coord_flip() +
  scale_x_reordered() +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    title = "Most uniquely important words per cluster",
    x = "",
    y = "TF-IDF (term frequency-inverse document frequency) (x10^3)"
  )
ggsave("./figs/tfidf_clusters02.jpg", w = 8, h = 4, dpi = 200)
# term frequency inverse document frequency bar plots for clusters
# 

# How do the topic clusters match with the chatGPT categorizations?
plot_grid(
  EIS_GPT_embeddings %>%
    filter(!is.na(favored_alternative_edit)) %>%
    ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = as.factor(favored_alternative_edit))) +
    geom_point(alpha = 0.5,
               pch = 16,
               size = 2) +
    labs(col = "",
         title = "Favoured alternative") +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    theme(
      legend.position = 'bottom'
    ),
  
  EIS_GPT_embeddings %>%
    filter(!is.na(opinion_strength_edit)) %>%
    ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = fct_relevel(as.factor(opinion_strength_edit), c("other", "mild", "somewhat strong", "strong", "very strong", "extremely strong")))) +
    geom_point(alpha = 0.5,
               pch = 16,
               size = 2) +
    labs(col = "",
         title = "Opinion strength") +
    scale_color_manual(values= c("grey", brewer.pal(6, "YlOrBr")[2:6])) +
    theme_void() +
    theme(
      legend.position = 'bottom'
    )
)
ggsave("./figs/tsne_favalt_opstrength.jpg", w = 10, h = 6, dpi = 200)

# How do the topic clusters match with the chatGPT categorizations?
plot_grid(
  EIS_embeddings_clustered %>%
    filter(!is.na(favored_alternative_edit)) %>%
    ggplot(aes(x = tsne_dim1, y = tsne_dim2, col = as.factor(favored_alternative_edit))) +
    geom_point(alpha = 0.5,
               pch = 16,
               size = 2) +
    labs(col = "",
         title = "Favoured alternative") +
    scale_color_brewer(palette = "Set1") +
    theme_void() +
    theme(
      legend.position = 'bottom'
    ),
  
  EIS_embeddings_clustered %>%
    filter(!is.na(favored_alternative_edit)) %>%
    ggplot(aes(x = favored_alternative_edit, fill = as.factor(favored_alternative_edit))) +
    geom_bar() +
    coord_flip() +
    facet_wrap(vars(cluster), scales = "free_x") +
    scale_fill_brewer(palette = "Set1") +
    scale_x_discrete(limits=rev) +
    labs(
      title = "Favored alternative by topical cluster",
      x = "",
      y = ""
    )
)
ggsave("./figs/tsne_favalt_by_cluster.jpg", w = 10, h = 6, dpi = 200)

## Chain of density prompting ====
cluster3_content <- 
  EIS_embeddings_clustered %>%
  filter(cluster == 3) %>%
  .$Content %>%
  paste(., collapse = ";")


paste0("You will generate increasingly concise entity-dense summaries of the semicolon separated comments included below.

The comments were submitted by a member of the public in response to the The Isle Royale National Park Moose-Wolf-Vegetation Management Plan/EIS. The Plan/EIS is a document that evaluates management alternatives for the moose and wolf populations on the island National Park land.

Now that you know the context, here are the semicolon separated survey response: '",
       cluster3_content,
       "'

Instructions: You will generate increasingly concise entity-dense summaries of the above semicolon separated comments. Repeat the following 2 steps 5 times.

Step 1: Identify 1-3 informative entities (delimited) from the comments which are missing from the previously generated summary.
Step 2: Write a new denser summary of identical length which covers every entity and detail from the previous summary plus the missing entities.

A missing entity is
- Relevant: to the main themes of the comments.
- Specific: descriptive yet concise (5 words or fewer).
- Novel: not in the previous summary.
- Faithful: present in the comments.
- Anywhere: located in the comments.

Guidelines:
- The first summary should be long (7 - 5 sentences, ~100 words), yet highly non-specific, containing little information beyond the entities marked as missing. Use overly verbose language and fillers (e.g., 'these comments discuss') to reach ~100 words.
- Make every word count. Rewrite the previous summary to improve flow and make space for additional entities.
- Make space with fusion, compression, and removal of uninformative phrases like 'these comments discuss'.
- The summaries should become highly dense and concise, yet self-contained, e.g., easily understood without having read the comments.
- Missing entities can appear anywhere in the new summary.
- Never drop entities from the previous summary. If space cannot be made, add fewer new entities.

Remember: Use the exact same number of words for each summary.

Answer in JSON. The JSON should be a list (length 5) of dictionaries whose keys are 'missing_entities' and 'denser_summary'.") %>%
  write_file(., file = paste0("./data/prompt_for_claude_", format(Sys.time(), "%d-%b-%Y_%H-%M-%S"), ".txt"))
