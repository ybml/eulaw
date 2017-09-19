# Get the "Treaty of Amsterdam" from Wikisource and export to CSV. --------------

# Libraries ---------------------------------------------------------------------
library(tidyverse)
library(rvest)
library(stringr)
library(purrrlyr)
library(data.table)

# Workspace preparation ---------------------------------------------------------
rm(list = ls())

# Get HTML and extract the links ------------------------------------------------
am <- read_html("https://en.wikisource.org/wiki/Treaty_of_Amsterdam")
links <- am %>%
	html_nodes("li>a") %>%
	html_attr("href")
links <- paste0("https://en.wikisource.org", links)
rm(am)

# Filter relevant links ---------------------------------------------------------
article_1_8_link <- links[str_detect(links, "/Article_\\d$")]
article_9_11_link <- links[str_detect(links, "9-11")] %>% 
  first() %>% 
  str_replace("#.*$", "")
article_12_15_link <- links[str_detect(links, "_-_G.*s$")]

links <- c(article_1_8_link, article_9_11_link, article_12_15_link)

rm(article_1_8_link, article_9_11_link, article_12_15_link)

# Get the content ---------------------------------------------------------------

h <- lapply(links, read_html)

# Extract the text --------------------------------------------------------------

# Get the articles as text
get_content <- function(html_l){

  # Extract the article text
  article <- html_l %>% 
    html_nodes(".mw-headline, tr, p") %>% 
    html_text() %>%
    paste(collapse = "\n")
 
  # Article Title
  art_title <- html_l %>% 
    html_nodes("#header_section_text") %>% 
    html_text()
  
  # Write to dataframe 
  df <- data.frame(article = art_title, text = article,
                          stringsAsFactors = FALSE)
  
  return(df)
}

# Put all together.
amsterdam <- lapply(h, get_content) %>% 
  bind_rows()

# Wikisource is different for Articles 9-15: ex-post fix ------------------------
# Extract and concatenate articles 9 - 15
articles_9_15 <- filter(amsterdam,
       article == "Articles 9-11" |
       article == "Part Three - General and Final Provisions"
       )  %>%
  summarise(paste(.$text, collapse = "\n")) %>%
  as.character

# Extract the article text
article_pattern <- c(paste0(rep("(?<=Article ", 6),
                   9:15,
                   "\\n)",
                   c(rep(".*(?=Article ", 6), ".*"),
                   c(10:15, ""),
                   c(rep("\\n)", 6), "")))

list_9_15 <- lapply(article_pattern,
                    function(article_i, text) {
                      str_extract(text, regex(article_i, dotall = TRUE))
                    },
                    text = articles_9_15)

df_9_15 <- data_frame(article = paste("Article", 9:15), text = unlist(list_9_15))

# Remove the old text, and add the new one.
amsterdam <- filter(amsterdam,
       article != "Articles 9-11" &
       article != "Part Three - General and Final Provisions"
       )  %>%
  bind_rows(df_9_15)

# Add the part titles to the treaty ---------------------------------------------
amsterdam <- amsterdam %>%
  mutate(part_title =
           case_when(
             row_number() < 6 ~ "PART ONE - SUBSTANTIVE AMENDMENTS",
             row_number() < 12 ~ "PART TWO - SIMPLIFICATION",
             row_number() <= 16 ~ "PART THREE - GENERAL AND FINAL PROVISIONS"
           )
         ) %>%
  select(part_title, article, text)

# Add ID to article -------------------------------------------------------------
amsterdam <- amsterdam %>%
  mutate(article_nr = row_number(),
         part_nr = c(rep(1, 5), rep(2, 6), rep(3, 4)),
         id = paste(7, part_nr, article_nr, sep = ".")) %>%
  select(id, part_nr, article_nr, part_title, article_title = article, text)

# Export as CSV -----------------------------------------------------------------
write_csv(amsterdam, path = "tables/amsterdam.csv")
# EOF
