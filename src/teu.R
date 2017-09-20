# --------------------------------------------------------------------------- #
# Treaty on European Union (Maastricht)
# --------------------------------------------------------------------------- #

# pdf to check numbers 
# https://europa.eu/european-union/sites/europaeu/files/docs/body/treaty_on_european_union_en.pdf

url = "https://en.wikisource.org/wiki/Treaty_on_European_Union"
links = c("/Title_I:_Common_Provisions",
          "/Title_II:_Provisions_Amending_the_Treaty_establishing_the_European_Economic_Community_with_a_view_to_establishing_the_European_Community",
          "/Title_III:_Provisions_Amending_the_Treaty_Establishing_the_European_Coal_and_Steel_Community",
          "/Title_IV:_Provisions_Amending_the_Treaty_Establishing_the_European_Atomic_Energy_Community",
          "/Title_V:_Provisions_on_a_Common_Foreign_and_Security_Policy",
          "/Title_VI:_Provisions_on_Cooperation_in_the_Fields_of_Justice_and_Home_Affairs",
          "/Title_VII:_Final_Provisions")

# scrape -------------------------------------------------------------------- #

# loop over links and scrape articles of every title
for (i in 1:length(links)) { 
  
  html = read_html(paste0(url, links[i]))
  
  treaty = html %>%
    html_nodes("h3,p,dl") %>%
    html_text()
  
  df = data.frame(text = treaty)
  
  df$index = seq_len(nrow(df))
  df$title = as.character(i)
  
  df$text = str_replace_all(df$text, "\\[edit\\]", " ")
  
  # delete rows and get articles title by title 
  if (links[i] == links[1]) {
    df = df[order(df$index),] # check
    
    # delete rows
    df = subset(df, df$index >= 3 & df$index <= 27)
    
    # get articles
    df$article = trim(gsub("Article", "", str_extract_all(df$text, "^Article [A-Z]")))
    df$article[df$article == "character(0)"] = NA
    df = df[order(df$index),]
    df = df %>% fill(article)
    
    df$subarticle = NA
    
    teu = df
  }
  
  if (links[i] == links[2]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 989)
    
    # article
    df$article = "G"
    
    # sub_article (for changes)
    df$subarticle = str_extract_all(df$text, "\\d{1,2}\\)")
    df$subarticle[df$subarticle == "character(0)"] = NA
    
    # fixes
    df$subarticle[df$index == 72] = NA
    df$subarticle[df$index == 87] = "11)"
    df$subarticle[df$index == 89] = "12)"
    df$subarticle[df$index == 124] = NA
    df$subarticle[df$index == 126] = NA
    df$subarticle[df$index == 127] = NA
    df$subarticle[df$index == 129] = NA
    df$subarticle[df$index == 142] = "18)"
    df$subarticle[df$index == 148] = NA
    df$subarticle[df$index == 155] = "22)"
    df$subarticle[df$index >= 175 & df$index < 407] = NA
    df$subarticle[df$index == 429] = "33)"
    df$subarticle[df$index > 478 & df$index < 641] = NA
    df$subarticle[df$index == 710] = NA
    df$subarticle[df$index == 775] = NA
    df$subarticle[df$index == 823] = NA
    df$subarticle[df$index == 839] = NA
    df$subarticle[df$index == 953] = NA
    df$subarticle[df$index == 968] = NA
    
    # make numbers
    df$subarticle = as.numeric( gsub(")", "", df$subarticle))
    
    # fill
    df = df[order(df$index),]
    df = df %>% fill(subarticle)
    
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[3]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 170)
    
    # article
    df$article = "H"
    
    # sub_article (for changes)
    df$subarticle = str_extract_all(df$text, "\\d{1,2}\\)")
    df$subarticle[df$subarticle == "character(0)"] = NA
    
    # fixes
    df$subarticle[df$index == 34] = NA
    
    # make numbers
    df$subarticle = as.numeric( gsub(")", "", df$subarticle))
    
    # fill
    df = df[order(df$index),]
    df = df %>% fill(subarticle)
    
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[4]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 203)
    
    # article
    df$article = "I"
    
    # sub_article (for changes)
    df$subarticle = str_extract_all(df$text, "\\d{1,2}\\)")
    df$subarticle[df$subarticle == "character(0)"] = NA
    
    # fixes
    df$subarticle[df$index == 74] = NA
    
    # make numbers
    df$subarticle = as.numeric( gsub(")", "", df$subarticle))
    
    # fill
    df = df[order(df$index),]
    df = df %>% fill(subarticle)
    
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[5]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 69)
    
    # get articles
    df$article = trim(gsub("Article", "", str_extract_all(df$text, "^Article [A-Z].\\d{1,}")))
    df$article[df$index == 3] = "J"
    df$article[df$article == "character(0)"] = NA
    df = df[order(df$index),]
    df = df %>% fill(article)
    
    df$subarticle = NA
    
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[6]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 54)
    
    # get articles
    df$article = trim(gsub("Article", "", str_extract_all(df$text, "^Article [A-Z].\\d{1,}")))
    df$article[df$index == 3] = "K"
    df$article[df$article == "character(0)"] = NA
    df = df[order(df$index),]
    df = df %>% fill(article)
    
    df$subarticle = NA
    
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[7]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 29)
    
    # get articles
    df$article = trim(gsub("Article", "", str_extract_all(df$text, "^Article [A-Z]")))
    df$article[df$article == "character(0)"] = NA
    df = df[order(df$index),]
    df = df %>% fill(article)
    
    df$subarticle = NA
    
    teu = bind_rows(teu, df)
  }
}
rm(df, links, url, i, html)

# concatenate text ---------------------------------------------------------- #

teu$text = trim(teu$text)

teu = teu %>%
  group_by(title, article, subarticle) %>%
  summarize(txt = paste(text, collapse = " "))

teu$txt = trim(teu$txt)

# create id variable -------------------------------------------------------- #

teu$treaty = 6

teu$article = str_replace_all(teu$article, "\\.", "")

teu = teu %>% 
  unite(id, c("treaty", "title", "article", "subarticle"), sep = ".", remove = FALSE)

teu$id = str_replace_all(teu$id, "NA", "X")

# cut if X is at the end 
teu$id = str_replace_all(teu$id, ".X$", "")

# Save ----------------------------------------------------------------------- #

write_csv(teu, path = "tables/teu.csv")

# Apply changes --------------------------------------------------------------- #
# Remove all variables except the functions.
rm(list = setdiff(ls(), lsf.str()))

# Load merger_changes and eulaw_1986 files.
teu_changes <- read_csv("tables/teu_changes.csv")
eulaw_1986 <- readRDS("data/eulaw_1986.rds")

# Pre-processing on the changes file.
teu_changes <- set_new_txt(teu_changes, "add") %>%
  # For "insert"s change_id instead of new_id was set.
  mutate(new_id = if_else(is.na(new_id) & action == "insert",
                          change_id,
                          new_id
                  ),
         change_id = if_else(action == "insert" & !is.na(change_id),
                             NA_character_,
                             change_id
                     )
  ) %>%
  set_action(old_action = "add", new_action = "insert") %>%
  set_new_id(., "insert", id_field = id)

# Apply the changes.
eulaw_1992 <- apply_changes(eulaw_1986, teu_changes, "1992") %>%
  filter_all(any_vars(!is.na(.))) %>%
  arrange(id_1992)

# Save ---------------------------------------------------------------------- #
saveRDS(eulaw_1992, file = "data/eulaw_1992.rds")
