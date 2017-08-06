# --------------------------------------------------------------------------- #
# Treaty on European Union (Maastricht)
# --------------------------------------------------------------------------- #

ma_url <- "https://en.wikisource.org/wiki/Treaty_on_European_Union"
ma <- read_html(ma_url)
write_html(ma, file = "src/htmls/ma.html")


# 1. Get Links
# get all links inside a <li>
links <- ma %>% 
  html_nodes("li>a") %>% 
  html_attr("href")

links <- paste0("https://en.wikisource.org", links)
rm(ma)

# preamble
preamble_link <- links[str_detect(links, "/wiki/Treaty_on_European_Union")
                       & str_detect(links, "Preamble")]

# only those that refer to /wiki/Treaty_on_European_Union and Provisions
prov_links <- links[str_detect(links, "/wiki/Treaty_on_European_Union")
                    & str_detect(links, "Provisions")]
rm(links)

# 2. Extract 
# 2.1. preamble
preamble_h <- read_html(preamble_link)

# in the preamble, all the p's form the text
preamble <- preamble_h %>% 
  html_nodes("p") %>% 
  html_text()

preamble <- data.frame(toctext = "Preamble", title = "Preamble",
                       article = "0", paragraph = NA, list_elem = NA, 
                       text_id = 1:length(preamble), text = preamble,
                       stringsAsFactors = F)


preamble_df <- preamble %>% 
  select(toctext, title, article, paragraph, list_elem, text_id, text)

# create id variable
preamble_df <- preamble_df %>% 
  unite(id, article:list_elem, sep = ".", remove = F)
preamble_df$id <- str_replace_all(preamble_df$id, "NA", "x")

# reorder the columns 
preamble_df <- preamble_df %>% 
  select(id, everything())


rm(preamble_h, preamble_link, preamble)

# 2.2. Provisions
# prepare the htmls -> introduce article tags
htmls <- lapply(prov_links, function(l){
  h <- read_html(l)
  h <- str_replace_all(h, "<h3>[\n]?<span", "</article> <article><h3><span")
  h <- str_replace(h, "</article>", "") # first closing tag has no beginning tag
  h <- read_html(h)
  return(h)
})

results <- lapply(htmls, function(h){
  # get the title 
  title <- h %>% 
    html_node("span#header_section_text") %>% 
    html_text()
  
  # still the amendments are contained within articles
  # get article id 
  ids <- h %>% 
    html_nodes("h3>span.mw-headline") %>% 
    html_attr("id")
  
  arts_text <- h %>% 
    html_nodes("h3>span.mw-headline") %>% 
    html_text() %>% 
    str_replace("Article ", "")
  
  # construct a data frame
  toc <- data.frame(title = title, a_id = ids, article = arts_text, 
                    stringsAsFactors = F)
  toc$article_num <- as.character(1:nrow(toc))
  
  articles <- get_nested_text(h, "article", "article_num", "p,li,dd")
  articles <- left_join(articles, toc, by = "article_num")
  articles <- extract_paragraphs(articles, regex = "^\\d+\\.")
  articles <- extract_list_elem(articles)
  
  return(articles)
})

provisions <- bind_rows(results)

# arrange
provisions <- provisions %>% 
  arrange(title, article, text_id)

# delete article num and a_id
provisions$article_num <- NULL
provisions$a_id <- NULL

# add variables that are not yet part of the data frame
# provisions$tocnumber <- NA
provisions$toctext <- provisions$title # in this case, this is the same
# provisions$chapter <- NA
# provisions$section <- NA
# provisions$part <- NA

# extract title number 
provisions$title <- unlist(str_replace_all(provisions$title, 
                                           "Title (.+?)\\:.+", "\\1"))

# article name includes "." -> remove them in order not to mess with the id later
provisions$article <- str_replace_all(provisions$article, "\\.", "")

provisions_df <- provisions %>% 
  select(toctext, title, article, paragraph, list_elem, text_id, text)

# create id variable
provisions_df <- provisions_df %>% 
  unite(id, title:list_elem, sep = ".", remove = F)
provisions_df$id <- str_replace_all(provisions_df$id, "NA", "x")

# reorder columns
provisions_df <- provisions_df %>% 
  select(id, everything())

# 3. Combine and export 
# bind together with preamble
maastricht <- bind_rows(preamble_df, provisions_df)

maastricht <- maastricht %>% 
  mutate(treaty = "maastricht") %>% 
  mutate(in_db = !str_detect(toctext, "[Aa]mending|Preamble")) %>% 
  select(treaty, everything())

# give text id anew because it is for now nested within the titles / preamble
maastricht$text_id <- 1:nrow(maastricht)

write.csv(maastricht, file = "data/maastricht.csv", row.names = F)
