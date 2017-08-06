# --------------------------------------------------------------------------- #
# Treaty on European Union (Maastricht)
# --------------------------------------------------------------------------- #

# August 6, 2017
url = "https://en.wikisource.org/wiki/Treaty_on_European_Union"
links = c("/Title_I:_Common_Provisions",
          "/Title_II:_Provisions_Amending_the_Treaty_establishing_the_European_Economic_Community_with_a_view_to_establishing_the_European_Community",
          "/Title_III:_Provisions_Amending_the_Treaty_Establishing_the_European_Coal_and_Steel_Community",
          "/Title_IV:_Provisions_Amending_the_Treaty_Establishing_the_European_Atomic_Energy_Community",
          "/Title_V:_Provisions_on_a_Common_Foreign_and_Security_Policy",
          "/Title_VI:_Provisions_on_Cooperation_in_the_Fields_of_Justice_and_Home_Affairs",
          "/Title_VII:_Final_Provisions")

# scrape 
for (i in 1:length(links)) { 

  html = read_html(paste0(url, links[i]))
  
  treaty = html %>%
    html_nodes("h3,p,dl") %>%
    html_text()
  
  df = data.frame(text = treaty)
  
  df$index = seq_len(nrow(df))
  df$title = as.character(i)
  
  df$text = str_replace_all(df$text, "\\[edit\\]", " ")
  
  # delete rows title by title
  if (links[i] == links[1]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 27)
    teu = df
  }
  
  if (links[i] == links[2]) {
    df = df[order(df$index),] # check
    # df = subset(df, df$index <= 989)
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[3]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 170)
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[4]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 203)
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[5]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 69)
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[6]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index <= 54)
    teu = bind_rows(teu, df)
  }
  
  if (links[i] == links[7]) {
    df = df[order(df$index),] # check
    df = subset(df, df$index >= 3 & df$index <= 29)
    teu = bind_rows(teu, df)
  }
}
rm(df, links, url)

# get articles -------------------------------------------------------------- #

# ...