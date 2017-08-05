# --------------------------------------------------------------------------- #
# useful functions
# --------------------------------------------------------------------------- #

# get_toc: get table of content from wikisource ----------------------------- #

get_toc <- function(html){
  
  wiki = read_html(html)
  
  toc_li = wiki %>% 
    html_node("div#toc") %>% 
    html_nodes("li") 
  
  toc <- data.frame(matrix(nrow = length(toc_li), ncol = 0))
  
  toc$levelsec <- toc_li %>% 
    html_attr("class") 
  
  toc <- toc %>% 
    separate(levelsec, sep = " ", into = c("toclevel", "tocsection"))
  
  toc$ref <- toc_li %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_replace("#", "")
  
  toc$toctext <- toc_li %>% 
    html_nodes("span.toctext") %>% 
    html_text()
  
  toc$tocnumber <- toc_li %>% 
    html_nodes("span.tocnumber") %>% 
    html_text()
  
  rm(toc_li)
  
  return(toc)
}

# trim: trim whitespace ----------------------------------------------------- #

trim = function (x) gsub("^\\s+|\\s+$", "", x)


# ideas for functions
# repeal_article: eine Funktion, in die man die Artikelnummer und den Vertrag einfügt (am Besten mehrere als Liste), und die dann die ID findet und den Artikel löscht. 
#	add_article: eine Funktion, die einen neuen Artikel hinzufügt
#	amend_article: eine Funktion, die den Artikel findet und den zusätzlichen Text hinzufügt.
#	insert_article: wenn zu dem Artikel 78, die Artikel 78a, 78b kommen, dann werden hier Strings als neue Artikel hinzugefügt
#	repeal_string: die Funktion findet einen String in dem Artikel und löscht ihn
#	change_string: die Funktion findet einen String und ersetzt ihn durch einen neuen, der gegeben wird. 

# repeal_article: repeal whole article -------------------------------------- #

# add_article: add new article ---------------------------------------------- #

# amend_article: add new text to a article ---------------------------------- #

# insert_article: add article to existing one  ------------------------------ #

# repeal_string: repeal part of article, e.g. paragraph --------------------- #

# change_string: alter a part of article, e.g. paragraph or word ------------ #















