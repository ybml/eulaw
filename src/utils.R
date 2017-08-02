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
