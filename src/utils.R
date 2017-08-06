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

# apply_changes: ------------------------------------------------------------ #

apply_changes <- function(df, which_treaty){
  
  apply_change_r <- function(row, ...){
    if(is.na(row$action)){
      return(row)
    } else if(row$action == "repeal"){
      row$text = ""
      row[, paste0(which_treaty, "_id")] <- "NULL"
    } else if(row$action == "repl_txt"){
      row$text <- str_replace_all(row$text, row$old_txt, row$new_txt)
    } else if(row$action %in% c("fork_ent", "add", "repl_ent")){
      row$text <- row$new_txt
    } else if(row$action == "amend"){
      row$text <- paste(row$text, row$new_txt, sep = " ")
    }
    return(row)
  }
  
  # split
  df$splitvar <- paste(df$treaty, df$current_id, sep = "-")
  pieces <- split(df, df$splitvar)
  
  # map to pieces
  pieces_mod <- map(pieces, .f = function(piece){
    print(piece)
    if(nrow(piece) <= 1){
      return(apply_change_r(piece))
    } else{
      if(all(piece$action == "add")||all(piece$action == "fork_ent")||all(piece$action == "repl_ent")){
        piece$text <- piece$new_txt 
        return(piece)
      } else if(all(piece$action %in% c("amend", "repl_txt"))){
        # iteratively 
        r <- apply_change_r(piece[1, ])
        for(i in 2:nrow(piece)){
          piece$text[i] <- r$text
          r <- apply_change_r(piece[i, ])
        }
        return(r)
      } else{
        stop("this case is not covered so far!", call. = TRUE)
      }
    }
  })
  
  retdf <- bind_rows(pieces_mod) %>% 
    select(-splitvar) %>% # remove other unnecessary columns here as well
    # mutate(current_id = if_else(is.na(merger_id), current_id, merger_id)) # current id
    return(bind_rows(pieces_mod) %>% select(-splitvar))
}








