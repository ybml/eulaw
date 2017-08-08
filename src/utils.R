# --------------------------------------------------------------------------- #
# useful functions
# --------------------------------------------------------------------------- #

# load packages
require(rvest)
require(purrr)
require(purrrlyr)
require(stringr)
require(tidyr)
require(dplyr)
require(readr)


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

apply_changes <- function(df, which_treaty_id){
  
  wt_id_q <- enquo(which_treaty_id)
  wt_id_name <- quo_name(wt_id_q)

  apply_change_r <- function(row, ...){
    if(is.na(row$action)){ # NA -> no action required
      return(row)
    } else if(row$action == "repeal"){ # delete text and treaty_id <- NULL
      row$text = ""
      row[, wt_id_name] <- "NULL"
    } else if(row$action == "repl_txt"){ # replace text
      row$text <- str_replace_all(row$text, row$old_txt, row$new_txt)
    } else if(row$action %in% c("fork_ent", "add", "repl_ent")){ # copy new_txt to text
      row$text <- row$new_txt
    } else if(row$action == "amend"){ # add new text at end of text
      row$text <- paste(row$text, row$new_txt, sep = " ")
    }
    return(row)
  }
  
  # split data frame into list of data frames based on current_id 
  # for most entities there is only 1 change, so that its data frame will only have 1 row
  df$splitvar <- paste(df$treaty, df$current_id, sep = "-")
  pieces <- split(df, df$splitvar)
  
  # for every current_id piece, apply the changes
  pieces_mod <- map(pieces, .f = function(piece){
    # print(piece) # for DEBUG
    
     if(nrow(piece) <= 1){
 
      # ONLY ONE CHANGE 
      # it's trivial - just apply it.
      return(apply_change_r(piece))
      
    } else{
      
      # MORE THAN ONE CHANGE
      # if there is more than one change, it's a bit more tricky.

      if(all(piece$action == "add")||all(piece$action == "fork_ent")||all(piece$action == "repl_ent")){
        
        # we can have changes, that are independent of each other, e.g. adding a new
        # article, forking an existing article etc. For all those, the where_id
        # will be the same (e.g. None for add), but the insertions do not depend on 
        # each other, hence, we can vectorize this. 
        
        piece$text <- piece$new_txt 
        return(piece)
        
      } else if(all(piece$action %in% c("amend", "repl_txt"))){
        
        # on the other hand we can have changes that build on each other, e.g.
        # multiple replace text or amend operations. Those have to be executed
        # after one another, taking the modified text as input for the next one.

        r <- apply_change_r(piece[1, ]) # first change
        
        for(i in 2:nrow(piece)){
          piece$text[i] <- r$text # take text of previous change 
          r <- apply_change_r(piece[i, ]) # execute next change
        }
        return(r)
        
      } else{
        
        # if we have combinations of actions within one piece that are not covered
        # so far, we throw an error and revisit this function 
        stop("this case is not covered so far!", call. = TRUE)
        
      }
    }
  })
  
  # we bind the modified pieces back together to a data frame and do some 
  # post - processing 
  retdf <- bind_rows(pieces_mod) %>% # bind together 
    select(treaty, ends_with("id"), text) %>% # remove unnecessary columns 
    mutate(current_id = if_else(is.na(!!wt_id_q), current_id, !!wt_id_q)) # current id
  
  return(retdf)
}

# lookup id --------------------------------------------------------------------
# treaty_df: a data frame that contains the parsed treaty
# idvar: name of id variable, e.g. merger_id, specified not as a string
lookup_id <- function(treaty_df, idvar, article){
  if(article == "None"){
    return("None")
  }
  treaty_q <- enquo(treaty_df)
  treaty_qn <- quo_name(treaty_q)
  
  if(!exists(treaty_qn)){
    # read in csv
    treaty_df <- read_csv(paste0("data/", treaty_qn, ".csv"))
    treaty_df <- treaty_df %>% 
      mutate_all(.funs = funs(as.character))
  }
  
  id_q <- enquo(idvar)
  regex <- paste0(".+?\\.", article, "$")
  id <- treaty_df %>% 
    filter(str_detect(!!id_q, regex) == TRUE) %>% 
    pull(!!id_q)
  return(id)
  
}

# apply_global_changes: --------------------------------------------------------
# function that applies global changes
apply_global_changes <- function(change_df, df){
  
}


# dl_ws: download sheets within a google sheet to data/ ------------------------
# sheet_name is the name of the treaty and the google sheet, e.g. merger or sea
dl_ws <- function(sheet_name){
  require(googlesheets)
  require(dplyr)
  require(purrr)
  s <- gs_title(sheet_name) # get access to google ws
  map(gs_ws_ls(s), function(ws_name) {
    s %>% 
      gs_download(ws = ws_name, to = paste0("data/", sheet_name, "_", ws_name, ".csv"),
                  overwrite = TRUE)
  })
}




# lookup_id_clip: same as lookup_id but with clipping functionality ------------------------------
# lookup id --------------------------------------------------------------------
# treaty_df: a data frame that contains the parsed treaty
# idvar: name of id variable, e.g. merger_id, specified not as a string
lookup_id_clip <- function(treaty_df, idvar, article){
  if (!require(clipr)){
    stop("this version only works with clipr installed. Please use lookup_id instead.")
  }

  if(!clipr_available()){
    stop("your clipboard is not available. Check system dependencies in the CRAN documentation.")
  }
  
  treaty_q <- enquo(treaty_df)
  treaty_qn <- quo_name(treaty_q)
  
  if(!exists(treaty_qn)){
    # read in csv
    treaty_df <- read_csv(paste0("data/", treaty_qn, ".csv"))
    treaty_df <- treaty_df %>% 
      mutate_all(.funs = funs(as.character))
  }
  
  id_q <- enquo(idvar)
  regex <- paste0(".+?\\.", article, "$")
  id <- treaty_df %>% 
    filter(str_detect(!!id_q, regex) == TRUE) %>% 
    pull(!!id_q)
  write_clip(id)
  return(id)
}
