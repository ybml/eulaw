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
library(tidyverse)

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

# get_old_id: get the old id from an eulaw_ data-frame --------------------- #

get_old_id <- function(data) {

  # data: an eulaw_ data-frame.
  
  old_id <- str_extract_all(colnames(data), "\\d{4}") %>%
    unlist() %>%
    as.numeric() %>%
    max() %>%
    paste0("id_", .)

  return(old_id)
  
}

# set_new_id: set "new_id" to "id_field" if "new_id" is NA. ------------------ #
set_new_id  <- function(changes, directive, id_field) {

  # changes: a _changes dataframe.
  # directive: action for which new id has to be added.

  quo_id <- enquo(id_field)
  
  changes <- changes %>%
    mutate(new_id = if_else(action == directive & is.na(new_id),
                            !!quo_id,
                            new_id
                    )
    )

  return(changes)
  
}

# set_new_txt: set the new_txt variable for "directive" if field is NA. ------ #
set_new_txt <- function(changes, directive) {

  # changes: a _changes dataframe.
  # directive: an action.

  changes <- changes %>%
    mutate(new_txt = if_else(
                       action == directive & is.na(new_txt),
                       txt,
                       new_txt
                     )
    )

  return(changes)

}

# set_action: set "old_action" to "new_action" in "changes".
set_action <- function(changes, old_action, new_action) {

  # changes: a _changes dataframe.
  # old_action: the replaced action name.
  # new_action: the replacement action name.
  
  changes <- changes %>%
    mutate(action = if_else(
                      action == old_action,
                      new_action,
                      action
                     )
    )

  return(changes) 

}

# repeal: set id and text of article specified by change_id in data to NA ---- #
repeal <- function(data, change_id) {

  # data: an eulaw_ dataframe.
  # change_id: id of article to repeal.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(!!old_id := if_else(get(old_id) == change_id,
                              NA_character_,
                              get(old_id)
                      ),
           txt = if_else(get(old_id) == change_id, NA_character_, txt)
    )
  
  return(data)

}

# repeal_txt: remove change_txt from article with change_id in data --------- #
repeal_txt <- function(data, change_id, change_txt) {

  # data: data frame consisting of 3 columns the ids of the two predecessing
  #       treaties, and txt, the text that goes along with the id.
  # change_id: the id of the article for which the change occurs.
  # change_txt: the text to be removed.

  old_id <- get_old_id(data)
  
  data <- data %>%
  mutate(txt = if_else(
                 change_id == get(old_id),
                 str_replace(txt, change_txt, replacement = ""),
                 txt
               )
  )

  return(data)

}

# replace: replace the article text of article with id in data by new text --- #
replace <- function(data, id, text) {

  # data: eulaw_ dataframe.
  # id: the id of the article where replacement occurs.
  # text: the replacement text.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(txt = if_else(id == get(old_id), text, txt))

  return(data)
  
}

# replace_txt: replace "text" in article "id" of "data" with "replacement_txt" -#
replace_txt <- function(data, id, text, replacement_txt) {

  # data: an eulaw_ dataframe.
  # id: id of article where text is replaced.
  # text: the text which is replaced.
  # replacement_txt: the replacement text.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(
      txt = if_else(
              get(old_id) == id,
              str_replace(txt, text, replacement_txt),
              txt
            )
    )

  return(data)
  
}

# replace_txt_globally: replace text globally in a treaty with "id". --------- #
replace_txt_globally <- function(data, id, pattern, replacement_txt) {

  # data: an eulaw_ dataframe.
  # id: the id of the treaty where the replacement occurs.
  # pattern: the text to replace.
  # replacement_txt: the new text.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(treaty_number = str_extract(get(old_id), pattern = "^\\d{1}"))

  data <- data %>%
    mutate(txt = if_else(treaty_number == id,
                         str_replace_all(txt, pattern, replacement_txt),
                         txt
                 )
    ) %>%
    select(-treaty_number)
                 
  return(data)

}

# insert: ammend "data" with "id" and "text". -------------------------------- #
insert <- function(data, id, txt) {

  # data: an eulaw_ dataframe.
  # id: the id of the ammended text.
  # text: the new text.

  old_id <- get_old_id(data)
  
  df <- data_frame(!!old_id := id, txt)
  data = bind_rows(data, df)
  
  return(data)

}

# insert_txt: ammend article specified by "change_id" in "data" with "new_txt" #
insert_txt <- function(data, change_id, new_txt) {

  # data: an eulaw_ dataframe.
  # change_id: id of the article to ammend.
  # new_text: the ammended text.

 old_id <- get_old_id(data)
  
  data <- data %>%
    mutate(txt = if_else(get(old_id) == change_id,
                   paste(txt, new_txt, sep = " "),
                   txt
                 )
    )

  return(data)
  
}

# renumber: set id for article identified by "change_id" to "new_id" -------- #
renumber <- function(data, change_id, new_id) {

  # data: an eulaw_ dataframe.
  # change_id: the id of the article to renumber.
  # new_id: the new id of an article.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(!!old_id := if_else(get(old_id) == change_id, new_id, get(old_id)))

  return(data)
  
}

# apply_changes: apply the changes in "changes" to "data" -------------------- #
apply_changes <- function(data, changes, year) {

  # data: an eulaw_ dataframe.
  # changes: a _changes dataframe.
  # year: the year postfix of the new id variable.

  # Step 1: construct the new data frame.

  current_id <- str_extract_all(colnames(data), "\\d{4}") %>%
    unlist() %>%
    as.numeric() %>%
    max() %>%
    paste0("id_", .)
    
  new_id <- paste0("id_", as.character(year))

  df <- data %>%
    mutate(!!new_id := get(current_id)) %>%
    select(current_id, new_id, txt)

  # Step 2: apply the changes.
  for (i in 1:nrow(changes)) {

    change <- changes[i, ] 
    action <- change$action 

    if (action == "insert") {
      df <- insert(df, id = change$new_id, txt = change$new_txt)
    } else if (action == "insert_txt") {
      df <- insert_txt(df,
                       change_id = change$change_id,
                       new_txt = change$new_txt
            )
    } else if (action == "renumber") {
      df <- renumber(df, change_id = change$change_id, new_id = change$new_id)
    } else if (action == "repeal") {
      df <- repeal(df, change$change_id)
    } else if (action == "repeal_txt") {
      df <- repeal_txt(df,
                       change_id = change$change_id,
                       change_txt = change$change_txt
            )
    } else if (action == "replace") {
      df <- replace(df, id = change$new_id, text = change$new_txt)
    } else if (action == "replace_txt") {
      df <- replace_txt(df,
                    id = change$change_id,
                    text = change$change_txt,
                    replacement_txt = change$new_txt
        )
    } else {
      print_txt <- paste("Catched action:", action)
      print(print_txt)
    }
    
  }

  return(df)

}

# get_founding_treaties: split up an eulaw_ data frame in the founding treaties - #
get_founding_treaties <- function(data) {

  # data: an eulaw_ dataframe.

  save_treaty <- function(i) {
    treaty <- dl[[i]] %>%
      select(!!"id" := old_id, "article", "txt")
      write_csv(treaty, path = file_names[i])
  }

  old_id <- get_old_id(data)

  id_split <- pull(data, old_id) %>%
    str_split(., pattern = "\\.")

  d <- data %>%
    mutate(treaty_nr = lapply(id_split, first) %>%
             unlist(),
           article = lapply(id_split, last) %>%
             unlist(),
           treaty_name = case_when(
                           treaty_nr == 1 ~ "ecsc",
                           treaty_nr == 2 ~ "euratom",
                           treaty_nr == 3 ~ "eec",
                           treaty_nr == 4 ~ "merger",
                           treaty_nr == 5 ~ "sea",
                           treaty_nr == 6 ~ "teu",
                           treaty_nr == 7 ~ "ams",
                           treaty_nr == 8 ~ "nice",
                           treaty_nr == 9 ~ "lisbon"
                         )
    )
             
  dl <- d %>%
    split(., .$treaty_name)
  file_names <- paste0("tables/tmp/",
                       names(dl),
                       "_",
                       str_extract(old_id, "\\d{4}"),
                       ".csv"
                )
  lapply(seq_along(dl), save_treaty)

}



## # lookup_id --------------------------------------------------------------------
## # insert, eulaw, treaty number, article number 
## lookup_id = function(data, treaty, article) {
  
##   options(warn=-1)
  
##   # select id
##   data$id = data$id_1957
##   names(data)[names(data)=="id_1957"] = "old_id"
  
##   # split id into treaty, .., and article
##   data = data %>% 
##     separate(id, sep = "\\.", 
##              into = c("treaty", 
##                       "X1", "X2", "X3", "X4", "X5", "X6", "X7", 
##                       "art"))
##   # add art if NA
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X7, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X6, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X5, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X4, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X3, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X2, art))
##   data = data %>% 
##     mutate(art = if_else(is.na(art), X1, art))
  
##   id = data$old_id[data$treaty == treaty & data$art == article]
  
##   return(id)
## }

# lookup id: look up the id specified in "idvar" of "article" in "treaty_df" - #
lookup_id <- function(treaty_df, idvar, article){

  # treaty_df: a data frame containing id, article numbers and text. 
  # idvar: bare name of the id variable.
  # article: article number as character to look up.

 if(article == "None"){
   return("None")
 }

 treaty_q <- enquo(treaty_df)
 treaty_qn <- quo_name(treaty_q)

 if(!exists(treaty_qn)){
   # read in csv (without printing column specifications)
   suppressMessages(
     treaty_df <- read_csv(paste0("tables/tmp/", treaty_qn, ".csv"))
   )
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

# lookup_id_clip: additionally copies the id to the clipboard ---------------- #
lookup_id_clip <- function(treaty_df, idvar, article) {

  # treaty_df: a data frame containing id, article numbers and text. 
  # idvar: bare name of the id variable.
  # article: article number as character to look up.

  if (!require(clipr)){
    stop_message <- paste("This version only works with clipr installed.",
                          "Please use lookup_id instead.")
    stop(stop_message)
  }

  if(!clipr_available()){
    stop_message <- paste("Your clipboard is not available.",
                          "Check system dependencies in the CRAN documentation.")
    stop(stop_message)
  }
  
  treaty_q <- enquo(treaty_df)
  treaty_qn <- quo_name(treaty_q)
  
  if(!exists(treaty_qn)){
    # read in csv (without printing column specifications)
    suppressMessages(
      treaty_df <- read_csv(paste0("tables/tmp/", treaty_qn, ".csv"))
    )
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

# EOF ------------------------------------------------------------------------- #
## # apply_changes: ------------------------------------------------------------ #

## apply_changes <- function(df, which_treaty_id){
  
##   wt_id_q <- enquo(which_treaty_id)
##   wt_id_name <- quo_name(wt_id_q)

##   apply_change_r <- function(row, ...){
##     if(is.na(row$action)){ # NA -> no action required
##       return(row)
##     } else if(row$action == "repeal"){ # delete text and treaty_id <- NULL
##       row$text = ""
##       row[, wt_id_name] <- "NULL"
##     } else if(row$action == "repl_txt"){ # replace text
##       row$text <- str_replace_all(row$text, row$old_txt, row$new_txt)
##     } else if(row$action %in% c("fork_ent", "add", "repl_ent")){ # copy new_txt to text
##       row$text <- row$new_txt
##     } else if(row$action == "amend"){ # add new text at end of text
##       row$text <- paste(row$text, row$new_txt, sep = " ")
##     }
##     return(row)
##   }
  
##   # split data frame into list of data frames based on current_id 
##   # for most entities there is only 1 change, so that its data frame will only have 1 row
##   df$splitvar <- paste(df$treaty, df$current_id, sep = "-")
##   pieces <- split(df, df$splitvar)
  
##   # for every current_id piece, apply the changes
##   pieces_mod <- map(pieces, .f = function(piece){
##     # print(piece) # for DEBUG
    
##      if(nrow(piece) <= 1){
 
##       # ONLY ONE CHANGE 
##       # it's trivial - just apply it.
##       return(apply_change_r(piece))
      
##     } else{
      
##       # MORE THAN ONE CHANGE
##       # if there is more than one change, it's a bit more tricky.

      
##       if(all(is.na(piece$action))){
        
##         # all are NA -> no action required. just return
##         return(piece)
        
##       } else if(all(piece$action == "add")||all(piece$action == "fork_ent")||all(piece$action == "repl_ent")){
        
##         # we can have changes, that are independent of each other, e.g. adding a new
##         # article, forking an existing article etc. For all those, the where_id
##         # will be the same (e.g. None for add), but the insertions do not depend on 
##         # each other, hence, we can vectorize this. 
        
##         piece$text <- piece$new_txt 
##         return(piece)
        
##       } else if(all(piece$action %in% c("amend", "repl_txt"))){
        
##         # on the other hand we can have changes that build on each other, e.g.
##         # multiple replace text or amend operations. Those have to be executed
##         # after one another, taking the modified text as input for the next one.

##         r <- apply_change_r(piece[1, ]) # first change
        
##         for(i in 2:nrow(piece)){
##           piece$text[i] <- r$text # take text of previous change 
##           r <- apply_change_r(piece[i, ]) # execute next change
##         }
##         return(r)
        
##       } else{
        
##         # if we have combinations of actions within one piece that are not covered
##         # so far, we throw an error and revisit this function 
##         stop("this case is not covered so far!", call. = TRUE)
        
##       }
##     }
##   })
  
##   # we bind the modified pieces back together to a data frame and do some 
##   # post - processing 
##   retdf <- bind_rows(pieces_mod) %>% # bind together 
##     select(treaty, ends_with("id"), text) %>% # remove unnecessary columns 
##     mutate(current_id = if_else(is.na(!!wt_id_q), current_id, !!wt_id_q)) # current id
  
##   return(retdf)
## }

## # apply_global_changes: --------------------------------------------------------
## # function that applies global changes
## apply_global_changes <- function(change_df, df){
  
## }

## # dl_ws: download sheets within a google sheet to data/ ------------------------
## # sheet_name is the name of the treaty and the google sheet, e.g. merger or sea
## dl_ws <- function(sheet_name){
##   require(googlesheets)
##   require(dplyr)
##   require(purrr)
##   s <- gs_title(sheet_name) # get access to google ws
##   map(gs_ws_ls(s), function(ws_name) {
##     s %>% 
##       gs_download(ws = ws_name, to = paste0("data/", sheet_name, "_", ws_name, ".csv"),
##                   overwrite = TRUE)
##   })
## }
