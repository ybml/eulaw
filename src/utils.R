# ---------------------------------------------------------------------------- #
# useful functions
# ---------------------------------------------------------------------------- #

# load packages
require(rvest)
require(purrr)
require(purrrlyr)
require(stringr)
require(tidyr)
library(tidyverse)

# get_toc: get table of content from wikisource ------------------------------ #

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

# trim: trim whitespace ------------------------------------------------------ #

trim = function (x) gsub("^\\s+|\\s+$", "", x)

# get_old_id: get the old id from an eulaw_ data-frame ----------------------- #

get_old_id <- function(data) {

  # data: an eulaw_ data-frame.
  
  old_id <- str_extract_all(colnames(data), "\\d{4}") %>%
    unlist() %>%
    as.numeric() %>%
    min() %>%
    paste0("id_", .)

  return(old_id)
  
}

# get_new_id: get the new id from an eulaw_ data-frame ----------------------- #

get_new_id <- function(data) {

  # data: an eulaw_ data-frame.
  
  new_id <- str_extract_all(colnames(data), "\\d{4}") %>%
    unlist() %>%
    as.numeric() %>%
    max() %>%
    paste0("id_", .)

  return(new_id)
  
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

# set_action: set "old_action" to "new_action" in "changes". ---------------- #
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
  new_id <- get_new_id(data)

  data <- data %>%
    mutate(
      !!new_id := if_else(
                    get(old_id) == change_id & !is.na(get(old_id)),
                    NA_character_,
                    get(new_id)
                  ),
      txt = if_else(
              get(old_id) == change_id & !is.na(get(old_id)),
              NA_character_,
              txt
            )
    )
  
  return(data)

}

# repeal_txt: remove change_txt from article with change_id in data ---------- #
repeal_txt <- function(data, change_id, change_txt) {

  # data: eulaw_ dataframe.
  # change_id: the id of the article where text is repealed. 
  # change_txt: the text to be repealed.

  old_id <- get_old_id(data)

  # Check whether the change_txt exists in txt.
  chk_txt <- filter(data, change_id == get(old_id) & !is.na(get(old_id))) %>%
    pull(txt)
  
  if(!all(str_detect(chk_txt, change_txt))) {
    warning(
      "Text \"",
      change_txt,
      "\" not discovered in ",
      change_id,
      " --- text not repealed."
    )
  } else {
      data <- data %>%
        mutate(
          txt = if_else(
                  change_id == get(old_id) & !is.na(get(old_id)),
                  str_replace(txt, change_txt, replacement = ""),
                  txt
                )
        )
  }

  return(data)

}

# replace: replace the article text of article with id in data by new text --- #
replace <- function(data, id, text) {

  # data: eulaw_ dataframe.
  # id: the id of the article where replacement occurs.
  # text: the replacement text.

  old_id <- get_old_id(data)

  data <- data %>%
    mutate(
      txt = if_else(id == get(old_id) & !is.na(get(old_id)),
                    text,
                    txt
            )
    )

  return(data)
  
}

# replace_txt: replace "text" in article "id" of "data" with "replacement_txt" -#
replace_txt <- function(data, id, text, replacement_txt) {

  # data: an eulaw_ dataframe.
  # id: id of article where text is replaced.
  # text: the text which is replaced.
  # replacement_txt: the replacement text.

  old_id <- get_old_id(data)

  # Check whether the change_txt exists in txt.
  chk_txt <- filter(data, id == get(old_id) & !is.na(get(old_id))) %>%
    pull(txt)
  
  if(!str_detect(chk_txt, text)) {
    warning(
      "Text \"",
      text,
      "\" not discovered in ",
      id,
      " --- no replacement made."
    )
  } else {
    data <- data %>%
      mutate(
        txt = if_else(
                get(old_id) == id & !is.na(get(old_id)),
                str_replace(txt, text, replacement_txt),
                txt
              )
      )
  }

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
  
  # Check whether pattern exists in txt.
  chk_txt <- filter(data, treaty_number == id & !is.na(treaty_number)) %>%
    pull(txt)

  if(!any(str_detect(chk_txt, regex(pattern, ignore_case = TRUE)))) {
    warning(
      "Text \"",
      pattern,
      "\" not discovered in treaty number ",
      id,
      " --- no global replacement possible."
    )
  } else {
    data <- data %>%
      mutate(
        txt = if_else(treaty_number == id & !is.na(treaty_number),
                      str_replace_all(
                                      txt,
                                      regex(pattern, ignore_case = TRUE),
                                      replacement_txt
                      ),
                      txt
              )
      ) %>%
      select(-treaty_number)
  }

  return(data)

}

# insert: ammend "data" with "id" and "text". -------------------------------- #
insert <- function(data, id, txt) {

  # data: an eulaw_ dataframe.
  # id: the id of the ammended text.
  # text: the new text.

  new_id <- get_new_id(data)
  
  df <- data_frame(!!new_id := id, txt)
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
    mutate(
      txt = if_else(
              get(old_id) == change_id & !is.na(get(old_id)),
              paste(txt, new_txt, sep = " "),
              txt
            )
    )

  return(data)
  
}

# renumber: set id for article identified by "change_id" to "new_id" --------- #
renumber <- function(data, change_id, new_id) {

  # data: an eulaw_ dataframe.
  # change_id: the id of the article to renumber.
  # new_id: the new id of an article.

  old_id <- get_old_id(data)
  renumbered_id <- get_new_id(data)

  data <- data %>%
    mutate(
      !!renumbered_id := if_else(
                    get(old_id) == change_id & !is.na(get(old_id)),
                    new_id,
                    get(renumbered_id)
        )
    )

  return(data)
  
}

# check whether the correct fields are set in changes ----------------------- #
sanity_checks <- function(data, changes) {

  # data: an eulaw_ dataframe.
  # changes: a changes dataframe.

  new_id <- get_new_id(data)
  w_row_number <- c()

  # Walk over the changes and check.
  for (i in 1:nrow(changes)) {
    
    change <- changes[i, ] 
    action <- change$action 

    # Check whether the supplied id exists.
    if (!(change$change_id %in% pull(data, new_id)) &
        change$action != "replace_txt_globally" &
        !is.na(change$change_id)) {
      warning("Invalid change_id supplied in row ", i)
      w_row_number <- c(w_row_number, i)
    }

    # Check the fields for all actions. 
    if (action == "insert") {
      if (is.na(change$new_id) | is.na(change$new_txt)) {
        warning("new_id or new_txt not set. Check row ", i, " in changes file.")
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "insert_txt") {
      if (is.na(change$change_id) | is.na(change$new_txt)) {
        warning("change_id or new_txt not set. Check row ", i, " in changes file.")
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "renumber") {
      if (is.na(change$change_id) | is.na(change$new_id)) {
        warning("change_id or new_id not set. Check row ", i, " in changes file.")
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "repeal") {
      if (is.na(change$change_id)) {
        warning("change_id not set. Check row ", i, " in changes file.")
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "repeal_txt") {
      if (is.na(change$change_id) | is.na(change$change_txt)) {
        warning("change_id or change_txt not set. Check row ",
             i,
             " in changes file."
        )
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "replace") {
      if (is.na(change$change_id) | is.na(change$new_txt)) {
        warning("change_id or new_txt not set. Check row ",
             i,
             " in changes file."
        ) 
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "replace_txt") {
      if (is.na(change$change_id) | is.na(change$change_txt) |
          is.na(change$new_txt)) {
        warning("change_id, change_txt, or new_txt not set. Check row ",
             i,
             " in changes file."
        ) 
      w_row_number <- c(w_row_number, i)
      }
    } else if (action == "replace_txt_globally") {
      if (is.na(change$change_id) | is.na(change$change_txt) |
          is.na(change$new_txt)) {
        warning("change_id, change_txt, or new_txt not set. Check row ",
             i,
             " in changes file."
        ) 
      w_row_number <- c(w_row_number, i)
      }
    }  
  }

  return(w_row_number)
}

# apply_changes: apply the changes in "changes" to "data" -------------------- #
apply_changes <- function(data, changes, year) {

  # data: an eulaw_ dataframe.
  # changes: a _changes dataframe.
  # year: the year postfix of the new id variable.
  
  # Step 1: construct the new data frame.
  current_id <- get_new_id(data)    
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
     df <- replace(df, id = change$change_id, text = change$new_txt)
    } else if (action == "replace_txt") {
      df <- replace_txt(df,
                    id = change$change_id,
                    text = change$change_txt,
                    replacement_txt = change$new_txt
            )
    } else if (action == "replace_txt_globally") {
      df <- replace_txt_globally(df,
                                 id  = change$change_id,
                                 pattern = change$change_txt,
                                 replacement = change$new_txt
            )
    } else {
      print_txt <- paste("Catched action:", action)
      print(print_txt)
    }
    
  }

  return(df)

}

# get_founding_treaties: split up an eulaw_ data frame in the founding treaties #
get_founding_treaties <- function(data) {

  # data: an eulaw_ dataframe.

  save_treaty <- function(i) {
    treaty <- dl[[i]] %>%
      select(!!"id" := new_id, "article", "txt")
      write_csv(treaty, path = file_names[i])
  }

  new_id <- get_new_id(data)

  id_split <- pull(data, new_id) %>%
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
                       str_extract(new_id, "\\d{4}"),
                       ".csv"
                )
  lapply(seq_along(dl), save_treaty)

}

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
#EOF
