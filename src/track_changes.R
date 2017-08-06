library(readr)
library(rvest)
library(purrr)
library(purrrlyr)
library(stringr)
library(tidyr)
library(dplyr)

rm(list = ls())

# function definition 

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

# I. MERGER
# 1. read in data

test <- read_rds("data/1965.rds")
merger_changes <- read_csv("data/changes_merger.csv")
t57 <- read_rds("data/1957_2.rds")

# 2. Data Manipulation
# prepare changes data frame 
merger_changes <- merger_changes %>% 
  select(treaty, current_id, action, old_txt, new_txt, merger_id = new_id) %>% 
  mutate(new_txt = if_else(is.na(new_txt) & !is.na(old_txt), "", new_txt)) %>% 
  fill(treaty)

# join merger_changes
t65 <- full_join(t57, merger_changes, by = c("treaty", "current_id"))

# 3. apply and save
t65_2 <- apply_changes(t65, "merger")
