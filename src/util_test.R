# Test the functions implementing the changes. 

# Libraries --------------------------------------------------------------------
library(tidyverse)

# Workspace preparation --------------------------------------------------------
rm(list = ls())

# Functions and data
source("src/utils.R")
load("tables/mwe.Rdata")


eulaw = readRDS("data/eulaw_1957.rds")

data = eulaw
changes = merger_changes



apply_changes = function(data, changes) {
  
  data$id = data$id_1957
  data$old_id = data$id_1957
 
  add_article(changes$id[i], changes$txt[i])
  
}

# insert changes$id[i], changes$txt[i]
add_article = function(data, id, txt) {
  
  df = data.frame(id, txt)
  data = bind_rows(data, df)
  
  return(data)
  
}

data = add_article(data, changes$id[1], changes$txt[1])

# insert changes$change_id[i]
repeal = function(change_id) {
  
  data$id[data$old_id == change_id] = NA
  data$txt[data$old_id == change_id] = NA
  
  return(data)
}

# insert changes$change_id[i], changes$change_txt[i]
repeal_txt = function(change_id, change_txt) {
  
  data$txt[data$old_id == change_id] = gsub(change_txt, "", data$txt[data$old_id == change_id])

  return(data)
}

repeal_txt(changes$change_id[5], changes$change_txt[5])

