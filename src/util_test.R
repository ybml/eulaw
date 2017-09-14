# Test the functions implementing the changes. 

# Libraries --------------------------------------------------------------------
library(tidyverse)

# Workspace preparation --------------------------------------------------------
rm(list = ls())

# Functions and data
source("src/utils.R")

# Examples of each "action" function -------------------------------------------

# repeal 

repeal_example <- filter(merger_changes, action == "repeal")
repeal_out <- repeal(eulaw_1957, repeal_example$change_id)

# repeal_txt

repeal_example <- filter(merger_changes, action == "repeal_txt")

repeal_out <- repeal_txt(data = eulaw_1957,
                         change_id = repeal_example$change_id,
                         change_txt = repeal_example$change_txt)

# replace

replace_example <- merger_changes %>%
  filter(action == "replace")

# Set new_id if non-existing (pre-processing!)
replace_example <- set_new_id(replace_example, "replace", change_id)

# Then replace.
replace_out <- replace(eulaw_1957,
                       replace_example$new_id,
                       replace_example$new_txt)

  
# replace_txt

replace_txt_example <- merger_changes %>%
  filter(action == "replace_txt")

replace_txt_out <- replace_txt(eulaw_1957,
                               id = replace_txt_example$change_id,
                               text = replace_txt_example$change_txt,
                               replacement_txt = replace_txt_example$new_txt)

# insert

insert_example <- filter(merger_changes, action == "insert")

insert_out <- insert(eulaw_1957,
                     id = insert_example$new_id,
                     txt = insert_example$new_txt)

# former add functionality

add_example <- set_new_txt(merger_changes, "add") %>%
  set_action(old_action = "add", new_action = "insert") %>%
  set_new_id(., "insert", id_field = id) %>%
  filter(action == "insert")

# Using insert instead of add.
add_out <- insert(eulaw_1957,
                  id = add_example$new_id,
                  txt = add_example$new_txt)

# insert_txt example

insert_txt_out <- insert_txt(eulaw_1957, "1.1.X.1", "This is the new text.")

# apply_changes: example for merger and sea ---------------------------------- #

# The data.
rm(list = ls())
source("src/utils.R")

eulaw_1957 <- readRDS("data/eulaw_1957.rds")
merger_changes <- read_csv("tables/merger_changes.csv")
sea_changes <- read_csv("tables/sea_changes.csv")

# Pre-processing on the changes files.
merger_changes <- set_new_txt(merger_changes, "add") %>%
  set_action(old_action = "add", new_action = "insert") %>%
  set_new_id(., "insert", id_field = id) %>%
  set_new_id(., "replace", change_id)

eulaw_1967 <- apply_changes(eulaw_1957, merger_changes, "1967")

sea_changes <- set_new_txt(sea_changes, "add") %>%
  mutate(action = if_else(action == "replace_text", "replace_txt", action)) %>%
  set_action(old_action = "add", new_action = "insert") %>%
  set_new_id(., "insert", id_field = id) %>%
  set_new_id(., "replace", change_id)

eulaw_1986 <- apply_changes(eulaw_1967, sea_changes, "1986") %>%
  filter(!is.na(txt)) %>%
  arrange(id_1986)

saveRDS(eulaw_1967, file = "eulaw_1967.rds")
saveRDS(eulaw_1986, file = "eulaw_1986.rds")

# Coding workflow example ----------------------------------------------------- #
# Step 1: apply changes of as given in the <previous treaty>_changes.csv
# Step 2: generate intermediate dataframes containing id, article number an txt
#         of for each treaty separately. These CSV-files are used to look up the
#         ids of the article which are changed and nothing more. Hence they are
#         saved to "tables/tmp/".


# Step 2 example: this generates the files escs_1986, eec_1986.csv, euratom_1986,
#                 merger_1986, sea_1986.
get_founding_treaties(eulaw_1986)

# Step 3: make the changes file by hand.
# Example of lookup_id functions ---------------------------------------------- #
lookup_id_clip(ecsc_1986, id, 4)
lookup_id(ecsc_1986, id, 4) # Frie's function.
# lookup_id(eulaw, 3, 4) # John's function.

teu <- read_csv("tables/teu.csv")
datatable(teu, width = 1500,
          options = list(autoWidth = TRUE, pageLength = nrow(teu),
          columnDefs = list(list(width = '1000px', targets = c(6)))))
