# Examples for the presentation.

# Libraries
library(DT)
library(tidyverse)

# Data
load("tables/mwe.Rdata")
teu_changes <- read_csv("tables/teu_changes.csv")
eulaw <- readRDS("data/eulaw.rds")
euratom <- read_csv("tables/euratom.csv")

# Treaty Example
# Euratom has title, chapter, section, article.
datatable(euratom)

# Changes example
teu_changes <- teu_changes %>%
  filter(action == "replace_txt_globally")
changes_example <- bind_rows(merger_changes, teu_changes)
datatable(changes_example)

# Final data
datatable(eulaw)

# Visualization example

#EOF
