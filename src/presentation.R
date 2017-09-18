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
datatable(select(euratom, -treaty), width = 1500,
          options = list(autoWidth = TRUE, pageLength = nrow(euratom),
          columnDefs = list(list(width = '600px', targets = c(6)))))

# Changes example
teu_changes <- teu_changes %>%
  filter(action == "replace_txt_globally")
changes_example <- bind_rows(merger_changes, teu_changes)
datatable(changes_example, width = 1500)

# Final data
datatable(eulaw, width = 1500,
          options = list(autoWidth = TRUE, pageLength = nrow(eulaw)))


#EOF
