# Join the eulaw_ data frames.

# Workspace preparation ------------------------------------------------------ #
rm(list = ls())
source("src/utils.R")

# The data ------------------------------------------------------------------- #
eulaw_1951 <- readRDS("data/eulaw_1951.rds")
eulaw_1957 <- readRDS("data/eulaw_1957.rds")
eulaw_1965 <- readRDS("data/eulaw_1965.rds")
eulaw_1986 <- readRDS("data/eulaw_1986.rds")
eulaw_1992 <- readRDS("data/eulaw_1992.rds")


# Join it.
eulaw <- full_join(eulaw_1951, eulaw_1957, by = c("id_1951" = "id_1951")) %>%
  select(id_1951, id_1957, txt_1951 = txt.x, txt_1957 = txt.y) %>%
  full_join(eulaw_1965, by = c("id_1957" = "id_1957")) %>%
  mutate(id_1965 = if_else(is.na(id_1965), "repealed", id_1965)) %>%
  select(id_1951, id_1957, id_1965, txt_1951, txt_1957, txt_1965 = txt) %>%
  full_join(eulaw_1986, by = c("id_1965" = "id_1965")) %>%
  mutate(id_1986 = if_else(is.na(id_1986), "repealed", id_1986)) %>%
  select(id_1951,
    id_1957,
    id_1965,
    id_1986,
    txt_1951,
    txt_1957,
    txt_1965,
    txt_1986 = txt) %>%
  full_join(eulaw_1992, by = c("id_1986" = "id_1986")) %>%
  select(id_1951,
    id_1957,
    id_1965,
    id_1986,
    id_1992,
    txt_1951,
    txt_1957,
    txt_1965,
    txt_1986,
    txt_1992 = txt) %>%
  mutate_at(vars(contains("id")),
            funs(if_else(. == "repealed", NA_character_, .))
  )

saveRDS(eulaw, file = "data/eulaw.rds")
