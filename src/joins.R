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
eulaw_1997 <- readRDS("data/eulaw_1997.rds")
eulaw_1998 <- readRDS("data/eulaw_1998.rds")
eulaw_2001 <- readRDS("data/eulaw_2001.rds")

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
  mutate(id_1992 = if_else(is.na(id_1992), "repealed", id_1992)) %>%
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
  # Amsterdam
  full_join(eulaw_1997, by = c("id_1992" = "id_1992")) %>%
  mutate(id_1997 = if_else(is.na(id_1997), "repealed", id_1997)) %>%
  select(id_1951,
    id_1957,
    id_1965,
    id_1986,
    id_1992,
    id_1997,
    txt_1951,
    txt_1957,
    txt_1965,
    txt_1986,
    txt_1992,
    txt_1997 = txt) %>%
  # Amsterdam renumbering 
  full_join(eulaw_1998, by = c("id_1997" = "id_1997")) %>%
  mutate(id_1998 = if_else(is.na(id_1998), "repealed", id_1998)) %>%
  select(id_1951,
    id_1957,
    id_1965,
    id_1986,
    id_1992,
    id_1997,
    id_1998,
    txt_1951,
    txt_1957,
    txt_1965,
    txt_1986,
    txt_1992,
    txt_1997,
    txt_1998 = txt) %>%
  # Nizza
  full_join(eulaw_2001, by = c("id_1998" = "id_1998")) %>%
  select(id_1951,
    id_1957,
    id_1965,
    id_1986,
    id_1992,
    id_1997,
    id_1998,
    id_2001,
    txt_1951,
    txt_1957,
    txt_1965,
    txt_1986,
    txt_1992,
    txt_1997,
    txt_1998,
    txt_2001 = txt) %>%
  mutate_at(vars(contains("id")),
            funs(if_else(. == "repealed", NA_character_, .))
  )

saveRDS(eulaw, file = "data/eulaw.rds")
