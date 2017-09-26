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
  ) %>%
  mutate(id_1997 = id_1998, txt_1997 = txt_1998) %>%
  select(-contains("1998"))

# eulaw long.
years <- c(1951, 1957, 1965, 1986, 1992, 1997, 1998, 2001)
eulaw_long <- lapply(years,
                     function(y) {

                       eulaw_n <- paste0("eulaw_", y)
                       eulaw_q <- quo(!!sym(eulaw_n))
                       id_n <- paste0("id_", y)
                       id_q <- quo(!!sym(id_n))
              
                       eval_tidy(eulaw_q) %>%
                         mutate(year = y) %>%
                         select(id = !!id_n, year, txt)
                     }
              ) %>%
  bind_rows() %>%
  filter(!is.na(id))

# Remove artificial year
eulaw_long <- eulaw_long %>%
  filter(year != 1997) %>%
  mutate(year = if_else(year == 1998, 1997, year))


# Add treaty name to long format.
eulaw_long <- eulaw_long %>%
  mutate(
    treaty_nr = str_extract(id, "^\\d"),
    treaty = case_when(
                treaty_nr == 1 ~ "ecsc",
                treaty_nr == 2 ~ "euratom",
                treaty_nr == 3 ~ "eec",
                treaty_nr == 4 ~ "merger",
                treaty_nr == 5 ~ "sea",
                treaty_nr == 6 ~ "teu",
                treaty_nr == 7 ~ "amsterdam",
                treaty_nr == 8 ~ "nice",
                treaty_nr == 9 ~ "lisbon"
             )
  ) %>%
  select(id, treaty, year, txt)
  
saveRDS(eulaw, file = "data/eulaw.rds")
saveRDS(eulaw_long, file = "data/eulaw_long.rds")
