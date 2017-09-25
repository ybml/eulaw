# --------------------------------------------------------------------------- #
# Treaty of Nice
# --------------------------------------------------------------------------- #

# Clean the workspace
rm(list = ls())
source("src/utils.R")

# Get html.
h <- read_html("https://en.wikisource.org/wiki/Treaty_of_Nice")
xp <- "//dl//dd/dl//dd|//p|//dl//dd[not(dl)]|//h3//span[@class='mw-headline']"

# Extract source
text <- h %>%
  html_nodes(xpath = xp) %>%
  html_text()

# Clean extracted text ------------------------------------------------------ #
# Remove the part "FINAL ACT"
text <- tibble(text,
              helper = if_else(str_detect(text, "The CONF"), 1, NA_real_)
       ) %>%
  fill(helper) %>%
  filter(is.na(helper)) %>%
  pull(text)

# Remove the note that the signatures were ommitted.
text <- text[!str_detect(text, "\\[")]

# Replace all linefeeds with a space.
text <- str_replace_all(text, pattern = "\\n", " ")

# Tibble one row one article, cols article number and text ------------------ #

ptrn = "(^Article \\d?:|^Article \\d{1,2}$)"
nice <- data_frame(
          art_ind = str_detect(text, ptrn),
          txt = text
        )

# Generate article number and remove article titles.
nice <-  nice %>%
  mutate(
    art_nr = if_else(art_ind,
                     str_extract(txt, "(?<=Article )\\d{1,2}"),
                     NA_character_
             ),
    
    ) %>%
  fill(art_nr) %>%
  filter(!art_ind & !is.na(art_nr))

# One row one article
nice <- nice %>%
  group_by(art_nr) %>%
  summarise(art_txt = paste0(txt, collapse = " ")) %>%
  ungroup() %>%
  select(art_nr, art_txt) %>%
  mutate(art_nr = as.numeric(art_nr)) %>%
  arrange(art_nr)


# Generate id --------------------------------------------------------------- #
nice <- nice %>%
  mutate(part_nr = if_else(art_nr < 7, 1, 2),
         id = paste("8", part_nr, art_nr, sep = ".")
  ) %>%
  select(id, part_nr, art_nr, txt = art_txt)

write_csv(nice, path = "tables/nice.csv")
#EOF
