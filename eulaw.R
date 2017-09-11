# --------------------------------------------------------------------------- #
# Treaties of the European Union
# --------------------------------------------------------------------------- #

# load packages
library(rvest)
library(purrr)
library(purrrlyr)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)

rm(list  = ls())
source("code/utils.R")

# ECSC ---------------------------------------------------------------------- #

source("code/ecsc.R") # 1

# EURATOM (Treaty of Rome) -------------------------------------------------- #

source("code/euratom.R") # 2

# EEC (Treaty of Rome) ------------------------------------------------------ #

source("code/eec.R") # 3

# Merger Treaty ------------------------------------------------------------- #

source("src/merger.R") # 4

# SEA ----------------------------------------------------------------------- #

source("src/sea.R") # 5

# TEU (Maastricht) ---------------------------------------------------------- #

source("src/teu.R") # 6

# Amsterdam Treaty ---------------------------------------------------------- #

source("src/ams.R") # 7

# Treaty of Nice ------------------------------------------------------------ #

source("src/nice.R") # 8

# Treaty of Lisbon ---------------------------------------------------------- #

# source("src/lisbon.R") # 9

# --------------------------------------------------------------------------- #
