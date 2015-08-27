# hi Korea

library(dplyr)
library(rvest)
library(stringr)

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

dir.create("raw"       , showWarnings = FALSE)
dir.create("raw/bills" , showWarnings = FALSE)
dir.create("raw/mps"   , showWarnings = FALSE)

plot = TRUE
mode = "kamadakawai"

source("functions.r")
source("01-data.r")
source("02-build.r")

save(list = ls(pattern = "(net|edges|bills)_\\d{1,2}"),
     file = "data/net_kr.rda")

# have a nice day
