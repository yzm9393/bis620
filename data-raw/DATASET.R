## code to prepare `DATASET` dataset goes here
library(haven)
library(dplyr)

pm_ae <- read_sas("NCT00339183/ae.sas7bdat") %>%
  select(SUBJID, AESTDYI, AESOC) %>%
  mutate(
    AESTDYI = sample(AESTDYI),
    AESOC = sample(AESOC)
  )

usethis::use_data(pm_ae, overwrite = TRUE)

Demo <- read_sas("data-raw/demo.sas7bdat")
Respeval <- read_sas("data-raw/respeval.sas7bdat")
Ae <- read_sas("data-raw/ae.sas7bdat")

usethis::use_data(Demo, overwrite = TRUE)
usethis::use_data(Respeval, overwrite = TRUE)
usethis::use_data(Ae, overwrite = TRUE)
