################################################################################
# Title: fix_tox_order.R

# Description:
# Combines CompTox toxicity data on new pharmaceuticals added to PharmUse and
# CompTox toxicity data on previously included pharmaceuticals that had
# insufficient data entry with original toxicity data from CompTox. Version
# 2.4.1 of CompTox has slightly different formatting than the version used for
# the new or overlooked drugs (version 2.6.0), and this script combines the
# toxicity data across versions in a consistent manner.

################################################################################

library(readxl)
library(dplyr)

# Read in spreadsheets for original CompTox data plus data on pharmaceuticals
# added to PharmUse and that got looked over before
tox <- read_excel("comptox_search_orig.xlsx",sheet=2) # original CompTox output
# new CompTox output for pharmaceuticals added to PharmUse
missed <- read_excel("previously_missed.xlsx",sheet=3) 
# new CompTox output for pharmaceuticals with incomplete data despite thei
# prior inclusion in PharmUse
ertu_dex = read_excel("ertugliflozin_dexlansoprazole.xlsx",sheet=3)

# Get the union of all column names from spreadsheets to select columns of 
# interest from each
all_cols <- Reduce(union, list(names(tox), names(missed), names(ertu_dex)))

# Ensure tox has all columns
tox_aligned <- tox
missing_tox <- setdiff(all_cols, names(tox))
if (length(missing_tox) > 0) {
  tox_aligned[missing_tox] <- NA
}
tox_aligned <- tox_aligned %>% select(all_of(all_cols))

# Ensure missed has all columns
missed_aligned <- missed
missing_missed <- setdiff(all_cols, names(missed))
if (length(missing_missed) > 0) {
  missed_aligned[missing_missed] <- NA
}
missed_aligned <- missed_aligned %>% select(all_of(all_cols))

# Ensure ertu_dex has all columns
ertu_dex_aligned <- ertu_dex
missing_ertu_dex <- setdiff(all_cols, names(ertu_dex))
if (length(missing_ertu_dex) > 0) {
  ertu_dex_aligned[missing_ertu_dex] <- NA
}
ertu_dex_aligned <- ertu_dex_aligned %>% select(all_of(all_cols))

# fix datatypes that are inconsistent
tox_aligned$SEX_ORIGINAL = as.character(tox_aligned$SEX_ORIGINAL)

# Now safe to combine
combined <- bind_rows(tox_aligned, missed_aligned, ertu_dex_aligned)

# fix incorrect names
combined = combined %>%
  mutate(SEARCHED_CHEMICAL = case_when(SEARCHED_CHEMICAL=="5-Aminosalicylic acid" ~ "Mesalamine",
                                       SEARCHED_CHEMICAL=="Penicillin VK" ~ "Penicillin V potassium",
                                       SEARCHED_CHEMICAL=="Isosorbide 5-mononitrate" ~ "Isosorbide mononitrate",
                                       SEARCHED_CHEMICAL=="O-Desmethylvenlafaxine" ~ "Desvenlafaxine",
                                       SEARCHED_CHEMICAL=="Isosorbide 2,5-dinitrate" ~ "Isosorbide dinitrate",
                                       SEARCHED_CHEMICAL=="Glybenclamide" ~ "Glyburide",
                                       SEARCHED_CHEMICAL=="5-Fluorouracil" ~ "Fluorouracil",
                                       SEARCHED_CHEMICAL=="Tacrolimus hydrate" ~ "Tacrolimus",
                                       SEARCHED_CHEMICAL=="PF-04971729" ~ "Ertugliflozin",
                                       .default=SEARCHED_CHEMICAL))