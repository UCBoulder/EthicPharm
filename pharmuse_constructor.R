################################################################################
# Title: pharmuse_constructor.R

# Description:
# Loads, cleans and exports data to construct the PharmUse database. Data
# sources include Medical Expenditure Panel Survey 2020 Prescribed Medicines
# File and CompTox Chemicals Dashboard.

# Functions used: 
#     * remove_drugs():
#       * Input: the full list of drugs and the list of drugs that need to be
#                removed
#       * Output: the truncated drug list

#     * fix_drug_coding():
#       * Input: data frame, column, column name, incorrect column entry, and
#                correct column entry
#       * Output: data frame with correct entry in row/column of interest

#     * remove_entries(): 
#       * Input: data frame, column, incorrect column entry
#       * Output: data frame with row with incorrect entry removed

#     * replace_na_with_missing_drug():
#       * Input: data frame with NA values, list of missing drug names
#       * Output:data frame with missing drugs appended to original data frame

#     * calculate_median():
#       * Input: dataframe for a particular physicochemical property from
#                CompTox
#       * Output: dataframe with median calculated for property (no return
#                 statement b/c of deployment with lapply)

#     * replace_na_with_missing_dtxsid():
#       * Input: data frame with NA values for DTXSID, list of DTXSIDs without
#                data available for given property in CompTox
#       * Output: data frame with NA replaced by DTXSID

#     * join_with_common_df():
#       * Input: dataframe in properties list with DTXSID only, dataframe from
#                ID/molecular formula/molar mass sheet with both DTXSID and
#                pharmaceutical name
#       * Output: dataframe that joins df and common_df (no return statement
#                 needed because used in map function)

#     * add_rows():
#       * Input: dataframe where rows are being added, name of column where
#                certain values are being added, values being added to column in
#                new rows
#       * Output: dataframe with new rows added

#     * rename_column():
#       * Input: dataframe where column name needs to be changed name(s) to
#                change column name to original column name(s)
#       * Output: dataframe with column name(s) changed

#     * reorder_columns():
#       * Input: dataframe where columns need reordering, list of column names
#                in order of how they should be changed
#       * Output: dataframe with columns reordered

################################################################################

# import libraries
library('dplyr')
library("readxl")
library("stringr")
library("tidyr")
library("purrr")

# load functions library
source("pharmuse_constructor_functions.R")

################################################################################
# Load and clean Medical Expenditure Panel Survey (MEPS) 2020 data.
################################################################################

# load MEPS dataset (all columns read in as characters)
meps = read.delim("MEPS_data_2020.txt", header=TRUE, sep = "\t",
                  colClasses = "character", dec = ".")

# extract desired data columns
meps = meps %>%
  select(c(DUPERSID, RXDRGNAM:RXDAYSUP))

# remove RXFRMUNT (not used)
meps = subset(meps, select = -c(RXFRMUNT))

# update to clearer column names
colnames(meps) = c("Person_ID", "Drug", "NDC", "Quantity", "Form", "Strength",
                   "Units", "Day_Supply")

# replace missing values (-15, -8, and -7 in original data) with NA
meps = meps %>%
  mutate(across(colnames(meps), ~ case_when(. =="-15" ~ NA,
                                            . =="-8" ~ NA,
                                            . == "-7" ~ NA,
                                            .=="-14" ~ NA,
                                            .=="-1" ~ NA,
                                            .default = .)))

# remove whitespace around entries
meps = meps %>%
  mutate(across(everything(), ~trimws(.)))

# remove ophthalmic, otic, and nasal labels from drug names (this prevents
# different forms of the same drug from being counted twice)
meps$Drug = gsub(paste0("\\b", "OPHTHALMIC", "\\b"), "", meps$Drug,
                 ignore.case = TRUE)
meps$Drug = gsub(paste0("\\b", "OTIC", "\\b"), "", meps$Drug,
                 ignore.case = TRUE)
meps$Drug = gsub(paste0("\\b", "NASAL", "\\b"), "", meps$Drug,
                 ignore.case = TRUE)

# group and remove topical drug forms
topical_forms = c("CREA", "OINT", "OIN", "LOTN", "LOT", "CRE", "PSTE",
                  "PAS", "GEL", "SHAM", "OIL","POWD","SOLG", "SHA",
                  "Topical-Unspecified")
for (form in topical_forms){
  meps = remove_entries(meps, meps$Form, form)
}

# remove ambiguous drug units
miscoded_units = c("OTHER", "U/ML", "U/ML/U/ML", "UNIT", "UNIT/ML", "UNIT/GM",
                   "U/GM", "UT/ML", "MCG/OTHER", "%/OTHER")
for (unit in miscoded_units){
  meps = remove_entries(meps, meps$Units, unit)
}

# remove unnecessary info from units and/or fix spelling/punctuation to be
# standard across all entries
fix_units_df = data.frame(miscoded_units = c("MCG/INH", "MG/ACT", "MCG/mg/Act",
                                             "MG/GM", "mg/Act", "GM/SCOOP",
                                             "MG/mg/Act", "MCG/BLIST",
                                             "MCG/SPRAY","MG/SPRAY", "MCG/ACT",
                                             "GM", "GM/ML","MG/ML/MG/ML"),
                          correct_units = c("MCG", "MG", "MCG/MG", "MG/G", "MG",
                                            "G","MG/MG", "MCG", "MCG", "MG",
                                            "MCG","G", "G/ML","MG/ML"))
for (i in seq_len(nrow(fix_units_df))){
  meps = fix_drug_coding(meps, meps$Units, "Units", fix_units_df[i,1],
                         fix_units_df[i,2])
}

# remove dashes from Form column (to avoid confusing special characters):
fix_forms_df = data.frame(miscoded_forms = c("CAP-Capsule", "INH-Inhaler",
                                             "INH-Inhalant"),
                          correct_forms = c("Capsule", "Inhaler", "Inhaler"))
for (i in seq_len(nrow(fix_forms_df))){
  meps = fix_drug_coding(meps, meps$Form, "Form", fix_forms_df[i,1],
                         fix_forms_df[i,2])
}

# remove unnecessary commas
meps$Drug = gsub(",", "", meps$Drug)
meps$Strength = gsub(",", "", meps$Strength)

# NOTE: all the corrections to miscoded info below are curated against known
# dosages/units

# remove miscoded drugs or drugs with vague/missing info
meps = meps[is.na(meps$Units)==FALSE,]
meps = meps[is.na(meps$Drug)==FALSE,]
meps = remove_entries(meps, meps$Form, "OTHER")
meps = remove_entries(meps, meps$NDC, "53002090672")
meps = remove_entries(meps, meps$NDC, "70147031316")
meps = remove_entries(meps, meps$Units, "ML")

# fix cases with miscoded strengths
meps = meps %>%
  mutate(Strength = case_when(Strength=="160-4.5/1" ~ "160-4.5",
                              .default=Strength))
meps = meps %>%
  mutate(Strength = case_when(Strength=="1.25/1.25/1.25/1.25" ~ "1.25/1.25",
                              .default=Strength))

# fix miscoded albuterol (108 mcg albuterol sulfate = 90 mcg albuterol)
meps = meps %>%
  mutate(Strength = case_when(Drug=="ALBUTEROL" & Strength=="108/1" ~ "90",
                              Drug=="ALBUTEROL" & Strength=="108" ~ "90",
                              .default=Strength))

# fix albuterol units
meps = meps %>%
  mutate(Units = case_when(Drug == "ALBUTEROL" & Units=="MCG/MG" ~ "MCG",
                           .default=Units))
meps = meps %>%
  mutate(Units = case_when(Drug == "ALBUTEROL" & Strength=="0.09/1" ~ "MG",
                           .default=Units))
meps = meps %>% 
  mutate(Units = case_when(Drug=="ALBUTEROL" & Strength=="90" ~ "MCG",
                           .default=Units))

# Remove whitespace
meps = meps %>%
  mutate_all(~str_squish(.))

# fix miscoded units for combination drugs
meps = meps %>%
  mutate(Units = case_when((grepl("/", Drug)==TRUE | grepl("-", Drug)==TRUE)
                           & Units=="MG" ~ "MG/MG",
                           .default = Units))
meps = meps %>%
  mutate(Units = case_when((grepl("-", Drug)==TRUE | grepl("/", Drug)==TRUE)
                           & Units=="MCG" ~ "MCG/MCG",
                           .default = Units))
meps = meps %>%
  mutate(Units = case_when((grepl("-", Drug)==TRUE | grepl("/", Drug)==TRUE)
                           & Units=="G" ~ "G/G",
                           .default = Units))
meps = meps %>%
  mutate(Units = case_when((grepl("-", Drug)==TRUE | grepl("/", Drug)==TRUE)
                           & Units=="%" ~ "%/%",
                           .default = Units))
meps = meps %>%
  mutate(Units = case_when((grepl("/", Drug)==TRUE | grepl("-", Drug)==TRUE)
                           & Units=="ML" ~ "ML/ML/ML",
                           .default = Units))

# remove rows where strength has no slash/dash and drug does
meps = meps %>%
  filter(!((grepl("/",Drug)==TRUE | grepl("-",Drug)==TRUE) & (grepl("/",Strength)==FALSE & grepl("-",Strength)==FALSE)))

# Separate combination drugs (and find ones that are miscoded as combination
# drugs)

# fix mismatches between slashes and dashes across Drug, Strength, Units columns
# (that is, make it either all slashes or all dashes)
meps = meps %>%
  mutate(Strength = case_when(grepl("-", Drug)==TRUE &
                                grepl("/", Strength) == TRUE ~
                                str_replace_all(Strength, "/", "-"),
                              .default = Strength))

meps = meps %>%
  mutate(Strength = case_when(grepl("/", Drug)==TRUE &
                                grepl("-", Strength) == TRUE ~
                                str_replace_all(Strength, "-", "/"),
                              .default = Strength))

meps = meps %>%
  mutate(Units = case_when(grepl("-", Drug)==TRUE & grepl("/", Units) == TRUE &
                             grepl("ML",Units)==FALSE & grepl("HR",Units)==FALSE ~ 
                                str_replace_all(Units, "/", "-"),
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(grepl("/", Drug)==TRUE & grepl("-", Units) == TRUE &
                             grepl("ML",Units)==FALSE & grepl("HR",Units)==FALSE ~
                             str_replace_all(Units, "-", "/"),
                           .default = Units))

# Remove drugs with dashes that are not combination drugs and will be removed
# anyway
drug_names_to_remove = c("OMEGA-3 POLYUNSATURATED FATTY ACIDS",
                          "ALPHA-LIPOIC ACID","ANTI-INFECTIVES",
                          "THIAZIDE AND THIAZIDE-LIKE DIURETICS",
                          "STEROIDS WITH ANTI-INFECTIVES",
                          "NONSTEROIDAL ANTI-INFLAMMATORY AGENTS")
meps = meps %>% filter(!Drug %in% drug_names_to_remove)

# Remove whitespace
meps = meps %>%
  mutate_all(~str_squish(.))

# Handle dash delimiters - no slash delimiters after data curation

# Make dummy column of all zeros initially
meps = meps %>%
  mutate(Is_Combo = rep(0, nrow(meps)))

# Make a duplicate row with Is_Combo set to 1 if drug is a combination drug

# Identify rows where 'Drug' contains a dash
rows_with_dash = meps %>% filter(grepl("-", Drug))

# Duplicate these rows and set 'Is_Combo' to 1
duplicated_rows = rows_with_dash %>% mutate(Is_Combo = 1)

# Combine the original dataframe with the duplicated rows
meps = bind_rows(meps, duplicated_rows) %>% arrange(row_number())

# Conditionally split the Units column based on the presence of a dash in the
# Drug column (all units with slash are not combos)
meps = meps %>%
  mutate(
    Units_Parts = if_else(
      grepl("-", Drug),  
      str_split(Units, "-", simplify = TRUE),  
        Units  
    )
  )

# Split the Drug, Strength columns into parts before and after the dash or slash
meps = meps %>%
  mutate(
    Drug_Parts = str_split(Drug, "-", simplify = TRUE),
    Strength_Parts = str_split(Strength, "[-/]", simplify = TRUE)
  )

# Use if_else to conditionally assign the correct part based on the Is_Combo
# column
meps = meps %>%
  mutate(
    Drug = if_else(Is_Combo == 0, Drug_Parts[, 1],
                   if_else(Is_Combo == 1, Drug_Parts[, 2],Drug)),
    Strength = if_else(Is_Combo == 0, Strength_Parts[, 1],
                       if_else(Is_Combo == 1, Strength_Parts[, 2], Strength)),
    Units = if_else(Is_Combo == 0, Units_Parts[, 1],
                    if_else(Is_Combo == 1, Units_Parts[, 2],Units))
  )

# Drop the helper columns
meps = meps %>%
  select(-Is_Combo, -Drug_Parts, -Strength_Parts, -Units_Parts)

# Generate drug list to loop through

# get list of all drugs listed in Drug column
drugs = unique(meps$Drug)

# remove missing drug names from drug list
drugs = na.omit(drugs)

# remove any reintroduced whitespace from drugs vector
drugs = trimws(drugs)

# remove vitamins, minerals, compounds from food, etc
food_drugs = unlist(as.list(read_excel("drugs_to_remove.xlsx", sheet=1,
                                       col_names=FALSE)))
drugs = remove_drugs(drugs, food_drugs)

# remove hormones and other naturally occurring compounds in the body too
inbody_drugs = unlist(as.list(read_excel("drugs_to_remove.xlsx", sheet=2,
                                         col_names=FALSE)))
drugs = remove_drugs(drugs, inbody_drugs)

# remove vague names and non-distinct structures
vague_drugs = unlist(as.list(read_excel("drugs_to_remove.xlsx", sheet=3,
                                        col_names=FALSE)))
drugs = remove_drugs(drugs, vague_drugs)

# remove topicals based on name
for (drug in drugs){
  if (grepl("TOPICAL", drug)==TRUE){
    drugs = remove_drugs(drugs, drug)
  }
}

# remove any duplicates introduced
drugs = unique(drugs)

# remove extra periods from data
meps$Strength = gsub("\\.+$", "", meps$Strength)

# initialize larger data frame to store cleaned data
meps_clean = vector("list", length = length(drugs))

# loop through drug list to complete data cleaning on MEPS dataset
for (i in 1:length(drugs)){
  drug_data = meps %>%
    filter(Drug==drugs[i])
  
  # Find avg Day_Supply and Quantity values for each drug (to be used for
  # imputing - no missing Strength values)
  not_missing_daysup = drug_data[is.na(drug_data$Day_Supply)==FALSE,]
  avg_daysup = sum(as.numeric(not_missing_daysup$Day_Supply))/
    length(not_missing_daysup$Day_Supply)
  not_missing_quanty = drug_data[is.na(drug_data$Quantity)==FALSE,]
  avg_quanty = sum(as.numeric(not_missing_quanty$Quantity))/
    length(not_missing_quanty$Quantity)

  # remove entries where Day_Supply missing for >95% of entries (imputing not
  # reliable in this case)
   if (sum(is.na(drug_data$Day_Supply)==TRUE) > 0.95*dim(drug_data)[1]){
     drug_data = subset(drug_data, is.na(drug_data$Day_Supply)==FALSE)
   }
   else {
    drug_data = drug_data %>%
      mutate(Day_Supply = case_when(is.na(Day_Supply)==TRUE ~ avg_daysup,
                                  .default=as.numeric(Day_Supply)))
   }

  # remove drugs where Quantity missing for >95% of entries (imputing not
  # reliable)
   if (sum(is.na(drug_data$Quantity)==TRUE) > 0.95*dim(drug_data)[1]){
     drug_data = subset(drug_data, is.na(drug_data$Quantity)==FALSE)
   }
   else {
    drug_data = drug_data %>%
      mutate(Quantity = case_when(is.na(Quantity)==TRUE ~ avg_quanty,
                                  .default=as.numeric(Quantity)))
   }

  # convert strength, Day_Supply, quantity to correct data type
  drug_data = drug_data %>%
    mutate(Day_Supply = as.numeric(Day_Supply)) %>%
    mutate(Quantity = as.numeric(Quantity)) %>%
    mutate(Strength = as.numeric(Strength))

  # Conversions to correct units in Strength column

  # convert MG to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "MG" ~ Strength*1000,
                                .default=Strength))

  # convert MG/ML to MCG (mass and volume often directly given (e.g. 3 mg/20 mL);
  # if volume not given, assuming 1 mL)
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "MG/ML" ~ Strength*1000,
                                .default=Strength))

  # convert MCG/HR to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units=="MCG/HR" ~ Strength*24,
                                .default=Strength))

  # Convert % to MCG (1% solution means 1 g of drug per 100 mL of solution - for
  # example, 1% solution is 1 g drug per 100 mL solvent, assume 100mL each)
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "%" ~ Strength*1000000,
                                .default=Strength))

  # Convert G/ML to MCG (mass and volume often directly given (e.g. 3 g/20 mL);
  # if volume not given, assuming 1 mL)
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "G/ML" ~ Strength*1000000,
                                .default=Strength))

  # Convert G to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units=="G" ~ Strength*1000000,
                                .default=Strength))

  # Convert MG/24HR to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units=="MG/24HR" ~ Strength*1000,
                                .default=Strength))

  # Convert MG/HR to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units=="MG/HR" ~ Strength*24*1000,
                                .default=Strength))

  # change all remaining units to MCG (everything is in MCG now, MCG is
  # micrograms or ug)
  drug_data = drug_data %>%
    mutate(Units = "MCG")

  # add cleaned data for each drug to its own data frame
  meps_clean[[i]] = drug_data
}

# bind each data frame for each drug into giant data frame
meps_clean = do.call(rbind, meps_clean)

# remove Units (all same units now)
meps_clean = subset(meps_clean, select=-c(Units))

# calculate daily frequency for each record
meps_clean = meps_clean %>%
  mutate(Daily_Frequency = Quantity/Day_Supply)

# calculate daily dosage for each record
meps_clean = meps_clean %>%
  mutate(Daily_Dosage = Strength*Daily_Frequency)

# find avg daily dosage for each drug and put into descending order (ug/day)
avg_daily_dosages = aggregate(meps_clean$Daily_Dosage,list(meps_clean$Drug),
                                FUN=mean)
colnames(avg_daily_dosages) = c("Pharmaceutical",
                                  "Average_Daily_Dosage")
avg_daily_dosages = avg_daily_dosages %>%
  arrange(desc(Average_Daily_Dosage))

# find total number of prescriptions per drug per year and put into descending
# order
total_prescribed = meps_clean %>%
  count(Drug) %>%
  arrange(desc(n))
colnames(total_prescribed) = c("Pharmaceutical", "Num_Prescriptions")

# find avg duration of prescription for each pharmaceutical, descending order
avg_durations = aggregate(meps_clean$Day_Supply,list(meps_clean$Drug),
                          FUN=mean)
colnames(avg_durations) = c("Pharmaceutical",
                                "Average_Duration_per_Prescription")
avg_durations = avg_durations %>%
  arrange(desc(Average_Duration_per_Prescription))

################################################################################
# Load and clean CompTox data. 
# NOTE: all properties are added as medians of all available values
################################################################################

# load first sheet (IDs, molecular formula, molar mass)
comptox_search_id_mf_mm = read_excel("comptox_search.xlsx", sheet=1)

# handle multiple identifiers and rename columns
comptox_search_id_mf_mm = comptox_search_id_mf_mm %>%
  select(-c(FOUND_BY, PREFERRED_NAME)) %>%
  group_by(INPUT) %>%
  summarize(SMILES = paste(SMILES, collapse = ", "),
            INCHIKEY = paste(INCHIKEY, collapse = ", "),
            INCHISTRING = paste(INCHISTRING, collapse = ", "),
            MOLECULAR_FORMULA = first(MOLECULAR_FORMULA),   
            AVERAGE_MASS = first(AVERAGE_MASS),
            DTXSID = first(DTXSID)) %>%   
  rename(Pharmaceutical = INPUT) %>%
  rename(Molecular_Formula = MOLECULAR_FORMULA) %>%
  rename(Molar_Mass = AVERAGE_MASS) %>% # units are g/mol
  select(Pharmaceutical, DTXSID, SMILES, INCHIKEY, INCHISTRING,
         Molecular_Formula, Molar_Mass) 

# for drugs that lack data, replace by NA (needed for merging later)
other_cols = setdiff(names(comptox_search_id_mf_mm), "Pharmaceutical")

na_cols = setNames(rep(NA, length(other_cols)), other_cols)

comptox_search_id_mf_mm = comptox_search_id_mf_mm %>%
  add_row(Pharmaceutical = "DEXLANSOPRAZOLE", !!!na_cols) %>%
  add_row(Pharmaceutical = "DULAGLUTIDE", !!!na_cols) %>%
  add_row(Pharmaceutical = "ERTUGLIFLOZIN", !!!na_cols) %>%
  add_row(Pharmaceutical = "SENNA", !!!na_cols)

# load second sheet: toxicity data
comptox_search_tox = read_excel("comptox_search.xlsx", sheet=2)

# remove unneeded columns
cols_to_keep = c("SEARCHED_CHEMICAL", "DTXSID", "TOXVAL_TYPE",
                 "TOXVAL_NUMERIC", "TOXVAL_UNITS", "SPECIES_COMMON",
                 "SPECIES_SUPERCATEGORY","HUMAN_ECO","CRITICAL_EFFECT") 
comptox_search_tox = comptox_search_tox[,cols_to_keep]
# rename SEARCHED_CHEMICAL
colnames(comptox_search_tox)[colnames(comptox_search_tox) == "SEARCHED_CHEMICAL"] = "Pharmaceutical"

# filter tox_data for NOEC (chronic) and LC50 (acute)
comptox_search_tox = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="NOEC" | TOXVAL_TYPE=="LC50") %>%
  group_by(Pharmaceutical)

# get molar mass for unit conversions
comptox_search_id_mf_mm$Pharmaceutical = paste0(toupper(substr(tolower(comptox_search_id_mf_mm$Pharmaceutical),
                                                                1, 1)),
                                                 substr(tolower(comptox_search_id_mf_mm$Pharmaceutical),
                                                        2,
                                                        nchar(comptox_search_id_mf_mm$Pharmaceutical)))
comptox_search_mm = comptox_search_id_mf_mm %>%
  select(c(Pharmaceutical,Molar_Mass))

# merge molar mass df with tox df
comptox_search_mm$Pharmaceutical = trimws(tolower(comptox_search_mm$Pharmaceutical))
comptox_search_tox$Pharmaceutical = trimws(tolower(comptox_search_tox$Pharmaceutical))
comptox_search_tox = comptox_search_tox %>%
  inner_join(comptox_search_mm, by = "Pharmaceutical")

# remove ambiguous units
comptox_search_tox = comptox_search_tox[comptox_search_tox$TOXVAL_UNITS != "% diet", ]
comptox_search_tox = comptox_search_tox[comptox_search_tox$TOXVAL_UNITS != "% of bdwt", ]
comptox_search_tox = comptox_search_tox %>%
  filter(!(SPECIES_COMMON == "Goldfish" & TOXVAL_UNITS == "mg")) %>%
  filter(!(SPECIES_COMMON == "Spiny Dogfish" & TOXVAL_UNITS == "mg"))

# convert all units to ug/L (assume that it's the concentration organism is
# exposed to in the water (equivalent water weight), so convert all toxicities
# expressed as mg/kg using density of water; also assume that full amount is
# taken up by organism)
# NOTE: uM/kg-minute should be 50 uM according to citation; ng eq/ml is just
# ng/ml estradiol according to citation; mg/kg-day is just mg/kg by food or body
# weight according to original sources; L should be 1000 ug/mL according to 
# citation; mg for axolotl is in 200 uL according to citation; mg for salmon is
# in 1 uL according to citation; ng/egg is in 0.5 nL according to citation; all
# citations are from the associated CompTox search result
comptox_search_tox = comptox_search_tox %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mg/m3" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mol/g" ~
                                      TOXVAL_NUMERIC*1000*Molar_Mass*1E+06,
                                  .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mol/g" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg/g diet" ~
                                      TOXVAL_NUMERIC*1E+06,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mg/g diet" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg/300 g" ~
                                      (TOXVAL_NUMERIC/300)*1E+06,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mg/300 g" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg/L" ~ TOXVAL_NUMERIC*1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mg/L" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg/L" ~ TOXVAL_NUMERIC*1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS=="mg/L" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="uM/kg-minute" ~
                                      50*Molar_Mass/1000*1E+06,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "uM/kg-minute" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ng eq/ml" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="ug/g bw" ~
                                      TOXVAL_NUMERIC*1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ug/g bw" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg/kg-day" ~
                                      TOXVAL_NUMERIC*1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "mg/kg-day" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>%
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="L" ~ 1000*1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "L" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="ppm" ~ TOXVAL_NUMERIC/1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ppm" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg" &
                                      SPECIES_COMMON=="Mexican Axolotl" ~
                                      TOXVAL_NUMERIC*1000*1000/200,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "mg" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="mg" &
                                      SPECIES_COMMON=="Chinook Salmon" ~
                                      TOXVAL_NUMERIC*1E+09,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "mg" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="ng/egg" ~
                                      TOXVAL_NUMERIC*1E+09/0.5/1000,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ng/egg" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="nM/org" ~
                                      TOXVAL_NUMERIC*Molar_Mass*1E+06/1E+09,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "nM/org" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ng/mL" ~ "ug/L",
                                  .default=TOXVAL_UNITS)) %>% 
  mutate(TOXVAL_NUMERIC = case_when(TOXVAL_UNITS=="ug/300 g bw" ~
                                      TOXVAL_NUMERIC*1000/300,
                                    .default=TOXVAL_NUMERIC)) %>%
  mutate(TOXVAL_UNITS = case_when(TOXVAL_UNITS == "ug/300 g bw" ~ "ug/L",
                                  .default=TOXVAL_UNITS))

# group by TOXVAL_TYPE, NAME, SPECIES_COMMON same
comptox_search_tox = comptox_search_tox %>%
  group_by(TOXVAL_TYPE, Pharmaceutical, SPECIES_COMMON, SPECIES_SUPERCATEGORY) 

# define vertebrate, invertebrate, plants/algae categories
vertebrates = c("Amphibians", "Fish", "Mammals")
invertebrates = c("Crustaceans", "Insects/Spiders", "Invertebrates", "Worms",
                  "Molluscs")
plants_algae = (c("Algae", "Flowers, Trees, Shrubs, Ferns"))

# remove miscellaneous super-category
comptox_search_tox = comptox_search_tox[comptox_search_tox$SPECIES_SUPERCATEGORY != "Miscellaneous", ]

####################
# LC50 computations
####################

# aggregate LC50 data by finding median of each group
lc50 = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="LC50") %>%
  mutate(Organism_category = case_when(SPECIES_SUPERCATEGORY %in% vertebrates ~
                                         "Vertebrate",
                                       SPECIES_SUPERCATEGORY %in% invertebrates ~
                                         "Invertebrate",
                                       SPECIES_SUPERCATEGORY %in% plants_algae ~
                                         "Plants_and_Algae")) %>%
  group_by(Pharmaceutical,Organism_category) %>%
  summarize(Median_tox=median(TOXVAL_NUMERIC))

# make LC50 so that each pharmaceutical only appears in 1 row
lc50 = lc50 %>%
  pivot_wider(
    id_cols = Pharmaceutical,
    names_from = Organism_category,
    values_from = Median_tox,
    values_fill = list(Median_tox = NA)
  )

# update names of data frame
lc50 = lc50 %>% 
  rename(Plants_Algae_Median_LC50 = Plants_and_Algae) %>%
  rename(Vertebrate_Median_LC50 = Vertebrate) %>%
  rename(Invertebrate_Median_LC50 = Invertebrate)

# estimate human LC50 by dividing min vertebrate LC50 by safety factor of 10
lc50_vert = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="LC50" & SPECIES_SUPERCATEGORY %in% vertebrates)

lc50_invert = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="LC50" & SPECIES_SUPERCATEGORY %in% invertebrates)

lc50_plants_algae = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="LC50" & SPECIES_SUPERCATEGORY %in% plants_algae)

lc50_human = lc50_vert %>%
  group_by(Pharmaceutical) %>%
  summarize(Human=min(TOXVAL_NUMERIC)/10)
  
# get drug names in correct format to merge MEPS and tox data
meps_clean$Drug = trimws(tolower(meps_clean$Drug))
lc50$Pharmaceutical = trimws(tolower(lc50$Pharmaceutical))

# add names of drugs with missing L50s as placeholders to LC50 data frame to
# merge with main PharmUse dataframe

# find drugs with missing LC50s to assign NA
missing_drugs = setdiff(unique(meps_clean$Drug),
                         unique(lc50$Pharmaceutical))

# set number of empty rows to add
num_empty_rows = length(missing_drugs)

# make data frame with the specified number of empty rows filled with NA
empty_rows = data.frame(matrix(NA, ncol = ncol(lc50), nrow = num_empty_rows))
colnames(empty_rows) = colnames(lc50)

# fix class of empty_rows columns
lc50 = lc50 %>%
  mutate_at(vars(-matches("Pharmaceutical")), as.numeric)

# add empty rows to the data frame and assign drug names
lc50 = rbind(lc50, empty_rows)
lc50 = replace_na_with_missing_drug(lc50, missing_drugs)

# bind with data frame with human LC50
lc50 = left_join(lc50, lc50_human, by = "Pharmaceutical")

# rename columns for clarity
lc50 = lc50 %>%
  rename(Human_LC50 = Human)

####################
# NOEC computations
####################

# aggregate data for NOEC by finding median of each group
noec = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="NOEC") %>%
  mutate(Organism_category = case_when(SPECIES_SUPERCATEGORY %in% vertebrates ~
                                         "Vertebrate",
                                     SPECIES_SUPERCATEGORY %in% invertebrates ~
                                       "Invertebrate",
                                     SPECIES_SUPERCATEGORY %in% plants_algae ~
                                       "Plants_and_Algae")) %>%
  group_by(Pharmaceutical,Organism_category) %>%
  summarize(Median_tox=median(TOXVAL_NUMERIC))

# make noec so that each pharmaceutical only appears in 1 row
noec = noec %>%
  pivot_wider(
    names_from = c(Organism_category),
    values_from = Median_tox,
    names_sep = "_",
    names_glue = "{Organism_category} NOEC"
  )

# rename columns to be more specific
colnames(noec) = c("Pharmaceutical", "Invertebrate_median_NOEC",
                   "Plants_algae_median_NOEC", "Vertebrate_median_NOEC")

# aggregate NOEC for vertebrate, invertebrate, plant/algae all values
noec_vert = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="NOEC" & SPECIES_SUPERCATEGORY %in% vertebrates)

noec_invert = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="NOEC" & SPECIES_SUPERCATEGORY %in% invertebrates)

noec_plants_algae = comptox_search_tox %>%
  filter(TOXVAL_TYPE=="NOEC" & SPECIES_SUPERCATEGORY %in% plants_algae)

# estimate human NOEC by multiplying min vertebrate NOEC by safety factor of 10
noec_human = noec_vert %>%
  group_by(Pharmaceutical) %>%
  summarize(Human=min(TOXVAL_NUMERIC)/10)

# get in correct format to merge MEPS and tox data
noec$Pharmaceutical = trimws(tolower(noec$Pharmaceutical))
noec_vert$Pharmaceutical = trimws(tolower(noec_vert$Pharmaceutical))
noec_invert$Pharmaceutical = trimws(tolower(noec_invert$Pharmaceutical))
noec_plants_algae$Pharmaceutical = trimws(tolower(noec_plants_algae$Pharmaceutical))
noec_human$Pharmaceutical = trimws(tolower(noec_human$Pharmaceutical))

# find drugs with missing NOEC to assign NA
missing_drugs = setdiff(unique(tolower(meps_clean$Drug)),
                         unique(noec$Pharmaceutical))

# find number of empty rows to add
num_empty_rows = length(missing_drugs)

# create data frame with the specified number of empty rows filled with NA
empty_rows = data.frame(matrix(NA, ncol = ncol(noec), nrow = num_empty_rows))
colnames(empty_rows) = colnames(noec)

# fix class of empty_rows columns
noec = noec %>%
  mutate_at(vars(-matches("Pharmaceutical")), as.numeric)

# add empty rows to data frame & assign drug names
noec = rbind(noec, empty_rows)
noec = replace_na_with_missing_drug(noec, tolower(missing_drugs))

# bind with dataframe with min vertebrate toxicity
noec = left_join(noec, noec_human, by = "Pharmaceutical")

# rename columns for clarity
noec = noec %>%
  rename(Human_NOEC = Human)

# Load sheet 3: physicochemical properties
# NOTE: units are consistent for each property
comptox_search_props = read_excel("comptox_search.xlsx",
                             sheet=3)
comptox_search_props = subset(comptox_search_props,
                              select=c(DTXSID, NAME, VALUE, UNITS))

# change VALUE column to numeric
comptox_search_props$VALUE = as.numeric(comptox_search_props$VALUE)

# make individual data frames for each property
comptox_search_props$NAME = trimws(comptox_search_props$NAME)
# split data frame by NAME
properties = comptox_search_props %>%
  group_split(NAME)

# apply the calculate_median function to each data frame in the list
median_list = lapply(properties, calculate_median)

# combine the results into a single data frame
properties = do.call(rbind, median_list)

# split into separate data frame for each property
properties = split(properties, properties$NAME)

# add in NA's for missing values (to make data frames same length for sorting)
for (i in seq_along(properties)) {
  # find the missing rows
  missing_rows = length(unique(meps_clean$Drug)) - nrow(properties[[i]])
  if (missing_rows > 0) {
    # create data frame with NA values for missing rows
    missing_df = data.frame(
      DTXSID = rep(NA, missing_rows),
      NAME = rep(names(properties)[i], missing_rows),
      median_VALUE = rep(NA, missing_rows),
      UNITS = rep(NA, missing_rows)
    )
    # bind missing rows to original data frame
    properties[[i]] = rbind(properties[[i]], missing_df)
  }
}

# get DTXSID & Pharmaceutical columns from first sheet
comptox_search_dtxsid = comptox_search_id_mf_mm %>%
  select(c(Pharmaceutical,DTXSID))

# prep new df for merge
comptox_search_dtxsid$Pharmaceutical = trimws(tolower(comptox_search_dtxsid$Pharmaceutical))

# initialize list to store missing DTXSID values for each data frame
missing_dtxsid = list()

# Loop through each data frame in the properties list
for (i in seq_along(properties)) {
  # find the missing DTXSID values in the current data frame
  missing_dtxsid[[i]] = setdiff(unique(comptox_search_dtxsid$DTXSID),
                                unique(properties[[i]]$DTXSID))
}

# replace NA values with missing DTXSID values for each data frame
properties[1:22] = map2(properties[1:22], missing_dtxsid[1:22],
                                   replace_na_with_missing_dtxsid)

# map each DTXSID to pharmaceutical name
properties = map(properties, ~ join_with_common_df(.x, comptox_search_dtxsid))

# give names of pharmaceuticals without DTXSID
pharma_no_dtxsid = c("dexlansoprazole", "dulaglutide", "ertugliflozin",
                     "senna")

# add in four pharmaceuticals without DTXSIDs
properties = map(properties, ~ add_rows(.x, "Pharmaceutical", pharma_no_dtxsid))

# remove extra columns not needed in main database
properties = lapply(properties, function(df) df[, !(names(df) %in% c("NAME",
                                                                     "DTXSID"))])

# fix order of columns
new_column_order = c("Pharmaceutical", "median_VALUE","UNITS")
properties = map(properties, ~ reorder_columns(.x, new_column_order))

# rename median column to be name of property, lowercase units column
properties = Map(rename_column, properties, names(properties),"median_VALUE")
properties = Map(rename_column, properties, "Units","UNITS")

################################################################################
# Generate PharmUse database from MEPS and CompTox data.
################################################################################

# prep to merge data into PharmUse database
avg_daily_dosages$Pharmaceutical = trimws(tolower(avg_daily_dosages$Pharmaceutical))
avg_durations$Pharmaceutical = trimws(tolower(avg_durations$Pharmaceutical))
total_prescribed$Pharmaceutical = trimws(tolower(total_prescribed$Pharmaceutical))
comptox_search_id_mf_mm$Pharmaceutical = trimws(tolower(comptox_search_id_mf_mm$Pharmaceutical))
for (i in 1:length(properties)){
  properties[[i]]$Pharmaceutical = trimws(tolower(properties[[i]]$Pharmaceutical))
}
lc50$Pharmaceutical = trimws(tolower(lc50$Pharmaceutical))
noec$Pharmaceutical = trimws(tolower(noec$Pharmaceutical))

# join data into PharmUse database
pharmuse = left_join(avg_daily_dosages, total_prescribed,
                     by="Pharmaceutical") %>%
  left_join(avg_durations, by="Pharmaceutical") %>%
  left_join(comptox_search_id_mf_mm, by="Pharmaceutical")

pharmuse = reduce(properties, left_join, by = "Pharmaceutical",
                  .init = pharmuse)

pharmuse = left_join(pharmuse, lc50, by="Pharmaceutical") %>%
  left_join(noec, by="Pharmaceutical")

# remove extraneous columns (they all start with X)
pharmuse = pharmuse[, !grepl("^X", names(pharmuse))]

# change column names in PharmUse to be more readable
colnames(pharmuse)[3] = "Number_of_Prescriptions"
colnames(pharmuse)[12] = "Boiling Point Units"
colnames(pharmuse)[14] = "Density Units"
colnames(pharmuse)[16] = "Flash Point Units"
colnames(pharmuse)[17] = "Henrys Law"
colnames(pharmuse)[18] = "Henrys Law Units"
colnames(pharmuse)[20] = "Index of Refraction Units"
colnames(pharmuse)[21] = "Liquid Chromatography Retention Time"
colnames(pharmuse)[22] = "Liquid Chromatography Retention Time Units"
colnames(pharmuse)[24] = "LogD5.5 Units"
colnames(pharmuse)[26] = "LogD7.4 Units"
colnames(pharmuse)[27] = "LogKoa Octanol Air"
colnames(pharmuse)[28] = "LogKoa Units"
colnames(pharmuse)[29] = "LogKow Octanol Water"
colnames(pharmuse)[30] = "LogKow Units"
colnames(pharmuse)[32] = "Melting Point Units"
colnames(pharmuse)[34] = "Molar Refractivity Units"
colnames(pharmuse)[36] = "Molar Volume Units"
colnames(pharmuse)[38] = "pKa Acidic Units"
colnames(pharmuse)[40] = "pKa Basic Units"
colnames(pharmuse)[42] = "Polarizability Units"
colnames(pharmuse)[43] = "Readily Biodegrades"
colnames(pharmuse)[46] = "Surface Tension Units"
colnames(pharmuse)[48] = "Thermal Conductivity Units"
colnames(pharmuse)[50] = "Vapor Pressure Units"
colnames(pharmuse)[52] = "Viscosity Units"
colnames(pharmuse)[54] = "Water Solubility Units"

# remove units column for readily biodegrades variable
pharmuse = pharmuse %>% select(-Units.x.x.x.x.x.x.x.x.x)

# export PharmUse database and toxicity values used in ensemble_exploration.R
write.csv(pharmuse, "pharmuse.csv")
write.csv(noec_vert,"noec_vert.csv")
write.csv(noec_invert,"noec_invert.csv")
write.csv(noec_human,"noec_human.csv")