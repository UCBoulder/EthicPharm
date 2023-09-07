# Note: this version is for developer use only - push another version without line below to GitHub
setwd("C:/Users/vgmay/research/CMBMGEM/cmbmgem/EthicPharm/EthicPharm")

# check code for correctness too!!

# check about drugs to add back in and assumptions to fix as cleaning/improving/testing code (try
# to make more functions)

# CHECK ALL CODE BEFORE MOVING TO NEXT PART TO ENSURE NO ERRORS (UNIT TESTS?) - wait till Python,
# that is: switch to Python first!

# may want to time code / check efficiency and through code in general before Python switch

# install packages and load libraries
install.packages("data.table")
install.packages("readxl")
install.packages("stringr")
install.packages("tidyr")
install.packages('tidyverse')
install.packages("webchem")
library("data.table")
library('dplyr')
library("readxl")
library("stringr")
library("tidyr")
library("webchem")

# load functions
source("meps_functions.R")

# load dataset (all columns read in as characters)
meps = read.delim("MEPS_data_2020.txt", header=TRUE, sep = "\t", colClasses = "character", dec = ".")

# extract desired data
meps = meps %>%
  select(RXDRGNAM:RXDAYSUP)

# remove RXFRMUNT (not used)
meps = subset(meps, select = -c(RXFRMUNT))

# update to clearer column names
colnames(meps) = c("Drug", "NDC", "Quantity", "Form", "Strength", "Units", "Day_Supply")

# replace missing values (-15, -8, and -7 in original data) with NA
meps = meps %>%
  mutate(across(colnames(meps), ~ case_when(. =="-15" ~ NA,
                                            . =="-8" ~ NA,
                                            . == "-7" ~ NA,
                                            .default = .)))

# remove ophthalmic, otic, and nasal labels from drug names (this prevents different
# forms of the same drug from being counted twice)
# TRY TO USE REGEX OR SOMETHING HERE, AVOID REPLACE FCN
meps$Drug = str_replace_all(meps$Drug, " OPHTHALMIC", "")
meps$Drug = str_replace_all(meps$Drug, "OPHTHALMIC ", "")
meps$Drug = str_replace_all(meps$Drug, " OPHTHALMIC ", "")
meps$Drug = str_replace_all(meps$Drug, " OTIC", "")
meps$Drug = str_replace_all(meps$Drug, "OTIC ", "")
meps$Drug = str_replace_all(meps$Drug, " OTIC ", "")
meps$Drug = str_replace_all(meps$Drug, " NASAL", "")
meps$Drug = str_replace_all(meps$Drug, "NASAL ", "")
meps$Drug = str_replace_all(meps$Drug, " NASAL ", "")

# remove miscoded codeine/guaifenesin
meps = subset(meps, meps$Drug != "CODEINE/GUAIFENESIN" & meps$Drug != "CODEINE-GUAIFENESIN")

# fix miscoded drugs (will be specific to each dataset)
# meps = meps %>%
#   mutate(Drug = case_when(Drug=="BROMPHENIRAMINE/DEXTROMETHORPHAN/PSE"
#                               ~ "BROMPHENIRAMINE/DEXTROMETHORPHAN/PSEUDOEPHEDRINE",
#                               .default=Drug))

#Drug = meps$Drug
meps = fix_drug_coding(meps, "BROMPHENIRAMINE/DEXTROMETHORPHAN/PSE",
                       "BROMPHENIRAMINE/DEXTROMETHORPHAN/PSEUDOEPHEDRINE")

meps = meps %>%
  mutate(Drug = case_when(Drug=="CITRIC ACID/MG OXIDE/NA PICOSULFATE"
                              ~ "CITRIC ACID/MAGNESIUM OXIDE/SODIUM PICOSULFATE",
                              .default=Drug))
meps = meps %>%
  mutate(Drug = case_when(Drug=="BROMPHENIRAMINE/DEXTROMETHORPH/PHENYLEPHRINE"
                              ~ "BROMPHENIRAMINE/DEXTROMETHORPHAN/PHENYLEPHRINE",
                              .default=Drug))
meps = meps %>%
  mutate(Drug = case_when(Drug=="DEXCHLORPHENIRAMINE/DEXTROMETHORPHAN/PE"
                              ~ "DEXCHLORPHENIRAMINE/DEXTROMETHORPHAN/PHENYLEPHRINE",
                              .default=Drug))
meps = meps %>%
  mutate(Drug = case_when(Drug=="CHLOPHEDIANOL/DEXBROMPHENIRAMINE/PSE"
                              ~ "CHLOPHEDIANOL/DEXBROMPHENIRAMINE/PSEUDOEPHEDRINE",
                              .default=Drug))

# classify drugs with multiple names under a single name
meps = meps %>%
  mutate(Drug = case_when(Drug=="5-AMINOSALICYLATES"
                              ~ "MESALAMINE",
                              .default=Drug))
meps = meps %>%
  mutate(Drug = case_when(Drug=="BACITRACIN"
                              ~ "BACITRACIN A",
                              .default=Drug))
meps = meps %>%
  mutate(Drug = case_when(Drug=="DIVALPROEX SODIUM"
                              ~ "DIVALPROEX",
                              .default=Drug))

# replace slashes in Drug with dashes to match delimiters in Strength
meps = meps %>%
  mutate(Drug = case_when(Drug=="HYDROCORTISONE/NEOMYCIN/POLYMYXIN B"
                              ~ "HYDROCORTISONE-NEOMYCIN-POLYMYXIN B",
                              .default=Drug))

# remove topical Forms
meps = meps[meps$Form != "CREA",]
meps = meps[meps$Form != "OINT",]
meps = meps[meps$Form != "OIN",]
meps = meps[meps$Form != "LOTN",]
meps = meps[meps$Form != "LOT",]
meps = meps[meps$Form != "SWAB",]
meps = meps[meps$Form != "CRE",]
meps = meps[meps$Form != "PSTE",]
meps = meps[meps$Form != "PAS",]
meps = meps[meps$Form != "GEL",]
meps = meps[meps$Form != "SHAM",]

# remove dashes from Form column (to avoid confusing special characters):
meps = meps %>%
  mutate(Form = case_when(Form=="CAP-Capsule" ~ "Capsule",
                          Form=="INH-Inhaler" ~ "Inhaler",
                          Form=="INH-Inhalant" ~ "Inhaler",
                          .default=Form))

# remove entries with vague or missing units (can't impute strings)
meps = meps[is.na(meps$Units)==FALSE,]
meps = meps[meps$Units != "OTHER",]
meps = meps[meps$Units != "U/ML",]
meps = meps[meps$Units != "U/ML/U/ML",]
meps = meps[meps$Units != "UNIT",]
meps = meps[meps$Units != "UNIT/ML",]
meps = meps[meps$Units != "UNIT/GM",]
meps = meps[meps$Units != "U/GM",]
meps = meps[meps$Units != "UT/ML",]
meps = meps[meps$Units != "MCG/OTHER",]
meps = meps[meps$Units != "%/OTHER",]

# fix miscoded units
meps = meps %>%
  mutate(Units = case_when(Units=="MG/ML/MG/ML" ~ "MG/ML",
                              .default = Units))
meps = meps %>%
  mutate(Units = case_when(grepl("/", Strength)==TRUE & Units=="MG" ~ "MG/MG",
                              .default = Units))
meps = meps %>%
  mutate(Units = case_when(grepl("-", Strength)==TRUE & Units=="MCG" ~ "MCG/MCG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(grepl("-", Strength)==TRUE & Units=="MG" ~ "MG/MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(grepl("-", Strength)==TRUE & Units=="%" ~ "%/%",
                              .default = Units))

# all cases with ML and dash delimiter are same triple combo drug (may want to use str_count)
meps = meps %>%
  mutate(Units = case_when(grepl("-", Strength)==TRUE & Units=="ML" ~ "ML/ML/ML",
                              .default = Units))


# remove unnecessary info from units and/or fix spelling/punctuation to be standard
meps = meps %>%
  mutate(Units = case_when(Units=="MCG/INH" ~ "MCG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MG/ACT" ~ "MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MCG/mg/Act" ~ "MCG/MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MG/GM" ~ "MG/G",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="mg/Act" ~ "MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="GM/SCOOP" ~ "G",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MG/mg/Act" ~ "MG/MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MCG/BLIST" ~ "MCG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MCG/SPRAY" ~ "MCG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MG/SPRAY" ~ "MG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="MCG/ACT" ~ "MCG",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="GM" ~ "G",
                              .default = Units))

meps = meps %>%
  mutate(Units = case_when(Units=="GM/ML" ~ "G/ML",
                              .default = Units))

# remove entries with missing Form (can't tell solubility,etc)
meps = meps[meps$Form != "OTHER",]

# fix miscoded drug Cefdinir (250 mg/5mL - just divide)
# note: only Cefdinir has this strength value
meps = meps %>%
  mutate(Strength = case_when(Strength=="250-5" ~ "50", .default=Strength))

# fix miscoded drug strength (should be dashes only)
meps = meps %>%
  mutate(Strength = case_when(Strength=="160-4.5/1" ~ "160-4.5", .default=Strength))

# fix miscoded drug strength (too many slashes)
meps = meps %>%
   mutate(Strength = case_when(Strength=="1.25/1.25/1.25/1.25" ~ "1.25/1.25", .default=Strength))
# meps = meps %>%
#   mutate(Strength = case_when(Strength=="2/1/20/1" ~ "2/20", .default=Strength))
meps = meps %>%
  mutate(Strength = case_when(Strength=="70/1/30/1" ~ "70/30", .default=Strength))

# fix miscoded albuterol
meps = meps %>%
  mutate(Strength = case_when(Strength=="108/1" ~ "90", .default=Strength))

# fix albuterol dosage (108 mcg albuterol sulfate = 90 mcg albuterol)
# note: only albuterol has this strength value
meps = meps %>%
  mutate(Strength = case_when(Strength=="108" ~ "90", .default=Strength))
# look at 10 drugs to see if others like this

# remove miscoded albuterol (2 entries)
meps = meps %>%
  filter(!(Drug=="ALBUTEROL" & Units == "ML"))

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

# perform separations for slash and dash cases

# change dash delimiters in Drug to match slash delimiters in associated Strength
# consider use of str_replace here a little more - is it correct?
meps = meps %>%
  mutate(Drug = case_when(grepl("-", Drug)==TRUE & grepl("/", Strength) == TRUE ~
                                str_replace_all(Drug, "-", "/"),
                              .default = Drug))

# change slash delimiters in Units to match dash delimiters in associated Strength
# consider use of str_replace here a little more - is it correct?
meps = meps %>%
  mutate(Units = case_when(grepl("/", Units)==TRUE & grepl("-", Strength) == TRUE ~
                                str_replace_all(Units, "/", "-"),
                              .default = Units))

# separate the combo active ingredients for dash delimiters
meps = meps %>%
  separate_longer_delim(c(Drug, Strength, Units), delim="-")

# slashes
# replace slashes in solution cases with @ for now!! - only need 1st number (2nd ignore)
# same thing with str_replace
meps = meps %>%
  mutate(Strength = case_when(grepl("/", Drug)==FALSE & grepl("/", Strength) == TRUE ~
                                str_replace(Strength, "/", "@"),
                              .default = Strength))

# do same for Units solution slashes
meps = meps %>%
  mutate(Units = case_when(grepl("/", Drug)==FALSE & grepl("/", Units) == TRUE ~
                                str_replace(Units, "/", "@"),
                              .default = Units))

meps = meps %>%
  separate_longer_delim(c(Drug, Strength, Units), delim="/")

# should we deal with IU and MEQ too? or folic acid with mg/mg

# get mass value per dosage for each solution (ADD IN Units TOO NOW??)
meps = meps %>%
  mutate(Strength = case_when(grepl("@",Strength)==FALSE ~
                                paste0(Strength, "@1"),
                              .default=Strength))
meps = meps %>%
  mutate(Strength = as.character(Strength))

meps = meps %>%
  separate_wider_delim(Strength, delim="@", names=c("mass", "volume"))

# fix extra punctuation that adds NAs in next line
meps = meps %>%
  mutate(mass = gsub(",", "", mass))

# FIX PERIODS AT END OF Strength, ok for now because it's drugs we remove anyway but
# will need to fix moving forward, Start here: meps$mass = str_replace_all(meps$mass, '$.','')

# come back to NA warning for next line and make sure fine
meps = meps %>%
  mutate(Strength=as.numeric(mass)) # you get this mass with each volume you take

# remove mass, volume columns and move Strength to original position
meps = subset(meps, select=-c(mass, volume))
meps = meps %>%
  relocate(Strength, .before=Units)

# replace ? in remaining Units
meps = meps %>%
  mutate(Units = case_when(grepl("@", Units)==TRUE ~ str_replace_all(Units, "@", "/"),
                              .default=Units))

# correct miscoded solution drugs (prescribed as % solutions)
meps = meps %>%
  mutate(Strength = case_when(Drug=="TRAVOPROST" & Units=="ML" ~
                                0.004,
                              Drug=="CIPROFLOXACIN" & Units=="ML" ~
                                0.3,
                              Drug=="NEOMYCIN" ~
                                0.35, 
                              .default=Strength))
meps = meps %>%
  mutate(Units = case_when(Units == "ML" ~ "%",
                              .default=Units))
  
# get list of all drugs listed in Drug
drugs = unique(meps$Drug)

# NOTE: removed drugs based on primary usage in Google search...

# remove missing drug names from drug list
drugs = na.omit(drugs)

# remove vitamins, minerals, compounds from food, etc
food_drugs = c("OMEGA","3 POLYUNSATURATED FATTY ACIDS","CYANOCOBALAMIN",
               "MULTIVITAMIN WITH MINERALS","VITAMIN E","POTASSIUM CHLORIDE",
               "ERGOCALCIFEROL","MAGNESIUM OXIDE","FERROUS SULFATE","FOLIC ACID",
               "THIAMINE","CHOLECALCIFEROL","CRANBERRY","BIOTIN","PYRIDOXINE",
               "MULTIVITAMIN WITH IRON","SODIUM CHLORIDE","CALCITRIOL",
               "CALCIUM", "VITAMIN D","POTASSIUM CITRATE","ASCORBIC ACID","PSYLLIUM",
               "CALCIUM CARBONATE","MAGNESIUM CITRATE","MULTIVITAMIN, PRENATAL",
               "MULTIVITAMIN","VITAMINS A, D, AND E TOPICAL","LITHIUM",
               "ZINC GLUCONATE","METHYLCELLULOSE","SODIUM BIPHOSPHATE","SODIUM PHOSPHATE",
               "FLUORIDE","NIACIN","SODIUM BICARBONATE","MULTIVITAMIN WITH FLUORIDE",
               "SODIUM CHLORIDE","CALCIUM CITRATE",
               "HYDROXYPROPYL METHYLCELLULOSE","CALCIUM ACETATE", "GLUCOSE",
               "ZINC SULFATE","POTASSIUM BICARBONATE","RIBOFLAVIN",
               "FERROUS FUMARATE","DIOSMIPLEX",
               "MAGNESIUM CHLORIDE","METHYLCOBALAMIN",
               "MAGNESIUM HYDROXIDE","CAFFEINE","CITRIC ACID",
               "MG OXIDE","AL HYDROXIDE","MG HYDROXIDE")
drugs = remove_drugs(drugs, food_drugs)

# remove hormones and other naturally occurring compounds in the body too
# note: alpha-lipoic acid should not be treated as a combo drug but is here since it is an 
# exception anyway
inbody_drugs = c("TESTOSTERONE","EPINEPHRINE","INSULIN REGULAR","INSULIN",
                 "THYROID DESICCATED","PANCRELIPASE","ADALIMUMAB","MELATONIN",
                 "BIFIDOBACTERIUM","LACTOBACILLUS","STREPTOCOCCUS","ERENUMAB",
                 "CHONDROITIN","GLUCOSAMINE","LIOTHYRONINE",
                 "INFLUENZA VIRUS VACCINE, INACTIVATED","LACTOBACILLUS ACIDOPHILUS",
                 "PROGESTERONE","ZOSTER VACCINE, INACTIVATED","LYSINE","EVOLOCUMAB",
                 "ALPHA","LIPOIC ACID","SACCHAROMYCES BOULARDII LYO",
                 "BIFIDOBACTERIUM INFANTIS","UREA TOPICAL","UBIQUINONE",
                 "DEHYDROEPIANDROSTERONE (PRASTERONE)","ENOXAPARIN")
drugs = remove_drugs(drugs, inbody_drugs)

# Quarantine to check if synthetic distinguishable from natural:
# NOTE: some might be missing, will be coming back and sorting with big list of 
# natural substances and synthetic versions (check classes too, i.e. antibodies)
synthetic_drugs = c("LEVOTHYROXINE","INSULIN ASPART","INSULIN DETEMIR","INSULIN LISPRO",
                    "INSULIN GLARGINE","INSULIN ASPART PROTAMINE",
                    "INSULIN ISOPHANE","ETHINYL ESTRADIOL","NORETHINDRONE",
                    "DESOGESTREL","TAMOXIFEN","NORGESTIMATE","DEXAMETHASONE",
                    "LIRAGLUTIDE","SITAGLIPTIN","ETONOGESTREL","LEVONORGESTREL",
                    "DULAGLUTIDE","ESTRADIOL","DROSPIRENONE","INSULIN DEGLUDEC",
                    "MEDROXYPROGESTERONE","SEMAGLUTIDE","PREDNISOLONE",
                    "HYDROCORTISONE","LINAGLIPTIN","NORGESTREL",
                    "INSULIN ISOPHANE (NPH)","CICLESONIDE",
                    "ETHYNODIOL","MOMETASONE","NORELGESTROMIN","ALOGLIPTIN","BUDESONIDE",
                    "TRIAMCINOLONE")
drugs = remove_drugs(drugs, synthetic_drugs)

# remove vague names
# some things treated as combo drugs that are not if they contain dash/slash - ok since they 
# are removed anyway (or nasal/ophthalmic/otic might similarly be missing)
vague_drugs = c("MISCELLANEOUS AGENTS","BRONCHODILATOR COMBINATIONS","ANTI","INFECTIVES",
                "CENTRAL NERVOUS SYSTEM AGENTS","ANTIMIGRAINE AGENTS",
                "GASTROINTESTINAL AGENTS","DIURETICS","ANGIOTENSIN II INHIBITORS",
                "URINARY ANTISPASMODICS","ANTINEOPLASTIC HORMONES","ANTINEOPLASTICS",
                "DERMATOLOGICAL AGENTS","TOPICAL STEROIDS","IMMUNOLOGIC AGENTS",
                "ANTIVIRAL AGENTS","CARDIOVASCULAR AGENTS",
                "ACE INHIBITORS WITH THIAZIDES","ANTIGOUT AGENTS","METABOLIC AGENTS",
                "TOPICAL AGENTS","H2 ANTAGONISTS","HORMONES","HORMONE MODIFIERS",
                "SEX HORMONES","MISCELLANEOUS CARDIOVASCULAR AGENTS",
                "PREPARATIONS","INFLAMMATORY AGENTS",
                "NARCOTIC ANALGESICS","CGRP INHIBITORS","PROTON PUMP INHIBITORS",
                "TOPICAL ANTIFUNGALS","CHOLINESTERASE INHIBITORS",
                "ELECTROLYTE REPLACEMENT SOLUTIONS, ORAL","ANTIDIABETIC AGENTS",
                "ANTIHYPERLIPIDEMIC AGENTS","BARBITURATE ANTICONVULSANTS",
                "ANTICONVULSANTS","PSYCHOTHERAPEUTIC AGENTS",
                "ANTICHOLINERGIC ANTIEMETICS","PLATELET AGGREGATION INHIBITORS",
                "AMEBICIDES","THIAZIDE AND THIAZIDE","LIKE DIURETICS",
                "ANTIDIABETIC COMBINATIONS","ANTIVIRAL COMBINATIONS",
                "RESPIRATORY AGENTS","ANTIHYPERTENSIVE COMBINATIONS",
                "NARCOTIC ANALGESIC COMBINATIONS","GROUP III ANTIARRHYTHMICS",
                "OCULAR LUBRICANT","FUNCTIONAL BOWEL DISORDER AGENTS",
                "ANGIOTENSIN CONVERTING ENZYME (ACE) INHIBITORS",
                "TRICYCLIC ANTIDEPRESSANTS","EMOLLIENTS, TOPICAL",
                "MISCELLANEOUS ANXIOLYTICS, SEDATIVES AND HYPNOT",
                "CARBONIC ANHYDRASE INHIBITORS","THIRD GENERATION CEPHALOSPORINS",
                "CNS STIMULANTS","PHENOTHIAZINE ANTIEMETICS","INHALED CORTICOSTEROIDS",
                "ANTIADRENERGIC AGENTS, PERIPHERALLY ACTING",
                "DOPAMINERGIC ANTIPARKINSONISM AGENTS","ANTIFUNGALS","IRON PRODUCTS",
                "SMOKING CESSATION AGENTS","SKELETAL MUSCLE RELAXANTS",
                "NUTRACEUTICAL PRODUCTS","GENITOURINARY TRACT AGENTS",
                "GROUP II ANTIARRHYTHMICS","GLUCOCORTICOIDS","AZOLE ANTIFUNGALS",
                "BETA BLOCKERS WITH THIAZIDES","ANALGESIC COMBINATIONS",
                "ANGIOTENSIN II INHIBITORS WITH THIAZIDES","COAGULATION MODIFIERS",
                "ADRENERGIC BRONCHODILATORS","POLYENES","FIRST GENERATION CEPHALOSPORINS",
                "STEROIDS WITH ANTI","BENZODIAZEPINE ANTICONVULSANTS",
                "ANTIANGINAL AGENTS","MISCELLANEOUS ANTIHYPERTENSIVE COMBINATIONS",
                "POTASSIUM SPARING DIURETICS WITH THIAZIDES",
                "ANTIEMETIC","ANTIVERTIGO AGENTS","MISCELLANEOUS ANTINEOPLASTICS",
                "CONJUGATED ESTROGENS","CONJUGATED ESTROGENS TOPICAL",
                "ANTICHOLINERGICS","ANTISPASMODICS","MULTIKINASE INHIBITORS",
                "AGENTS FOR PULMONARY HYPERTENSION","BRONCHODILATORS","VASODILATORS",
                "MISCELLANEOUS GENITOURINARY TRACT AGENTS",
                "NONSTEROIDAL ANTI","MISCELLANEOUS ANTIBIOTICS",
                "ANTIMETABOLITES","ANTIDEPRESSANTS","ANTIDOTES",
                "MISCELLANEOUS CENTRAL NERVOUS SYSTEM AGENTS","GROUP I ANTIARRHYTHMICS",
                "BISPHOSPHONATES","NARCANALGESICS","NARCANALGESIC COMBINATIONS")
drugs = remove_drugs(drugs, vague_drugs)

# remove pharmaceuticals not in PubChem
no_pubchem = c("HORSE CHESTNUT","POLYETHYLENE GLYCOL 3350",	
              "POLYETHYLENE GLYCOL 3350 WITH ELECTROLYTES","POLYCARBOPHIL","POLYMYXIN B")
drugs = remove_drugs(drugs, no_pubchem)

# remove pharmaceuticals with insufficient info in CompTox
no_comptox = c("KETOTIFEN")
drugs = remove_drugs(drugs, no_comptox)

# remove topicals - assume all are vague amt's for now (can add in pumps, etc later)
# may want to double check these for substances in food, natural to body, etc
# see what % of drugs are topical and what topicals have other forms of administration

# remove topicals based on name
for (drug in drugs){
  if (grepl("TOPICAL", drug)==TRUE){
    drugs = remove_drugs(drugs, drug)
  }
}

# read in solubility, density data from CompTox and clean up
solub_dens_data = read_excel("batch_search_comptox_8_3_23.xlsx",
                             sheet=3)

# filter for desired data only
solub_dens_data = solub_dens_data %>%
  filter(NAME=="Density" | NAME=="Water Solubility")

# find NA's:
solub_dens_data_na = solub_dens_data %>%
  filter(is.na(VALUE)==TRUE) %>%
  mutate(Median_Density=NA) %>%
  mutate(Median_Solubility=NA)

# remove NA's from main df:
solub_dens_data = solub_dens_data %>%
  filter(is.na(VALUE)==FALSE)

# get median density and solubility values
solub_dens_data = solub_dens_data %>%
  mutate(VALUE = as.numeric(VALUE)) %>%
  group_by(DTXSID) %>%
  mutate(Median_Density = case_when(NAME=="Density" ~ median(VALUE))) %>%
  mutate(Median_Solubility = case_when(NAME=="Water Solubility" ~ median(VALUE)))

# convert VALUE column to char for both df's to combine
solub_dens_data_na = solub_dens_data_na %>%
  mutate(VALUE=as.character(VALUE)) 

solub_dens_data = solub_dens_data %>%
  mutate(VALUE=as.character(VALUE))

# add NA's back in:
solub_dens_data = rbind(solub_dens_data, solub_dens_data_na)

# fix data type on VALUE column:
solub_dens_data = solub_dens_data %>%
  mutate(VALUE = case_when(is.na(VALUE)==FALSE ~ as.numeric(VALUE))) # no default
#needed here since want NA's (will cause error if you try b/c diff data types in VALUE)

# make new data frames for density and solubility
dens_cols = c("DTXSID", "NAME", "UNITS", "Median_Density")
sol_cols = c("DTXSID", "NAME", "UNITS", "Median_Solubility")
densities = as.data.frame(solub_dens_data[,dens_cols])
solubilities = as.data.frame(solub_dens_data[,sol_cols])

# remove NA values & duplicate rows
densities = densities %>%
  filter(NAME!="Water Solubility")
densities = densities %>% distinct()
solubilities = solubilities %>%
  filter(NAME!="Density")
solubilities = solubilities %>% distinct()

# # compile lists of missing drugs
# missing_drugs = c("FLUTICASONE","SOLIFENACIN","DOCUSATE","DOXYCYCLINE","DOXEPIN",
#                    "DEXLANSOPRAZOLE","RIVAROXABAN","APIXABAN","METHSCOPOLAMINE","TIMOLOL",
#                    "LINACLOTIDE","TIOTROPIUM","PITAVASTATIN","UMECLIDINIUM","CLINDAMYCIN",
#                    "BECLOMETHASONE","BENZTROPINE","DIVALPROEX","EMPAGLIFLOZIN",
#                    "LEVETIRACETAM","MINOCYCLINE","VORTIOXETINE","BISACODYL","DAPAGLIFLOZIN",
#                    "MIRABEGRON","DABIGATRAN","LEVOCETIRIZINE","LEVALBUTEROL","PHENAZOPYRIDINE",
#                    "HYOSCYAMINE","LURASIDONE","CANAGLIFLOZIN","VARENICLINE","ERTUGLIFLOZIN",
#                    "SENNA","BEPOTASTINE","DEXTROMETHORPHAN","SUCRALFATE")
#  
# # add the 6 tricky drugs to missing (from above - see iPad):
# missing_drugs = c(missing_drugs,"SULFASALAZINE","COLCHICINE","NITROFURANTOIN",
#                    "PENICILLIN V POTASSIUM","CYCLOSPORIN A","BISMUTH SUBSALICYLATE", "CYCLOSPORINE")
# # note: cyclosporin A, cyclosporine are same! - fix later
#
# 
# drugs = remove_drugs(drugs, missing_drugs)

# ADD MISSING DENSITIES FROM ANOTHER SOURCE LATER?? more efficient way?

# fix order of densities, solubilities to be order of drugs (correct order)

# read in correct order of ID's
correct = read_excel("correct_order.xlsx", sheet=1)

reorder_index_dens = match(correct$DTXSID, densities$DTXSID)
reorder_index_sol = match(correct$DTXSID, solubilities$DTXSID)

densities = densities[reorder_index_dens,]
solubilities = solubilities[reorder_index_sol,]

# add in names and molar masses for correct ordering of drugs
name_mass = read_excel("names_masses_correct_order.xlsx", sheet=1)

densities = cbind(densities, name_mass)
solubilities = cbind(solubilities, name_mass)

# clean up density and solubility data frames
densities = subset(densities, select=-c(NAME))
densities = subset(densities, select = -c(UNITS))
colnames(densities)[colnames(densities)=="Median_Density"] = "Median_Density (g per mL)"
densities = densities %>%
  relocate(Names, .before=DTXSID) %>%
  mutate(Names = toupper(Names))

solubilities = solubilities %>%
  mutate(`Masses (g per mol)`=as.numeric(`Masses (g per mol)`))

solubilities = subset(solubilities, select=-c(NAME))
solubilities = solubilities %>%
  relocate(Names, .before=DTXSID) %>%
  mutate(Names = toupper(Names)) %>%
  mutate(Median_Solubility = as.numeric(Median_Solubility)*as.numeric(`Masses (g per mol)`)/1000) # converts to g/mL
solubilities = subset(solubilities, select = -c(UNITS))
colnames(solubilities)[colnames(solubilities)=="Median_Solubility"] = "Median_Solubility (g per mL)"
  
# USE COL NAMES INSTEAD COL NUMBERS WHEN UNCOMMENTING BELOW
# ALSO: CHANGE SOLID/LIQUID TO 1 and 2 and CHECK FOR SLASHES AND SUCH
# make sure all drugs included (incl. special dens/sol cases)
# # find CID numbers 
# cid = get_cid(drugs, match="first")[2]
# # below: need to extract data out of df as list for position 1!!!!
# states = na.omit(pc_sect(as.data.frame(cid[1:272,])[1:272,],"Physical Description"))
# states = as.data.frame(states)
# 
# # ADD BELOW BACK IN AFTER (eliminate these 6 for now - see iPad for list):
# # fix entries that aren't listed as solid, liquid or gas explicitly
# # states[75,3] = "Solid"
# # states[114,3] = "Solid"
# # states[128,3] = "Solid"
# # states[151,3] = "Solid"
# # states[202,3] = "Solid"
# # states = states[-203,3]
# # states = as.data.frame(states)
# # states[280,3] = "Solid"
# # states = states[c(-281,-282),3]
# # states = as.data.frame(states)
# 
# # remove duplicate hits that don't explicitly give state
# states = states %>%
#   filter(Result=="Solid" | Result=="Liquid")
# 
# states_names = states$Name
# states = states$Result


# 
# # get indices where drugs are missing
# missing_ind = which(drugs %in% missing_drugs)
# 
# # remove missing drugs - SEE IF BETTER WAY!
# drugs = drugs[! drugs %in% missing_drugs]
# 
# # remove missing CIDs
# cid = cid %>% filter(!row_number() %in% missing_ind)

# KEEP BELOW FOR NOW
# find averages for imputing! - may want to change to all NA's?? or at least same missing?
#drug_indices = meps[meps$Drug %in% drugs == TRUE,] # records without missing drugs (compare
# later with meps_clean, may be more efficient)
# SEE INSIDE LOOP - avg for each drug instead of same global avg

# initialize larger data frame to store cleaned data
meps_clean = vector("list", length = length(drugs))

# loop through drug list and do data cleaning
for (i in 1:length(drugs)){
  drug_data = meps %>%
    filter(Drug==drugs[i]) %>%
    #mutate(cid = case_when(Drug==drugs[i] ~ cid[i,])) %>%
    #mutate(state = case_when(Drug==drugs[i] ~ states[i])) %>%
    mutate(Density = case_when(Drug==drugs[i] ~ densities$`Median_Density (g per mL)`[i])) %>%
    mutate(Molar_Mass = case_when(Drug==drugs[i] ~ densities$`Masses (g per mol)`[i])) %>%
    mutate(Solubility = case_when(Drug==drugs[i] ~ solubilities$`Median_Solubility (g per mL)`[i])) 
  
  # avg DAYSUP and QUANTY values for each drug (to be used for imputing)
  not_missing_daysup = drug_data[is.na(drug_data$Day_Supply)==FALSE,]
  avg_daysup = sum(as.numeric(not_missing_daysup$Day_Supply))/length(not_missing_daysup$Day_Supply)
  not_missing_quanty = drug_data[is.na(drug_data$Quantity)==FALSE,]
  avg_quanty = sum(as.numeric(not_missing_quanty$Quantity))/length(not_missing_quanty$Quantity)
  
  # remove entries where strength is missing (only 1 entry left after processing)
  drug_data = subset(drug_data, is.na(drug_data$Strength)==FALSE)

  # remove entries where Day_Supply missing for >95% of entries (imputing not reliable)
  # (check size of each being deleted manually)
   if (sum(is.na(drug_data$Day_Supply)==TRUE) > 0.95*dim(drug_data)[1]){
     drug_data = subset(drug_data, is.na(drug_data$Day_Supply)==FALSE)
   }
   else {
    drug_data = drug_data %>%
      mutate(Day_Supply = case_when(is.na(Day_Supply)==TRUE ~ avg_daysup,
                                  .default=as.numeric(Day_Supply)))
   }

  # remove drugs where Quantity missing for >95% of entries (imputing not reliable)
  # (check size of each being deleted manually)
   if (sum(is.na(drug_data$Quantity)==TRUE) > 0.95*dim(drug_data)[1]){
     drug_data = subset(drug_data, is.na(drug_data$Quantity)==FALSE)
   }
   else {
    drug_data = drug_data %>%
      mutate(Quantity = case_when(is.na(Quantity)==TRUE ~ avg_quanty,
                                  .default=as.numeric(Quantity)))
   }
  


  # UNITS PROCESSING
  
  # convert strength, Day_Supply, quantity to correct data type
  drug_data = drug_data %>%
    mutate(Day_Supply = as.numeric(Day_Supply)) %>%
    mutate(Quantity = as.numeric(Quantity)) %>%
    mutate(Strength = as.numeric(Strength))
  
  # assume: for those that are per act, per inh, per spray, should account for 
  # full daily dose b/c we account for frequency per day - so we don't need to do
  # anything 'extra' with those units - COME BACK TO THIS - if Quantity < Day_Supply, may not
  # take every day!! but that's ok because you can say an avg of 1.6 pills/day for example
  # EXCEPTION: patches since they are released continuously - so we need a way to except them
  # - use PT24, PTWK! assume that's all patches in data
  # also, assume 12-hr capsules/tablets are accounted for too!


  # Conversions to correct units in Strength column
  
  # convert MG to MCG
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "MG" ~ Strength*1000,
                                .default=Strength))

  # convert MG/ML to MCG
  # NOTE: no combo drugs with MG/ML units (single only)
  # example: 250 mg / 5 mL is 250 mg of drug in 5 mL solution per dose
  # so we have to fix dosages to not be divided (we get 5 mL and 250 mg each time, not 50 mg
  # each time in 1 mL dose)
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "MG/ML" ~ Strength*1000,
                                .default=Strength))

  # convert MCG/HR to MCG
  # Note: this is for a patch, so we will multiply by 24, and that's daily dosage
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units=="MCG/HR" ~ Strength*24,
                                .default=Strength))

  # Convert % to MCG
  # Note: 1% solution means 1 g of drug per 100 mL solution (% = g drug / mL
  # solution) - this would be an assumption (cite paper on % confusion), 1%=0.01=1g/100mL
  
  # same assumption as mg/mL conversions, e.g. 6% is 6 g/100mL, that is each dose has 6 g
  # active ingredient per 100 mL solution (we multiply by 1 million to convert to mcg/mL),
  # as before, we can ignore the volume and just care about the mass 
  drug_data = drug_data %>%
    mutate(Strength = case_when(Units == "%" ~ Strength*1000000,
                                .default=Strength))

  # Convert G/ML to MCG
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
  
  # change all remaining units (Units) to MCG
  drug_data = drug_data %>%
    mutate(Units = "MCG")

################################################################################
  # add cleaned data for each drug to its own data frame
  meps_clean[[i]] = drug_data
}

# bind each data frame for each drug into giant data frame
meps_clean = do.call(rbind, meps_clean)

# remove Units (all same units now)
meps_clean = subset(meps_clean, select=-c(Units))

# # fix densities and solubilities info that got randomly deleted (ADD BACK IN BUT BETTER)
# meps_clean[186715:186719,11] = 0.0072695
# meps_clean[186715:186719,12] = "g/mL"
# meps_clean[186715:186719,13] = 344.411
# meps_clean[186715:186719,14] = 0.002503696
# meps_clean[186715:186719,15] = "g/mL"
# 
# meps_clean[186720,11] = 0.170373
# meps_clean[186720,12] = "g/mL"
# meps_clean[186720,13] = 135.21
# meps_clean[186720,14] = 0.02303613
# meps_clean[186720,15] = "g/mL"
# 
# meps_clean[186721,11] = 0.170373
# meps_clean[186721,12] = "g/mL"
# meps_clean[186721,13] = 135.21
# meps_clean[186721,14] = 0.02303613
# meps_clean[186721,15] = "g/mL"

# calculate daily frequency (except the patches! these are already calculated as rates)
meps_clean = meps_clean %>%
  mutate(Daily_Frequency = case_when(Form != "PTWK" & Form != "PT24" ~ Quantity/Day_Supply,
                               Form == "PTWK" ~ 1,
                               Form == "PT24" ~ 1))

# calculate daily dosage
meps_clean = meps_clean %>%
  mutate(Daily_Dosage = Strength*Daily_Frequency)

# find sum for each drug and put into descending order of DDD
total_daily_dosages = aggregate(meps_clean$Daily_Dosage,list(meps_clean$Drug),FUN=sum)
colnames(total_daily_dosages) = c("Pharmaceutical","Defined Daily Dosage (ug/day)")
total_daily_dosages = total_daily_dosages %>%
  arrange(desc(`Defined Daily Dosage (ug/day)`))

# find total number of prescriptions per drug
total_prescribed = meps_clean %>%
  count(Drug) %>%
  arrange(desc(n))
colnames(total_prescribed) = c("Pharmaceutical", "# Prescriptions")

# Check for NA's
# print(sum(is.na(meps_clean)))
# which(is.na(meps_clean), arr.ind=TRUE)
# all missing values in dens/sol/MM/FRMUNT/NDC - do they matter?