setwd("C:/Users/vgmay/research/CMBMGEM/cmbmgem/CMBMGEM-research-group/content/project/pharmaceuticals")

# look into str_replace usage in testing

# check code for correctness too!!

# Helpful: View(meps[grepl("-", meps$RXSTRENG)==TRUE & meps$RXSTRENG != -15,])

# switch to text file (after fixing csv) - check if MCG/MG metformin units fix when NDC not cut off
# and check about NA's in RXSTRENG from dates

# check about drugs to add back in and assumptions to fix as cleaning/improving/testing code

# PTWK is a patch for 1 week, these types of rxforms may be helpful for long term vs
# short term drugs

# NOTE: RXSTRUNT is amt active ingredient (what we care about), RXFRMUNT is 
# amt of total medication (including inactive ingredients)

# CHECK ALL CODE BEFORE MOVING TO NEXT PART TO ENSURE NO ERRORS (UNIT TESTS?) - wait till Python,
# that is: switch to Python first!

# after units all handled, just do 1 line for switch to MCG
# may want to time code / check efficiency and through code in general before Python switch

# 222 drugs total currently

# later: will want to add back: ipratropium, valsartan, cerfuroxime, bupropion, 
# glimepiride - these are the drugs that have only solubilities listed, have solid
# state, and are not present in properties output from CompTox (except ipratropium
# but this is still missing density)

# state, density, solubility are assumed to be at room temp

install.packages("data.table")
install.packages('tidyverse')
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringi")
install.packages("stringr")
install.packages("webchem")
install.packages("rlang")
install.packages("MASS")
install.packages("readxl")
library("webchem")
library("MASS")
library("rlang")
library("stringr")
library("stringi")
library("tidyr")
library("data.table")
library('dplyr')
library("ggplot2")
library("readxl")

# load functions
source("meps_functions.R")

# load dataset (note: all -15's replaced with NOCOMPUTE and all -8's replaced with
# DK in the loaded csv file)
meps = read.csv(file="MEPS_data_2020.csv") #stringsAsFactors=TRUE

# extract desired data
meps = meps %>%
  select(RXDRGNAM:RXDAYSUP)

# remove ophthalmic, otic, and nasal labels from drug names from dataset and drug vector
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, " OPHTHALMIC", "")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, "OPHTHALMIC ", "")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, " OTIC", "")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, "OTIC ", "")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, " NASAL", "")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, "NASAL ", "")

# fix drug names
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "BROMPHENIRAMINE/DEXTROMETHORPHAN/PSE",
                                 "BROMPHENIRAMINE/DEXTROMETHORPHAN/PSEUDOEPHEDRINE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "CITRIC ACID/MG OXIDE/NA PICOSULFATE",
                                 "CITRIC ACID/MAGNESIUM OXIDE/SODIUM PICOSULFATE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "BROMPHENIRAMINE/DEXTROMETHORPH/PHENYLEPHRINE",
                                 "BROMPHENIRAMINE/DEXTROMETHORPHAN/PHENYLEPHRINE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "DEXCHLORPHENIRAMINE/DEXTROMETHORPHAN/PE",
                                 "DEXCHLORPHENIRAMINE/DEXTROMETHORPHAN/PHENYLEPHRINE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, # these are the same thing
                                 "5-AMINOSALICYLATES",
                                 "MESALAMINE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "CHLOPHEDIANOL/DEXBROMPHENIRAMINE/PSE",
                                 "CHLOPHEDIANOL/DEXBROMPHENIRAMINE/PSEUDOEPHEDRINE")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, # these are the same thing
                            "BACITRACIN",
                            "BACITRACIN A")
meps$RXDRGNAM = str_replace(meps$RXDRGNAM, # these are the same thing
                            "DIVALPROEX SODIUM",
                            "DIVALPROEX")

# replace slashes in this drug with dashes to match delim in RXSTRENG
meps$RXDRGNAM = str_replace(meps$RXDRGNAM,
                                 "HYDROCORTISONE/NEOMYCIN/POLYMYXIN B OTIC",
                                 "HYDROCORTISONE-NEOMYCIN-POLYMYXIN B OTIC")

# remove rows with missing data (WILL NEED TO UPDATE FOR IMPUTING!!)
# meps = subset(meps, meps$RXSTRENG != "NOCOMPUTE")
# meps = subset(meps, meps$RXSTRENG != "DK")
# meps = subset(meps, meps$RXDAYSUP != "REFUSED")
# meps = subset(meps, meps$RXDAYSUP != "DK")
# meps = subset(meps, meps$RXQUANTY != "NOCOMPUTE")

# remove topical RXFORMs
meps = meps[meps$RXFORM != "CREA",]
meps = meps[meps$RXFORM != "OINT",]
meps = meps[meps$RXFORM != "OIN",]
meps = meps[meps$RXFORM != "LOTN",]
meps = meps[meps$RXFORM != "LOT",]
meps = meps[meps$RXFORM != "SWAB",]
meps = meps[meps$RXFORM != "CRE",]
meps = meps[meps$RXFORM != "PSTE",]
meps = meps[meps$RXFORM != "PAS",]
meps = meps[meps$RXFORM != "GEL",]
meps = meps[meps$RXFORM != "SHAM",]

# remove entries with vague or missing units (can't impute strings)
meps = meps[meps$RXSTRUNT != "NOCOMPUTE",]
meps = meps[meps$RXSTRUNT != "OTHER",]
meps = meps[meps$RXSTRUNT != "U/ML",]
meps = meps[meps$RXSTRUNT != "UNIT",]
meps = meps[meps$RXSTRUNT != "UNIT/ML",]
meps = meps[meps$RXSTRUNT != "MCG/OTHER",]
meps = meps[meps$RXSTRUNT != "%/OTHER",]

# remove entries with missing RXFORM (can't tell solubility,etc)
meps = meps[meps$RXFORM != "OTHER",]

# fix miscoded RXSTRENG before separation

# fix miscoded drug Cefdinir (250 mg/5mL - just divide)
# note: only Cefdinir has this strength value
meps$RXSTRENG = str_replace(meps$RXSTRENG, "250-5", "50")

# fix miscoded drug strength (should be dashes only)
meps$RXSTRENG = str_replace(meps$RXSTRENG, "160-4.5/1", "160-4.5")

# fix miscoded drug strength (too many slashes)
meps$RXSTRENG = str_replace(meps$RXSTRENG, "1.25/1.25/1.25/1.25", "1.25/1.25")
meps$RXSTRENG = str_replace(meps$RXSTRENG, "2/1/20/1", "2/20")
meps$RXSTRENG = str_replace(meps$RXSTRENG, "70/1/30/1", "70/30") 

# fix miscoded drug strength (-8 got replaced)
meps$RXSTRENG = str_replace(meps$RXSTRENG, "50DK.6", "50-8.6")

# fix miscoded albuterol
meps$RXSTRENG = str_replace(meps$RXSTRENG, "108/1", "90")

# fix albuterol dosage (108 mcg albuterol sulfate = 90 mcg albuterol)
# note: only albuterol has this strength value
meps$RXSTRENG = str_replace(meps$RXSTRENG, "108", "90")
# look at 10 drugs to see if others like this

# perform separations for slash and dash cases

# change dash delimiters in RXDRGNAM to match slash delimiters in associated RXSTRENG
meps = meps %>%
  mutate(RXDRGNAM = case_when(grepl("-", RXDRGNAM)==TRUE & grepl("/", RXSTRENG) == TRUE ~
                                str_replace(RXDRGNAM, "-", "/"),
                              # is.na(RXDRGNAM) ~ "nope", THIS IS A CHECK
                              .default = RXDRGNAM))

# separate the combo active ingredients for dash delimiters
meps = meps %>%
  separate_longer_delim(c(RXDRGNAM, RXSTRENG), delim="-")

# slashes
# replace slashes in concentration cases with % for now!!
meps = meps %>%
  mutate(RXSTRENG = case_when(grepl("/", RXDRGNAM)==FALSE & grepl("/", RXSTRENG) == TRUE ~
                                str_replace(RXSTRENG, "/", "%"),
                              #is.na(RXSTRENG) ~ "nope", THIS IS A TEST
                              .default = RXSTRENG))

meps = meps %>%
  separate_longer_delim(c(RXDRGNAM, RXSTRENG), delim="/")

# divide concentrations RXSTRENG's - I think this is wrong now but SAVE CODE FOR NOW!!
meps = meps %>%
  mutate(RXSTRENG = case_when(grepl("%",RXSTRENG)==FALSE ~
                                paste0(RXSTRENG, "%1"),
                              .default=RXSTRENG))
meps = meps %>%
  mutate(RXSTRENG = as.character(RXSTRENG))

meps = meps %>%
  separate_wider_delim(RXSTRENG, delim="%", names=c("mass", "volume")) %>%
  #mutate(RXSTRENG=as.numeric(mass)/as.numeric(volume))
  mutate(RXSTRENG=as.numeric(mass)) # you get this mass with each volume you take
  
# get list of all drugs listed in RXDRGNAM
drugs = unique(meps$RXDRGNAM)

# NOTE: removed drugs based on primary usage in Google search...

# remove missing drug names from drug list (represented by NOCOMPUTE, DK)
missing_drugs = c("NOCOMPUTE", "DK")
drugs = remove_drugs(drugs, missing_drugs)

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

#preCount = solub_dens_data %>% count(DTXSID)

# filter for desired data only
solub_dens_data = solub_dens_data %>%
  filter(NAME=="Density" | NAME=="Water Solubility")

#postCount = solub_dens_data %>% count(DTXSID)

# get median density and solubility values
solub_dens_data = solub_dens_data %>%
  mutate(VALUE = as.numeric(VALUE)) %>%
  group_by(DTXSID) %>%
  mutate(Median_Density = case_when(NAME=="Density" ~ median(VALUE))) %>%
  mutate(Median_Solubility = case_when(NAME=="Water Solubility" ~ median(VALUE)))

# make new data frames for density and solubility
densities = as.data.frame(solub_dens_data[,c(1,3,5,6)])
solubilities = as.data.frame(solub_dens_data[,c(1,3,5,7)])

# remove NA values & duplicate rows
densities = na.omit(densities)
densities = densities %>% distinct()
solubilities = na.omit(solubilities)
solubilities = solubilities %>% distinct()

# remove missing drugs from solubilities for now (add back into main df after sol/dens sep)
solubilities = solubilities %>%  # ipratropium (missing from densities)
  filter(DTXSID != "DTXSID9048437")

# fix order of densities, solubilities to be order of drugs (correct order)

# read in correct order of ID's
correct = read_excel("correct_order.xlsx", sheet=1)

reorder_index = match(correct$DTXSID, densities$DTXSID)

densities = densities[reorder_index,]
solubilities = solubilities[reorder_index,]

# add in names and molar masses for correct ordering of drugs
name_mass = read_excel("names_masses_correct_order.xlsx", sheet=1)

densities = cbind(densities, name_mass)
solubilities = cbind(solubilities, name_mass)

# clean up density and solubility data frames
densities = densities[,-2]
densities = densities %>%
  relocate(UNITS, .after=Median_Density)
densities = densities %>%
  relocate(Names, .before=DTXSID) %>%
  mutate(Names = toupper(Names)) %>%
  mutate(UNITS = "g/mL")

solubilities = solubilities[,-2]
solubilities = solubilities %>%
  relocate(UNITS, .after=Median_Solubility)
solubilities = solubilities %>%
  relocate(Names, .before=DTXSID) %>%
  mutate(Names = toupper(Names)) %>%
  mutate(Median_Solubility = (Median_Solubility*`Masses (g/mol)`)/1000) %>% # converts to g/mL
  mutate(UNITS = "g/mL")
  
# find CID numbers 
cid = get_cid(drugs, match="first")[2]
# below: need to extract data out of df as list for position 1!!!!
states = na.omit(pc_sect(as.data.frame(cid[1:272,])[1:272,],"Physical Description"))
states = as.data.frame(states)

# ADD BELOW BACK IN AFTER (eliminate these 6 for now - see iPad for list):
# fix entries that aren't listed as solid, liquid or gas explicitly
# states[75,3] = "Solid"
# states[114,3] = "Solid"
# states[128,3] = "Solid"
# states[151,3] = "Solid"
# states[202,3] = "Solid"
# states = states[-203,3]
# states = as.data.frame(states)
# states[280,3] = "Solid"
# states = states[c(-281,-282),3]
# states = as.data.frame(states)

# remove duplicate hits that don't explicitly give state
states = states %>%
  filter(Result=="Solid" | Result=="Liquid")

states_names = states$Name
states = states$Result

missing_drugs = c("FLUTICASONE","SOLIFENACIN","DOCUSATE","DOXYCYCLINE","DOXEPIN",
                  "DEXLANSOPRAZOLE","RIVAROXABAN","APIXABAN","METHSCOPOLAMINE","TIMOLOL",
                  "LINACLOTIDE","TIOTROPIUM","PITAVASTATIN","UMECLIDINIUM","CLINDAMYCIN",
                  "BECLOMETHASONE","BENZTROPINE","DIVALPROEX","EMPAGLIFLOZIN",
                  "LEVETIRACETAM","MINOCYCLINE","VORTIOXETINE","BISACODYL","DAPAGLIFLOZIN",
                  "MIRABEGRON","DABIGATRAN","LEVOCETIRIZINE","LEVALBUTEROL","PHENAZOPYRIDINE",
                  "HYOSCYAMINE","LURASIDONE","CANAGLIFLOZIN","VARENICLINE","ERTUGLIFLOZIN",
                  "SENNA","BEPOTASTINE","DEXTROMETHORPHAN","SUCRALFATE")

# add the 6 tricky drugs to missing (from above - see iPad):
missing_drugs = c(missing_drugs,"SULFASALAZINE","COLCHICINE","NITROFURANTOIN",
                  "PENICILLIN V POTASSIUM","CYCLOSPORIN A","BISMUTH SUBSALICYLATE", "CYCLOSPORINE")
# note: cyclosporin A, cyclosporine are same! - fix later

# add to missing (to add back in main df): 
missing_drugs = c(missing_drugs, "IPRATROPIUM", "VALSARTAN", "CEFUROXIME", "BUPROPION",
                  "GLIMEPIRIDE")

# get indices where drugs are missing
missing_ind = which(drugs %in% missing_drugs)

# remove missing drugs
drugs = drugs[! drugs %in% missing_drugs]

# remove missing CIDs
cid = cid %>% filter(!row_number() %in% missing_ind)

# find averages for imputing! - may want to change to all NA's?? or at least same missing?
#drug_indices = meps[meps$RXDRGNAM %in% drugs == TRUE,] # records without missing drugs (compare
# later with meps_clean, may be more efficient)
# SEE INSIDE LOOP - avg for each drug instead of same global avg

# initialize larger data frame to store cleaned data
meps_clean = vector("list", length = length(drugs))

# loop through drug list and do data cleaning
for (i in 1:length(drugs)){
  drug_data = meps %>%
    filter(RXDRGNAM==drugs[i]) %>%
    mutate(cid = case_when(RXDRGNAM==drugs[i] ~ cid[i,])) %>%
    mutate(state = case_when(RXDRGNAM==drugs[i] ~ states[i])) %>%
    mutate(density = case_when(RXDRGNAM==drugs[i] ~ densities$Median_Density[i])) %>%
    mutate(density_units = case_when(RXDRGNAM==drugs[i] ~ densities$UNITS[i])) %>%
    mutate(molar_mass = case_when(RXDRGNAM==drugs[i] ~ densities$`Masses (g/mol)`[i])) %>%
    mutate(solubility = case_when(RXDRGNAM==drugs[i] ~ solubilities$Median_Solubility[i])) %>%
    mutate(solubility_units = case_when(RXDRGNAM==drugs[i] ~ solubilities$UNITS[i]))
  
  # avg DAYSUP and QUANTY values for each drug (to be used for imputing)
  not_missing_daysup = drug_data[drug_data$RXDAYSUP != "DK" & drug_data$RXDAYSUP != "REFUSED",]
  avg_daysup = sum(as.numeric(not_missing_daysup$RXDAYSUP))/length(not_missing_daysup$RXDAYSUP)
  not_missing_quanty = drug_data[drug_data$RXQUANTY != "NOCOMPUTE",]
  avg_quanty = sum(as.numeric(not_missing_quanty$RXQUANTY))/length(not_missing_quanty$RXQUANTY)
  
  # remove drugs where strength is missing (only 3 entries left after above processing)
  drug_data = subset(drug_data, drug_data$RXSTRENG != "NOCOMPUTE")
  
  # remove drugs where RXDAYSUP missing for >95% of entries
  # (check size of each being deleted manually)
  if (sum(drug_data$RXDAYSUP == "REFUSED" | drug_data$RXDAYSUP == "DK") > 0.95*dim(drug_data)[1]){
    drug_data = subset(drug_data, drug_data$RXDAYSUP != "REFUSED")
    drug_data = subset(drug_data, drug_data$RXDAYSUP != "DK")
  }
  drug_data = drug_data %>%
    mutate(RXDAYSUP = case_when(RXDAYSUP == "REFUSED" | RXDAYSUP == "DK" ~ avg_daysup,
                                .default=as.numeric(RXDAYSUP)))

  # remove drugs where RXQUANTY missing for >95% of entries
  # (check size of each being deleted manually)
  if (sum(drug_data$RXQUANTY == "NOCOMPUTE") > 0.95*dim(drug_data)[1]){
    drug_data = subset(drug_data, drug_data$RXQUANTY != "NOCOMPUTE")
  }
  drug_data = drug_data %>%
    mutate(RXQUANTY = case_when(RXQUANTY == "NOCOMPUTE" ~ avg_quanty,
                                .default=as.numeric(RXQUANTY)))  

#   # remove topicals based on form (some may also have "topical" in name)
#   drug_data = drug_data[drug_data$RXFORM != "CREA",]
#   drug_data = drug_data[drug_data$RXFORM != "OINT",]
#   drug_data = drug_data[drug_data$RXFORM != "OIN",]
#   drug_data = drug_data[drug_data$RXFORM != "LOTN",]
#   drug_data = drug_data[drug_data$RXFORM != "LOT",]
#   drug_data = drug_data[drug_data$RXFORM != "SWAB",]
#   drug_data = drug_data[drug_data$RXFORM != "CRE",]
#   drug_data = drug_data[drug_data$RXFORM != "PSTE",]
#   drug_data = drug_data[drug_data$RXFORM != "PAS",]
#   drug_data = drug_data[drug_data$RXFORM != "GEL",]
#   drug_data = drug_data[drug_data$RXFORM != "SHAM",]
# 
# ################################################################################
#   # UNITS ROUND 1
# 
#   # remove entries with vague or missing units (can't impute strings)
#   drug_data = drug_data[drug_data$RXSTRUNT != "NOCOMPUTE",]
#   drug_data = drug_data[drug_data$RXSTRUNT != "OTHER",]
#   drug_data = drug_data[drug_data$RXSTRUNT != "U/ML",]
#   drug_data = drug_data[drug_data$RXSTRUNT != "UNIT",]
#   drug_data = drug_data[drug_data$RXSTRUNT != "UNIT/ML",]
#   drug_data = drug_data[drug_data$RXSTRUNT != "MCG/OTHER",]
#   
#   # remove entries with missing RXFORM (can't tell solubility,etc)
#   drug_data = drug_data[drug_data$RXFORM != "OTHER",]
   
  ################################################################################
  # UNITS ROUND 2
  
  # convert strength, day supply, quantity to correct data type
  drug_data = drug_data %>%
    mutate(RXDAYSUP = as.numeric(RXDAYSUP)) %>%
    mutate(RXQUANTY = as.numeric(RXQUANTY)) %>%
    mutate(RXSTRENG = as.numeric(RXSTRENG))
  
  # assume: for those that are per act, per inh, per spray, should account for 
  # full daily dose b/c we account for frequency per day - so we don't need to do
  # anything 'extra' with those units - COME BACK TO THIS - if RXQUANTY < RXDAYSUP, may not
  # take every day!! but that's ok because you can say an avg of 1.6 pills/day for example
  # EXCEPTION: patches since they are released continuously - so we need a way to except them
  # - use PT24, PTWK! assume that's all patches in data
  # also, assume 12-hr capsules/tablets are accounted for too!
  
  # MCG/mg/Act
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/mg/Act" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/ACT
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "MG/ACT" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/ACT" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/SPRAY
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/SPRAY" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/ML
  # NOTE: no combo drugs with MG/ML units (single only)
  # example: 250 mg / 5 mL is 250 mg of drug in 5 mL solution per dose
  # so we have to fix dosages to not be divided (we get 5 mL and 250 mg each time, not 50 mg
  # each time in 1 mL dose)
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "MG/ML" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/ML" ~ "MCG",
                                .default=RXSTRUNT))
  
  # ML
  # remove weird 200 mL dosages of albuterol
  for (entry in drug_data$RXDRGNAM){
    if (entry == "ALBUTEROL"){
      drug_data = drug_data[drug_data$RXSTRUNT != "ML",]
    }
  }
  # drug_data = drug_data %>%
  #   mutate(RXSTRENG = case_when(RXDRGNAM=="TRAVOPROST" & RXSTRUNT=="ML" ~ 0.004)) #%>%
    #mutate(RXSTRENG = case_when(RXDRGNAM=="CIPROFLOXACIN" & RXSTRUNT=="ML" ~ 0.3))
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXDRGNAM=="TRAVOPROST" & RXSTRUNT=="ML" ~
                                  0.004,
                                RXDRGNAM=="CIPROFLOXACIN" & RXSTRUNT=="ML" ~
                                  0.3,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "ML" ~ "%",
                                .default=RXSTRUNT))

  # MG/MG
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "MG/MG" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/MG" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/MG
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXNDC=="00006057761" & RXDRGNAM=="METFORMIN" ~
                                  "MCG",
                                RXNDC=="00006057761" & RXDRGNAM=="SITAGLIPTIN" ~
                                  "MCG",
                                .default=RXSTRUNT))
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXNDC=="00006057761" & RXDRGNAM=="SITAGLIPTIN" ~
                                  RXSTRENG*1000,
                                .default=RXSTRENG))

  # MCG/HR
  # Note: this is for a patch, so we will multiply by 24, and that's daily dosage
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MCG/HR" ~ RXSTRENG*24,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/HR" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/SPRAY
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG/SPRAY" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/SPRAY" ~ "MCG",
                                .default=RXSTRUNT))

  # %
  # Note: 1% solution means 1 g of drug per 100 mL solution (% = g drug / mL
  # solution) - this would be an assumption (cite paper on % confusion), 1%=0.01=1g/100mL
  
  # same assumption as mg/mL conversions, e.g. 6% is 6 g/100mL, that is each dose has 6 g
  # active ingredient per 100 mL solution (we multiply by 1 million to convert to mcg/mL),
  # as before, we can ignore the volume and just care about the mass 
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "%" ~ RXSTRENG/100*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "%" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/MCG
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/MCG" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/INH
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/INH" ~ "MCG",
                                .default=RXSTRUNT))

  # GM/ML
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "GM/ML" ~ RXSTRENG*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "GM/ML" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/BLIST (treat like act, inh, etc)
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/BLIST" ~ "MCG",
                                .default=RXSTRUNT))

  # GM/SCOOP
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="GM/SCOOP" ~ RXSTRENG*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "GM/SCOOP" ~ "MCG",
                                .default=RXSTRUNT))

  # G/ML
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT == "G/ML" ~ RXSTRENG*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "G/ML" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/24HR
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG/24HR" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/24HR" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/HR
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG/HR" ~ RXSTRENG*24*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/HR" ~ "MCG",
                                .default=RXSTRUNT))

  # MCG/ACT
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MCG/ACT" ~ "MCG",
                                .default=RXSTRUNT))

  # mg/Act
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="mg/Act" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "mg/Act" ~ "MCG",
                                .default=RXSTRUNT))

  # # MMG - miscoded, it's a solution, and MMG is MCG
  # drug_data$RXSTRUNT = str_replace(drug_data$RXSTRUNT, "MMG", "MG")
  # # need to do solubility conversion in part 2

  # MG/mg/Act - miscoded (only albuterol, no combo)
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG/mg/Act" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/mg/Act" ~ "MCG",
                                .default=RXSTRUNT))

  # GM
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="GM" ~ RXSTRENG*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "GM" ~ "MCG",
                                .default=RXSTRUNT))

  # MG
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG" ~ "MCG",
                                .default=RXSTRUNT))

  # G
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="G" ~ RXSTRENG*1000000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "G" ~ "MCG",
                                .default=RXSTRUNT))

  # MG/MCG
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXNDC=="50090115800" ~
                                  "MCG",
                                RXNDC=="00254100752" ~
                                  "MCG",
                                .default=RXSTRUNT))
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXNDC=="50090115800" ~
                                  RXSTRENG,
                                RXNDC=="00254100752" ~
                                  RXSTRENG*1000,
                                .default=RXSTRENG))
  
  # MG/ML/MG/ML
  drug_data = drug_data %>%
    mutate(RXSTRENG = case_when(RXSTRUNT=="MG/ML/MG/ML" ~ RXSTRENG*1000,
                                .default=RXSTRENG))
  drug_data = drug_data %>%
    mutate(RXSTRUNT = case_when(RXSTRUNT == "MG/ML/MG/ML" ~ "MCG",
                                .default=RXSTRUNT))

################################################################################
  # add cleaned data for each drug to its own data frame
  meps_clean[[i]] = drug_data
}

# CHECK CODEBOOK FOR EACH OF NEEDED COLUMNS

# bind each data frame for each drug into giant data frame
meps_clean = do.call(rbind, meps_clean)

# remove mass, volume columns and move RXSTRENG to original position
meps_clean = meps_clean[,-c(6,7)]
meps_clean = meps_clean %>%
  relocate(RXSTRENG, .before=RXSTRUNT)

# fix densities and solubilities info that got randomly deleted
meps_clean[186715:186719,11] = 0.0072695
meps_clean[186715:186719,12] = "g/mL"
meps_clean[186715:186719,13] = 344.411
meps_clean[186715:186719,14] = 0.002503696
meps_clean[186715:186719,15] = "g/mL"

meps_clean[186720,11] = 0.170373
meps_clean[186720,12] = "g/mL"
meps_clean[186720,13] = 135.21
meps_clean[186720,14] = 0.02303613
meps_clean[186720,15] = "g/mL"

meps_clean[186721,11] = 0.170373
meps_clean[186721,12] = "g/mL"
meps_clean[186721,13] = 135.21
meps_clean[186721,14] = 0.02303613
meps_clean[186721,15] = "g/mL"

# calculate daily frequency (except the patches! these are already calculated as rates)
meps_clean = meps_clean %>%
  mutate(DAILYFREQ = case_when(RXFORM != "PTWK" & RXFORM != "PT24" ~ RXQUANTY/RXDAYSUP,
                               RXFORM == "PTWK" ~ 1,
                               RXFORM == "PT24" ~ 1))

# calculate daily dosage
meps_clean = meps_clean %>%
  mutate(DAILYDOSAGE = RXSTRENG*DAILYFREQ)

# find sum for each drug and put into descending order of DDD
total_daily_dosages = aggregate(meps_clean$DAILYDOSAGE,list(meps_clean$RXDRGNAM),FUN=sum)
colnames(total_daily_dosages) = c("Pharmaceutical","Defined Daily Dosage (ug/day)")
total_daily_dosages = total_daily_dosages %>%
  arrange(desc(`Defined Daily Dosage (ug/day)`))

# find total number of prescriptions per drug
total_prescribed = meps_clean %>%
  count(RXDRGNAM) %>%
  arrange(desc(n))
colnames(total_prescribed) = c("Pharmaceutical", "# Prescriptions")

# CHECK ONE MORE TIME AGAIN AFTER IMPUTING!!
#print(sum(is.na(meps_clean))) # note: extra NA's from beyond size of df
#which(is.na(meps_clean), arr.ind=TRUE)