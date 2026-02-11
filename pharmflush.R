################################################################################
# Title: pharmflush.R

# Description:
# Using PharmUse database, runs binomial probability distribution model
# PharmFlush and generates 100 concentration and mass load profiles for each
# pharmaceutical in PharmUse. Averaging over these profiles gives the average
# profile for each pharmaceutical. Predicted concentrations are compared across
# sewershed sizes by running this script with different values of
# wastewater_pop_size parameter. The results are visualized with ggplot2.
# Additionally, a preliminary analysis is conducted to determine the
# pharmaceuticals most likely to end up at the water resource recovery facility
# (WRRF) based on their high excretion fraction and likelihood to biotransform,
# sorb, or hydrolyze on the way there.
 
################################################################################

# import libraries
library("stats")
library("tidyverse")
library("svglite")
library("patchwork")
library("cowplot")
library("readxl")
library("scales")

################################################################################
# PharmFlush model creation
################################################################################

# load PharmUse database
pharmuse = read.csv("pharmuse.csv")

# Define model parameters
n = 26847 # size of representative sample (obtained from MEPS 2020
          # documentation, Table 3.2)

# Use lines 40-45 if unknown daily flow and known population 
ww_volume_per_capita = 310.404 # average American wastewater production in L per
                               # capita per day
wastewater_pop_size = 110262 # size of sewershed (manually change for different sizes)
wastewater_pop_size = round(wastewater_pop_size) # for numerical stability in dbinom
# total wastewater volume for sewershed of given size
pop_wastewater_volume = wastewater_pop_size * ww_volume_per_capita 

# Uncomment and use lines 48-50 if daily flow and population known
#pop_wastewater_volume = 1.268112e+07 # manually change for different volumes
#wastewater_pop_size = 4.085361e+04 # manually change for different populations
#ww_volume_per_capita = pop_wastewater_volume/wastewater_pop_size

x = 0:wastewater_pop_size # range of number of prescriptions detected in
                          # sewershed any given day for given drug
p = 0 # initialize probability of detecting certain pharmaceutical on any given day
avg_mass = 0 # initialize avg mass per day variable for each pharmaceutical

# mutate PharmUse to get excreted_dose column from the percent excreted 
pharmuse = pharmuse %>%
  mutate(Excretion_fraction = Excretion_percentage/100) %>%
  mutate(Excreted_dose = Average_Daily_Dosage*Excretion_fraction)

# change column order
pharmuse = pharmuse %>%
  relocate(c(Excretion_fraction, Excreted_dose), .after = Average_Daily_Dosage)


# update PharmUse to be total excreted dose for all administration routes
# instead of the excreted dose for each route
# (average of average excreted doses per route, sum of number of prescriptions
# per route, average of average duration of prescription per route)
pharmuse2 = pharmuse %>%
  group_by(Pharmaceutical) %>%
  summarise(
    Excreted_dose = mean(Excreted_dose, na.rm = TRUE),
    Number_of_Prescriptions = sum(Number_of_Prescriptions, na.rm = TRUE),
    Average_Duration_per_Prescription = mean(Average_Duration_per_Prescription,
                                             na.rm = TRUE),
    Molar_Mass = first(Molar_Mass),
    .groups = "drop"
  )

# pull relevant variables from updated PharmUse
drugs = pharmuse2$Pharmaceutical
excreted_dose = pharmuse2$Excreted_dose
prescrips = pharmuse2$Number_of_Prescriptions
durations = pharmuse2$Average_Duration_per_Prescription
molar_masses = pharmuse2$Molar_Mass

# calculate probability of detecting each pharmaceutical on any given day and 
# create list of average daily mass for each pharmaceutical
for (i in 1:length(drugs)){
  p[i] = (prescrips[i]*durations[i])/(n*365) 
  avg_mass[i] = excreted_dose[i]
}

# Calculate probability P of detecting exactly x prescriptions for given drug in
# sewershed of wastewater_pop_size with detection probability p
P = list()
for (i in 1:length(drugs)){ 
  new_P = dbinom(x,wastewater_pop_size,p[i]) 
  P = append(P, list(new_P))
}

# convert P to data frame and label each column by drug name
P = data.frame(P)
names(P) = drugs

# add number of prescriptions detected (x variable) to P
P = P %>%
  mutate(Number_prescrips_detected = x)

# name common column and all drug columns to allow splitting into separate df's
Number_prescrips_detected = "Number_prescrips_detected"
drug_col_names = colnames(P)[!colnames(P) %in% Number_prescrips_detected]

# Split the data frame into separate data frames for each drug with number of
# prescriptions detected and probability of each number of prescriptions being
# detected 
drug_dataframes = lapply(drug_col_names, function(drug_col) {
  split_df = P[c(Number_prescrips_detected, drug_col)]
  colnames(split_df)[2] = "Probability"  
  return(split_df)
})
names(drug_dataframes) = drugs

# randomly pull (# prescriptions, Probability) pairs for each drug to randomly
# construct wastewater flows by bootstrapping (generating 100 possible drug
# concentration/mass load profiles or ensembles)
set.seed(123) 
sampled_dataframes = list()
sample_size = 100 

for (i in 1:length(drugs)) {
  drug_df = drug_dataframes[[i]] 
  prob_distribution = drug_df[2]
  sampled_rows = sample(nrow(drug_df), size = sample_size, replace = TRUE,
                        prob=unlist(prob_distribution)) 
  sampled_df = drug_df[sampled_rows, ]
  sampled_df$Drug = drugs[i]
  sampled_df$Molar_Mass = molar_masses[i]
  sampled_dataframes[[i]] = sampled_df
}

final_sampled_data = do.call(rbind, sampled_dataframes)

# randomly combine results to get sample_size number of drug profiles
ensemble_drug_profiles = list()
for (i in 1:sample_size){
selected_rows = final_sampled_data[seq(i, nrow(final_sampled_data),
                                       by = sample_size), ]
ensemble = data.frame(
  Drug = selected_rows$Drug,
  Molar_Mass = selected_rows$Molar_Mass,
  Number_prescrips_detected = selected_rows$Number_prescrips_detected,
  Probability = selected_rows$Probability
)
ensemble = ensemble %>%
  mutate(Average_daily_mass = avg_mass) %>%
  mutate(Predicted_mass = Average_daily_mass*Number_prescrips_detected) %>%
  mutate(Predicted_concentration = Predicted_mass/pop_wastewater_volume) %>%
  # divide by 4 to correct for medication non-compliance of 50% and employees'
  # contributions to wastewater flow (assume equal number of non-resident
  #  employees to residents)
  mutate(Predicted_concentration = Predicted_concentration/4) %>%
  mutate(Predicted_mass_load = Predicted_concentration*ww_volume_per_capita) %>%
  arrange(desc(Predicted_concentration)) %>%
  # convert to get molar concentrations
  mutate(Predicted_molar_concentration = Predicted_concentration/(1000000*Molar_Mass)) %>%
  arrange(desc(Predicted_molar_concentration))
ensemble_drug_profiles[[i]] = ensemble
}

# get separate data frame for each drug's ensemble prediction (100 per drug)

# Initialize an empty list to store the 313 new dataframes
indiv_drug_profiles = list()

# Loop over each pharmaceutical name
for (drug in drugs) {
  
  # Initialize an empty list to collect data for this pharmaceutical across all
  # dataframes
  pharma_data = list()
  
  # Loop over the list of dataframes
  for (ensemble_drug_profile in ensemble_drug_profiles) {
    
    # Extract rows corresponding to the current pharmaceutical
    pharma_row = ensemble_drug_profile[ensemble_drug_profile$Drug == drug, ]
    
    # Add the row to the pharma_data list
    pharma_data = rbind(pharma_data, pharma_row)
  }
  
  # Combine all the data for this pharmaceutical into a single dataframe
  indiv_drug_profiles[[drug]] = pharma_data
}

# average predicted concentration and mass load for each drug to form final drug
# concentration/mass load profile that averages across all 100 ensembles for
# each drug
average_concs = list()
average_molars = list()
average_loads = list()
average_prescrips = list()
average_masses = list()
sem_loads = list() # SEM = standard error of mean
molar_masses_final = list()

for (i in 1:length(indiv_drug_profiles)) {
  avg_conc = mean(unlist(indiv_drug_profiles[[i]][7]))
  avg_conc = matrix(avg_conc, ncol = 1)
  average_concs[i] = avg_conc
  avg_molars = mean(unlist(indiv_drug_profiles[[i]][9]))
  avg_molars = matrix(avg_molars, ncol = 1)
  average_molars[i] = avg_molars
  molar_masses = mean(unlist(indiv_drug_profiles[[i]][2]))
  molar_masses = matrix(molar_masses, ncol = 1)
  molar_masses_final[i] = molar_masses
  avg_load = mean(unlist(indiv_drug_profiles[[i]][8]))
  avg_load = matrix(avg_load, ncol = 1)
  average_loads[i] = avg_load
  avg_mass = mean(unlist(indiv_drug_profiles[[i]][6]))
  avg_mass = matrix(avg_mass, ncol = 1)
  average_masses[i] = avg_mass
  stan_dev_load = sd(unlist(indiv_drug_profiles[[i]][8]))
  sem_load = stan_dev_load / sqrt(sample_size)
  sem_load = matrix(sem_load, ncol = 1)
  sem_loads[i] = sem_load
  
  avg_prescrip = mean(unlist(indiv_drug_profiles[[i]][3]))
  avg_prescrip = matrix(avg_prescrip, ncol = 1)
  average_prescrips[i] = avg_prescrip
}

# put in form that is compatible with data frame
average_concs = unlist(average_concs)
average_molars = unlist(average_molars)
average_loads = unlist(average_loads)
average_prescrips = unlist(average_prescrips)
average_masses = unlist(average_masses)
sem_loads = unlist(sem_loads)
molar_masses_final = unlist(molar_masses_final)

# make data frame for average profile for each drug
average_profile = data.frame(Drug=drugs,
                             Molar_Mass = molar_masses_final,
                             Average_Number_Prescriptions=average_prescrips,
                             Average_Masses=average_masses,
                             Average_Predicted_Concentration=average_concs,
                             Average_Predicted_Molar_Concentration=average_molars,
                             Average_Predicted_Mass_Load=average_loads,
                             Standard_Error_Predicted_Mass_Load=sem_loads)
average_profile = average_profile %>%
  arrange(desc(Average_Predicted_Concentration))

################################################################################
# prep data frame for visualization as stacked bar graph with all ensembles and
# and for printing stacked ensemble output to csv files
################################################################################

# Initialize an empty dataframe to store the results
stacked_ensembles = data.frame()

# Loop through each dataframe in the list
for (i in seq_along(ensemble_drug_profiles)) {
  
  # Extract the current dataframe
  ensemble = ensemble_drug_profiles[[i]]
  
  # Add a column indicating the index
  ensemble$Ensemble_number = i
  
  # Select relevant columns 
  ensemble_subset = ensemble[, c('Ensemble_number', 'Drug',
                                 'Predicted_concentration',
                                 'Predicted_molar_concentration',
                                 'Predicted_mass_load')]
  
  # Append the dataframe to the combined dataframe
  stacked_ensembles = rbind(stacked_ensembles, ensemble_subset)
}

# export all 100 ensembles for each pharmaceutical to csv (make sure this is 
# saving for the correct sewershed size!)
write.csv(stacked_ensembles,"all_ensembles_1mil.csv")

# aggregate average of all 313 pharmaceuticals for each sewershed size for export
stacked_ensembles_avg_conc = aggregate(stacked_ensembles$Predicted_concentration,
                                  list(stacked_ensembles$Drug),FUN=mean)
colnames(stacked_ensembles_avg_conc) = c("Pharmaceutical","Predicted_concentration")
stacked_ensembles_avg_conc$Pharmaceutical = str_to_title(stacked_ensembles_avg_conc$Pharmaceutical)

stacked_ensembles_avg_molar = aggregate(stacked_ensembles$Predicted_molar_concentration,
                                       list(stacked_ensembles$Drug),FUN=mean)
colnames(stacked_ensembles_avg_molar) = c("Pharmaceutical","Predicted_molar_concentration")
stacked_ensembles_avg_molar$Pharmaceutical = str_to_title(stacked_ensembles_avg_molar$Pharmaceutical)

stacked_ensembles_avg_load = aggregate(stacked_ensembles$Predicted_mass_load,
                                       list(stacked_ensembles$Drug),FUN=mean)
colnames(stacked_ensembles_avg_load) = c("Pharmaceutical","Predicted_mass_load")
stacked_ensembles_avg_load$Pharmaceutical = str_to_title(stacked_ensembles_avg_load$Pharmaceutical)

# the averaged concentration/mass load of each pharmaceutical across all 100
# ensembles (make sure this is saving for the correct sewershed size!)
write.csv(stacked_ensembles_avg_conc,"stacked_ensembles_avg_conc_1mil.csv")
write.csv(stacked_ensembles_avg_molar,"stacked_ensembles_avg_molar_1mil.csv")
write.csv(stacked_ensembles_avg_load,"stacked_ensembles_avg_load_1mil.csv")

################################################################################
# Data visualization for different sewershed sizes
################################################################################

# Figure 4 code in associated manuscript:

# get all pharmaceuticals over 10 ensembles with concentration < 1E-08 M
# (arbitrary cutoff for visualization)
stacked_ensembles_lessthan = stacked_ensembles %>%
  filter(Ensemble_number <= 10) %>%
  filter(Predicted_molar_concentration < 1E-08)
# sum up concentrations less than 1E-08 ug/L
stacked_ensembles_lessthan = aggregate(stacked_ensembles_lessthan$Predicted_molar_concentration,
                                        list(stacked_ensembles_lessthan$Ensemble_number),
                                        FUN=sum)
stacked_ensembles_lessthan = stacked_ensembles_lessthan %>%
  mutate(Drug = "Others") %>%
  relocate(Drug, .after=Group.1)
colnames(stacked_ensembles_lessthan) = c("Ensemble_number","Drug",
                                          "Predicted_molar_concentration")

# get all pharmaceuticals over 10 ensembles with concentration >= 1E-08 ug/L
stacked_ensembles_greaterthan = stacked_ensembles %>%
  filter(Ensemble_number <= 10) %>%
  filter(Predicted_molar_concentration >= 1E-08) %>%
  select(Ensemble_number, Drug, Predicted_molar_concentration)
stacked_ensembles_greaterthan$Drug = str_to_title(stacked_ensembles_greaterthan$Drug)
  

# merge sums < 1E-08 for each ensemble with pharmaceuticals > 1E-08 for each ensemble
stacked_ensembles_filtered = rbind(stacked_ensembles_greaterthan,
                                   stacked_ensembles_lessthan)

# for below, ensure correct color palette for population size is selected (these
# are the available palettes for figure 4 in the manuscript)

# color palette for wastewater_pop_size = 100
  color_palette = c(
    "Metformin"="#0072B2",
    "Allopurinol"="#3F00FF",
    "Gabapentin"="#56B4E9",
    "Ketoconazole"="#009E73",
    "Levetiracetam"="#FFD700",
    "Others"="#999999",
    "Lactulose"="#E41A1C",
    "Pregabalin"="#654321",
    "Valacyclovir"="#8A9B0F",
    "Diclofenac"="#D95F02",
    "Acyclovir"="#BBA8FF",
    "Mesalamine"="#00BFC4",
    "Docusate"="#FB8072", 
    "Ibuprofen"="#E69F00",
    "Lidocaine"="#556B2F",
    "Ciprofloxacin"="#A62A29",
    "Amoxicillin"='#C19A6B',
    "Nitroglycerin" = "#FF00FF",
    "Olopatadine" = "#311064",
    "Dorzolamide" = "#6A0DAD"
  )

# color palette for wastewater_pop_size = 1000
color_palette = c(
  "Metformin"="#0072B2",
  "Allopurinol"="#3F00FF",
  "Gabapentin"="#56B4E9",
  "Ibuprofen"="#E69F00",
  "Levetiracetam"="#FFD700",
  "Others"="#999999",
  "Lactulose"="#E41A1C",
  "Mesalamine"="#00BFC4",
  "Fluorouracil"="#413839",
  "Pregabalin"="#654321",
  "Valacyclovir"="#8A9B0F",
  "Acyclovir"="#BBA8FF",
  "Amoxicillin"="#C19A6B",
  "Diclofenac" ="#D95F02",
  "Lidocaine"="#556B2F",   
  "Imiquimod"="#ff007f",
  "Benzoyl Peroxide"="#8E44AD",
  "Ketoconazole"="#009E73",
  "Ciclopirox" = "#C0392B",
  "Diphenhydramine" = "#1E272E"
)

#color palette for wastewater_pop_size = 100000 and 1 million
color_palette = c("Metformin"="#0072B2", "Diclofenac"="#D95F02",
                   "Gabapentin"="#56B4E9", "Ketoconazole"="#009E73",
                   "Benzoyl Peroxide"="#8E44AD",  "Levetiracetam"="#FFD700",
                   "Ibuprofen"="#E69F00", "Allopurinol"="#3F00FF",
                   "Fluorouracil"="#413839", "Mesalamine"="#00BFC4",
                   "Others"="#999999")

# create plot of stacked ensembles for concentrations > 1E-08 ug/L
drug_stacked = ggplot(stacked_ensembles_filtered, aes(x = as.factor(Ensemble_number),
                                       y = Predicted_molar_concentration,
                                       fill = Drug)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette) + 
  labs(x = "Concentration Profile",
       y = "Predicted Concentration (mol/L)",
       fill="Pharmaceutical") +
  theme_minimal() + 
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  guides(fill="none") 
# check this line to make sure saving the simulation for the correct sewershed
# size!
ggsave("drug_ensembles_1mil.svg", plot = drug_stacked, width = 5, height = 5,
         device="svg")

################################################################################
# Preliminary analysis of drugs most likely to be at WRRF 
################################################################################

# identify the pharmaceuticals most likely to end up at WRRF
average_profile = average_profile %>%
   rename(Pharmaceutical = Drug)

# find drugs in top 25% for predicted mass load
molar_conc_threshold = quantile(average_profile$Average_Predicted_Molar_Concentration,
                               probs = 0.75, na.rm = TRUE)

top_25_percent_drugs = average_profile %>%
  filter(Average_Predicted_Molar_Concentration >= molar_conc_threshold) %>%
  arrange(desc(Average_Predicted_Molar_Concentration))

pharmuse_wrrf_relevant = pharmuse %>%
  select(Pharmaceutical, LogKow_Octanol_Water,
         EnviPath_Biodegradation_Prediction, Hydrolyzes)
 
# define threshold for Log Kow cutoff
log_kow_cutoff = 3.5

# find pharmaceuticals with LogKow < 3.5 and that are not expected to biodegrade
# or hydrolyze (those that meet the condition get marked to highlighted_red in 
# Highlight_Condition column and those that do not get marked to normal_gray)
pharmuse_wrrf_relevant = pharmuse_wrrf_relevant %>%
  mutate(
    Highlight_Condition = case_when(
      LogKow_Octanol_Water < log_kow_cutoff &
        EnviPath_Biodegradation_Prediction == 0 &
        Hydrolyzes == 0 ~ "Highlighted_Red",
      TRUE ~ "Normal_Gray" 
    )
  )

# join the top 25% of drugs and WRRF-relevant parameters to get the
# pharmaceuticals (with highlighted_red condition) most likely to end up at WRRF
# at highest concentration
highest_likelihood_drugs = top_25_percent_drugs %>%
  left_join(pharmuse_wrrf_relevant, by="Pharmaceutical")
highest_likelihood_drugs = unique(highest_likelihood_drugs)