################################################################################
# Title: pharmflush.R

# Description:
# Using PharmUse database, runs binomial probability distribution model
# PharmFlush and generates 100 concentration profiles for each pharmaceutical in
# PharmUse. Averaging over these profiles gives the average profile for each 
# pharmaceutical. Predicted concentrations are compared across sewershed sizes
# by running this script with different values of wastewater_pop_size parameter.
# Values are compared to literature-reported concentrations. Various data
# visualizations are provided to examine these relationships, and the readily
# biodegrades, Henry's Law, and log Kow parameters from PharmUse are
# also examined and visualized.
 
################################################################################

# import libraries
library("stats")
library("tidyverse")
library("viridis")
library("svglite")
library("patchwork")

################################################################################
# PharmFlush model creation
################################################################################

# load PharmUse database
pharmuse = read.csv("pharmuse.csv")

# Define model parameters
n = 26847 # size of representative sample (obtained from MEPS 2020
          # documentation, Table 3.2)
ww_volume_per_capita = 310.404 # average American wastewater production per
                               # capita per day, source: Dieter et al., 2018
wastewater_pop_size = 100000 # size of sewershed (change for different sizes)
pop_wastewater_volume = wastewater_pop_size * ww_volume_per_capita # total wastewater
                                                                   # volume for sewershed
                                                                   # of given size
x = 0:wastewater_pop_size # range of number of prescriptions detected in
                          # sewershed any given day for given drug
p = 0 # initialize probability of detecting certain pharmaceutical on any given day
avg_mass = 0 # initialize avg mass per day variable for each pharmaceutical

# pull relevant variables from PharmUse
drugs = pharmuse$Pharmaceutical
avg_DD = pharmuse$Average_Daily_Dosage
prescrips = pharmuse$Number_of_Prescriptions
durations = pharmuse$Average_Duration_per_Prescription

# calculate probability of detecting each pharmaceutical on any given day and 
# create list of average daily mass for each pharmaceutical
for (i in 1:length(drugs)){
  p[i] = (prescrips[i]*durations[i])/(n*365) 
  avg_mass[i] = avg_DD[i]
}

# Calculate probability P of detecting exactly x prescriptions for given drug in
# sewershed of size n with detection probability p
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
# concentration profiles or ensembles)
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
  arrange(desc(Predicted_concentration))
ensemble_drug_profiles[[i]] = ensemble
}

# get separate data frame for each drug's ensemble prediction (100 per drug)

# Initialize an empty list to store the 290 new dataframes
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

# average predicted concentration for each drug to form final drug concentration
# profile that averages across all 100 ensembles for each drug
average_concs = list()
average_prescrips = list()
average_masses = list()
sem_concs = list() # SEM = standard error of mean

for (i in 1:length(indiv_drug_profiles)) {
  avg_conc = mean(unlist(indiv_drug_profiles[[i]][6]))
  avg_conc = matrix(avg_conc, ncol = 1)
  average_concs[i] = avg_conc
  avg_mass = mean(unlist(indiv_drug_profiles[[i]][5]))
  avg_mass = matrix(avg_mass, ncol = 1)
  average_masses[i] = avg_mass
  stan_dev_conc = sd(unlist(indiv_drug_profiles[[i]][6]))
  sem_conc = stan_dev_conc / sqrt(sample_size)
  sem_conc = matrix(sem_conc, ncol = 1)
  sem_concs[i] = sem_conc
  
  avg_prescrip = mean(unlist(indiv_drug_profiles[[i]][2]))
  avg_prescrip = matrix(avg_prescrip, ncol = 1)
  average_prescrips[i] = avg_prescrip
}

# put in form that is compatible with data frame
average_concs = unlist(average_concs)
average_prescrips = unlist(average_prescrips)
average_masses = unlist(average_masses)
sem_concs = unlist(sem_concs)

# make data frame for average profile for each drug
average_profile = data.frame(Drug=drugs,
                             Average_Number_Prescriptions=average_prescrips,
                             Average_Masses=average_masses,
                             Average_Predicted_Concentration=average_concs,
                             Standard_Error_Predicted_Concentration=sem_concs)
average_profile = average_profile %>%
  arrange(desc(Average_Predicted_Concentration))

# prep data frame for stacked bar graph with all ensembles

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
                                 'Predicted_concentration')]
  
  # Append the dataframe to the combined dataframe
  stacked_ensembles = rbind(stacked_ensembles, ensemble_subset)
}

# export all 100 ensembles for each pharmaceutical to csv
write.csv(stacked_ensembles,"all_ensembles_100k.csv")

# aggregate average of all 290 pharmaceuticals for each sewershed size for export
stacked_ensembles_avg = aggregate(stacked_ensembles$Predicted_concentration,
                                  list(stacked_ensembles$Drug),FUN=mean)
colnames(stacked_ensembles_avg) = c("Pharmaceutical","Predicted_concentration")
stacked_ensembles_avg$Pharmaceutical = str_to_title(stacked_ensembles_avg$Pharmaceutical)
# the averaged concentration of each pharmaceutical across all 100 ensembles
write.csv(stacked_ensembles_avg,"stacked_ensembles_avg_100k.csv")

################################################################################
# Load literature-reported concentration data for comparison.
################################################################################

# load all literature values (each observation) and pre-process
lit_search_all = read.csv("lit_values_all.csv")
lit_search_all = na.omit(lit_search_all)
lit_search_all = lit_search_all %>%
  select(-c("Sampling.date","Sampling.location","Sampling.type","Reference")) %>%
  filter(Influent_concentration != "ND") %>%
  filter(Influent_concentration != "78-10900") %>%
  mutate(Influent_concentration = case_when(Influent_concentration=="1 373.5" ~
                                              "1373.5",
                                            Influent_concentration=="1 280.8" ~
                                              "1280.8",
                                            Influent_concentration=="1 096.0" ~
                                              "1096.0",
                                            .default=Influent_concentration)) %>%
  mutate(Influent_concentration = trimws(gsub(",",'',Influent_concentration))) %>%
  mutate(Influent_concentration = as.numeric(Influent_concentration)) %>%
  mutate(Influent_concentration = case_when(Pharmaceutical=="Pseudoephedrine + ephedrine" ~
                                              Influent_concentration/2,
                                            .default=Influent_concentration)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="Pseudoephedrine + ephedrine" ~
                                      "Pseudoephedrine",
                          .default=Pharmaceutical)) %>%
  mutate(Influent_concentration = Influent_concentration/1000) # convert ng/L to ug/L

# find standard error of lit reported values
lit_search_std_error = lit_search_all %>%
  group_by(Pharmaceutical) %>%
  summarise(
    Count = n(),
    Standard_Error_Lit_Concentration = sd(Influent_concentration, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Standard_Error_Lit_Concentration = ifelse(is.na(Standard_Error_Lit_Concentration),
                                                   0, Standard_Error_Lit_Concentration))

# create data frame to compare predicted to lit reported values
average_profile_comp_all = average_profile %>%
  rename(Pharmaceutical=Drug)
average_profile_comp_all$Pharmaceutical = trimws(tolower(average_profile_comp_all$Pharmaceutical))
lit_search_all$Pharmaceutical = trimws(tolower(lit_search_all$Pharmaceutical))
lit_comp_all = lit_search_all %>%
  left_join(average_profile_comp_all, by = "Pharmaceutical") 

# join with standard error dataframe
lit_comp_all$Pharmaceutical = trimws(lit_comp_all$Pharmaceutical)
lit_search_std_error$Pharmaceutical = trimws(lit_search_std_error$Pharmaceutical)
lit_search_std_error$Pharmaceutical = tolower(lit_search_std_error$Pharmaceutical)
lit_comp_all = lit_comp_all %>%
  left_join(lit_search_std_error, by="Pharmaceutical") %>% 
  select(-c("Count","Average_Number_Prescriptions","Average_Masses"))

# load summary statistics of literature-reported data (mean, median, etc.)
lit_search_summ = read.csv("lit_values_summary.csv")
lit_search_summ$Pharmaceutical = toupper(lit_search_summ$Pharmaceutical)

# pre-process summary data
lit_search_summ = lit_search_summ %>%
  select(c("Pharmaceutical","Mean","Median")) %>%
  filter(Pharmaceutical != "WARFARIN" & Pharmaceutical != "NALTREXONE" & Pharmaceutical != "RISPERIDONE") %>%
  mutate(Mean = as.numeric(Mean)) %>%
  mutate(Median = as.numeric(Median)) %>%
  mutate(Mean = case_when(Pharmaceutical=="PSEUDOEPHEDRINE + EPHEDRINE" ~
                            Mean/2,
                          .default=Mean)) %>%
  mutate(Median = case_when(Pharmaceutical=="PSEUDOEPHEDRINE + EPHEDRINE" ~
                              Median/2,
                            .default=Median)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="PSEUDOEPHEDRINE + EPHEDRINE" ~
                                      "PSEUDOEPHEDRINE",
                                     .default=Pharmaceutical)) %>%
  mutate(Median = Median/1000) %>% # convert ng/L to ug/L
  mutate(Mean = Mean/1000) # convert ng/L to ug/L

# join with predicted concentrations for comparison
average_profile_comp_summ = average_profile %>%
  rename(Pharmaceutical=Drug)
average_profile_comp_summ$Pharmaceutical = trimws(tolower(average_profile_comp_summ$Pharmaceutical))
lit_search_summ$Pharmaceutical = trimws(tolower(lit_search_summ$Pharmaceutical))
lit_comp_summ = lit_search_summ %>%
  left_join(average_profile_comp_summ, by = "Pharmaceutical") %>%
  arrange(desc(Average_Predicted_Concentration)) %>%
  select(-c(Average_Number_Prescriptions,Average_Masses,
            Standard_Error_Predicted_Concentration)) %>%
  mutate(Difference = abs(Mean-Average_Predicted_Concentration)) %>%
  mutate(PEC_MEC_ratio = Average_Predicted_Concentration/Mean)

################################################################################
# Data visualization
################################################################################

# Figure 2 code: Average predicted concentration across 100 ensembles vs average
# number of prescriptions 
median_val = median(average_profile$Average_Predicted_Concentration)
labels = c("Median = 0.04 \U00B5g/L",
            "4e-04 \U00B5g/L",
            "4 \U00B5g/L")
annotations = data.frame(
  x = c(6700,7250,7500),  
  y = c(median_val, 4e-04, 4),  
  label = labels
)
avg_plot = ggplot(average_profile, aes(x=Average_Number_Prescriptions,
                            y=Average_Predicted_Concentration)) + 
  scale_y_log10() + 
  annotate("segment", x = 0, y = median_val, xend = 8000, yend = median_val,
           color = "black",
           linewidth=1) + 
  annotate("segment", x = 0, y = 4e-04, xend = 8000, yend = 4e-04, 
           color = "black",
           linewidth=1) + 
  annotate("segment", x = 0, y = 4, xend = 8000, yend = 4, color = "black",
           linewidth=1) + 
  geom_text(data = annotations, aes(x = x, y = y, label = label), vjust = -0.5,
            color="black") +
  geom_point(color = "darkgray",alpha=0.5) + 
  guides(color = "none") +
  labs(x="Number of prescriptions",
       y=expression("Log Predicted Concentration ("*mu*"g/L)"),
       color="Pharmaceuticals") +
  theme_bw() + 
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) 
ggsave("average_drug_conc_plot.svg", plot = avg_plot, width = 5, height = 5,
       device="svg")

# Figure 3 code:

# get all pharmaceuticals over 10 ensembles with concentration < 4 ug/L
# (arbitrary cutoff for visualization)
stacked_ensembles_lessthan4 = stacked_ensembles %>%
  filter(Ensemble_number <= 10) %>%
  filter(Predicted_concentration < 4)
# sum up concentrations less than 4 ug/L
stacked_ensembles_lessthan4 = aggregate(stacked_ensembles_lessthan4$Predicted_concentration,
                                        list(stacked_ensembles_lessthan4$Ensemble_number),
                                        FUN=sum)
stacked_ensembles_lessthan4 = stacked_ensembles_lessthan4 %>%
  mutate(Drug = "Others") %>%
  relocate(Drug, .after=Group.1)
colnames(stacked_ensembles_lessthan4) = c("Ensemble_number","Drug",
                                          "Predicted_concentration")

# get all pharmaceuticals over 10 ensembles with concentration >= 4 ug/L
stacked_ensembles_greaterthan4 = stacked_ensembles %>%
  filter(Ensemble_number <= 10) %>%
  filter(Predicted_concentration >= 4)
stacked_ensembles_greaterthan4$Drug = str_to_title(stacked_ensembles_greaterthan4$Drug)

# merge sums < 4 for each ensemble with pharmaceuticals > 4 for each ensemble
stacked_ensembles_filtered = rbind(stacked_ensembles_greaterthan4,
                                   stacked_ensembles_lessthan4)

# # color palette for wastewater_pop_size = 100
# color_palette = c(
#   "Metformin"="#0072B2",  
#   "Naproxen"="#CC7722",  
#   "Gabapentin"="#56B4E9",  
#   "Ibuprofen"="#009E73",  
#   "Levetiracetam"="#8E44AD", 
#   "Others"="#999999", 
#   "Icosapent"="#E41A1C",  
#   "Labetalol"="#4682B4",  
#   "Bupropion"="#8A9B0F",  
#   "Aspirin"="#E69F00",  
#   "Acyclovir"="#5E3370",  
#   "Mesalamine"="#00BFC4",  
#   "Acetaminophen"="#FB8072",  
#   "Valacyclovir"="#007C7A",  
#   "Gemfibrozil"="#556B2F",  
#   "Hydrocodone"="#A62A29",  
#   "Methocarbamol"="#FFC300",  
#   "Divalproex Sodium"="#DDA0DD",  
#   "Oxcarbazepine"="#FF8C00", 
#   "Carbamazepine"="#DA70D6",  
#   "Losartan"="#8B4513",  
#   "Amoxicillin"="#2E8B57",  
#   "Cephalexin"="#FF00FF",  
#   "Dorzolamide"="#00B140", 
#   "Celecoxib"="#F4A460"   
# )

# # color palette for wastewater_pop_size = 100
# color_palette = c(
#   "Metformin"="#0072B2",  
#   "Naproxen"="#CC7722",  
#   "Gabapentin"="#56B4E9",  
#   "Ibuprofen"="#009E73",  
#   "Levetiracetam"="#8E44AD",  
#   "Others"="#999999",  
#   "Icosapent"="#E41A1C",  
#   "Bupropion"="#8A9B0F", 
#   "Aspirin"="#E69F00",  
#   "Mesalamine"="#00BFC4",  
#   "Valacyclovir"="#007C7A",
#   "Methocarbamol"="#FFC300",  
#   "Amoxicillin"="#2E8B57",
#   "Lactulose" ="#D95F02"
# )

# color palette for wastewater_pop_size = 100000 or 1 million
color_palette = c("Metformin"="#0072B2", "Lactulose"="#D95F02",
                   "Gabapentin"="#56B4E9", "Ibuprofen"="#009E73",
                   "Levetiracetam"="#8E44AD","Others"="#999999")

drug_stacked = ggplot(stacked_ensembles_filtered, aes(x = as.factor(Ensemble_number),
                                       y = Predicted_concentration,
                                       fill = Drug)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = color_palette) + 
  labs(x = "Concentration Profile",
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill="Pharmaceutical") +
  theme_minimal() + 
  coord_cartesian(ylim = c(0, 250)) + # use for 100k plot only
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  guides(fill="none") 
ggsave("drug_ensembles_100k.svg", plot = drug_stacked, width = 5, height = 5,
         device="svg")

# Figure 4 code:

# 4a: compare lit search data to model output in scatterplot

# prep error bars with 95% CI

# find t score
alpha = 0.05
degrees_of_freedom = dim(lit_comp_all)[1] - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)

# find margin of error  
lit_comp_all = lit_comp_all %>%
  mutate(Margin_error_predicted = t_score*Standard_Error_Predicted_Concentration) %>%
  mutate(Margin_error_measured = t_score*Standard_Error_Lit_Concentration) %>%
  group_by(Pharmaceutical) %>%
  mutate(Avg_measured = mean(Influent_concentration))

# update dataframe to include 95% confidence intervals with upper & lower bounds
ci_lit_comp = lit_comp_all %>%
  mutate(
    ymin = Average_Predicted_Concentration - Margin_error_predicted,
    ymax = Average_Predicted_Concentration + Margin_error_predicted,
    xmin = Avg_measured - Margin_error_measured,
    xmax = Avg_measured + Margin_error_measured
  )

# update dataframe to have negative values cut off to avoid NaN for error bars
ci_lit_comp$ymin = ifelse(ci_lit_comp$ymin < 0, 1E-04, ci_lit_comp$ymin)
ci_lit_comp$ymax = ifelse(ci_lit_comp$ymax < 0, 1E-04, ci_lit_comp$ymax)
ci_lit_comp$xmin = ifelse(ci_lit_comp$xmin < 0, 1E-04, ci_lit_comp$xmin)
ci_lit_comp$xmax = ifelse(ci_lit_comp$xmax < 0, 1E-04, ci_lit_comp$xmax)

comp_scatter = ggplot(ci_lit_comp, aes(x = Avg_measured, y = Average_Predicted_Concentration)) +
  geom_point(size = 3, color = "gray", alpha = 0.75) +
  scale_x_log10(expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "blue", alpha = 0.2) +  
  geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2, color = "red", alpha = 0.2) + 
  labs(x = expression("Log Measured Concentration ("*mu*"g/L)"), 
       y = expression("Log Predicted Concentration ("*mu*"g/L)")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 11),  
        axis.title.y = element_text(size = 11, margin = margin(r = 10)),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        plot.margin = margin(20, 20, 20, 30)) +  
  theme(aspect.ratio = 1)

ggsave("comp_scatter.svg", plot = comp_scatter, width = 6, height = 6,
       device = "svg")

# 4b: make a bar graph for comparison of pharmaceuticals with 10 highest
# predicted concentrations
lit_comp_top10 = lit_comp_all %>%
  arrange(desc(Average_Predicted_Concentration)) %>%
  select(-c(Standard_Error_Predicted_Concentration,
            Standard_Error_Lit_Concentration, Margin_error_predicted,
            Margin_error_measured)) %>%
  distinct(Pharmaceutical, .keep_all = TRUE) 

lit_comp_top10 = lit_comp_top10[1:10,]

# define function to change pharmaceutical names to title case
to_title_case = function(x) {
  words = strsplit(x, " ")[[1]]
  words = paste(toupper(substr(words, 1, 1)), tolower(substr(words, 2,
                                                             nchar(words))),
                sep = "", collapse = " ")
  return(words)
}

# Apply the function to the Pharmaceutical column
lit_comp_top10$Pharmaceutical = sapply(lit_comp_top10$Pharmaceutical,
                                       to_title_case)

# Prep dataset to plot
lit_comp_long = lit_comp_top10 %>%
  pivot_longer(cols = c(Average_Predicted_Concentration, Avg_measured),
               names_to = "Variable",
               values_to = "Value") %>%
  arrange(desc(Value))

comp_bar = ggplot(lit_comp_long, aes(x = reorder(Pharmaceutical, -Value),  
                                y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "Pharmaceutical", y = expression("Concentration ("*mu*"g/L)"),
       fill = "") +
  theme_bw() +
  scale_fill_viridis(discrete = TRUE, labels = c("Predicted Concentration",
                                                 "Measured Concentration")) + 
  theme(legend.position = "bottom",
        legend.direction="horizontal",
        legend.box="horizontal") +
  guides(fill = guide_legend(ncol=2, title='')) +
  theme(aspect.ratio=1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10)) +
  theme(axis.title.x = element_text(size = 11),  
        axis.title.y = element_text(size = 11),  
        axis.text.y = element_text(size = 10)) +
  theme(legend.text = element_text(size = 8)) +
  theme(plot.margin = margin(10, 10, 10, 10))

ggsave("comp_bar.svg", plot = comp_bar, width = 6, height = 6,
      device="svg")

# combine Fig 4a and 4b into 1 plot
 combined_plot = comp_scatter + comp_bar + 
   plot_layout(ncol = 2, widths = c(3, 3))
 ggsave("combined_plot.svg",plot=combined_plot,width=10,height=5,device="svg")

# Figure 5b
# biotransformation plot prep
lit_comp_biotransf = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(Readily.Biodegrades))

lit_comp_biotransf$Readily.Biodegrades <- factor(
  lit_comp_biotransf$Readily.Biodegrades,
  levels = c(0, 1),
  labels = c("No", "Yes")
)

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
biotransf = ggplot(lit_comp_biotransf, aes(x = Mean,
                                           y = Average_Predicted_Concentration,
                                           fill = Readily.Biodegrades)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") + 
  scale_fill_manual(values = c("Yes" = "blue", "No" = "red")) +  
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Readily biodegrades?") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("biotransf.svg", plot = biotransf, width = 5, height = 5,
       device="svg")

# Figure S1:
# Henry's law constant plot prep
lit_comp_HL = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(Henrys.Law)) %>%
  mutate(henry_law_category = case_when(Henrys.Law > 1E-03 & Molar_Mass < 200 ~
                                          "High",
                                        Henrys.Law <= 1E-03 & Henrys.Law > 1E-06 &
                                          Molar_Mass ~ "Intermediate",
                                        Henrys.Law <= 1E-06 & Molar_Mass ~ "Low"))

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
HL = ggplot(lit_comp_HL, aes(x = Mean, y = Average_Predicted_Concentration,
                                           fill = henry_law_category)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") +
  scale_fill_manual(values = c("Low" = "blue", "Intermediate" = "red", 
  "High" = "violet")) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Henry's law constant") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("HL.svg", plot = HL, width = 5, height = 5,
       device="svg")

# Figure 5a
# LogKow plot prep
lit_comp_logKow = lit_comp_summ %>%
  left_join(pharmuse,by="Pharmaceutical") %>%
  filter(!is.na(LogKow.Octanol.Water)) %>%
  mutate(logKow_category = case_when(LogKow.Octanol.Water >= 5 ~ "High",
                                     LogKow.Octanol.Water < 5 & LogKow.Octanol.Water >=2 ~
                                       "Intermediate",
                                     LogKow.Octanol.Water < 2 ~ "Low"))

# NOTE: below y=x line, lit-reported concentration > predicted; above line is
# predicted > lit_reported
logKow = ggplot(lit_comp_logKow, aes(x = Mean, y = Average_Predicted_Concentration,
                             fill = logKow_category)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(shape = 21, size = 4, color = "black") +  
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "black") + 
  scale_fill_manual(values = c("Low" = "blue", "Intermediate" = "violet",
                               "High" = "red")) +   
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(x=expression("Measured Concentration ("*mu*"g/L)"),
       y = expression("Predicted Concentration ("*mu*"g/L)"),
       fill = "Log Kow") +
  theme(axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)) +
  theme(legend.text = element_text(size = 12)) +
  theme(legend.title = element_text(size = 12))
ggsave("logKow.svg", plot = logKow, width = 5, height = 5,
       device="svg")