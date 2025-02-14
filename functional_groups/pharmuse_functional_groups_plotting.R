#' Description: 
#'  Reads in PharmUse database with functional groups (created in
#'  pharmuse_with_functional_groups.py) and average_profile.csv (created in 
#'  pharmflush.R), merges them, and plots trends 
  
# Read in required libraries
library(ggplot2)
library(tidyverse)

# Import required csv files
pharmuse_func_groups = read.csv('pharmuse_db_with_functional_groups.csv')
average_profile = read.csv('average_profile.csv')

# Prep average_profile for merging
average_profile = average_profile %>%
  rename(Pharmaceutical = Drug) %>%
  select(-c(X, SEM_Number_Prescriptions, SEM_Predicted_Concentration))

# Merge average_profile with pharmuse_func_groups
pharmuse_pharmflush = left_join(pharmuse_func_groups, average_profile,
                     by="Pharmaceutical") 

# Reorder to have PharmFlush output closer to front
pharmuse_pharmflush = pharmuse_pharmflush %>%
  select(
    1:3,              
    Average_Number_Prescriptions, Average_Predicted_Concentration,      
    everything()      
  ) %>%
  relocate(Average_Number_Prescriptions, .before = SMILES) %>%
  relocate(Average_Predicted_Concentration,
           .after = Average_Number_Prescriptions)

# Change colnames to avoid confusion
pharmuse_pharmflush = pharmuse_pharmflush %>%
  rename(MEPS_Prescribed_Daily_Dosage = Prescribed_Daily_Dosage) %>%
  rename(MEPS_Num_Prescriptions = Num_Prescriptions) %>%
  rename(PharmFlush_100k_Avg_Num_Prescrips = Average_Number_Prescriptions) %>%
  rename(PharmFlush_100k_Avg_Pred_Conc = Average_Predicted_Concentration)

# Count and organize most abundant functional groups
functional_groups <- pharmuse_pharmflush %>%
  select(Amides, Ethers, Tertiary.amines, Alkyl.halides, Secondary.amines,
         Aliphatic.alcohols,Aromatic.alcohols, Carboxylic.acids, Sulfonamides,
         Primary.amines, Ureas, Esters,Nitriles, Ketones, Sulfides, Carbamates,
         Sulfones, Guanidines, Amidines, Thiols, Imines, Imides)

functional_group_counts <- functional_groups %>%
  summarise(across(everything(), sum))

functional_group_counts_tidy <- functional_group_counts %>%
  pivot_longer(everything(), names_to = "Functional_Group",
               values_to = "Count") %>%
  arrange(desc(Count))

# Fix names in functional_group_counts_tidy for plotting
functional_group_counts_tidy[1,1] = "Tertiary amines"
functional_group_counts_tidy[3,1] = "Aliphatic alcohols"
functional_group_counts_tidy[4,1] = "Secondary amines"
functional_group_counts_tidy[5,1] = "Carboxylic acids"
functional_group_counts_tidy[8,1] = "Primary amines"
functional_group_counts_tidy[10,1] = "Aromatic alcohols"
functional_group_counts_tidy[15,1] = "Alkyl halides"

# Plot most abundant functional groups as bar graph
ggplot(functional_group_counts_tidy, aes(x = reorder(Functional_Group, Count),
                                         y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip the coordinates for better readability
  labs(
    x = "Functional Group",
    y = "Count"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12), 
    axis.title = element_text(size = 14)    
  )

# Print out functional groups associated with 10 highest concentration
# pharmaceuticals
top_10_pharma = pharmuse_pharmflush %>%
  slice_head(n = 10)  

functional_group_summary = top_10_pharma %>%
  select(Pharmaceutical, Amides, Ethers, Tertiary.amines, Alkyl.halides,
         Secondary.amines, Aliphatic.alcohols, Aromatic.alcohols,
         Carboxylic.acids, Sulfonamides, Primary.amines, Ureas, Esters,
         Nitriles, Ketones, Sulfides, Carbamates, Sulfones, Guanidines,
         Amidines, Thiols, Imines, Imides) %>%
  pivot_longer(cols = -Pharmaceutical, names_to = "Functional_Group",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  group_by(Pharmaceutical) %>%
  summarise(Functional_Groups = paste(Functional_Group, collapse = ", ")) %>%
  arrange(Pharmaceutical)

print(functional_group_summary)

# Prepare to plot
functional_group_count_top10 <- top_10_pharma %>%
  select(Amides, Ethers, Tertiary.amines, Alkyl.halides, Secondary.amines, 
         Aliphatic.alcohols, Aromatic.alcohols, Carboxylic.acids, Sulfonamides,
         Primary.amines, Ureas, Esters, Nitriles, Ketones, Sulfides, Carbamates,
         Sulfones, Guanidines, Amidines, Thiols, Imines, Imides) %>%
  pivot_longer(cols = everything(), names_to = "Functional_Group",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Functional_Group) 

# Fix names in functional_group_count_top10 for plotting
functional_group_count_top10[1,1] = "Aliphatic alcohols"
functional_group_count_top10[3,1] = "Aromatic alcohols"
functional_group_count_top10[4,1] = "Carboxylic acids"
functional_group_count_top10[8,1] = "Primary amines"
functional_group_count_top10[9,1] = "Secondary amines"
functional_group_count_top10[11,1]= "Tertiary amines"

# Plot the abundance of functional groups associated with highest concentration
# pharmaceuticals
ggplot(functional_group_count_top10, aes(x = reorder(Functional_Group, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates to have functional groups on y-axis
  labs(x = "Functional Group",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14)) 

# Isolate pharmaceuticals over the human or eco tox NOEC
high_noec = pharmuse_pharmflush %>%
  filter(Pharmaceutical %in% c("METHOCARBAMOL", "METFORMIN", "RANITIDINE",
                               "ATORVASTATIN", "AMOXICILLIN", "IBUPROFEN"))

# Prepare to plot high tox
functional_group_count_high_noec <- high_noec %>%
  select(Amides, Ethers, Tertiary.amines, Alkyl.halides, Secondary.amines, 
         Aliphatic.alcohols, Aromatic.alcohols, Carboxylic.acids, Sulfonamides,
         Primary.amines, Ureas, Esters, Nitriles, Ketones, Sulfides, Carbamates,
         Sulfones, Guanidines, Amidines, Thiols, Imines, Imides) %>%
  pivot_longer(cols = everything(), names_to = "Functional_Group",
               values_to = "Presence") %>%
  filter(Presence == 1) %>%
  count(Functional_Group) 

# Fix names in functional_group_count_high_noec for plotting
functional_group_count_high_noec[1,1] = "Aliphatic alcohols"
functional_group_count_high_noec[3,1] = "Aromatic alcohols"
functional_group_count_high_noec[4,1] = "Carboxylic acids"
functional_group_count_high_noec[8,1] = "Primary amines"
functional_group_count_high_noec[9,1] = "Secondary amines"
functional_group_count_high_noec[11,1]= "Tertiary amines"

# Plot the abundance of functional groups associated with pharmaceuticals whose
# predicted concentration is greater than NOEC
ggplot(functional_group_count_high_noec, aes(x = reorder(Functional_Group, n),
                                             y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates to have functional groups on y-axis
  labs(x = "Functional Group",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        axis.title = element_text(size = 14)) 