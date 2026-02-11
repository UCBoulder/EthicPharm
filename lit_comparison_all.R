################################################################################
# Title: lit_comparison_all.R

# Description:
# Runs the PharmFlush model for each sewershed represented in the literature
# search before comparing the literature-reported mass loads with the predicted
# mass loads. Data visualizations are provided comparing the predictions with 
# the reported values, as well as highlighting the comparison between specific
# water resource recovery facilities' (WRRFs) reported values and the national
# baseline values, provided by the predictions.

# Functions used: 
#     * pharmflush_model():
#       * Input: the typical daily flow or design capacity of the WRRF and size
#                of the sewershed population
#       * Output: the average profile dataframe that includes the average
#                 predicted concentration and mass load for all pharmaceuticals

################################################################################

# load necessary libraries
library("stats")
library("tidyverse")
library("svglite")
library("patchwork")
library("cowplot")
library("readxl")
library("scales")

# load file with pharmflush_model function and literature-reported values
# (including details on the sewershed for each WRRF in the lit search)
source("pharmflush_function.R")
wrrf_profiles = read_excel("wrrf_profiles.xlsx")

# define the columns to keep from PharmFlush output
TARGET_OUTPUT_COLUMNS <- c("Drug", "Molar_Mass", "Average_Predicted_Mass_Load",
                           "Standard_Error_Predicted_Mass_Load",
                           "Average_Predicted_Concentration",
                           "Average_Predicted_Molar_Concentration")

# get unique WRRF info per row
predictions_lit_comp <- wrrf_profiles %>%
  group_by(WRRF_ID) %>%
  slice(1) %>%
  ungroup() %>%
  
  # select and clean the desired columns
  select(WRRF_ID, WRRF_capacity_liters_per_day, Population_served) %>%
  mutate(
    WRRF_capacity_liters_per_day = round(WRRF_capacity_liters_per_day),
    Population_served = round(Population_served)
  ) %>%
  
  # set PharmFlush to run for each individual row
  rowwise() %>%
  
  # run PharmFlush for each row
  mutate(
    model_output = list(
      pharmflush_model(
        flow_capacity = WRRF_capacity_liters_per_day, 
        population = Population_served
      )
    )
  ) %>%
  
  # expand PharmFlush results and bind them to the original columns
  unnest(model_output) %>%
  
  # select desired output columns
  select(
    WRRF_ID,
    all_of(TARGET_OUTPUT_COLUMNS)
  ) %>%
  ungroup()

# write predictions for each literature-reported sewershed to file (running the 
# model for each sewershed is time-consuming, this allows the output to be
# re-used without having to re-run all the above code)
write.csv(predictions_lit_comp, "predictions_lit_comp.csv")

################################################################################
# Load literature-reported concentration data for comparison.
################################################################################

# load predicted values for each sized WRRF to compare
predictions_lit_comp = read.csv("predictions_lit_comp.csv")

# load population-normalized lit reported mass loads
wrrf_profiles = read_excel("wrrf_profiles.xlsx", sheet=1)

# select and clean relevant columns (conserve ND (non-detect) values for heatmap
# visualization below)
wrrf_profiles = wrrf_profiles %>%
  select(WRRF_ID, Pharmaceutical, Mass_load_ug_per_capita_per_day,
         Concentration_ug_per_liter) %>%
  mutate(Pharmaceutical  = trimws(tolower(Pharmaceutical))) %>%
  rename(Reported_mass_load = Mass_load_ug_per_capita_per_day) %>%
  rename(Reported_concentration = Concentration_ug_per_liter) %>%
  mutate(
    Load_Numeric = as.numeric(Reported_mass_load),
    Load_Rounded = round(Load_Numeric, digits = 3),
    Reported_mass_load = if_else(
      is.na(Load_Rounded), 
      "ND", 
      as.character(Load_Rounded)
    )
  ) %>%
  mutate(
    Conc_Numeric = as.numeric(Reported_concentration),
    Conc_Rounded = round(Conc_Numeric, digits = 3),
    Reported_concentration = if_else(
      is.na(Conc_Rounded), 
      "ND", 
      as.character(Conc_Rounded)
    )
  ) %>%
  select(-c(Conc_Numeric, Conc_Rounded, Load_Numeric, Load_Rounded))

# define WRRF profiles without ND and perform additional cleaning steps
wrrf_profiles_detected = wrrf_profiles %>%
  filter(Reported_mass_load != "ND") %>%
  mutate(Reported_mass_load = as.numeric(Reported_mass_load)) %>%
  # assuming pseudoephedrine and ephedrine equally make up the reported
  # pseudoephedrine + ephedrine
  mutate(Reported_mass_load = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                          Reported_mass_load/2,
                                        .default=Reported_mass_load)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                      "pseudoephedrine",
                                    .default=Pharmaceutical)) %>%
  filter(Reported_concentration != "ND") %>%
  mutate(Reported_concentration = as.numeric(Reported_concentration)) %>%
  mutate(Reported_concentration = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                          Reported_concentration/2,
                                        .default=Reported_concentration)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                      "pseudoephedrine",
                                    .default=Pharmaceutical))

# find standard error of mean of lit reported values
lit_search_std_error = wrrf_profiles_detected %>%
  group_by(Pharmaceutical) %>%
  summarise(
    Count = n(),
    Standard_Error_Mass_Load = sd(Reported_mass_load, na.rm = TRUE) / sqrt(n())
  ) %>%
  mutate(Standard_Error_Mass_Load = ifelse(is.na(Standard_Error_Mass_Load),
                                           0, Standard_Error_Mass_Load))

# prep to join reported and predicted dataframes
predictions_lit_comp = predictions_lit_comp %>%
  rename(Pharmaceutical=Drug)
predictions_lit_comp$Pharmaceutical = trimws(tolower(predictions_lit_comp$Pharmaceutical))
wrrf_profiles_detected$Pharmaceutical = trimws(tolower(wrrf_profiles_detected$Pharmaceutical))
lit_search_std_error$Pharmaceutical = trimws(tolower(lit_search_std_error$Pharmaceutical))
wrrf_profiles_detected = wrrf_profiles_detected %>%
  left_join(lit_search_std_error, by="Pharmaceutical") %>% 
  rename(Standard_Error_Reported_Mass_Load = Standard_Error_Mass_Load) %>%
  select(-Count)

# complete join
lit_comp = wrrf_profiles_detected %>%
  full_join(predictions_lit_comp, by = c("WRRF_ID","Pharmaceutical")) #%>%
  #select(-X)

# convert from mass loads to mole loads
lit_comp = lit_comp %>%
  mutate(Predicted_mole_load = Average_Predicted_Mass_Load/(1000000*Molar_Mass)) %>%
  mutate(Reported_mole_load = Reported_mass_load/(1000000*Molar_Mass))

# remove NA in resulting dataframe
lit_comp = na.omit(lit_comp)

# calculate PML/RML ratio for each detection
lit_comp = lit_comp %>%
  mutate(PML_RML_ratio = Average_Predicted_Mass_Load/Reported_mass_load)

# remove NA and infinite values from ratio
lit_comp = lit_comp %>%
  filter(is.na(PML_RML_ratio)==FALSE & is.nan(PML_RML_ratio) == FALSE & is.infinite(PML_RML_ratio)==FALSE) 

################################################################################
# Data visualizations
################################################################################

# Code for Figure 2 in associated manuscript:

# 2a: compare lit search data to model output in scatterplot

# prep error bars with 95% CI

# find t score
alpha = 0.05
degrees_of_freedom = dim(lit_comp)[1] - 1
t_score = qt(p=alpha/2, df=degrees_of_freedom,lower.tail=F)

# find margin of error  
lit_comp_scatter_df = lit_comp %>%
  group_by(Pharmaceutical) %>%
  rename(Site_specific_avg_predicted_ML = Average_Predicted_Mass_Load) %>%
  summarise(
    Average_Predicted_Mass_Load = mean(Site_specific_avg_predicted_ML,
                                       na.rm=TRUE),
    Average_Reported_Mass_Load = mean(Reported_mass_load, na.rm=TRUE),
    Average_Predicted_Mole_Load = mean(Predicted_mole_load, na.rm=TRUE),
    Average_Reported_Mole_Load = mean(Reported_mole_load, na.rm=TRUE),
    Count = n(),
    SD_Predicted_ML = ifelse(Count == 1, 0, sd(Site_specific_avg_predicted_ML,
                                               na.rm = TRUE)),
    SD_Reported_ML = ifelse(Count == 1, 0, sd(Reported_mass_load, na.rm = TRUE))) %>%
  mutate(SE_Predicted_ML = SD_Predicted_ML / sqrt(Count),
         SE_Reported_ML = SD_Reported_ML / sqrt(Count),
         ME_Predicted_ML = t_score*SE_Predicted_ML,
         ME_Reported_ML = t_score*SE_Reported_ML) %>%
  ungroup()

# update dataframe to include 95% confidence intervals with upper & lower bounds
lit_comp_scatter_df = lit_comp_scatter_df %>%
  mutate(
    ymin = Average_Predicted_Mass_Load - ME_Predicted_ML,
    ymax = Average_Predicted_Mass_Load + ME_Predicted_ML,
    xmin = Average_Reported_Mass_Load - ME_Reported_ML,
    xmax = Average_Reported_Mass_Load + ME_Reported_ML
  )

# update dataframe to have negative values cut off to avoid NaN for error bars
# since log scaling
# note: xmax is always positive and not an issue, lowest positive values for
# ymin, xmin, ymax are on order of 1E-04
lit_comp_scatter_df$ymin = ifelse(lit_comp_scatter_df$ymin <= 0, 1E-04,
                                  lit_comp_scatter_df$ymin)
lit_comp_scatter_df$ymax = ifelse(lit_comp_scatter_df$ymax <= 0, 1E-04,
                                  lit_comp_scatter_df$ymax)
lit_comp_scatter_df$xmin = ifelse(lit_comp_scatter_df$xmin <= 0, 1E-04,
                                  lit_comp_scatter_df$xmin)

# make predicted mass load equal to 1E-4 when it is zero since it will
# otherwise be undefined for log scaling (reported is never 0)
lit_comp_scatter_df = lit_comp_scatter_df %>%
  mutate(Average_Predicted_Mass_Load = case_when(Average_Predicted_Mass_Load==0 ~
                                                   1E-4,
                                                 .default=Average_Predicted_Mass_Load))
# remove overlapping error bars
lit_comp_scatter_df =   lit_comp_scatter_df %>%
  distinct(Pharmaceutical, .keep_all = TRUE)

# make scatterplot
comp_scatter = ggplot(lit_comp_scatter_df, aes(x = Average_Reported_Mass_Load,
                                               y = Average_Predicted_Mass_Load)) +
  geom_point(size = 3, color = "gray", alpha = 0.75) +
  scale_x_log10(expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_log10() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0.2, color = "blue",
                alpha = 0.5) +  
  geom_errorbarh(aes(xmin = xmin, xmax = xmax), height = 0.2, color = "red",
                 alpha = 0.5) + 
  labs(x = expression("Log Reported Mass Load ("*mu*"g/cap/d)"), 
       y = expression("Log Predicted Mass Load ("*mu*"g/cap/d)")) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.title.x = element_text(size = 11),  
        axis.title.y = element_text(size = 11, margin = margin(r = 10)),  
        axis.text.x = element_text(size = 10),  
        axis.text.y = element_text(size = 10),
        plot.margin = margin(20, 20, 20, 30)) +  
  theme(aspect.ratio = 1) + 
  theme(panel.border = element_blank())

ggsave("comp_scatter.svg", plot = comp_scatter, width = 6, height = 6,
       device = "svg")

# 2b: make a bar graph for comparison of pharmaceuticals with 10 highest
# predicted concentrations
lit_comp_bar_df = lit_comp_scatter_df %>%
  arrange(desc(Average_Predicted_Mole_Load)) %>%
  select(c(Pharmaceutical, Average_Predicted_Mole_Load,
           Average_Reported_Mole_Load)) %>%
  distinct(Pharmaceutical, .keep_all = TRUE) 

lit_comp_bar_df = lit_comp_bar_df[1:10,]

# define function to change pharmaceutical names to title case
to_title_case = function(x) {
  words = strsplit(x, " ")[[1]]
  words = paste(toupper(substr(words, 1, 1)), tolower(substr(words, 2,
                                                             nchar(words))),
                sep = "", collapse = " ")
  return(words)
}

# Apply the function to the Pharmaceutical column
lit_comp_bar_df$Pharmaceutical = sapply(lit_comp_bar_df$Pharmaceutical,
                                       to_title_case)

# Prep dataset to plot
lit_comp_long = lit_comp_bar_df %>%
  pivot_longer(cols = c(Average_Predicted_Mole_Load, Average_Reported_Mole_Load),
               names_to = "Variable",
               values_to = "Value") %>%
  arrange(desc(Value))

# make bar plot
comp_bar = ggplot(lit_comp_long, aes(x = reorder(Pharmaceutical, -Value),  
                                     y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(x = "", y = "Mole Load (mol/cap/d)",
       fill = "") +
  theme_bw() +
  scale_fill_manual(values = c("blue", "red"), labels = c("Predicted Mole Load",
                                                 "Reported Mole Load")) + 
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
  theme(plot.margin = margin(10, 10, 10, 10)) + 
  theme(panel.grid.major = element_line(color = "white"), 
        panel.grid.minor = element_line(color = "white"), 
        panel.border = element_blank(),
        legend.position = "none") +
  geom_hline(yintercept = seq(0, 0.00015, by = 0.00005),
             color = "white",
             linewidth = 0.5,
             linetype = "solid")

ggsave("comp_bar.svg", plot = comp_bar, width = 6, height = 6,
       device="svg")

# combine Fig 2a and 2b into 1 plot
combined_plot = comp_scatter + comp_bar + 
  plot_layout(ncol = 2, widths = c(1, 1)) & 
  theme(plot.margin = margin(5,5,5,5))
ggsave("combined_plot.svg",plot=combined_plot,width=10,height=5,device="svg")

# make Figure 5 in associated manuscript (heat map comparing literature-reported
# values from specific water resource recovery facilities (WRRFs) to national
# baseline, aka predicted values)

# take median for each drug of reference 5's (from lit search) values since it's
# all 1 plant (for better visualization)
ref_five_median <- wrrf_profiles %>%
  filter(WRRF_ID == 11) %>%
  mutate(Load_Numeric = as.numeric(
    if_else(Reported_mass_load == "ND", NA_character_, Reported_mass_load))) %>%
  group_by(Pharmaceutical) %>%
  summarize(
    Detected_Count = sum(!is.na(Load_Numeric)),
    Median_mass_load = case_when(
      Detected_Count == 0 ~ "ND", 
      TRUE ~ as.character(round(median(Load_Numeric, na.rm = TRUE), 3))),
    .groups = 'drop') %>%
  select(-Detected_Count)

# add the median mass loads from reference 5 to the overall WRRF profiles to 
# replace the other individual values from reference 5, this is WRRF 11
ref_five_median = ref_five_median %>%
  mutate(WRRF_ID=11) %>%
  rename(Reported_mass_load = Median_mass_load) %>%
  relocate(WRRF_ID, .before = Pharmaceutical)

wrrf_profiles = wrrf_profiles %>%
  filter(WRRF_ID != 11) %>%
  mutate(
    Reported_mass_load_numeric = as.numeric(
      if_else(Reported_mass_load == "ND", NA_character_, Reported_mass_load))) %>%
  mutate(Reported_mass_load_numeric = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                                  Reported_mass_load_numeric/2,
                                                .default=Reported_mass_load_numeric)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                      "pseudoephedrine",
                                    .default=Pharmaceutical)) %>%
  mutate(Reported_mass_load_final = case_when(
    Reported_mass_load == "ND" ~ "ND",
    TRUE ~ as.character(round(Reported_mass_load_numeric, 3)) 
  )) %>%
  select(-c(Reported_mass_load, Reported_mass_load_numeric, Reported_concentration)) %>%
  rename(Reported_mass_load = Reported_mass_load_final)

wrrf_profiles = rbind(wrrf_profiles, ref_five_median)

# select WRRFs for heat map
wrrf_profiles_heatmap = wrrf_profiles %>%
  filter(WRRF_ID %in% c(3,4,5,6,11,36,37,38,39,40)) %>%
  mutate(WRRF_ID = case_when(WRRF_ID == 3 ~ "A",
                              WRRF_ID == 4 ~ "B",
                              WRRF_ID == 5 ~ "C",
                              WRRF_ID == 6 ~ "D",
                              WRRF_ID == 11 ~ "E",
                              WRRF_ID == 36 ~ "F",
                              WRRF_ID == 37 ~ "G",
                              WRRF_ID == 38 ~ "H",
                              WRRF_ID == 39 ~ "I",
                              WRRF_ID == 40 ~ "J"))

# join normalized predictions and normalized reported for plotting
all_wrrf_ids <- unique(wrrf_profiles_heatmap$WRRF_ID)
all_pharmas <- unique(wrrf_profiles_heatmap$Pharmaceutical)

# make master grid using all pharmaceuticals included at these WRRFs and all 10
# plant IDs (A-J)
master_grid <- expand_grid(
  Pharmaceutical = all_pharmas,
  WRRF_ID = all_wrrf_ids
)

# prep to join by cleaning and selecting same columns in predicted
predictions_heatmap = predictions_lit_comp %>%
  mutate(WRRF_ID = as.character(WRRF_ID))

predictions_heatmap = predictions_heatmap %>%
  filter(WRRF_ID %in% c(3,4,5,6,11, 36,37,38,39,40)) %>%
  mutate(WRRF_ID = case_when(WRRF_ID == 3 ~ "A",
                             WRRF_ID == 4 ~ "B",
                             WRRF_ID == 5 ~ "C",
                             WRRF_ID == 6 ~ "D",
                             WRRF_ID == 11 ~ "E",
                             WRRF_ID == 36 ~ "F",
                             WRRF_ID == 37 ~ "G",
                             WRRF_ID == 38 ~ "H",
                             WRRF_ID == 39 ~ "I",
                             WRRF_ID == 40 ~ "J"))

# join predicted and reported mass loads for heatmap
heatmap_long <- master_grid %>%
  left_join(predictions_heatmap, by = c("Pharmaceutical","WRRF_ID")) %>%
  left_join(wrrf_profiles_heatmap, by = c("Pharmaceutical", "WRRF_ID"))

# remove unneeded columns
heatmap_long = heatmap_long %>%
  select(-c(X, Standard_Error_Predicted_Mass_Load,
            Average_Predicted_Concentration))

# set categorical variables to distinguish missing pharmaceuticals for each WRRF
# from ones measured but not detected (NA vs ND); also calculate difference
# ratio between reported and predicted and note if the reported exceeds
# predicted by at least a magnitude of 10
heatmap_long <- heatmap_long %>%
  mutate(
    Cell_Category = case_when(
      Reported_mass_load == "ND" ~ "Not Detected (ND)",
      is.na(Reported_mass_load) ~ "Not Measured (NA)",
      TRUE ~ "Difference Ratio"
    ),
    Reported_mass_load = suppressWarnings(as.numeric(Reported_mass_load)),
    Ratio = Average_Predicted_Mass_Load / Reported_mass_load,
    Log10_Ratio = log10(Ratio),
    Log10_Ratio = if_else(
      is.infinite(Log10_Ratio),
      -1,
      Log10_Ratio
    ),
    Major_Discrepancy = Log10_Ratio <= -1
  ) %>%
  mutate(
    Log10_Ratio = if_else(Cell_Category != "Difference Ratio", NA_real_, Log10_Ratio),
    Major_Discrepancy = if_else(Cell_Category != "Difference Ratio", NA, Major_Discrepancy)
  )

# add flag for if absolute difference is greater than order of magnitude and
# make sure discrepancy flag is only for measured/detected values
heatmap_long = heatmap_long %>%
  mutate(
    Abs_Log10_Ratio = abs(Log10_Ratio),
    Major_Discrepancy_New = case_when(
      Cell_Category == "Difference Ratio" & Log10_Ratio <= -1 ~ TRUE,
      TRUE ~ FALSE 
    )
  )

# separate by cell category to prep for plotting different colors
data_difference <- heatmap_long %>% filter(Cell_Category == "Difference Ratio")
data_nd <- heatmap_long %>% filter(Cell_Category == "Not Detected (ND)")
data_na <- heatmap_long %>% filter(Cell_Category == "Not Measured (NA)")

# define colors for not measured and not detected
COLOR_ND <- "gray40" # Not Detected
COLOR_NA <- "gray90" # Not Measured

# plot the heatmap
heatmap_plot <- ggplot(heatmap_long, aes(x = WRRF_ID, y = Pharmaceutical)) +
  geom_tile(data = data_na, fill = COLOR_NA, color = "white", linewidth = 0.1) +
  geom_tile(data = data_nd, fill = COLOR_ND, color = "white", linewidth = 0.1) +
  geom_tile(data = data_difference, 
            aes(fill = Abs_Log10_Ratio), 
            color = "white", linewidth = 0.1) +
  scale_fill_gradient(
    low = "lightblue", high = "darkblue", 
    limits = c(0, max(data_difference$Abs_Log10_Ratio, na.rm = TRUE)),
    name = "|log10(Predicted/Reported)|",
    guide = "colorbar"
  ) +
  geom_point(data = data_difference %>% filter(Major_Discrepancy_New == TRUE),
             aes(shape = "Major Discrepancy (RML > 10x PML)"), 
             color = "red",
             size = 1,
             stroke = 0.5) +
  geom_tile(data = heatmap_long %>% filter(Cell_Category != "Difference Ratio"),
            aes(colour = Cell_Category), 
            fill = NA, 
            linewidth = 0) +
  scale_colour_manual(
    name = "Cell Status",
    values = c("Not Detected (ND)" = COLOR_ND, 
               "Not Measured (NA)" = COLOR_NA),
    breaks = c("Not Detected (ND)", "Not Measured (NA)"),
    guide = guide_legend(override.aes = list(
      fill = c(COLOR_ND, COLOR_NA), 
      colour = "white"
    ))
  ) +
  scale_shape_manual(
    name = "Discrepancy",
    values = c("Major Discrepancy (RML > 10x PML)" = 17), 
    guide = guide_legend(override.aes = list(
      color = "red",
      size = 3,
      fill = NA
    ))
  ) +
  labs(
    x = "WRRF ID",
    y = "Pharmaceuticals"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 6), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_colorbar(order = 1), 
         colour = guide_legend(order = 2),
         shape = guide_legend(order = 3))

ggsave("wrrf_profile_heatmap.svg", plot = heatmap_plot, width = 6, height = 6,
       device="svg")