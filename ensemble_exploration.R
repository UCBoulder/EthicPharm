################################################################################
# Title: ensemble_exploration.R

# Description:
# Loads predicted mass loads across 100 ensembles for all 313
# pharmaceuticals for sewersheds of size 100, 1000, 110,262, 1 million. Compares 
# predicted mass loads to literature-reported mass loads and human,
# vertebrate, and invertebrate No Observed Effect Mass Loads, converted from the
# No Observed Effect Concentrations reported in CompTox. Results are plotted as
# facet-wrapped scatterplots for each pharmaceutical.

################################################################################

# load libraries
library(dplyr)
library(ggplot2)
library(ggforce)
library(pdftools)
library(readxl)

# read in the data frames
en100      <- read.csv("all_ensembles_100.csv")
en1000    <- read.csv("all_ensembles_1000.csv")
en110000  <- read.csv("all_ensembles_lit_size.csv")
en1000000 <- read.csv("all_ensembles_1mil.csv")

lit = read_excel("wrrf_profiles.xlsx", sheet=1)
noec_vert <- read.csv("noec_vert.csv")
noec_invert <- read.csv("noec_invert.csv")
noec_human <- read.csv("noec_human.csv")
lc50_vert <- read.csv("lc50_vert.csv")
lc50_invert <- read.csv("lc50_invert.csv")
lc50_human <- read.csv("lc50_human.csv")

# for toxicity data, convert concentration to mass load assuming 310.404
# L/d/person 
noec_vert = noec_vert %>%
  mutate(Mass_load = TOXVAL_NUMERIC*310.404)
noec_invert = noec_invert %>%
  mutate(Mass_load = TOXVAL_NUMERIC*310.404)
noec_human = noec_human %>%
  mutate(Mass_load = Human*310.404)
lc50_vert = lc50_vert %>%
  mutate(Mass_load = TOXVAL_NUMERIC*310.404)
lc50_invert = lc50_invert %>%
  mutate(Mass_load = TOXVAL_NUMERIC*310.404)
lc50_human = lc50_human %>%
  mutate(Mass_load = Human*310.404)

# prepare data frames to merge (consistent column names, correct column
# selection, etc.)
noec_vert$Drug <- noec_vert$Pharmaceutical
noec_invert$Drug <- noec_invert$Pharmaceutical
noec_human$Drug <- noec_human$Pharmaceutical
lc50_vert$Drug <- lc50_vert$Pharmaceutical
lc50_invert$Drug <- lc50_invert$Pharmaceutical
lc50_human$Drug <- lc50_human$Pharmaceutical

lit = na.omit(lit)

lit = lit %>%
  select(Pharmaceutical, Mass_load_ug_per_capita_per_day) %>%
  mutate(Pharmaceutical  = trimws(tolower(Pharmaceutical))) %>%
  rename(Reported_mass_load = Mass_load_ug_per_capita_per_day) %>%
  filter(Reported_mass_load != "ND") %>% #removing non-detect measurements
  mutate(Reported_mass_load = as.numeric(Reported_mass_load)) %>%
  # assuming pseudoephedrine and ephedrine equally make up the reported
  # pseudoephedrine + ephedrine
  mutate(Reported_mass_load = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                          Reported_mass_load/2,
                                        .default=Reported_mass_load)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="pseudoephedrine + ephedrine" ~
                                      "pseudoephedrine",
                                    .default=Pharmaceutical)) %>%
  rename(Drug=Pharmaceutical)

# roll up the ensembles for each sewershed size and assign sewershed (catchment)
# sizes
en100$CatchmentSize <- "Serving 100"
en1000$CatchmentSize <- "Serving 1000"
# 110,000 approximated on graph, 110,262 is actual number
en110000$CatchmentSize <- "Serving 110000" 
en1000000$CatchmentSize <- "Serving 1000000"

en <- rbind(en100, en1000, en110000, en1000000)

en$CatchmentSize <- factor(en$CatchmentSize, levels = c("Serving 100",
                                                        "Serving 1000",
                                                        "Serving 110000",
                                                        "Serving 1000000"))

catchment_levels <- levels(factor(en$CatchmentSize))

lit_expand <- merge(lit, data.frame(CatchmentSize = catchment_levels))
noec_vert_expand <- merge(noec_vert, data.frame(CatchmentSize = catchment_levels))
noec_invert_expand <- merge(noec_invert,
                           data.frame(CatchmentSize = catchment_levels))
noec_human_expand <- merge(noec_human, 
                          data.frame(CatchmentSize = catchment_levels))
lc50_vert_expand <- merge(lc50_vert, data.frame(CatchmentSize = catchment_levels))
lc50_invert_expand <- merge(lc50_invert,
                            data.frame(CatchmentSize = catchment_levels))
lc50_human_expand <- merge(lc50_human, 
                           data.frame(CatchmentSize = catchment_levels))

# assign data sources for plotting (Note that NOEL is no observed effect load,
# and LL50 is Lethal Load 50 here)
lit_expand$Source <- "Reported mass load" # this is read in directly as mass
# load without conversion needed
noec_human_expand$Source <- "Human NOEL"
noec_vert_expand$Source <- "Vertebrate NOEL"
noec_invert_expand$Source <- "Invertebrate NOEL"
lc50_human_expand$Source <- "Human LL50"
lc50_vert_expand$Source <- "Vertebrate LL50"
lc50_invert_expand$Source <- "Invertebrate LL50"
en$Source <- "Predicted mass load"

# combine all data sources
combined_data <- bind_rows(
  en %>% select(CatchmentSize, Predicted_mass_load, Drug, Source),
  lit_expand %>% rename(Predicted_mass_load = Reported_mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  noec_human_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  noec_vert_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  noec_invert_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  lc50_human_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  lc50_vert_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source),
  lc50_invert_expand %>% rename(Predicted_mass_load = Mass_load) %>% 
    select(CatchmentSize, Predicted_mass_load, Drug, Source)
  
)

combined_data = combined_data %>%
  rename(Mass_load=Predicted_mass_load)

combined_data$Drug <- tolower(combined_data$Drug)

# transform data into plotting groups
transformed_data = combined_data %>%
  mutate(PlotGroup=case_when(Source=="Predicted mass load" ~
                               paste0("Predicted mass load, ",
                                      CatchmentSize),
                             Source=="Human NOEL" ~ Source,
                             Source=="Vertebrate NOEL" ~ Source,
                             Source=="Invertebrate NOEL" ~ Source,
                             Source=="Human LL50" ~ Source,
                             Source=="Vertebrate LL50" ~ Source,
                             Source=="Invertebrate LL50" ~ Source,
                             Source=="Reported mass load" ~
                               Source))

# remove zero-concentration values for log scaling
transformed_data <- transformed_data %>% filter(Mass_load > 0)

# calculate the total number of pages needed (302 graphs, 35 per page)
num_pages <- ceiling(length(unique(transformed_data$Drug)) / 8)

# check if reported or predicted mass loads ever exceed LL50 values
LL50_SOURCES <- c("Human LL50", "Vertebrate LL50", "Invertebrate LL50")

max_ll50_mass_load <- transformed_data %>%
  filter(Source %in% LL50_SOURCES) %>%
  group_by(Drug) %>%
  summarise(Max_LL50 = max(Mass_load, na.rm = TRUE))

comparison_df <- transformed_data %>%
  filter(Source %in% c("Reported mass load", "Predicted mass load")) %>%
  left_join(max_ll50_mass_load, by = "Drug")

exceeding_mass_loads <- comparison_df %>%
  filter(Mass_load > Max_LL50) %>%
  select(Drug, Source, Mass_load, Max_LL50) %>%
  arrange(Source, Drug)

# remove LL50 since it is never violated
transformed_data = transformed_data %>%
  filter(!(Source %in% LL50_SOURCES))

# define color blind friendly palette
color_blind_palette <- c(
  "Reported mass load" = "black",
  "Predicted mass load" = "#0072B2", 
  "Human NOEL" = "red",             
  "Vertebrate NOEL" = "#D55E00",        
  "Invertebrate NOEL" = "#CC79A7"      
)

# define axis label order
axis_label_order <- c(
  "Reported mass load",
  "Predicted mass load, Serving 100",
  "Predicted mass load, Serving 1000",
  "Predicted mass load, Serving 110000",
  "Predicted mass load, Serving 1000000",
  "Human NOEL",
  "Vertebrate NOEL",
  "Invertebrate NOEL"
)

# define legend label order
legend_label_order <- c(
  "Reported mass load",
  "Predicted mass load",
  "Human NOEL",
  "Vertebrate NOEL",
  "Invertebrate NOEL"
)

# update AxisLabel column and set as a factor with the desired levels
transformed_data <- transformed_data %>%
  mutate(
    AxisLabel = case_when(
      grepl("Predicted mass load", Source) ~ PlotGroup,
      TRUE ~ Source
    ),
    LegendLabel = ifelse(
      grepl("Predicted mass load", Source),
      "Predicted mass load",
      Source
    ),
    AxisLabel = factor(AxisLabel, levels = axis_label_order),
    LegendLabel = factor(LegendLabel, levels = legend_label_order)
  )

# remove any extra whitespaces in drug
transformed_data$Drug <- trimws(transformed_data$Drug)

# generate paginated plots and save to PDF
pdf("Lit_pred_tox_mass_load_graphs.pdf", height = 24, width = 32)

for (i in 1:num_pages) {
  print(
    ggplot(
      transformed_data %>%
        filter(Source %in% c("Predicted mass load", "Human NOEL",
                             "Vertebrate NOEL", "Invertebrate NOEL",
                             "Reported mass load")),
      aes(
        x = AxisLabel,
        y = Mass_load,
        color = LegendLabel 
      )
    ) +
      geom_point(
        position = position_dodge(width = 0.6),
        size = 3,
        alpha = 0.5,
        na.rm = TRUE
      ) +
      facet_wrap_paginate(~ Drug, scales = "free_y", nrow = 2, ncol = 4,
                          page = i) +
      labs(
        y = "Log Mass Load (\u03bcg/cap/d)",
        x = NULL,
        color = "Mass load value"
      ) +
      scale_y_log10() +
      scale_color_manual(values = color_blind_palette) +
      theme_minimal() +
      theme(
        text = element_text(size = 30),               
        axis.text.x = element_text(angle = 90, hjust = 1, size = 26),  
        axis.text.y = element_text(size = 26),         
        axis.title = element_text(size = 28),        
        strip.text = element_text(size = 28),          
        legend.text = element_text(size = 26),        
        legend.title = element_text(size = 28),        
        plot.margin = margin(t = 15, r = 15, b = 35, l = 15),
        legend.position = "none"                     
      )
  )
}
dev.off()