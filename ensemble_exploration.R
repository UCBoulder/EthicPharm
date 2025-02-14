################################################################################
# Title: ensemble_exploration.R

# Description:
# Loads predicted concentrations across 100 ensembles for all 290
# pharmaceuticals for sewersheds of size 100, 1000, 100k, 1 million. Compares 
# predicted concentrations to literature-reported concentrations and human,
# vertebrate, and invertebrate No Observed Effect Concentrations (NOEC) reported
# in CompTox. Results are plotted as facet-wrapped scatterplot for each 
# pharmaceutical.

################################################################################

# load libraries
library(dplyr)
library(ggplot2)
library(ggforce)
library(pdftools)

# read in the data frames
en100      <- read.csv("all_ensembles_100.csv")
en1000    <- read.csv("all_ensembles_1000.csv")
en100000  <- read.csv("all_ensembles_100k.csv")
en1000000 <- read.csv("all_ensembles_1mil.csv")

lit <- read.csv("lit_values_all.csv")
tox_vert <- read.csv("noec_vert.csv")
tox_invert <- read.csv("noec_invert.csv")
tox_human <- read.csv("noec_human.csv")

# prepare data frames to merge
lit$Drug <- lit$Pharmaceutical
tox_vert$Drug <- tox_vert$Pharmaceutical
tox_invert$Drug <- tox_invert$Pharmaceutical
tox_human$Drug <- tox_human$Pharmaceutical

lit = na.omit(lit)

lit = lit %>%
  filter(Influent_concentration != "ND") %>%
  filter(Influent_concentration != "78-10900") %>%
  mutate(Influent_concentration = trimws(gsub(",","",Influent_concentration))) %>%
  mutate(Influent_concentration = trimws(gsub(" ","",Influent_concentration))) %>%
  mutate(Influent_concentration = as.numeric(Influent_concentration)) %>%
  mutate(Influent_concentration = case_when(Drug=="Pseudoephedrine + ephedrine" ~
                                              Influent_concentration/2,
                                            .default=Influent_concentration)) %>%
  mutate(Drug = case_when(Drug=="Pseudoephedrine + ephedrine" ~
                            "Pseudoephedrine",
                          .default=Drug)) %>%
  mutate(Pharmaceutical = case_when(Pharmaceutical=="Pseudoephedrine + ephedrine" ~
                                      "Pseudoephedrine",
                                    .default=Pharmaceutical))


# roll up the ensembles
en100$CatchmentSize <- "Serving 100"
en1000$CatchmentSize <- "Serving 1000"
en100000$CatchmentSize <- "Serving 100000"
en1000000$CatchmentSize <- "Serving 1000000"

en <- rbind(en100, en1000, en100000, en1000000)

en$CatchmentSize <- factor(en$CatchmentSize, levels = c("Serving 100",
                                                        "Serving 1000",
                                                        "Serving 100000",
                                                        "Serving 1000000"))

catchment_levels <- levels(factor(en$CatchmentSize))

lit_expand <- merge(lit, data.frame(CatchmentSize = catchment_levels))
tox_vert_expand <- merge(tox_vert, data.frame(CatchmentSize = catchment_levels))
tox_invert_expand <- merge(tox_invert,
                           data.frame(CatchmentSize = catchment_levels))
tox_human_expand <- merge(tox_human, 
                          data.frame(CatchmentSize = catchment_levels))

# combine into one data frame
lit_expand$Source <- "Literature-reported concentration"
tox_human_expand$Source <- "Human NOEC"
tox_vert_expand$Source <- "Vertebrate NOEC"
tox_invert_expand$Source <- "Invertebrate NOEC"
en$Source <- "Predicted concentration"

combined_data <- bind_rows(
  en %>% select(CatchmentSize, Predicted_concentration, Drug, Source),
  lit_expand %>% rename(Predicted_concentration = Influent_concentration) %>% 
    select(CatchmentSize, Predicted_concentration, Drug, Source),
  tox_human_expand %>% rename(Predicted_concentration = Human) %>% 
    select(CatchmentSize, Predicted_concentration, Drug, Source),
  tox_vert_expand %>% rename(Predicted_concentration = TOXVAL_NUMERIC) %>% 
    select(CatchmentSize, Predicted_concentration, Drug, Source),
  tox_invert_expand %>% rename(Predicted_concentration = TOXVAL_NUMERIC) %>% 
    select(CatchmentSize, Predicted_concentration, Drug, Source)
)

combined_data = combined_data %>%
  rename(Concentration=Predicted_concentration)

combined_data$Drug <- tolower(combined_data$Drug)

# prep to plot
transformed_data = combined_data %>%
  mutate(PlotGroup=case_when(Source=="Predicted concentration" ~
                               paste0("Predicted concentration, ",
                                      CatchmentSize),
                             Source=="Human NOEC" ~ Source,
                             Source=="Vertebrate NOEC" ~ Source,
                             Source=="Invertebrate NOEC" ~ Source,
                             Source=="Literature-reported concentration" ~
                               Source))

# remove zero-concentration values for log scaling
transformed_data <- transformed_data %>% filter(Concentration > 0)

# calculate the total number of pages needed (290 graphs, 35 per page)
num_pages <- ceiling(length(unique(transformed_data$Drug)) / 8)

# consolidate "Predicted concentration" into one label
transformed_data$Source <- ifelse(
  grepl("Predicted concentration", transformed_data$Source),
  "Predicted concentration",
  transformed_data$Source
)

# define color blind friendly palette
color_blind_palette <- c(
  "Literature-reported concentration" = "black",
  "Predicted concentration" = "#0072B2", 
  "Human NOEC" = "red",             
  "Vertebrate NOEC" = "#D55E00",        
  "Invertebrate NOEC" = "#CC79A7"      
)

# define custom order for AxisLabel
axis_label_order <- c(
  "Literature-reported concentration",
  "Predicted concentration, Serving 100",
  "Predicted concentration, Serving 1000",
  "Predicted concentration, Serving 100000",
  "Predicted concentration, Serving 1000000",
  "Human NOEC",
  "Vertebrate NOEC",
  "Invertebrate NOEC"
)

# define custom order for LegendLabel
legend_label_order <- c(
  "Literature-reported concentration",
  "Predicted concentration",
  "Human NOEC",
  "Vertebrate NOEC",
  "Invertebrate NOEC"
)

# update AxisLabel column and set as a factor with the desired levels
transformed_data <- transformed_data %>%
  mutate(
    AxisLabel = case_when(
      grepl("Predicted concentration", Source) ~ PlotGroup,
      TRUE ~ Source
    ),
    LegendLabel = ifelse(
      grepl("Predicted concentration", Source),
      "Predicted concentration",
      Source
    ),
    AxisLabel = factor(AxisLabel, levels = axis_label_order),
    LegendLabel = factor(LegendLabel, levels = legend_label_order)
  )

# remove any extra whitespaces in drug
transformed_data$Drug <- trimws(transformed_data$Drug)

# generate paginated plots and save to PDF
pdf("Lit_pred_tox_concentration_graphs.pdf", height = 24, width = 32)

for (i in 1:num_pages) {
  print(
    ggplot(
      transformed_data %>%
        filter(Source %in% c("Predicted concentration", "Human NOEC",
                             "Vertebrate NOEC", "Invertebrate NOEC",
                             "Literature-reported concentration")),
      aes(
        x = AxisLabel,
        y = Concentration,
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
        y = "Log Concentration (\u03bcg/L)",
        x = NULL,
        color = "Concentration value"
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