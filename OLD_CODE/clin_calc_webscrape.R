#' Scrapes prescription data for top 200 drugs prescribed in 2020 including
#' daily dose (g/day) using dosage and frequency data, specifically designed to
#' scrape from clincalc.com DrugStats database

# load libraries
library("rvest")
library("dplyr")
library("stringr")
library("tidyr")

# Read in HTML data as tibble from clincalc webpage (gets main page contents)
pharm_data <- read_html("https://clincalc.com/DrugStats/") %>%
  html_nodes("table") %>%
  html_table()  # inputs as tibble in R instead of HTML code

# Convert to data frame
pharm_data_df <- as.data.frame(pharm_data)
# pharm_data_df

# Get unique link endings for each link to more info for each drug (to get mass
# info for each drug)
pharm_data_spec <- read_html("https://clincalc.com/DrugStats/") %>%
  html_nodes("a") %>%
  html_attr('href')
# pharm_data_spec

# Convert to data frame
pharm_data_spec <- as.data.frame(pharm_data_spec)

# Extract the relevant link endings only
pharm_data_spec <- pharm_data_spec[12:211,]

# Scrape dosage tables from each drug's specific page:
all_drugs_doses <- vector(mode='list', length=200) # will store data for each drug

for (i in 1:200) {
drug <- read_html(gsub(" ", "", paste("https://clincalc.com/DrugStats/",
                                                  pharm_data_spec[i]))) %>%
html_nodes("table") %>%
html_table()

all_drugs_doses[i] <- drug[4]
}

# Combine dosage table with main drug table
pharm_data_df$dosages <- all_drugs_doses

# Remove row 192 (associated with null data on ClinCalc website), row 192 is not
# collected for drug supply data since those data are missing
pharm_data_df <- pharm_data_df[-192,]
# Adjust indexing after deletion of row 192 (note that rank still goes to 200, 192
# is just skipped, index itself goes to 199):
rownames(pharm_data_df) <- 1:nrow(pharm_data_df)

# Redefine pharm_data_spec to exclude row 192
pharm_data_spec <- pharm_data_spec[-192]

# Scrape days supplied/prescription (requires special treatment from being in 
# Javascript wrapper):

# Initialize lists to make drug supply data frame (these will be data frames 
# within data frames, with a data frame for each drug defined inside loop):
supply_df <- data.frame(matrix(ncol = 1, nrow = 0))
cols <- c("Drug Supply")
colnames(supply_df) <- cols

# Extract drug supply data for all 200 drugs (except null one at rank 192):
for (i in 1:199){
  # Read HTML page source and extract drug supply data from pie chart code:
  page_source <- readLines(gsub(" ", "", paste("https://clincalc.com/DrugStats/",
                                               pharm_data_spec[i])))
  supply <- page_source[grep("data.addRows", page_source)]
  
  # Extract the fourth line containing "data.addRows" (always the same in
  # ClinCalc) from page_source (this is where supply[4] comes from) and use
  # regular expressions to process the supply data:
  
  # Removes all punctuation except comma, period, forward slash:
  supply <- gsub('[^[:alnum:][:space:],|.|/]', "", supply[4])
  # Removes unwanted Javascript code:
  supply <- gsub('data.addRows', "", supply)
  # Converts string to a comma-delimited list (with each element of string as 
  # element of list):
  supply <- as.list(strsplit(supply, ',')[[1]])
  
  Number_days <- vector(mode='list', length=0)
  Percent_distrib <- vector(mode='list', length=0)
  
  # Put correct data in correct lists (odd index goes in Number_days, even index
  # goes in Percent_distrib):
  for (k in 1:length(supply)){
    if ((k %% 2) != 0) {
      Number_days <- append(Number_days, supply[k])
    }
    if ((k %% 2) == 0) {
      Percent_distrib <- append(Percent_distrib, supply[k])
    }
  }
  indiv_supply_df <- do.call(rbind, Map(data.frame, "Drug"=gsub("Drugs/", "", pharm_data_spec[i]),
                                        "Number of days"=Number_days,
                                        "Percent distribution"=Percent_distrib))
  supply_df <- rbind(supply_df, indiv_supply_df)
}

# Group by drug to make a nested data frame
supply_df<- supply_df %>% group_by(Drug) %>% nest()
# supply_df

# Combine with main table
pharm_data_df <- cbind(pharm_data_df, supply=supply_df)

# Remove duplicate column (drug name listed twice):
pharm_data_df <- subset(pharm_data_df, select = -c(supply.Drug))