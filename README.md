### PharmUse and PharmFlush
<div style="display: flex; justify-content: space-around;">
    <img src="pharmuse.png" alt="PharmUse logo" width="300">
    <img src="pharmflush.png" alt="PharmFlush logo" width="300">
</div>


## Description:
PharmUse is a database compiling information on prescription drug consumption, physicochemical properties,
and literature-reported toxicity values for 290 pharmaceuticals. PharmUse integrates data from the 2020
Agency for Healthcare Research & Quality Medical Expenditure Panel Survey (MEPS) Prescribed Medicines File,
accessible from [here](https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-220A),
with the Environmental Protection Agency's [CompTox Chemicals Dashboard](https://comptox.epa.gov/dashboard/). Notably,
the MEPS survey is from a nationally representative sample of United States citizens. In the associated scripts, the MEPS
data are downloaded directly from the provided website and converted to a text file, which is used in the script pharmuse_constructor.R.
The physicochemical properties and toxicity values are downloaded directly from CompTox by conducting a batch search for Chemical Name, SMILES,
InChi String, InChiKey, Molecular Formula, Average Mass, ToxValDB Details, and Physicochemical Property Values. 

PharmFlush is a binomial distribution model that predicts the concentration profile of the 290 pharmaceuticals in a sewershed of a given size, compares
sewersheds of different sizes, and compares the predicted concentration profile to literature-reported concentrations. It uses data provided by
PharmUse to run. PharmFlush is run using pharmflush.R.

The script ensemble_exploration.R uses csv files generated in pharmuse_constructor.R and pharmflush.R to compare literature-reported, predicted, and 
No Observed Effect Concentrations.

## Dependencies:
To run pharmuse_constructor.R, first run the following in your console:  
`install.packages("dplyr")`  
`install.packages("readxl")`    
`install.packages("stringr")`    
`install.packages("tidyr")`  
`install.packages("purrr")`

Alternatively, these packages can be installed at once by running:  
`install.packages("tidyverse")`

Additionally, the functions library pharmuse_constructor_functions.R, the MEPS data file MEPS_data_2020.txt, drugs_to_remove.xlsx, and comptox_search.xlsx
must be in the working directory to run this script.

To run pharmflush.R, first run the following in your console:  
`install.packages("tidyverse")`  
`install.packages("viridis")`  
`install.packages("svglite")`  
`install.packages("patchwork")`  

Additionally, pharmuse.csv, lit_values_all.csv, and lit_values_summary.csv must be in the working directory to run this script.

To run ensemble_exploration.R, first run the following in your console:  
`install.packages("tidyverse")`  
`install.packages("ggforce")`  

Additionally, all_ensembles_100.csv, all_ensembles_1000.csv, all_ensembles_100k.csv, all_ensembles_1mil.csv, lit_values_all.csv, noec_vert.csv, noec_invert.csv,
and noec_human.csv must be in the working directory to run this script.
