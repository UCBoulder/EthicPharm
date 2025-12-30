# SeweRx
<div style="display: flex; justify-content: space-around;">
    <img src="sewerx logo.png" alt="PharmUse logo" width="400">
</div>


## Description:
PharmUse is a database compiling information on prescription drug consumption, excretion, physicochemical properties,
fate and transport parameters, and literature-reported toxicity values for 313 pharmaceuticals. PharmUse integrates data from the nationally representative 2020
U.S. Agency for Healthcare Research & Quality Medical Expenditure Panel Survey (MEPS) Prescribed Medicines File,
accessible from [here](https://meps.ahrq.gov/mepsweb/data_stats/download_data_files_detail.jsp?cboPufNumber=HC-220A),
with the U.S. Environmental Protection Agency's (EPA) [CompTox Chemicals Dashboard](https://comptox.epa.gov/dashboard/), drug labels from the U.S. Food & Drug
Administration's [Drugs@FDA](https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm) database, the EPA's [Chemical Transformation Simulator](https://qed.epa.gov/cts/), and eawag and the University of Auckland's biotransformation database [enviPath](https://envipath.org/). The MEPS
data are downloaded directly from the provided website and converted to a text file. The physicochemical properties and toxicity values are downloaded directly from CompTox by conducting a batch search for Chemical Name, SMILES, InChi String, InChiKey, Molecular Formula, Average Mass, ToxValDB Details, and Physicochemical Property Values. Individual drug labels for each available administration route were downloaded from Drugs@FDA and manually curated for each pharmaceutical to determine its excretion route. The Chemical Transformation Simulator was used to predict if a given pharmaceutical would hydrolyze and the batch search feature was used to process pharmaceuticals by their SMILES identifier 9 at a time. enviPath was queried for each pharmaceutical to determine its likelihood to biotransform. The script pharmuse_constructor.R was used to load, pre-process, and integrate these data.

PharmFlush is a binomial distribution model that uses data from PharmUse to predict the concentration and mass load profiles of 313 pharmaceuticals in wastewater influent in a sewershed of a given size. PharmFlush was validated by conducting a literature search for pharmaceutical concentration measurements in wastewater influent in the U.S. between 2019 and 2024 and converting them to mass loads. These reported mass loads were then compared against predicted mass loads for sewersheds of the same size as the given study. PharmFlush is run using pharmflush.R.

The script ensemble_exploration.R uses csv files generated in pharmuse_constructor.R and pharmflush.R to compare literature-reported, predicted, and 
No Observed Effect Mass Loads.

The script get_png_from_pdf.py converts each page of the PDF file generated in ensemble_exploration.R into
separate PNG files. 

## Software:
All code is compatible with R 4.4.3 for Windows 11 and RStudio 2025.09.2.418, except the script
get_png_from_pdf.py, which is compatible with Python 3.13.9 for Windows 11 and Visual Studio Code 1.107.1.

## Dependencies:
All packages are R packages that can be downloaded from CRAN except pdf2image (see below).

The script ensemble_exploration.R requires tidyverse Version 2.0.0, ggforce Version 0.4.2, and 
pdftools Version 3.5.0. Additionally, all_ensembles_100.csv, all_ensembles_1000.csv, all_ensembles_lit_size.csv, all_ensembles_1mil.csv, wrrf_profiles.xlsx, noec_vert.csv, noec_invert.csv, noec_human.csv, lc50_vert.csv, lc50_invert.csv, and lc50_human.csv must be in the working directory to run this script.

The script fix_tox_order.R requires tidyverse Version 2.0.0. Additionally, comptox_search_orig.xlsx, previously_missed.xlsx, and ertugliflozin_dexlansoprazole.xlsx must be in the working directory to run this script.

The script get_png_from_pdf.py requires the package pdf2image Version 1.17.0, which can be downloaded using
pip (pip Version 25.0.1 was used in this case). This version of pdf2image requires Poppler Release 24.08.0.0, which can be downloaded for Windows from
[here](https://github.com/oschwartz10612/poppler-windows/releases/tag/v24.08.0-0) as Release-24.08.0-0.zip 
(for macOS and Linux, see below).
On Windows, Poppler must be on the correct path for pdf2image to work. This can be achieved by these steps:
1) Extract all files in Poppler zip file and copy the path to the bin folder.
2) Press Win + S and type Environment Variables.
3) Click on "Edit the system environment variables" and then click the "Environment Variables" button in the
dialog.
4) Under System variables, find and select the Path variable.
5) Click Edit.
6) Click New and paste the path to the bin folder (e.g., C:\path\to\poppler\bin).
7) Click OK to close all dialogs.
8) Open a new Command Prompt (Win + R, type cmd, hit Enter). Type: where pdfinfo
If it returns the path to pdfinfo.exe, the setup is successful.

For macOS, Poppler can be installed using Homebrew. To install Homebrew, use the following steps:
1) Run the following in your terminal:
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
2) Follow the on-screen instructions.
3) Verify Homebrew has successfully been installed by running brew --version in the terminal.

Then Poppler can be installed by running the following in the terminal:
brew install poppler

For Linux, Poppler can be installed by running the following in the terminal:
sudo apt-get install poppler-utils

In addition to these dependencies, Lit_pred_tox_mass_load_graphs.pdf must be in the same directory as
get_png_from_pdf.py for the script to run. This PDF can be generated using ensemble_exploration.R.

The script pharmuse_constructor.R requires tidyverse Version 2.0.0. Additionally, the functions library pharmuse_constructor_functions.R, the MEPS data file MEPS_data_2020.txt, drugs_to_remove.xlsx, and comptox_search.xlsx must be in the working directory to run this script.

The script pharmflush.R requires tidyverse Version 2.0.0, viridis Version 0.6.5, svglite Version 2.1.3,
patchwork Version 1.3.0, and cowplot Version 1.1.3. Additionally, pharmuse.csv, lit_values_all.csv, and lit_values_summary.csv must be in the working directory
to run this script.