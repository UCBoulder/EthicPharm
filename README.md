# PharmUse and PharmFlush
<div style="display: flex; justify-content: space-around;">
    <img src="pharmuse.png" alt="PharmUse logo" width="275">
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

The script get_png_from_pdf.py converts each page of the PDF file generated in ensemble_exploration.R into
separate PNG files. 

## Software:
All code is compatible with R 4.4.3 for Windows 11 and RStudio 2024.12.1.563, except the script
get_png_from_pdf.py, which is compatible with Python 3.13.2 for Windows 11 and Visual Studio Code 1.98.2.

## Dependencies:
All packages are R packages that can be downloaded from CRAN except pdf2image (see below).

The script pharmuse_constructor.R requires tidyverse Version 2.0.0.

Additionally, the functions library pharmuse_constructor_functions.R, the MEPS data file MEPS_data_2020.txt,
drugs_to_remove.xlsx, and comptox_search.xlsx must be in the working directory to run this script.

The script pharmflush.R requires tidyverse Version 2.0.0, viridis Version 0.6.5, svglite Version 2.1.3,
patchwork Version 1.3.0, and cowplot Version 1.1.3.

Additionally, pharmuse.csv, lit_values_all.csv, and lit_values_summary.csv must be in the working directory
to run this script.

The script ensemble_exploration.R requires tidyverse Version 2.0.0, ggforce Version 0.4.2, and 
pdftools Version 3.5.0.

Additionally, all_ensembles_100.csv, all_ensembles_1000.csv, all_ensembles_100k.csv, all_ensembles_1mil.csv, lit_values_all.csv, noec_vert.csv, noec_invert.csv,
and noec_human.csv must be in the working directory to run this script.

The script get_png_from_pdf.py requires the package pdf2image Version 1.17.0, which can be downloaded using
pip Version 25.0.1. This package requires Poppler Release 24.08.0.0, which can be downloaded for Windows from
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

In addition to these dependencies, Lit_pred_tox_concentration_graphs.pdf must be in the same directory as
get_png_from_pdf.py for the script to run.
