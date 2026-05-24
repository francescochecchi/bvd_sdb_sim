## Simulation of Safe and Dignified Burial for control of Bundibugyo Ebola virus epidemics
### Description of input dataset and R analysis scripts
May 2026

## General description
This repository contains data and R scripts needed to replicate the above analysis. The input datasets required are found in the `\in` folder, and are read automatically when the code is run.
All the code is found in the `\code` folder. To replicate the analysis, follow these steps:
* Download and unzip the repository to any folder in your computer (other than the Downloads folder, which usually gets wiped automatically). The folder is identified automatically when the code is run.
* Download R and RStudio (see download links on [https://posit.co/download/rstudio-desktop/]). While R is sufficient to run the analysis, it is recommended to instead run the scripts from the RStudio interface.
* Open and run the entire `00_cotrol_script.R` script (just press Alt+Ctrl+R). This will create an `\out` folder with further sub-folders, to which output tables and graphs will be saved automatically. As this scripts calls all the others, it alone is sufficient to replicate the analysis, but note below steps if you wish to alter the analysis.

## Description of input files
* `evd_sdb_sim_parameters.xlsx` contains a list of model parameters which that user can modify as wished. Note that the `n_sim` parameter heavily affects computing power requirements: on a cheap 2020 Laptop, 1000 simulation runs are OK for about 20-30 individual scenarios (and take <10 min to run), but anything beyond this may require a powerful PC or a high-performance cluster.
* `out_dose_resp_rn_p_success_hi.csv` and `out_dose_resp_rn_p_success_pw.csv` are generated from a separate analysis, described in Checchi et al. (2025) (https://researchonline.lshtm.ac.uk/id/eprint/4678756/) and which the user can reproduce entirely from the following repository: 
