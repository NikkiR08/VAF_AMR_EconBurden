# The Value Attribution Framework for Vaccines against AMR Economic Burden Estimation

# The Economic Burden of Drug Resistant Infections & The Potential Impact of Vaccines

> This is the repo (2) for the paper for the paper: Naylor, Nichola R. and Hasso-Agopsowicz, Mateusz and Kim, Chaelin and Ma, Yixuan and Frost, Isabel and Abbas, Kaja and Aguilar, Gisela and Fuller, Naomi M. and Robotham, Julie V. and Jit, Mark, The Global Economic Burden of Antibiotic Resistant Infections and the Potential Impact of Bacterial Vaccines: A Modelling Study. Available at SSRN: https://ssrn.com/abstract=4676946 or http://dx.doi.org/10.2139/ssrn.4676946

> Repo (1), which provides some of the inputs to this work, is the Antmicrobial Resistance Unit Cost Repository: Available @ https://github.com/NikkiR08/AMR-UCR


## Description

This repo combines epidemiology estimates (provided by Chaelin Kim based on her work in this paper:https://www.medrxiv.org/content/10.1101/2022.05.08.22274821v2 & code in this repository : https://github.com/vaccine-impact/vaccine_amr ), with economic unit cost estimates (from https://github.com/NikkiR08/AMR-UCR). Inflation and exchange rate data are used to preserve local currency units and local economic shifts throughout, where possible. All cost results are then presented in 2019 USD.

Follow the numerical order of the scripts, starting with those in the "1_creating_DRI_estimates" subfolder, then following from "1a_sampling...". 

## Contents

The prefix-numbering system on the files gives an indication in the order of files needing to be run. 

For large input and intermediate files, a link to a Dropbox folder storing these are available upon request
(nichola.r.naylor@gmail.com).

Folder/File | Description
-----|------------
[sampling_whoc_costs](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/1a_sampling_whoc_costs.R) | R script to create a sample of whoc unit costs per bed days
[creating_DRI_estimates](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/1_creating_DRI_estimates) | R script estimating the unit hsospital costs associated with drug resistant infections 
[reshaping_epi_inputs](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/2_reshaping_epi_inputs.R) | R script to reshape epi inputs into format needed
[combining_epi_econ](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/3_combining_epi_econ.R) | R script combining the epi and econ estimates
[labour_productivity](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/4_labour_productivity.R) | R script estimating human capital losses through excess deaths
[plot_creation](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/5_plot_creation.R) | R script with plot code - additional plots e.g. bubble plots are available in "6_Additional_Plots.R" that were made at a later date.
[outputs](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/outputs) | Outputs from the analyse
[data_all](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/data_all) | This contains data not-uploaded to github but used across stages of the analyses (e.g. population estimates)
[data_inputs](https://github.com/NikkiR08/VAF_AMR_EconBurden/tree/main/data_all) | This contains data not-uploaded to github but to input into the analyses from the econ and epi work packages

## How to Cite this Code

Nichola R. Naylor, The Value Attribution Framework for Vaccines against AMR Economic Burden Estimation (VAF_AMR_EconBurden). GitHub (https://github.com/NikkiR08/VAF_AMR_EconBurden) [Access Date: ]


## 👂 Further information & Feedback

Please feel free to raise an issue on this GitHub, using the Issues functionality on the GitHub Repository, though the grant funding this project has been finished so responses and capacity for changes may be limited. 
