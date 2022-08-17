# size-spectra-life-history

This file contains the model code and output for the Kindsvater et al. manuscript posted on BioArxiv on 22 August 2022. All of the material used to generate the results in this manuscript are contained herein.

# The contents of each folder are as follows:

## Codes

The primary script is called Full dynamic model.R. This R script is modified according to the details of each scenario: seasonality, temperature, kappa, etc. It is also used to generate Figs 1 and 3 in the main text.

The folder also includes code to generate the model results in all other figures in the main text and supplementary material (except Figures 2 and 4, which are conceptual). Each figure has a script that reads the model output from a specific folder in Model_output, summarizes it, and generates a plot.

Note that Supplemental Figure 3 can be generated from the script called Figure 4.R, if the lowess smoother is removed from the plot statement.

## Model output

This folder contains .csv files all of the results of the dynamic model (the forward simulation) that are presented in the main text (Figs 5-7) and in the supplementary material. For all files therein, each column stores the age specific length, reproduction, state (lipid condition), and survival for that run. There are two identical individuals stored in each .csv, though only the first individual (row) is used in plotting.

The parameter values for each run are in the file name, starting with temperature in Kelvin. f_h in the filename corresponds to h in the manuscript (the predator gut fullness). Phi_p and Kappa are discussed in the prey and mortality equations. The "reprolimit" is the lowercase phi discussed in supplemental figure 5. Tmax refers to the maximum age possible in months (equivalent to 18 years).

The folder called "Results_by_temperature" contains folders for model runs for each constant temperature scenario (referenced in Kelvin). They are the results presented in Figures 5 and 6, as well as Supplemental figures 3 and 4.

The folder called "Results_Case_Studies_Figure_7" contains model outputs for a combination of select constant scenarios (Groups1 and 2) and a seasonal scenario (Group 3).
