# size-spectra-life-history
This file contains the model code and output for  Kindsvater et al. Fear and foraging in the ecosystem size spectrum generate diversity in fish life histories. The revised manuscript was posted on BioArxiv on 24 February 2023.

The authors are: 

Kindsvater, Holly K. - Dept. of Fish and Wildlife Conservation, Virginia Polytechnic Institute and State University, 310 W. Campus Dr., Blacksburg, VA 24061, USA

Juan-Jordá, Maria-José - Earth to Ocean Research Group, Dept. of Biological Sciences, Simon Fraser University, Burnaby, BC V5A 1S6 Canada, and AZTI, Marine Research, Basque Research and Technology Alliance (BRTA). Herrera Kaia, Portualdea z/g, 20110 Pasaia – Gipuzkoa, Spain 

Dulvy, Nicholas K.- Earth to Ocean Research Group, Dept. of Biological Sciences, Simon Fraser University, Burnaby, BC V5A 1S6 Canada

Horswill, Cat - ZSL Institute of Zoology, Regent’s Park, London, NW1 4RY, UK, and Centre for Biodiversity and Environmental Research, Department of Genetics, Evolution and Environment, University College London, Gower Street, London, WC1E 6BT, UK, and Department of Zoology, University of Cambridge, Downing St, Cambridge CB2 3EJ, UK

Matthiopoulos, Jason - Institute of Biodiversity, Animal Health and Comparative Medicine, Graham Kerr Building, University of Glasgow, Glasgow, G12 8QQ, Scotland

Mangel, Marc - Theoretical Ecology Group, Dept. of Biology, University of Bergen, Bergen 5020, Norway 
 and  Institute of Marine Sciences and Dept. of Applied Mathematics and Statistics, University of California, Santa Cruz, CA 95064, USA.   
 
Understanding how changing environmental conditions affect fish growth and reproduction is critical to predict the consequences of climate change, yet studies focused on the physiological effects of temperature upon life histories often ignore size-dependent foraging and risk of predation.  We embedded a state-dependent energetic model in an ecosystem size spectrum model to characterize prey availability (foraging) and risk of predation (fear) experienced by individual fish as they grow. We examined how spectrum richness and temperature interact to shape growth, reproduction, and survival; we found that richer spectra led to larger body sizes, but effects of temperature on body size were small.

All code was written and run by Holly K. Kindsvater.  All of the material used to generate the results in this manuscript are contained herein.

# The contents of each folder are as follows:

## Codes

The primary script is called Full dynamic model.R. This R script is modified according to the details of each scenario: seasonality, temperature, kappa, etc. It is also used to generate Figs 1 and 3 in the main text. This script was run using R version 4.1.1 and occasionally run as an R Script from the bash shell on a Mac Pro, as described in the Appendix of the manuscript. 

The folder also includes code to generate the model results in all other figures in the main text and supplementary material (except Figures 2 and 4, which are conceptual). Each figure has a, R script that reads the model output from a specific folder in Model_output, summarizes it, and generates a plot. These scripts were produced in RStudio. The directory path for each of those scripts will need to be modified to reproduce the figures. 

Note that Supplemental Figure 3 can be generated from the script called Figure 5.R. There is a chunk of code included where the lowess smoother is removed from the plot() statement.

## Model_output

This folder contains .csv files all of the results of the dynamic model (the forward simulation) that are presented in the main text (Figs 5-7) and in the supplementary material. For all files therein, each column stores the age specific length, reproduction, state (lipid condition), and survival for that run. There are two identical individuals stored in each .csv, though only the first individual (row) is used in plotting.

The parameter values for each run are in the file name, starting with temperature in Kelvin. f_h in the filename corresponds to h in the manuscript (the predator gut fullness). Phi_p and Kappa are discussed in the prey and mortality equations. The "reprolimit" is the lowercase phi discussed in supplemental figure 5. Tmax refers to the maximum age possible in months (equivalent to 18 years).

Each figure script in the Codes folder pulls these files, summarizes them, and produces a figure. 

The folder called "Results_by_temperature" contains folders for model runs for each constant temperature scenario (referenced in Kelvin). They are the results presented in Figures 5 and 6, as well as Supp figures 3 and 4.

The folder called "Results_Case_Studies_Figure_7" contains model outputs for a combination of select constant scenarios (Groups1 and 2) and a seasonal scenario (Group 3).

The model output needed to recreate Supp figures 5-7 are each in their own folder. 
