# Code and model output from: Fear and foraging in the ecosystem size spectrum generate diversity in fish life histories

------------------------------------------------------------------------

These folders represent the model output ("data") produced with the dynamic programming method described in the accompanying manuscript under review at the American Naturalist. The data in each figure were analyzed with the included .R scripts, with names corresponding to each figure.

### Description of the data and file structure

Included are two folders, "Model_output" and "Code". They contain files of data and analysis scripts, with extensions .csv and .R, respectively.

**Software**

All programming was done using R 4.1.1.

To produce the .csv files, storing model output, we ran the \*\*Full_dynamic_model.R\* script (in the *Code* folder) using Rscript commands from the Linux shell of a 2019 MacPro with 16 cores and 96 GB of RAM. Jobs were run in parallel and the runtime of each job was between 60 and 120 minutes.

Figures were produced in the Rstudio IDE.To produce the figures, we used additional R packages that were loaded in 2022 under R version 4.1.1. They were: ggplot2, fields, and wesanderson. Each of these packages is called at the head of the R script when required.

**Data generation**

Each run of the *Full_dynamic_model.R* script produces 4 .csv files, containing two identical rows of data. These four files describe the dynamics of two state variables (body length and energetic state) for each month of life, as well as the corresponding reproductive output and survival probability.

The filepath is first set to make sure the output files are stored in a unique directory.The parameters determining environmental scenario are defined at the top of this master script (lines 7-38). There is a toggle to turn seasonality on and off (line 9).

Lines 41-83 calculate the income and metabolic costs for every possible state for that environmental scenario, to look up later.

Lines 88-249 represent the dynamic model (the solution to the dynamic programming equation) Line and the forward simulation are in the same file.

Lines 252-264, if uncommented, are used to plot Supp. Fig. 3.

Lines 270-274 are used in the forward simultion of the optimal life history.

The output files are written on lines 379-383. The key parameter values (metadata) are stored in the output file names. There are a few key pieces of information necessary to interpret each set of four files that are not in the filename, such as whether they are seasonal scenarios, or a special sensitivity scenario. These correspond to the folder structure.

**Folder structure**

The raw model output folder structure is organized so that the model runs represented in Figs 4-5 and the constant scenarios in Fig 6 are found in folders of corresponding names. The results presented in main text Fig 6C and 6F, as well most of the sensitivity analyses in Fig. 7 are in the *seasonal_results* subfolder, which contains subfolders corresponding to each seasonal scenario. For example, for the seasonal tuna life history in Fig. 6C and 6F, there is a folder named *tuna*. If there are subsets of model runs that are used to make specific comparisons, as in the supplemental tables, these may be duplicated and stored in their own subdirectory. However, as explained below, inside each script files, the filepath and filenames can be combined to identify all metadata for the model outputs presented.

The folder *Code* contains a representative version of the main model script (Full_dynamic_model.R, described above). This script can be adjusted manually to recreate each model run (according to kappa, temperature, seasonality, as well as other parameters). This folder also holds all scripts necessary to combine and analyze the raw model outputs and create each figure, including main text Figs. 4-7 and all 10 supplementary figures.

-   The code to produce Fig.1 and Supp. Figs. 1 and 2 (which illustrate model assumptions) have their own scripts.

-   The code to create Supp. Fig. 3 is found in the *Full_dynamic_model* script, as it illustrates one of the internal data structures of the model (described above).

-   Supp. Fig. 4 can be made with the script *Figure 4.R* as it presents the same data in a different way.

To make the Figs 4-7 and supplemental figs 5-10, the primary model outputs of length and reproduction were combined, along with survival data, using functions that are defined in the first half of each figure script.

For example, in the script *Figure 4.R*, lines 1-64 define functions that are used repreatedly to generate the summary figures in the main text results (and supplementary material). These functions are applied to multiple files in a folder to plot raw model outputs in a systematic way. The files in the appropriate working directory are read in (lines 70-87) and the analysis functions are applied to multipe files to combine model outputs in a table format, which is necessary for the summary plot in the main text, which synthesizes information from five different model scenarios (lines 114-152). The code for the alternate plot presented in Supp. Fig. 4 follows (lines 161-194).

If you want to trace the data points (model output) presented in Figs 4-7 and supplemental figs 5-10 to their source files, the complete filepath (including the working directory) called in each figure script has the metadata necessary to know the environmental scenario parameters corresponding to each data point.

For example, in Fig. 5, there are 80 corresponding output files.

-   There is a file for each of the 20 body length data points presented in panel A.

-   The 20 survival output files are necessary to calculate expected lifetime reproduction and combined with 20 reproductive output points in panel B.

-   The remaining 20 state data files were used in preliminary analyses and model debugging, and are now stored just for completeness.
