# Script: 6_tables_graphs_script.R
# Author: V. van Hees, Accelting
# Date: 2022
# R script to process accelerometer data

## Instructions for running with old output folder:
# - Move elsewhere or Delete config.csv
# - Keep folder meta/basic, but delete (or move elsewhere): meta/ms2.out, meta/ms3.out, meta/ms4.out and meta/ms5.out/
# - The expected folder structure of the output folder is:
#  output_colausggir/
#     meta/
#       basic/ <= here you will put all your RData files
#     results/
#       QC/

## Installation instructions:
install.packages(remotes)
remotes::install_github("wadpac/GGIR", ref = "2.4-1")


# no need to update datadir below.
# the key is that it ends with 'colausggir' and that this word is consistent with the output folder names 'output_colausggir'.
datadir = "/imaginary/path/no/need/to/update/this/colausggir" 

#==================================================================
# INPUT NEEDED:
# Specify your output directory, this should not be identical to datadir
# so if you put output_colausggir inside C:/mydata
# then this should read: datadir = C:/mydata
outputdir = "/location/of/the/parent/directory/of/output/your/folder" 

#==================================================================
f0 = c()
f1 = c()
t.start = Sys.time()
library(GGIR)

g.shell.GGIR(#=======================================
             # INPUT NEEDED:
             #-------------------------------
             # General parameters
             #-------------------------------
             mode = c(2:5), #<= add 1 if also part 1 needs to be processed
             datadir = datadir, #specify above
             outputdir = outputdir, #specify above
             do.report = c(2, 4, 5), #for what parts does and report need to be generated? (option: 2, 4 and 5)
             f0 = f0, #specify above
             f1 = f1, #specify above
             overwrite = TRUE, #overwrite previous milestone data?
             do.parallel = T,
             idloc = 1, #<= CHECK need to be 1
             print.filename = TRUE,
             storefolderstructure = FALSE,
             # data_cleaning_file = data_cleaning_file,
             desiredtz = "Europe/London",
             #-------------------------------
             # Part 1 parameters:
             #-------------------------------
             # Key functions: reading file, auto-calibration, and extracting features
             do.enmo = TRUE, #Needed for physical activity analysis
             do.anglez = TRUE, #Needed for sleep detection
             chunksize = 1, #size of data chunks to be read (value = 1 is maximum)
             printsummary = TRUE,
             #-------------------------------
             # Part 2 parameters:
             #-------------------------------
             # Key functions: Non-wear detection, imputation, and basic descriptives
             strategy = 1, #Strategy (see tutorial for explanation)
             hrs.del.start = 1, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
             hrs.del.end = 1, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
             maxdur = 15, # How many DAYS of measurement do you maximumally expect?
             includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
             M5L5res = 10, #resolution in minutes of M5 and L5 calculation
             winhr = c(5,10), # size of M5 and L5 (5 hours by default)
             qlevels = c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
             qwindow = c(0,24), #window over which to calculate quantiles
             ilevels = c(seq(0,400, by = 50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
             iglevels = TRUE, # intensitygradient levels
             mvpathreshold = c(100), #MVPA (moderate and vigorous physical activity threshold
             IVIS.activity.metric = 2,
             #-------------------------------
             # Part 3 parameters:
             #-------------------------------
             # Key functions: Sleep detection
             timethreshold = 5, #10
             anglethreshold = 5,
             ignorenonwear = TRUE, # if TRUE non-wear is not detected as sleep (if FALSE then it will work with imputed data)
             do.part3.pdf = FALSE,
             #-------------------------------
             # Part 4 parameters:
             #-------------------------------
             # Key functions: Integrating sleep log (if available) with sleep detection, storing day and person specific summaries of sleep
             excludefirstlast = TRUE, # Exclude first and last night for sleep analysis?
             includenightcrit = 16, # number of minimum valid hours in a day to attempt sleep analysis
             
             # If sleep log is available:
             loglocation = c(), # loglocation, # full directory and name of the log (if available, otherwise leave value as c() )
             # outliers.only = FALSE,# <= Check
             # criterror = 4,
             relyonguider = FALSE,
             do.visual = FALSE,
             nnights = 14, #number of nights in the sleep log

             #-------------------------------
             # Part 5 parameters:
             #-------------------------------
             # Key functions: Merging physical activity with sleep analyses
             excludefirstlast.part5 = FALSE,
             threshold.lig = c(40, 50, 60), # <= Check (NEEDS MORE INVESTIGATION)
             threshold.mod = c(100, 110, 120), # <= Check (NEEDS MORE INVESTIGATION)
             threshold.vig = c(400), # <= Check
             boutcriter.in = 1, # <= Check
             boutcriter.lig = 1, # <= Check
             boutcriter.mvpa = 1, # <= Check
             boutdur.in = c(10, 30), # duration of bouts to be calculated
             boutdur.lig = c(10), # duration of bouts to be calculated
             boutdur.mvpa = c(10), # duration of bouts to be calculated
             timewindow = c("WW"), #
             save_ms5rawlevels = FALSE,
             part5_agg2_60seconds = TRUE, #<= Check
             includedaycrit.part5 = 2/3,
             minimum_MM_length.part5 = 23,
             frag.metrics = "all", #<= Check

             #-----------------------------------
             # Report generation
             #-------------------------------
             # Key functions: Generating reports based on meta-data
             visualreport = FALSE,
             dofirstpage = FALSE, #first page of pdf-report with simple summary histograms
             viewingwindow = 1) #viewingwindow of visual report: 1 centres at day and 2 centers at night
t.end = Sys.time()

delta_t = difftime(t.end, t.start, units = "mins")