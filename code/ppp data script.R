# R version used with this script: 4.0.2
# RStudio used with this script: 1.3.1056
# packages used with this script last updated: July 24th 2020

rm(list=ls()) # clean up workspace before beginning

# load all needed libraries upfront
library("tidyverse") # used for merging the various CSV files and manipulating the data

# set your local working directory (until we get a shared location)
setwd("C:/Users/John/Dropbox/CARES Act/PPP/")

# ideally we will use standardized directory structure atop the working directory, specified here for All Data by State, a direct extract of the SBA data zip files
reldir <- "data/All Data by State/All Data by State"

dat_files <- list.files(reldir, full.names = T, recursive = T, pattern = ".*.csv") # scan through all directories and subdirectories for all CSVs

# read in each CSV, all as character values, to allow for a clean import with no initial manipulation
# for each file, attached the name of the data source file
all_data_by_state <- map_df(dat_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                              mutate(source_file = str_remove_all(.x, "data/20200722/All Data by State/All Data by State/"))
                           )
adbs <- all_data_by_state

### Data Check: ZipCodes ###

# generate table to check lengths and also run a simple grep ZIP validator
table(grepl("\\d{5}([ \\-]\\d{4})?", adbs$Zip), nchar(adbs$Zip),useNA = "always", dnn = c("Passes Simple ZIP Validation","Number of Characters"))

# OUTPUT:
#                             Number of Characters
# Passes Simple ZIP Validation        5    <NA>
#                         FALSE       0     224
#                         TRUE  4885164       0
#                         <NA>        0       0
#                        
# result indicates that all present values are of valid length as simple 5 digit zips, however 224 are missing entirely and are recorded in original data as NAs

# next let's use a table to determine if the zips codes are 'real' rather than just 'valid', and if they map to the specified states recorded in row or source csv name
