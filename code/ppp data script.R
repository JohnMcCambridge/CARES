# R version used with this script: 4.0.2
# RStudio used with this script: 1.3.1056
# packages used with this script last updated: July 24th 2020
# PPP data downloaded 642pm BST, July 22nd 2020 from: https://sba.app.box.com/s/tvb0v5i57oa8gc6b5dcm9cyw7y2ms6pp 


# Setup -------------------------------------------------------------------

rm(list=ls()) # clean up workspace before beginning

# load all needed libraries upfront
library("tidyverse") # used for merging the various CSV files and manipulating the data
library("stringdist") # for fuzzy matching

# Read --------------------------------------------------------------------

# ideally we will use standardized directory structure atop the working directory, specified here for All Data by State, a direct extract of the SBA data zip files
reldir <- "../data/All Data by State/All Data by State"

dat_files <- list.files(reldir, full.names = T, recursive = T, pattern = ".*.csv") # scan through all directories and subdirectories for all CSVs

# read in each CSV, all as character values, to allow for a clean import with no initial manipulation
# for each file, attached the name of the data source file
all_data_by_state <- map_df(dat_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                              mutate(source_file = str_remove_all(.x, "data/20200722/All Data by State/All Data by State/"))
                           )
adbs <- all_data_by_state


# Clean -------------------------------------------------------------------

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
# let's code this into a new variable, so that we can later evaluate each row for validation along various checks
adbs <- adbs %>% 
  mutate(Zip_Valid_Format = case_when(grepl("\\d{5}", Zip)        ~ "Pass: 5 Digit Format",
                                      grepl("\\d{5}-\\d{4}", Zip) ~ "Pass: 5dash4 Digit Format ",
                                      TRUE ~ "Fail"))




### Data Check: City Names ###
# check City values against a large list of likely names, via: https://simplemaps.com/data/us-cities
uscities <- read.csv("../data/simplemaps_uscities_basicv1.6/uscities.csv")

citydict <- sort(unique(tolower(gsub("[[:digit:][:space:][:punct:]]", "", uscities$city))))
adbscities <- sort(unique(tolower(gsub("[[:digit:][:space:][:punct:]]", "", adbs$City))))

citymatch_01 <- amatch(adbscities, citydict, method = "lv", maxDist = 0.1)
citymatch_05 <- amatch(adbscities, citydict, method = "lv", maxDist = 0.5)
citymatch_10 <- amatch(adbscities, citydict, method = "lv", maxDist = 1.0)

adbscities <- as.data.frame(adbscities)
adbscities$match_01 <- citydict[citymatch_01]
adbscities$match_05 <- citydict[citymatch_05]
adbscities$match_10 <- citydict[citymatch_10]

# confirm all Jobs Retained values are integers (whole numbers) or NAs
summary(near(as.numeric(adbs$JobsRetained), as.integer(as.numeric(adbs$JobsRetained))))




### Create unified Loan Amount / Loan Range cuts
adbs <- adbs %>% 
  mutate(LoanRange_Unified = case_when(!is.na(LoanRange) ~ LoanRange,
                                       is.na(LoanRange) & as.numeric(LoanAmount) > 125000 & as.numeric(LoanAmount) <= 150000 ~ "f $125,000 - $150,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) > 100000 & as.numeric(LoanAmount) <= 125000 ~ "g $100,000 - $125,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  75000 & as.numeric(LoanAmount) <= 100000 ~ "h  $75,000 - $100,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  50000 & as.numeric(LoanAmount) <=  75000 ~ "i  $50,000 -  $75,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  25000 & as.numeric(LoanAmount) <=  50000 ~ "j  $25,000 -  $50,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >   1000 & as.numeric(LoanAmount) <=  25000 ~ "k   $1,000 -  $25,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >    100 & as.numeric(LoanAmount) <=   1000 ~ "l     $100 -    $1000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >     10 & as.numeric(LoanAmount) <=    100 ~ "m      $10 -     $100",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >      0 & as.numeric(LoanAmount) <=     10 ~ "n           Up to $10",
                                       is.na(LoanRange) & as.numeric(LoanAmount) ==     0                                    ~ "o                Zero",
                                       is.na(LoanRange) & as.numeric(LoanAmount) <      0                                    ~ "p      Less than Zero",
                                       TRUE ~ "Unknown"))

# create for each loan that has no specific LoanAmount a numeric max/min value, to allow for quick computation of max/min totals
# for entries with specific LoanAmount values, use those as they are



### Create Jobs Retained cuts
adbs <- adbs %>%
  mutate(JobsRetained_Grouped = case_when(as.numeric(JobsRetained) > 400 & as.numeric(JobsRetained) <= 500 ~ "a 400 - 500",
                                          as.numeric(JobsRetained) > 300 & as.numeric(JobsRetained) <= 400 ~ "b 300 - 400",
                                          as.numeric(JobsRetained) > 200 & as.numeric(JobsRetained) <= 300 ~ "c 200 - 300",
                                          as.numeric(JobsRetained) > 100 & as.numeric(JobsRetained) <= 200 ~ "d 100 - 200",
                                          as.numeric(JobsRetained) >  50 & as.numeric(JobsRetained) <= 100 ~ "e  50 - 100",
                                          as.numeric(JobsRetained) >  25 & as.numeric(JobsRetained) <=  50 ~ "f  25 -  50",
                                          as.numeric(JobsRetained) >  10 & as.numeric(JobsRetained) <=  25 ~ "g  10 -  25",
                                          as.numeric(JobsRetained) >   5 & as.numeric(JobsRetained) <=  10 ~ "h   5 -  10",
                                          as.numeric(JobsRetained) >   1 & as.numeric(JobsRetained) <=   5 ~ "i   2 -   5",
                                          as.numeric(JobsRetained) >   0 & as.numeric(JobsRetained) <=   1 ~ "j         1",
                                          as.numeric(JobsRetained) ==     0                                ~ "k      Zero",
                                          as.numeric(JobsRetained) <      0                                ~ "l  Negative",
                                          is.na(JobsRetained) ~ NA_character_,
                                          TRUE ~ "Unknown"))   


# Florida Subset ----------------------------------------------------------

# adbs_fl <- adbs[adbs$State=="FL",]
# 
# # Write to CSV
# write.csv(adbs_fl,
#           "data/adbs_fl.csv")

# Cleanup -----------------------------------------------------------------

rm(dat_files, 
   all_data_by_state,
   reldir)
