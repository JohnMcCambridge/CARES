### Merge all PPP loan data files from SBA
### john.patrick.mccambridge@gmail.com
### This script combines all PPP csvs into a single unified file and create basic needed variables to allow analysis across all fields for all rows
### updated to load Dec 2020 release

### Working Directory Reminder  ---------------------------------------------

#setwd("C:/Users/John/Dropbox/GitHub/CARES/")
cat(sprintf("This script expects your working directory to be the base directory of the /CARES/ github repo structure, i.e., /CARES\nCurrent working directory: %s\n", getwd()))

### Libraries ---------------------------------------------------------------

library("tidyverse")  # merging, manipulating


### Read --------------------------------------------------------------------

# set relative directory to search then scan through subdirectories for CSVs
csv_dir <- paste(getwd(),"/data/120120 Paycheck Protection Program Data", sep="")
cat(sprintf("Looking for data files in: %s\n", csv_dir))
csv_files <- list.files(csv_dir, full.names = T, recursive = T, pattern = ".*.csv") 

# read in each CSV as character values, to allow for a clean import, attach the name of the data source file
adbs <- map_df(csv_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                  mutate(source_file = str_remove_all(.x, ".*/"))
               )

### Clean -------------------------------------------------------------------

# Create unified Loan Amount / Loan Range cuts
# as of Dec 2020 data the Loan Range variable no longer exists, we will recreate it

adbs <- adbs %>% 
  mutate(LoanRange_Unified = case_when(as.numeric(LoanAmount) > 5000000 & as.numeric(LoanAmount) <= 10000000 ~ "a $5 million - $10 million",
                                       as.numeric(LoanAmount) > 2000000 & as.numeric(LoanAmount) <=  5000000 ~ "b $2 million - $5 million",
                                       as.numeric(LoanAmount) > 1000000 & as.numeric(LoanAmount) <=  2000000 ~ "c $1 million - $2 million",
                                       as.numeric(LoanAmount) >  350000 & as.numeric(LoanAmount) <=  1000000 ~ "d $350,000 - $1 million",
                                       as.numeric(LoanAmount) >  150000 & as.numeric(LoanAmount) <=   350000 ~ "e $150,000 - $350,000",
                                       as.numeric(LoanAmount) >  125000 & as.numeric(LoanAmount) <=   150000 ~ "f $125,000 - $150,000",
                                       as.numeric(LoanAmount) >  100000 & as.numeric(LoanAmount) <=   125000 ~ "g $100,000 - $125,000",
                                       as.numeric(LoanAmount) >   75000 & as.numeric(LoanAmount) <=   100000 ~ "h  $75,000 - $100,000",
                                       as.numeric(LoanAmount) >   50000 & as.numeric(LoanAmount) <=    75000 ~ "i  $50,000 -  $75,000",
                                       as.numeric(LoanAmount) >   25000 & as.numeric(LoanAmount) <=    50000 ~ "j  $25,000 -  $50,000",
                                       as.numeric(LoanAmount) >    1000 & as.numeric(LoanAmount) <=    25000 ~ "k   $1,000 -  $25,000",
                                       as.numeric(LoanAmount) >     100 & as.numeric(LoanAmount) <=     1000 ~ "l     $100 -    $1000",
                                       as.numeric(LoanAmount) >      10 & as.numeric(LoanAmount) <=      100 ~ "m      $10 -     $100",
                                       as.numeric(LoanAmount) >       0 & as.numeric(LoanAmount) <=       10 ~ "n           Up to $10",
                                       as.numeric(LoanAmount) ==      0                                      ~ "o                Zero",
                                       as.numeric(LoanAmount) <       0                                      ~ "p      Less than Zero",
                                       TRUE ~ "Unknown"))

# Create Jobs Retained cuts

adbs$JobsRetained <- "Source Field No Longer Available as of 0808"
adbs$JobsRetained_Grouped <- "Computed Field No Longer Available as of 0808"
  
adbs <- adbs %>%
  mutate(JobsReported_Grouped = case_when(as.numeric(JobsReported) > 400 & as.numeric(JobsReported) <= 500 ~ "a 400 - 500",
                                          as.numeric(JobsReported) > 300 & as.numeric(JobsReported) <= 400 ~ "b 300 - 400",
                                          as.numeric(JobsReported) > 200 & as.numeric(JobsReported) <= 300 ~ "c 200 - 300",
                                          as.numeric(JobsReported) > 100 & as.numeric(JobsReported) <= 200 ~ "d 100 - 200",
                                          as.numeric(JobsReported) >  50 & as.numeric(JobsReported) <= 100 ~ "e  50 - 100",
                                          as.numeric(JobsReported) >  25 & as.numeric(JobsReported) <=  50 ~ "f  25 -  50",
                                          as.numeric(JobsReported) >  10 & as.numeric(JobsReported) <=  25 ~ "g  10 -  25",
                                          as.numeric(JobsReported) >   5 & as.numeric(JobsReported) <=  10 ~ "h   5 -  10",
                                          as.numeric(JobsReported) >   1 & as.numeric(JobsReported) <=   5 ~ "i   2 -   5",
                                          as.numeric(JobsReported) >   0 & as.numeric(JobsReported) <=   1 ~ "j         1",
                                          as.numeric(JobsReported) ==     0                                ~ "k      Zero",
                                          as.numeric(JobsReported) <      0                                ~ "l  Negative",
                                          is.na(JobsReported) ~ NA_character_,
                                          TRUE ~ "Unknown"))   


### Tidy Workspace ---------------------------------------------------------

rm(csv_dir,
   csv_files)
