library("tidyverse") # used for merging the various CSV files and manipulating the data


### Read -----------------------------------------------------------------------
#setwd("C:/Users/John/Dropbox/GitHub/CARES/")
cat(sprintf("This script expects your working directory to be the base directory of the /CARES/ github repo structure, i.e., /CARES\nCurrent working directory: %s\n", getwd()))


csv_dir <- paste(getwd(),"/data/All Data by State", sep="")
cat(sprintf("Looking for data files in: %s\n", csv_dir))
csv_files <- list.files(csv_dir, full.names = T, recursive = T, pattern = ".*.csv") 

# read in each CSV, all as character values, to allow for a clean import with no initial manipulation
# for each file, attached the name of the data source file
adbs_jul2020 <- map_df(csv_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                mutate(source_file = str_remove_all(.x, ".*/"))
                )



# set relative directory to search then scan through subdirectories for CSVs
csv_dir <- paste(getwd(),"/data/All Data 0808", sep="")
cat(sprintf("Looking for data files in: %s\n", csv_dir))
csv_files <- list.files(csv_dir, full.names = T, recursive = T, pattern = ".*.csv") 

# read in each CSV as character values, to allow for a clean import, attach the name of the data source file
adbs_aug2020 <- map_df(csv_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                mutate(source_file = str_remove_all(.x, ".*/"))
                )



# set relative directory to search then scan through subdirectories for CSVs
csv_dir <- paste(getwd(),"/data/120120 Paycheck Protection Program Data", sep="")
cat(sprintf("Looking for data files in: %s\n", csv_dir))
csv_files <- list.files(csv_dir, full.names = T, recursive = T, pattern = ".*.csv") 

# read in each CSV as character values, to allow for a clean import, attach the name of the data source file
adbs_dec2020 <- map_df(csv_files, ~read_csv(.x, col_types = cols(.default = "c")) %>%
                mutate(source_file = str_remove_all(.x, ".*/"))
                )



### Merge Rows -----------------------------------------------------------------

adbs <- bind_rows(list("1. Jul 2020" = adbs_jul2020, 
                       "2. Aug 2020" = adbs_aug2020, 
                       "3. Dec 2020" = adbs_dec2020), .id = "source")


### Synthesize Fields ----------------------------------------------------------

adbs <- adbs %>% 
  mutate(LoanRange_Unified = case_when(!is.na(LoanRange) ~ LoanRange,
                                       is.na(LoanRange) & as.numeric(LoanAmount) > 5000000 & as.numeric(LoanAmount) <= 10000000 ~ "a $5-10 million",
                                       is.na(LoanRange) & as.numeric(LoanAmount) > 2000000 & as.numeric(LoanAmount) <=  5000000 ~ "b $2-5 million",
                                       is.na(LoanRange) & as.numeric(LoanAmount) > 1000000 & as.numeric(LoanAmount) <=  2000000 ~ "c $1-2 million",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  350000 & as.numeric(LoanAmount) <=  1000000 ~ "d $350,000-1 million",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  150000 & as.numeric(LoanAmount) <=   350000 ~ "e $150,000-350,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  125000 & as.numeric(LoanAmount) <=   150000 ~ "f $125,000-150,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >  100000 & as.numeric(LoanAmount) <=   125000 ~ "g $100,000-125,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >   75000 & as.numeric(LoanAmount) <=   100000 ~ "h  $75,000-100,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >   50000 & as.numeric(LoanAmount) <=    75000 ~ "i  $50,000- 75,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >   25000 & as.numeric(LoanAmount) <=    50000 ~ "j  $25,000- 50,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >    1000 & as.numeric(LoanAmount) <=    25000 ~ "k   $1,000- 25,000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >     100 & as.numeric(LoanAmount) <=     1000 ~ "l     $100-   1000",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >      10 & as.numeric(LoanAmount) <=      100 ~ "m      $10-    100",
                                       is.na(LoanRange) & as.numeric(LoanAmount) >       0 & as.numeric(LoanAmount) <=       10 ~ "n        Up to $10",
                                       is.na(LoanRange) & as.numeric(LoanAmount) ==      0                                      ~ "o             Zero",
                                       is.na(LoanRange) & as.numeric(LoanAmount) <       0                                      ~ "p   Less than Zero",
                                       TRUE ~ "Uncategorized"))

adbs <- adbs %>%
  mutate(JobsRetained_Grouped = case_when(!is.na(JobsRetained) & as.numeric(JobsRetained) > 400 & as.numeric(JobsRetained) <= 500 ~ "a 400 - 500",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) > 300 & as.numeric(JobsRetained) <= 400 ~ "b 300 - 400",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) > 200 & as.numeric(JobsRetained) <= 300 ~ "c 200 - 300",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) > 100 & as.numeric(JobsRetained) <= 200 ~ "d 100 - 200",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >  50 & as.numeric(JobsRetained) <= 100 ~ "e  50 - 100",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >  25 & as.numeric(JobsRetained) <=  50 ~ "f  25 -  50",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >  10 & as.numeric(JobsRetained) <=  25 ~ "g  10 -  25",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >   5 & as.numeric(JobsRetained) <=  10 ~ "h   5 -  10",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >   1 & as.numeric(JobsRetained) <=   5 ~ "i   2 -   5",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) >   0 & as.numeric(JobsRetained) <=   1 ~ "j         1",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) ==     0                                ~ "k      Zero",
                                          !is.na(JobsRetained) & as.numeric(JobsRetained) <      0                                ~ "l  Negative",
                                          is.na(JobsRetained) ~ "No Data Available",
                                          TRUE ~ "Uncategorized"))   

adbs <- adbs %>%
  mutate(JobsReported_Grouped = case_when(!is.na(JobsReported) & as.numeric(JobsReported) > 400 & as.numeric(JobsReported) <= 500 ~ "a 400 - 500",
                                          !is.na(JobsReported) & as.numeric(JobsReported) > 300 & as.numeric(JobsReported) <= 400 ~ "b 300 - 400",
                                          !is.na(JobsReported) & as.numeric(JobsReported) > 200 & as.numeric(JobsReported) <= 300 ~ "c 200 - 300",
                                          !is.na(JobsReported) & as.numeric(JobsReported) > 100 & as.numeric(JobsReported) <= 200 ~ "d 100 - 200",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >  50 & as.numeric(JobsReported) <= 100 ~ "e  50 - 100",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >  25 & as.numeric(JobsReported) <=  50 ~ "f  25 -  50",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >  10 & as.numeric(JobsReported) <=  25 ~ "g  10 -  25",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >   5 & as.numeric(JobsReported) <=  10 ~ "h   5 -  10",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >   1 & as.numeric(JobsReported) <=   5 ~ "i   2 -   5",
                                          !is.na(JobsReported) & as.numeric(JobsReported) >   0 & as.numeric(JobsReported) <=   1 ~ "j         1",
                                          !is.na(JobsReported) & as.numeric(JobsReported) ==     0                                ~ "k      Zero",
                                          !is.na(JobsReported) & as.numeric(JobsReported) <      0                                ~ "l  Negative",
                                          is.na(JobsReported) ~ "No Data Available",
                                          TRUE ~ "Uncategorized"))   


adbs <- adbs %>% 
  mutate(Jobs_Numeric = case_when(source == "1. Jul 2020" ~ as.numeric(JobsRetained),
                                  source == "2. Aug 2020" ~ as.numeric(JobsReported),
                                  source == "3. Dec 2020" ~ as.numeric(JobsReported),
                                  TRUE ~ NA_real_))

adbs <- adbs %>% 
  mutate(Jobs_Grouped = case_when(!is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) > 400 & as.numeric(Jobs_Numeric) <= 500 ~ "a 400 - 500",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) > 300 & as.numeric(Jobs_Numeric) <= 400 ~ "b 300 - 400",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) > 200 & as.numeric(Jobs_Numeric) <= 300 ~ "c 200 - 300",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) > 100 & as.numeric(Jobs_Numeric) <= 200 ~ "d 100 - 200",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >  50 & as.numeric(Jobs_Numeric) <= 100 ~ "e  50 - 100",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >  25 & as.numeric(Jobs_Numeric) <=  50 ~ "f  25 -  50",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >  10 & as.numeric(Jobs_Numeric) <=  25 ~ "g  10 -  25",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >   5 & as.numeric(Jobs_Numeric) <=  10 ~ "h   5 -  10",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >   1 & as.numeric(Jobs_Numeric) <=   5 ~ "i   2 -   5",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) >   0 & as.numeric(Jobs_Numeric) <=   1 ~ "j         1",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) ==     0                                ~ "k      Zero",
                                  !is.na(Jobs_Numeric) & as.numeric(Jobs_Numeric) <      0                                ~ "l  Negative",
                                  is.na(Jobs_Numeric) ~ "No Data Available",
                                  TRUE ~ "Uncategorized"))   



adbs <- adbs %>% 
  mutate(LoanRangeMin = case_when(!is.na(LoanAmount) ~ as.numeric(LoanAmount),
                                  is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric( 5000000),
                                  is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 2000000),
                                  is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 1000000),
                                  is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric(  350000),
                                  is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  150000),
                                  TRUE ~ NA_real_))

adbs <- adbs %>% 
  mutate(LoanRangeMid = case_when(!is.na(LoanAmount) ~ as.numeric(LoanAmount),
                                  is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric( 7500000),
                                  is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 3500000),
                                  is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 1500000),
                                  is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric(  675000),
                                  is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  250000),
                                  TRUE ~ NA_real_))

adbs <- adbs %>% 
  mutate(LoanRangeMax = case_when(!is.na(LoanAmount) ~ as.numeric(LoanAmount),
                                  is.na(LoanAmount) & LoanRange == "a $5-10 million"       ~ as.numeric(10000000),
                                  is.na(LoanAmount) & LoanRange == "b $2-5 million"        ~ as.numeric( 5000000),
                                  is.na(LoanAmount) & LoanRange == "c $1-2 million"        ~ as.numeric( 2000000),
                                  is.na(LoanAmount) & LoanRange == "d $350,000-1 million"  ~ as.numeric( 1000000),
                                  is.na(LoanAmount) & LoanRange == "e $150,000-350,000"    ~ as.numeric(  350000),
                                  TRUE ~ NA_real_))



