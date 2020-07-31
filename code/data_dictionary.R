### Data Dictionary Exploration
### kbmorales@protonmail.com
###


# Setup -------------------------------------------------------------------

# Data prep should be performed first
# source("code/ppp data script.R")

library(tidyverse)
library(lubridate)
library(knitr)
library(stringdist) # for fuzzy matching

# Summaries ---------------------------------------------------------------

glimpse(adbs)

# Data Check: ZipCodes ----------------------------------------------------

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

table(adbs$Zip_Valid_Format, useNA = "always")


### Data Check: State Names -----------------------------------------------
# check against US Census data: American National Standards Institute (ANSI) Codes for States, the District of Columbia, Puerto Rico, and the Insular Areas of the United States
#via: https://www.census.gov/library/reference/code-lists/ansi.html
statecodes <- read.table("../data/census/state.txt", sep = "|", header = TRUE)

table(adbs$State[(!adbs$State %in% statecodes$STUSAB)]) # list counts of unmatched results, showing 1 AE, 1 FI, 210 XX

adbs[adbs$State == "FI",] # based on the ZIP code, this should be FL, and can be 'fixed' easily enough as part of final data cleaning
adbs[adbs$State == "AE",] # when viewing ZIP, Lending data, this does indeed appear to be tied to a military address in Europe, Middle East, Africa or Canada


### Data Check: City Names -------------------------------------------------
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

# this output is showing issues: for example, "schicago" matches to chicago, but really it is more likely to be "South Chicago"
# a hand built list, with the above as a baseline, may be most effective.

### Data Check: Jobs Retained ----------------------------------------------

#confirm all Jobs Retained values are integers (whole numbers) or NAs
summary(near(as.numeric(adbs$JobsRetained), as.integer(as.numeric(adbs$JobsRetained))))

# Further clean -----------------------------------------------------------

### Coersions
adbs = adbs %>% 
  mutate(JobsRetained = as.numeric(JobsRetained),
         DateApproved = as.Date(DateApproved,
                                 "%m/%d/%Y"),
         LoanAmount = as.numeric(LoanAmount))

### Duplicates

## Takes a long time! Be patient.

# sum(duplicated(adbs))
# 4353 exact duplicates

adbs_dupes=adbs[duplicated(adbs),]

View(adbs_dupes)

# Check for missingness ---------------------------------------------------

## Missingness summary table
adbs %>% 
  select(-source_file,
         -Zip_Valid_Format,
         -LoanRange_Unified,
         -JobsRetained_Grouped) %>% 
  mutate_all(is.na) %>% 
  summarise_all(~sum(.)) %>% 
  gather("variable", "n_missing") %>% 
  mutate(perc_missing = round(n_missing / nrow(adbs) * 100, 1)) %>% 
  kable()

# EDA ---------------------------------------------------------------------

## Loan Amount

range(adbs$LoanAmount[!is.na(adbs$LoanAmount)])
# Negative values?

# 46 with negative values
adbs %>% 
  filter(LoanAmount <= 0) %>% 
  summarise(n = n())

adbs %>% 
  filter(LoanAmount > 0) %>% 
  ggplot(aes(x = LoanAmount)) +
  geom_histogram()

## State
adbs %>% 
  count(State) %>% 
  arrange(desc(n)) %>% 
  filter(row_number()<=10) %>% 
  ggplot(aes(x = reorder(State,n),
             y = n)) +
  geom_col()


