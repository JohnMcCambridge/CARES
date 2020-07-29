### Data Dictionary Exploration
### kbmorales@protonmail.com
###


# Setup -------------------------------------------------------------------

# Data prep should be performed first
# source("code/ppp data script.R")

library(tidyverse)
library(lubridate)
library(knitr)

# Summaries ---------------------------------------------------------------

glimpse(adbs)

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
