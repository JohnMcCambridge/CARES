### Data Dictionary Exploration
### kbmorales@protonmail.com
###


# Setup -------------------------------------------------------------------

# Data prep should be performed first
# source("code/ppp data script.R")

library(tidyverse)
library(lubridate)

# Summaries ---------------------------------------------------------------

glimpse(adbs)


# Further clean -----------------------------------------------------------

adbs %>% 
  mutate(JobsRetained = as.numeric(JobsRetained),
         DateApproved = as.Date(DateApproved,
                                 "%m/%d/%Y")
         )


# EDA ---------------------------------------------------------------------

# LoanAmount  
hist(as.numeric(adbs$LoanAmount))

# State
adbs %>% 
  count(State) %>% 
  arrange(desc(n)) %>% 
  filter(row_number()<=10) %>% 
  ggplot(aes(x = reorder(State,n),
             y = n)) +
  geom_col()
