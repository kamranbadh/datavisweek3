library(tidyverse)
library(haven)
library (tibble)
library (dplyr)
install.packages("rmarkdown")
install.packages("knitr")

# outcome 1: load data with readxl package
install.packages('readxl')
library(readxl)

usma <- read_excel("C:\\Users\\kamra\\OneDrive\\Documents\\
                   Seattle University - Postgrad\\5210 - Data Visualization\\
                   datavisweek3\\USMA_Progression.xlsx")
view (usma)

# outcome 2: recreate femalespeers, malespeers, and totpeople columns 
usma <- usma %>%
  mutate(
    femalespeers2 = 
    malespeers2 = 
    totpeople2 = 
  )

# outcome 3: investigate issues, do you trust yourself or the recreation more?
errors <- usma %>%
  filter(femalespeers2 != femalespeers |
           malespeers2 != malespeers |
           totpeople2 != totpeople)
view (errors)
# Any ideas what the issue might be? Do you trust the original or your 
# recreation more?

# the issue could be a few of the following:
# 1. The data could be entered incorrectly.
# 2. There could be errors in the calculations.
# 3. There could be missing or incorrect data. 

# to be completely honest, I trust the original more, because there might have
# been mistakes in calculation or the program, the fact that there are
# these errors makes me believe that the original may be more reliable.

# outcome 4: create columns for company and division
usma <- usma %>%
  mutate(
    company = substr(company_n, 1, 1),
    division = as.numeric(substr(company_n, 3, 3))
  )

# outcome 5: Limit the data just to years in which you have all four 
# classes present in full quantity
fullclass <- usma %>%
  group_by(year) %>%
  filter(n_distinct(class) == 4) %>%
  pull(year) %>%
  unique()

classfull <- fullclass %>%
  filter(year %in% fullclass)

# outcome 6: make the following tables

# 6a: Top four companies (A, B, C, etc., not A-1, A-2) with the highest 
# continue_or_grad rates
top_companies <- classfull %>%
  group_by(company) %>%
  summarise(gradrate = mean(continue_or_grad)) %>%
  top_n(4, wt = gradrate) %>%
  arrange(desc(gradrate))

view (top_companies)

# 6b: continue_or_grad rates by class
class_grad_rates <- classfull %>%
  group_by(class) %>%
  summarise(gradrate = mean(continue_or_grad))

view (class_grad_rates)

# 6c: continue_or_grad rates of women by class
womenclass <- classfull %>%
  filter(gender == "F") %>%
  group_by(class) %>%
  summarise(gradrate = mean(continue_or_grad))

view (womenclass)




