# Authors: Zuoyu Wang
# Contact: zuoyu.wang@mail.utoronto.ca
# Date: 21 December 2020
# License: MIT
# Purpose: The purpose of this code is to clean-up the 2017 GSS data obtained 
# from the U of T library. (http://www.chass.utoronto.ca/)

# Pre-reqs: You need to have downloaded the data from the library. To do that: 
## 1. Go to: http://www.chass.utoronto.ca/
## 2. Data centre --> UofT users or http://dc.chass.utoronto.ca/myaccess.html
## 3. Click SDA @ CHASS, should redirect to sign in. Sign in.
## 4. Continue in English (you're welcome to use the French, but we probably can't
## help you too much).
## 5. Crtl F GSS, click
## 6. Click "Data" on the one you want. We used 2017, but you may want a different 
## wave. In particular the General Social Survey on social identity (cycle 27), 
## 2013 has some variables on voter participation if you're into that sort of 
## thing. You're welcome to pick any year but this code applies to 2017.
## 7. Click download
## 8. Select CSV data file, data definitions for STATA (gross, but stick with it for now).
## 9. Can select all variables by clicking button next to green colored "All". Then continue.
## 10. Create the files, download and save
# Check: 
## You WILL need to change the raw data name. Search for .csv - line 41
## You may need to adjust the filepaths depending on your system. Search for: read_

#### Workspace set-up ####
library(janitor)
library(tidyverse)

# Load the data dictionary and the raw data and correct the variable names
setwd("/Users/tonystark/Desktop/304 Final/")
raw_data <- read_csv("AAmQKrC5.csv")

dict <- read_lines("gss_dict.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("gss_labels.txt")

#### Set-up the dictionary ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))


#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>% 
  select(#6-9 missing
         sex,
         rlr_110,
         brthcan,
         cow_10,
         chrinhdc,
         
         # 96-99 missing
         prv,
         famincg2,
         lanmt,
         agegr10,
         marstat,
         hsdsizec,
         ehg3_01b,
         agedc,
  ) %>%
  mutate_at(vars(prv:agedc), .funs = funs(ifelse(. >= 96, NA, .))) %>%
  mutate_at(vars(sex:chrinhdc), .funs = funs(ifelse(. >= 6, NA, .)))%>%
  mutate_at(.vars = vars(sex:ehg3_01b),
            .funs = funs(eval(
              parse(
                text = cw_statements %>%
                  filter(variable_name == deparse(substitute(.))) %>%
                  select(cw_statement) %>%
                  pull()
              )
            )))

# Fix the names
gss <- gss %>%
  clean_names() %>%
  rename(
    Province = prv,
    Gender = sex,
    education = ehg3_01b,
    religion_importance = rlr_110,
    born_canada = brthcan,
    language_mothertongue = lanmt,
    employee = cow_10,
    num_child = chrinhdc,
    family_income = famincg2,
    marriage_status = marstat,
    household_size = hsdsizec,
    age = agedc,
    age_group = agegr10,
  )


#### Clean up ####

# Province ()
gss <- gss %>% filter(Province != 'NA')

# Gender (Female/Male)
gss <- gss %>% filter(Gender != 'NA')

# Education (whether above University degree)
gss <- gss %>%
  filter(education != 'NA') %>%
  mutate(Education = ifelse(
    education %in% c(
      "Less than high school diploma or its equivalent",
      "High school diploma or a high school equivalency certificate",
      "Trade certificate or diploma",
      "College, CEGEP or other non-university certificate or di..."
    ),
    'Below University Degree',
    'University Degree and Above'
  ))

# Religion (Important or NotImportant)
gss <- gss %>%
  filter(religion_importance != 'NA') %>%
  mutate(
    Religion = case_when(
      religion_importance %in% c("Somewhat important", "Very important") ~ 'Important',
      religion_importance %in% c("Not very important", "Not at all important") ~ "NotImportant",
    )
  )

# BornInCA (Yes or No)
gss <- gss %>%
  filter(born_canada != 'NA') %>%
  mutate(
    BornInCA = case_when(
      born_canada == "Born in Canada" ~ 'Yes',
      born_canada == "Born outside Canada" ~ 'No',
    )
  )

# MotherTongue (English/French/NonOfficial/Multiple)
gss <- gss %>%
  filter(language_mothertongue != 'NA') %>%
  mutate(
    MotherTongue = case_when(
      language_mothertongue == "English" ~ 'English',
      language_mothertongue == "French" ~ 'French',
      language_mothertongue == "Non-official languages" ~ 'NonOfficial',
      TRUE ~ 'Multiple',
    )
  )

# Employment (Employed/Other)
gss <- gss %>%
  filter(employee != 'NA') %>%
  mutate(Employment = case_when(
    employee %in% c("Employee", "Self-employed") ~ 'Employed',
    TRUE ~ 'Other'
  ))

# HaveChild (Yes/No)
gss <- gss %>%
  filter(num_child != 'NA') %>%
  mutate(HaveChild = case_when(num_child == "No child" ~ 'No',
                               TRUE ~ 'Yes'))

# FamilyIncome (Below $50,000/$50,000-$99,999/$100,000 and Above)
gss <- gss %>%
  filter(family_income != 'NA') %>%
  mutate(
    FamilyIncome = case_when(
      family_income %in% c('Less than $25,000', '$25,000 to $49,999') ~ 'Below $50,000',
      family_income %in% c('$50,000 to $74,999', '$75,000 to $99,999') ~ '$50,000-$99,999',
      TRUE ~ '$100,000 and Above'
    )
  )


# MarriageStatus (Married/Other)
gss <- gss %>%
  filter(marriage_status != 'NA') %>%
  mutate(
    MarriageStatus = case_when(
      marriage_status == 'Married' ~ 'Married',
      TRUE ~ 'Other'
    )
  )

# HouseholdSize (One/Two/Three/Four/Five or More)
gss <- gss %>%
  filter(household_size != 'NA') %>%
  mutate(
    HouseholdSize = case_when(
      household_size == 'One person household' ~ 'One',
      household_size == 'Two person household' ~ 'Two',
      household_size == 'Three person household' ~ 'Three',
      household_size == 'Four person household' ~ 'Four',
      TRUE ~ 'Five or More'
    )
  )

# AgeGroup (18-34/35-54/55 and Higher )
gss <- gss %>%
  filter(age != 'NA') %>%
  mutate(age = age + 2) %>% 
  mutate(
    AgeGroup = case_when(
      (18 <= age & age <= 34) ~ '18-34',
      (35 <= age & age <= 54) ~ '35-54',
      (age >= 55) ~ '55 and Higher',
    )
  ) %>% filter(!is.na(AgeGroup))

gss <- gss %>% select(
  Gender,
  Province ,
  Education ,
  Religion ,
  BornInCA ,
  MotherTongue ,
  Employment,
  HaveChild,
  FamilyIncome,
  AgeGroup
  
)
write_csv(gss, "gss.csv")
