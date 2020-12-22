# Authors: Zuoyu Wang
# Contact: zuoyu.wang@mail.utoronto.ca
# Date: 21 December 2020
# License: MIT
# Purpose: The purpose of this code is to clean-up the 2019 CES data obtained 
# from the 2019 CES website. (https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V)

# Pre-reqs: You need to have downloaded the data from the library. To do that: 
## 1. Go to: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/DUS88V
## 2. Download 2019 Canadian Election Study - Online Survey v1.0.dta

library(haven)
library(tidyverse)

survey_data <- read_dta("2019 Canadian Election Study - Online Survey v1.0.dta")

ces <- survey_data %>% 
  select(
    cps19_votechoice,
    cps19_vote_unlikely,
    cps19_province,
    cps19_gender,
    cps19_education,
    cps19_rel_imp,
    cps19_bornin_canada,
    cps19_language_68,
    cps19_language_69,
    cps19_language_70,
    cps19_language_71,
    cps19_language_72,
    cps19_language_73,
    cps19_language_74,
    cps19_language_75,
    cps19_language_76,
    cps19_language_77,
    cps19_language_78,
    cps19_language_79,
    cps19_language_80,
    cps19_language_81,
    cps19_language_82,
    cps19_language_83,
    cps19_language_84,
    cps19_language_85,
    cps19_employment, 
    cps19_children,
    cps19_income_number,
    cps19_marital,
    cps19_household,
    cps19_age
        )
         

ces <- ces %>% 
  rename(
    vote=cps19_votechoice,
    vote_unlikely=cps19_vote_unlikely,
    province=cps19_province,
    sex=cps19_gender,
    education=cps19_education,
    religion_importance=cps19_rel_imp,
    born_canada=cps19_bornin_canada,
    english=cps19_language_68,
    french=cps19_language_69,
    aboriginal=cps19_language_70,
    arabic=cps19_language_71,
    chinese=cps19_language_72,
    filipino=cps19_language_73,
    german=cps19_language_74,
    indian=cps19_language_75,
    italian=cps19_language_76,
    korean=cps19_language_77,
    pakistani=cps19_language_78,
    persian=cps19_language_79,
    russian=cps19_language_80,
    spanish=cps19_language_81,
    tamil=cps19_language_82,
    vietnamese=cps19_language_83,
    other=cps19_language_84,
    DK=cps19_language_85,
    employment = cps19_employment, 
    have_child=cps19_children,
    family_income=cps19_income_number,
    marriage_status=cps19_marital,
    household_size=cps19_household,
    age=cps19_age
         )

#### Clean up ####

# Province
ces <- ces %>% mutate(
  Province = case_when(
    province== 14 ~ 'Alberta',
    province== 15 ~ "British Columbia",
    province== 16 ~ "Manitoba",
    province== 17 ~ "New Brunswick",
    province== 18 ~ "Newfoundland and Labrador",
    province== 19 ~ "Northwest Territories",
    province== 20 ~ "Nova Scotia",
    province== 21 ~ "Nunavut",
    province== 22 ~ "Ontario",
    province== 23 ~ "Prince Edward Island",
    province== 24 ~ "Quebec",
    province== 25 ~ "Saskatchewan",
    province== 26 ~ "Yukon",
  )
)

# Sex
ces <- ces %>% mutate(
  Gender = case_when(
    sex== 1 ~ 'Male',
    sex== 2 ~ "Female",
    sex== 3 ~ "Other",
  )
)

# Education (whether above University degree)
ces <- ces %>% 
  filter(education!=12) %>%
  mutate(
    Education = case_when(
      education %in% c(1,2,3,4,5,6,7) ~ 'Below University Degree',
      TRUE ~ 'University Degree and Above',
    )                     
  )

# Religion (Important or NotImportant)
ces <- ces %>% 
  filter(religion_importance!= 5) %>%
  mutate(
    Religion = case_when(
      religion_importance %in% c(1,2) ~ 'Important',
      religion_importance %in% c(3,4) ~ "NotImportant",
    )
  )

# BornInCA (Yes or No)
ces <- ces %>% 
  filter(born_canada!=3) %>%
  mutate(
    BornInCA = case_when(
      born_canada == 1 ~ 'Yes',
      born_canada == 2 ~ 'No',
    )
  )

# number of languages
ces <- ces %>% rowwise() %>%
  mutate(num = sum(!is.na(
    c(
      english,
      french,
      aboriginal,
      arabic,
      chinese,
      filipino,
      german,
      indian,
      italian,
      korean,
      pakistani,
      persian,
      russian,
      spanish,
      tamil,
      vietnamese,
      other
    )
  )))

# MotherTongue (English/French/NonOfficial/Multiple)
ces <- ces %>%
  filter(is.na(DK)) %>%
  mutate(
    MotherTongue = case_when(
      !is.na(english) & num == 1 ~ 'English',!is.na(french) &
        num == 1 ~ 'French',
      is.na(english) & is.na(french) & num == 1 ~ 'NonOfficial',
      TRUE ~ 'Multiple',
    )
  )

# Employment (Employed/Other)
ces <- ces %>%
  filter(employment != 13) %>%
  mutate(Employment = case_when(employment %in% c(1, 2, 3, 9, 11) ~ 'Employed',
                                TRUE ~ 'Other'))

# HaveChild (Yes/No)
ces <- ces %>%
  filter(have_child != 3) %>%
  mutate(HaveChild = case_when(have_child == 2 ~ 'No',
                               TRUE ~ 'Yes'))

# FamilyIncome (Below $50,000/$50,000-$99,999/$100,000 and Above)
ces <- ces %>%
  filter(!is.na(family_income)) %>%
  mutate(
    FamilyIncome = case_when(
      family_income < 50000 ~ 'Below $50,000',
      (50000 <= family_income & family_income <= 99999) ~ '$50,000-$99,999',
      (family_income >= 100000) ~ '$100,000 and Above'
    )
  )

# MarriageStatus (Married/Other)
ces <- ces %>%
  filter(marriage_status != 7) %>%
  mutate(
    MarriageStatus = case_when(
      marriage_status == 1 ~ 'Married',
      TRUE ~ 'Other'
    )
  )

# HouseholdSize (One/Two/Three/Four/Five or More),
ces <- ces %>%
  filter(!is.na(household_size)) %>%
  mutate(
    HouseholdSize = case_when(
      household_size == 1 ~ 'One',
      household_size == 2 ~ 'Two',
      household_size == 3 ~ 'Three',
      household_size == 4 ~ 'Four',
      TRUE ~ 'Five or More'
    )
  )

# AgeGroup (18-34/35-54/55 and Higher )
ces <- ces %>%
  filter(!is.na(age)) %>%
  mutate(AgeGroup = case_when(
    (18 <= age & age <= 34) ~ '18-34',
    (35 <= age & age <= 54) ~ '35-54',
    (age >= 55) ~ '55 and Higher',
  ))

# VoteLiberal (Yes/No)
ces <- mutate(ces, VoteLiberal = coalesce(vote, vote_unlikely))
ces <- ces %>%
  filter(!is.na(VoteLiberal)) %>%
  mutate(VoteLiberal = case_when(VoteLiberal == 1 ~ 'Yes',
                                 TRUE ~ 'No'))


ces <- ces %>% select(
  VoteLiberal,
  Gender,
  Province,
  Education,
  Religion,
  BornInCA,
  MotherTongue,
  Employment,
  HaveChild,
  FamilyIncome,
  MarriageStatus,
  HouseholdSize,
  AgeGroup,
)


write_csv(ces, "ces.csv")
