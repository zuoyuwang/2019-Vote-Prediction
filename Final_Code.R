library(tidyverse)

survey_data <-  read.csv("ces.csv")

## Full Logistic Regression
model_logit <-
  glm(
    as.factor(VoteLiberal) ~   Gender + Province + Education + Religion + BornInCA + MotherTongue + Employment + HaveChild +
      FamilyIncome + MarriageStatus + HouseholdSize + AgeGroup,
    data = survey_data,
    family = binomial
  )

summary(model_logit)

# model selection
library(MASS)

#step AIC regression
step_AIC <- model_logit %>% stepAIC(k = 2)

AIC_model <-
  glm(
    as.factor(VoteLiberal) ~   Gender + Province + Education + Religion + BornInCA + MotherTongue + Employment + HaveChild +
      FamilyIncome + AgeGroup,
    data = survey_data,
    family = binomial
  )

summary(AIC_model)

#step BIC regression
step_BIC <- model_logit %>% stepAIC(k = log(nrow(survey_data)))

BIC_model <-
  glm(
    as.factor(VoteLiberal) ~   Province + Education + BornInCA + MotherTongue + HaveChild + AgeGroup,
    data = survey_data,
    family = binomial
  )

summary(BIC_model)



# postratification
census_data <- read.csv("gss.csv")

n <- nrow(census_data)

census_data <- census_data %>% group_by_all() %>% summarise(n = n())

census_data$estimate_vote <-
  step_AIC %>% predict(newdata = census_data, type = "response")

census_data <- census_data %>%
  mutate(alp_predict_prop = estimate_vote * n)


predict_vote <- sum(census_data$alp_predict_prop) / sum(n)
predict_vote




# figures

#model variables
Names <-
  c(
    "VoteLiberal",
    "Gender",
    "Province",
    "Education",
    "Religion",
    "BornInCA",
    "MotherTongue",
    "Employment",
    "HaveChild",
    "FamilyIncome" ,
    "MarriageStatus" ,
    "HouseholdSize" ,
    "AgeGroup"
  )
Types <-
  c(
    "Binary",
    "Categorical",
    "Categorical",
    "Binary",
    "Binary",
    "Binary",
    "Categorical",
    "Binary",
    "Binary",
    "Categorical",
    "Binary",
    "Categorical",
    "Categorical"
  )
Levels <-
  c(
    "[Yes/No]",
    "[Male/Female/Other]",
    "[Alberta/British Columbia/Manitoba/...]",
    "[Below University Degree/University Degree and Above]",
    "[Important/NotImportant]",
    "[Yes/No]",
    "[English/French/NonOfficial/Multiple]",
    "[Employed/Other]",
    "[Yes/No]",
    "[Below $50,000/$50,000-$99,999/$100,000 and Above]",
    "[Married/Other]",
    "[One/Two/Three/Four/Five or More]",
    "[18-34/35-54/55 and Higher]"
  )
variables_table <- tibble(Names, Types, Levels)
knitr::kable(variables_table, caption = "Table 1. Variables Summary Table")


# odds ratio and confidence interval
cimat = Pmisc::ciMat(0.95)
coef_table = summary(AIC_model)$coef[, rownames(cimat)] %*% cimat
knitr::kable(exp(coef_table),
             caption = "Table 2. Odds Ratio and 95% Confidence Interval of the Model Coefficients")

# vif
library(car)
knitr::kable(vif(AIC_model), caption = "VIF Table of predictor variables")

# AUC-ROC curve
library(pROC)
p <- predict(AIC_model,  type = "response")
roc_logit <- roc(survey_data$VoteLiberal ~ p)
TPR <- roc_logit$sensitivities
FPR <- 1 - roc_logit$specificities
plot(
  FPR,
  TPR,
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "False Positive Rate",
  ylab = "True Positive Rate",
  family = "Times New Roman",
  type = 'l',
  lty = 1,
  lwd = 2,
  col = 'red',
  bty = "n"
)
title(
  main = "Figure 1. AUC-ROC curve of logistic regression model",
  cex.main = 0.8,
  font.main = 1,
  family = "Times New Roman"
)
abline(a = 0,
       b = 1,
       lty = 2,
       col = 'blue')
text(0.7, 0.4, label = paste("AUC = ", round(auc(roc_logit), 2)), family =
       "Times New Roman")

# cell population and prediction
post_table = data.frame(
  Measure = c("Population size", "Vote Probability Estimation"),
  Min = c(min(census_data$n), min(census_data$estimate_vote)),
  Mean  = c(mean(census_data$n), mean(census_data$estimate_vote)),
  SD = c(sd(census_data$n), sd(census_data$estimate_vote)),
  Max = c(max(census_data$n), max(census_data$estimate_vote))
)
knitr::kable(post_table, caption = "VIF Table of predictor variables")


#table1
Names <-
  c(
    "VoteLiberal",
    "Gender",
    "Province",
    "Education",
    "Religion",
    "BornInCA",
    "MotherTongue",
    "Employment",
    "HaveChild",
    "FamilyIncome" ,
    "AgeGroup"
  )
Types <-
  c(
    "Binary",
    "Categorical",
    "Categorical",
    "Binary",
    "Binary",
    "Binary",
    "Categorical",
    "Binary",
    "Binary",
    "Categorical",
    "Categorical"
  )
Levels <-
  c(
    "[Yes/No]",
    "[Male/Female/Other]",
    "[Alberta/British Columbia/Manitoba/...]",
    "[Below University Degree/University Degree and Above]",
    "[Important/NotImportant]",
    "[Yes/No]",
    "[English/French/NonOfficial/Multiple]",
    "[Employed/Other]",
    "[Yes/No]",
    "[Below $50,000/$50,000-$99,999/$100,000 and Above]",
    "[18-34/35-54/55 and Higher]"
  )
variables_table <- tibble(Names, Types, Levels)
knitr::kable(variables_table, caption = "Model Variables Summary Table")




