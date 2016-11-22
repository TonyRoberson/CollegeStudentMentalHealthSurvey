#### College Student Mental Health Survey: YIPS CFA ####

## Load relevant packages

library(lavaan)
library(semTools)
library(psych)
library(data.table)

## Import full data set 
csmhs <- read.csv(file = "College Student Mental Health Survey 4-20-16_1.csv",
                  stringsAsFactors = FALSE)

## Subset YIPS items and demographics
yips.cf <- data.frame(
  csmhs[,c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", "Diagnosis",
           "YIPS1", "YIPS2", "YIPS3", "YIPS4", "YIPS5", 
           "YIPS6", "YIPS7", "YIPS8", "YIPS9", "YIPS10")])
# Give variables meaningful names
setnames(x = yips.cfa,
         old = c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", 
                 "Diagnosis",
                 "YIPS1", "YIPS2", "YIPS3", "YIPS4", "YIPS5", 
                 "YIPS6", "YIPS7", "YIPS8", "YIPS9", "YIPS10"), 
         new = c("id", "sex", "race", "college", "therapyC", "therapyP", "diagnosis",
                 "yips1_nervousAfraid", "yips2_tiredDrained", "yips3_relaxSettleDown",
                 "yips4_bothered", "yips5_uncomfortableTense", "yips6_moodyGrumpy",
                 "yips7_panicLoseControl", "yips8_noEnjoyment",
                 "yips9_worthlessLonely", "yips10_achesPains"))


## Specify the model
yips.model <- 'yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
                        yips3_relaxSettleDown + yips4_bothered +
                        yips5_uncomfortableTense + yips6_moodyGrumpy +
                        yips7_panicLoseControl + yips8_noEnjoyment + 
                        yips9_worthlessLonely + yips10_achesPains'

## Fit the model as a CFA
fit.yips <- cfa(model = yips.model, 
                estimator = "WLSMV",
                ordered = names(yips.cfa),
                data = yips.cfa, missing = "listwise")
# Print fit summary
summary(object = fit.yips,
        fit.measures = TRUE,
        standardized = TRUE)


