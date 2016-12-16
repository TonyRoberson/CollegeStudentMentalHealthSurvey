#### College Student Mental Health Survey: YEPS CFA ####

## Load relevant packages!

library(lavaan)
library(semTools)
library(psych)
library(MBESS)
library(data.table)
library(dplyr)

## Import full data set 
csmhs <- read.csv(file = "College Student Mental Health Survey 4-20-16_1.csv",
                  stringsAsFactors = FALSE)

## Calculate variable for any therapy involvement ever
# 1 = In therapy now OR has had therapy in the past
# 2 = Has never had therapy
csmhs <- mutate(.data = csmhs, 
                therapyAny = ifelse(TherapyC == "1" | TherapyP == "1", "1", "2"))

## Specify appropriate variables as factor
csmhs$id_code <- as.factor(csmhs$id_code)
csmhs$Sex <- as.factor(csmhs$Sex)
csmhs$Race <- as.factor(csmhs$Race)
csmhs$College <- as.factor(csmhs$College)
csmhs$TherapyC <- as.factor(csmhs$TherapyC)
csmhs$TherapyP <- as.factor(csmhs$TherapyP)
csmhs$therapyAny <- as.factor(csmhs$therapyAny)
csmhs$Diagnosis <- as.factor(csmhs$Diagnosis)


## Subset YEPS items and demographics
yeps.cfa <- data.frame(
  csmhs[,c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", 
           "therapyAny", "Diagnosis",
           "YEPS1", "YEPS2", "YEPS3", "YEPS4", "YEPS5", 
           "YEPS6", "YEPS7", "YEPS8", "YEPS9", "YEPS10")])
# Give variables meaningful names
setnames(x = yeps.cfa,
         old = c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", 
                 "therapyAny", "Diagnosis",
                 "YEPS1", "YEPS2", "YEPS3", "YEPS4", "YEPS5", 
                 "YEPS6", "YEPS7", "YEPS8", "YEPS9", "YEPS10"), 
         new = c("id", "sex", "race", "college", "therapyC", "therapyP",
                 "therapyAny", "diagnosis",
                 "yeps1_loseTemper", "yeps2_sitStill", "yeps3_fightArgue",
                 "yeps4_breakRules", "yeps5_talkInterrupt", "yeps6_sayDoMeanThings",
                 "yeps7_hardTimeFocusing", "yeps8_annoyUpsetPeople",
                 "yeps9_distractedLittleThings", "yeps10_dontFollowDirections"))


#### General Descriptive Statistics ####

summary(yeps.cfa)
describe(yeps.cfa)

# Export to csv file in working directory
yeps.desc <- as.data.frame(describe(yeps.cfa))
write.csv(x = yeps.desc, file = "yeps_desc.csv")

# Normality Tests
# SKEWNESS: g1p = 36.41, chi.skew = 4836.923, p < .000
# KURTOSIS: g2p = 213.84, z.kurtosis = 85.506, p < .000
mardiaTest(data = yeps.cfa[, c("yeps1_loseTemper", "yeps2_sitStill", 
                               "yeps3_fightArgue", "yeps4_breakRules", 
                               "yeps5_talkInterrupt", "yeps6_sayDoMeanThings",
                               "yeps7_hardTimeFocusing", "yeps8_annoyUpsetPeople",
                               "yeps9_distractedLittleThings", "yeps10_dontFollowDirections")],
           qqplot = TRUE)


#### CFA 1 ####


## Specify the model
yeps.model <- 'YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections'
## Fit the model as a CFA
yeps.fit <- cfa(model = yeps.model,
                estimator = "WLSMV",
                ordered = names(yeps.cfa),
                data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit, sort. = TRUE)



## Specify model with covariance between items 4 and 9
yeps.model.2 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings + 
yeps10_dontFollowDirections
## Specify covariance term
yeps4_breakRules ~~ yeps9_distractedLittleThings'

## Fit the model as a CFA
yeps.fit.2 <- cfa(model = yeps.model.2, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.2,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.2)



## Specify model with item 9 removed
yeps.model.3 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections'

## Fit the model as a CFA
yeps.fit.3 <- cfa(model = yeps.model.3, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.3,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.3)



## Specify model with covariance between items 1 and 5
yeps.model.4 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt'

## Fit the model as a CFA
yeps.fit.4 <- cfa(model = yeps.model.4, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.4,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.4)



## Specify model with additional covariance between items 3 and 7
yeps.model.5 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing'

## Fit the model as a CFA
yeps.fit.5 <- cfa(model = yeps.model.5, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.5,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.5)



## Specify model with additional covariance between items 2 and 4
## NOTE: This model met fit criteria for all indices but chi-squared
yeps.model.6 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules'

## Fit the model as a CFA
yeps.fit.6 <- cfa(model = yeps.model.6, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.6,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.6)



#### CFA 1: Continue adding covariance parameters until chi-squared p > .05 ####

## Specify model with additional covariance between items 1 and 8
yeps.model.7 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople'

## Fit the model as a CFA
yeps.fit.7 <- cfa(model = yeps.model.7, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.7,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.7)



## Specify model with additional covariance between items 8 and 10
yeps.model.8 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections'

## Fit the model as a CFA
yeps.fit.8 <- cfa(model = yeps.model.8, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.8,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.8)



## Specify model with additional covariance between items 5 and 8
yeps.model.9 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople'

## Fit the model as a CFA
yeps.fit.9 <- cfa(model = yeps.model.9, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.9,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.9)



## Specify model with additional covariance between items 2 and 6
yeps.model.10 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople
yeps2_sitStill ~~ yeps6_sayDoMeanThings'

## Fit the model as a CFA
yeps.fit.10 <- cfa(model = yeps.model.10, 
                   estimator = "WLSMV",
                   ordered = names(yeps.cfa),
                   data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.10,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(yeps.fit.10)



## Specify model with additional covariance between items 1 and 2
yeps.model.11 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps10_dontFollowDirections
## Specify covariance terms
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople
yeps2_sitStill ~~ yeps6_sayDoMeanThings
yeps1_loseTemper ~~ yeps2_sitStill'

## Fit the model as a CFA
yeps.fit.11 <- cfa(model = yeps.model.11, 
                   estimator = "WLSMV",
                   ordered = names(yeps.cfa),
                   data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.11,
        fit.measures = TRUE,
        standardized = TRUE)



#### CFA 2: Alternative Fit, no items dropped ####



## Specify the model
yeps.model <- 'YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections'
## Fit the model as a CFA
yeps.fit <- cfa(model = yeps.model,
                estimator = "WLSMV",
                ordered = names(yeps.cfa),
                data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit,
           sort. = TRUE)



## Specify model with covariance between items 4 and 9
yeps.model.2 <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings + 
yeps10_dontFollowDirections
## Specify covariance term
yeps4_breakRules ~~ yeps9_distractedLittleThings'

## Fit the model as a CFA
yeps.fit.2 <- cfa(model = yeps.model.2, 
                  estimator = "WLSMV",
                  ordered = names(yeps.cfa),
                  data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.2,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.2, 
           sort. = TRUE)



## Specify model with covariance between items 1 and 5
yeps.model.3.b <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections
## Specifiy covariance terms
yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt'


## Fit the model as a CFA
yeps.fit.3.b <- cfa(model = yeps.model.3.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.3.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.3.b, 
           sort. = TRUE)



## Specify model with covariance between items 2 and 9
yeps.model.4.b <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections
## Specifiy covariance terms
yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings'


## Fit the model as a CFA
yeps.fit.4.b <- cfa(model = yeps.model.4.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.4.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.4.b,
           sort. = TRUE)



## Specify model with covariance between items 3 and 7
yeps.model.5.b <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections
## Specifiy covariance terms
yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing'


## Fit the model as a CFA
yeps.fit.5.b <- cfa(model = yeps.model.5.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.5.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.5.b,
           sort. = TRUE)



## Specify model with covariance between items 2 and 4
#### CFA 2: PREFERRED MODEL! Acceptable fit ####

yeps.model.6.b <- '
## Specify latent structure
YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections
## Specifiy covariance terms
yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules'

## Fit the model as a CFA
yeps.fit.6.b <- cfa(model = yeps.model.6.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.6.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.6.b,
           sort. = TRUE)


## Specify model with covariance between items 8 and 10

yeps.model.7.b <- '
## Specify latent structure

YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections

## Specifiy covariance terms

yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections '

## Fit the model as a CFA
yeps.fit.7.b <- cfa(model = yeps.model.7.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.7.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.7.b,
           sort. = TRUE)


## Specify model with covariance between items 1 and 8
#### CFA 2: RMSEA < .05 Strong fit ####

yeps.model.8.b <- '
## Specify latent structure

YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections

## Specifiy covariance terms

yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections 
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople'

## Fit the model as a CFA
yeps.fit.8.b <- cfa(model = yeps.model.8.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.8.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.8.b,
           sort. = TRUE)


## Specify model with covariance between items 5 and 8

yeps.model.9.b <- '
## Specify latent structure

YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections

## Specifiy covariance terms

yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections 
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople'

## Fit the model as a CFA
yeps.fit.9.b <- cfa(model = yeps.model.9.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.9.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.9.b,
           sort. = TRUE)


## Specify model with covariance between items 6 and 9

yeps.model.10.b <- '
## Specify latent structure

YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections

## Specifiy covariance terms

yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections 
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople
yeps6_sayDoMeanThings ~~ yeps9_distractedLittleThings'

## Fit the model as a CFA
yeps.fit.10.b <- cfa(model = yeps.model.10.b, 
                    estimator = "WLSMV",
                    ordered = names(yeps.cfa),
                    data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.10.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.10.b,
           sort. = TRUE)

## Specify model with covariance between items 2 and 6
#### CFA 2: X2 p > .05 Strongest fit ####

yeps.model.11.b <- '
## Specify latent structure

YEPS =~ yeps1_loseTemper + yeps2_sitStill + yeps3_fightArgue +
yeps4_breakRules + yeps5_talkInterrupt + 
yeps6_sayDoMeanThings + yeps7_hardTimeFocusing + 
yeps8_annoyUpsetPeople + yeps9_distractedLittleThings +
yeps10_dontFollowDirections

## Specifiy covariance terms

yeps4_breakRules ~~ yeps9_distractedLittleThings
yeps1_loseTemper ~~ yeps5_talkInterrupt
yeps2_sitStill ~~ yeps9_distractedLittleThings
yeps3_fightArgue ~~ yeps7_hardTimeFocusing
yeps2_sitStill ~~ yeps4_breakRules
yeps8_annoyUpsetPeople ~~ yeps10_dontFollowDirections 
yeps1_loseTemper ~~ yeps8_annoyUpsetPeople
yeps5_talkInterrupt ~~ yeps8_annoyUpsetPeople
yeps6_sayDoMeanThings ~~ yeps9_distractedLittleThings
yeps2_sitStill ~~ yeps6_sayDoMeanThings'

## Fit the model as a CFA
yeps.fit.11.b <- cfa(model = yeps.model.11.b, 
                     estimator = "WLSMV",
                     ordered = names(yeps.cfa),
                     data = yeps.cfa)
# Print fit summary
summary(object = yeps.fit.11.b,
        fit.measures = TRUE,
        standardized = TRUE)

## Evaluate modification indices
modindices(object = yeps.fit.11.b,
           sort. = TRUE)

#### YEPS Scale Descriptives ####

# Categorical Omega Reliability
# .915[.887,.937], se = .0126
ci.reliability(data = yeps.cfa[, c("yeps1_loseTemper", 
                                   "yeps2_sitStill", 
                                   "yeps3_fightArgue",
                                   "yeps4_breakRules", 
                                   "yeps5_talkInterrupt", 
                                   "yeps6_sayDoMeanThings",
                                   "yeps7_hardTimeFocusing", 
                                   "yeps8_annoyUpsetPeople",
                                   "yeps9_distractedLittleThings", 
                                   "yeps10_dontFollowDirections")],
               type = "categorical",
               interval.type = "bca",
               B = 10000)



###### Multigroup Invariance Analyses #######




## Indicate the relevant fit indices
fit.indices <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", 
                 "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr", "cfi.scaled")


#### Invariance by sex ####

## Individual groups

# Male
male <- cfa(model = yeps.model.6.b, 
            data = yeps.cfa,
            group = "sex",
            group.label = "1",
            estimator = "WLSMV", 
            ordered = names(yeps.cfa))
summary(object = yesDiagnosis,
        fit.measures = TRUE,
        standardized = TRUE)
# Female
female <- cfa(model = yeps.model.6.b, 
              data = yeps.cfa,
              group = "sex",
              group.label = "2",
              estimator = "WLSMV", 
              ordered = names(yeps.cfa))
summary(object = noDiagnosis,
        fit.measures = TRUE,
        standardized = TRUE)


## Measurement Invariance

# Configural fit
sex.configural.fit <- cfa(model = yeps.model.6.b, 
                          data = yeps.cfa,
                          group = "sex",
                          estimator = "WLSMV", 
                          ordered = names(yeps.cfa))
fitMeasures(sex.configural.fit, fit.indices)
# Weak fit
sex.weak.fit <- cfa(model = yeps.model.6.b, 
                    data = yeps.cfa,
                    group = "sex",
                    estimator = "WLSMV", 
                    ordered = names(yeps.cfa),
                    group.equal = c("loadings"))
fitMeasures(sex.weak.fit, fit.indices)
## Inspect modification indices
modindices(object = sex.weak.fit, 
           sort. = TRUE) 

# Weak fit 2
sex.weak.fit.2 <- cfa(model = yeps.model.6.b, 
                      data = yeps.cfa,
                      group = "sex",
                      estimator = "WLSMV", 
                      ordered = names(yeps.cfa),
                      group.equal = c("loadings"), 
                      group.partial = "yeps3_fightArgue~*~yeps3_fightArgue")
fitMeasures(sex.weak.fit.2, fit.indices)

# Strong fit
sex.strong.fit <- cfa(model = yeps.model.6.b, 
                      data = yeps.cfa,
                      group = "sex",
                      estimator = "WLSMV", 
                      ordered = names(yeps.cfa),
                      group.equal = c("loadings", "intercepts"), group.partial = c("yeps10_dontFollowDirections~1"))
fitMeasures(sex.strong.fit, fit.indices)
modindices(object = sex.strong.fit, sort. = TRUE)
# Strict fit
sex.strict.fit <- cfa(model = yeps.model.6.b, 
                      data = yeps.cfa,
                      group = "sex",
                      estimator = "WLSMV", 
                      ordered = names(yeps.cfa),
                      group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(sex.strict.fit, fit.indices)

## Structural Invariance

# Add latent variance constraint
sex.var.fit <- cfa(model = yeps.model.6.b, 
                   data = yeps.cfa,
                   group = "sex",
                   estimator = "WLSMV", 
                   ordered = names(yeps.cfa),
                   group.equal = c("loadings", "intercepts", "residuals", 
                                   "lv.variances"))
fitMeasures(sex.var.fit, fit.indices)
# Add latent means constraint
sex.mean.fit <- cfa(model = yeps.model.6.b, 
                    data = yeps.cfa,
                    group = "sex",
                    estimator = "WLSMV", 
                    ordered = names(yeps.cfa),
                    group.equal = c("loadings", "intercepts", "residuals", 
                                    "lv.variances", "means"))
fitMeasures(sex.mean.fit, fit.indices)

## Scaled-shifted chi-square difference test
lavTestLRT(sex.configural.fit, sex.weak.fit, sex.strong.fit, 
           sex.var.fit, sex.mean.fit, method = "satorra.bentler.2010")



#### Invariance by past therapy experience ####

## Individual groups

# Had therapy in the past
pastTherapy <- cfa(model = yeps.model.6.b, 
                   data = yeps.cfa,
                   group = "therapyP",
                   group.label = "1",
                   estimator = "WLSMV", 
                   ordered = names(yeps.cfa))
summary(object = pastTherapy,
        fit.measures = TRUE,
        standardized = TRUE)
# No past therapy
noPastTherapy <- cfa(model = yeps.model.6.b, 
                     data = yeps.cfa,
                     group = "therapyP",
                     group.label = "2",
                     estimator = "WLSMV", 
                     ordered = names(yeps.cfa))
summary(object = noPastTherapy,
        fit.measures = TRUE,
        standardized = TRUE)

## Measurement Invariance

# Configural fit
therapyP.configural.fit <- cfa(model = yeps.model.6.b, 
                               data = yeps.cfa,
                               group = "therapyP",
                               estimator = "WLSMV", 
                               ordered = names(yeps.cfa))
fitMeasures(therapyP.configural.fit, fit.indices)
# Weak fit
therapyP.weak.fit <- cfa(model = yeps.model.6.b, 
                         data = yeps.cfa,
                         group = "therapyP",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings"))
fitMeasures(therapyP.weak.fit, fit.indices)
# Strong fit
therapyP.strong.fit <- cfa(model = yeps.model.6.b, 
                           data = yeps.cfa,
                           group = "therapyP",
                           estimator = "WLSMV", 
                           ordered = names(yeps.cfa),
                           group.equal = c("loadings", "intercepts"))
fitMeasures(therapyP.strong.fit, fit.indices)
# Strict fit
therapyP.strict.fit <- cfa(model = yeps.model.6.b, 
                           data = yeps.cfa,
                           group = "therapyP",
                           estimator = "WLSMV", 
                           ordered = names(yeps.cfa),
                           group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(therapyP.strict.fit, fit.indices)

## Structural Invariance

# Add latent variance constraint
therapyP.var.fit <- cfa(model = yeps.model.6.b, 
                        data = yeps.cfa,
                        group = "therapyP",
                        estimator = "WLSMV", 
                        ordered = names(yeps.cfa),
                        group.equal = c("loadings", "intercepts", "residuals", 
                                        "lv.variances"))
fitMeasures(therapyP.var.fit, fit.indices)
# Add latent means constraint
therapyP.mean.fit <- cfa(model = yeps.model.6.b, 
                         data = yeps.cfa,
                         group = "therapyP",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings", "intercepts", "residuals", 
                                         "lv.variances", "means"))
fitMeasures(therapyP.mean.fit, fit.indices)

## Scaled-shifted chi-square difference test
lavTestLRT(therapyP.configural.fit, therapyP.weak.fit, therapyP.strong.fit, 
           therapyP.var.fit, therapyP.mean.fit, method = "satorra.bentler.2010")



#### Invariance by current therapy experience ####

## Individual groups

# Currently in therapy
currentTherapy <- cfa(model = yeps.model.6.b, 
                      data = yeps.cfa,
                      group = "therapyC",
                      group.label = "1",
                      estimator = "WLSMV", 
                      ordered = names(yeps.cfa))
summary(object = currentTherapy,
        fit.measures = TRUE,
        standardized = TRUE)
# Not currently in therapy
noCurrentTherapy <- cfa(model = yeps.model.6.b, 
                        data = yeps.cfa,
                        group = "therapyC",
                        group.label = "2",
                        estimator = "WLSMV", 
                        ordered = names(yeps.cfa))
summary(object = noCurrentTherapy,
        fit.measures = TRUE,
        standardized = TRUE)

## Measurement Invariance

# Configural fit
therapyC.configural.fit <- cfa(model = yeps.model.6.b, 
                               data = yeps.cfa,
                               group = "therapyC",
                               estimator = "WLSMV", 
                               ordered = names(yeps.cfa))
fitMeasures(therapyC.configural.fit, fit.indices)
# Weak fit
therapyC.weak.fit <- cfa(model = yeps.model.6.b, 
                         data = yeps.cfa,
                         group = "therapyC",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings"))
fitMeasures(therapyC.weak.fit, fit.indices)
# Strong fit
therapyC.strong.fit <- cfa(model = yeps.model.6.b, 
                           data = yeps.cfa,
                           group = "therapyC",
                           estimator = "WLSMV", 
                           ordered = names(yeps.cfa),
                           group.equal = c("loadings", "intercepts"))
fitMeasures(therapyC.strong.fit, fit.indices)
# Strict fit
therapyC.strict.fit <- cfa(model = yeps.model.6.b, 
                           data = yeps.cfa,
                           group = "therapyC",
                           estimator = "WLSMV", 
                           ordered = names(yeps.cfa),
                           group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(therapyC.strict.fit, fit.indices)

## Structural Invariance

# Add latent variance constraint
therapyC.var.fit <- cfa(model = yeps.model.6.b, 
                        data = yeps.cfa,
                        group = "therapyC",
                        estimator = "WLSMV", 
                        ordered = names(yeps.cfa),
                        group.equal = c("loadings", "intercepts", "residuals", 
                                        "lv.variances"))
fitMeasures(therapyC.var.fit, fit.indices)
# Add latent means constraint
therapyC.mean.fit <- cfa(model = yeps.model.6.b, 
                         data = yeps.cfa,
                         group = "therapyC",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings", "intercepts", "residuals", 
                                         "lv.variances", "means"))
fitMeasures(therapyC.mean.fit, fit.indices)

## Scaled-shifted chi-square difference test
lavTestLRT(therapyC.configural.fit, therapyC.weak.fit, therapyC.strong.fit, 
           therapyC.var.fit, therapyC.mean.fit, method = "satorra.bentler.2010")



#### Invariance by diagnosis status ####

## Individual groups

# Has been given psychiatric diagnosis
yesDiagnosis <- cfa(model = yeps.model.6.b, 
                    data = yeps.cfa,
                    group = "diagnosis",
                    group.label = "1",
                    estimator = "WLSMV", 
                    ordered = names(yeps.cfa))
summary(object = yesDiagnosis,
        fit.measures = TRUE,
        standardized = TRUE)
# Never given psychiatric diagnosis
noDiagnosis <- cfa(model = yeps.model.6.b, 
                   data = yeps.cfa,
                   group = "diagnosis",
                   group.label = "2",
                   estimator = "WLSMV", 
                   ordered = names(yeps.cfa))
summary(object = noDiagnosis,
        fit.measures = TRUE,
        standardized = TRUE)

## Measurement Invariance

# Configural fit
diagnosis.configural.fit <- cfa(model = yeps.model.6.b, 
                                data = yeps.cfa,
                                group = "diagnosis",
                                estimator = "WLSMV", 
                                ordered = names(yeps.cfa))
fitMeasures(diagnosis.configural.fit, fit.indices)
# Weak fit
diagnosis.weak.fit <- cfa(model = yeps.model.6.b, 
                          data = yeps.cfa,
                          group = "diagnosis",
                          estimator = "WLSMV", 
                          ordered = names(yeps.cfa),
                          group.equal = c("loadings"))
fitMeasures(diagnosis.weak.fit, fit.indices)
# Strong fit
diagnosis.strong.fit <- cfa(model = yeps.model.6.b, 
                            data = yeps.cfa,
                            group = "diagnosis",
                            estimator = "WLSMV", 
                            ordered = names(yeps.cfa),
                            group.equal = c("loadings", "intercepts"))
fitMeasures(diagnosis.strong.fit, fit.indices)
# Strict fit
diagnosis.strict.fit <- cfa(model = yeps.model.6.b, 
                            data = yeps.cfa,
                            group = "diagnosis",
                            estimator = "WLSMV", 
                            ordered = names(yeps.cfa),
                            group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(diagnosis.strict.fit, fit.indices)

## Structural Invariance

# Add latent variance constraint
diagnosis.var.fit <- cfa(model = yeps.model.6.b, 
                         data = yeps.cfa,
                         group = "diagnosis",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings", "intercepts", "residuals", 
                                         "lv.variances"))
fitMeasures(diagnosis.var.fit, fit.indices)
# Add latent means constraint
diagnosis.mean.fit <- cfa(model = yeps.model.6.b, 
                          data = yeps.cfa,
                          group = "diagnosis",
                          estimator = "WLSMV", 
                          ordered = names(yeps.cfa),
                          group.equal = c("loadings", "intercepts", "residuals", 
                                          "lv.variances", "means"))
fitMeasures(diagnosis.mean.fit, fit.indices)

## Scaled-shifted chi-square difference test
lavTestLRT(diagnosis.configural.fit, diagnosis.weak.fit, diagnosis.strong.fit, 
           diagnosis.var.fit, diagnosis.mean.fit, method = "satorra.bentler.2010")



