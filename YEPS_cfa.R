#### College Student Mental Health Survey: YEPS CFA ####

## Load relevant packages

library(lavaan)
library(semTools)
library(psych)
library(data.table)

## Import full data set 
csmhs <- read.csv(file = "College Student Mental Health Survey 4-20-16_1.csv",
                  stringsAsFactors = FALSE)

## Subset YEPS items and demographics
yeps.cfa <- data.frame(
  csmhs[,c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", "Diagnosis",
        "YEPS1", "YEPS2", "YEPS3", "YEPS4", "YEPS5", 
        "YEPS6", "YEPS7", "YEPS8", "YEPS9", "YEPS10")])
# Give variables meaningful names
setnames(x = yeps.cfa,
         old = c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", 
                 "Diagnosis",
                 "YEPS1", "YEPS2", "YEPS3", "YEPS4", "YEPS5", 
                 "YEPS6", "YEPS7", "YEPS8", "YEPS9", "YEPS10"), 
         new = c("id", "sex", "race", "college", "therapyC", "therapyP", "diagnosis",
                 "yeps1_loseTemper", "yeps2_sitStill", "yeps3_fightArgue",
                 "yeps4_breakRules", "yeps5_talkInterrupt", "yeps6_sayDoMeanThings",
                 "yeps7_hardTimeFocusing", "yeps8_annoyUpsetPeople",
                 "yeps9_distractedLittleThings", "yeps10_dontFollowDirections"))



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
modindices(yeps.fit)



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



## Specify model with additional covariance between items 1 and 5
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

## Evaluate modification indices
modindices(yeps.fit.11)


