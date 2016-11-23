#### College Student Mental Health Survey: YEPS CFA ####

## Load relevant packages

library(lavaan)
library(semTools)
library(psych)
library(data.table)

## Import full data set 
csmhs <- read.csv(file = "College Student Mental Health Survey 4-20-16_1.csv",
                  stringsAsFactors = FALSE)
# Calculate variable for any therapy involvement ever
# 1 = In therapy now OR has had therapy in the past
# 2 = Has never had therapy
csmhs <- mutate(csmhs, therapyAny = ifelse(TherapyC == "1" | TherapyP == "1", "1", "2"))
# Specify appropriate variables as factor
csmhs$id_code <- as.factor(csmhs$id_code)
csmhs$Sex <- as.factor(csmhs$Sex)
csmhs$Race <- as.factor(csmhs$Race)
csmhs$College <- as.factor(csmhs$College)
csmhs$TherapyC <- as.factor(csmhs$TherapyC)
csmhs$TherapyP <- as.factor(csmhs$TherapyP)
csmhs$therapyAny <- as.factor(csmhs$therapyAny)
csmhs$Diagnosis <- as.factor(csmhs$Diagnosis)
# Get variable names
vars <- data.frame(colnames(csmhs))

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



#### Continue adding covariance parameters until chi-squared p > .05 ####

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




###### MEASUREMENT AND STRUCTURAL INVARIANCE #######


## Indicate the relevant fit indices
fit.indices <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
                 "rmsea.ci.upper.scaled", "srmr", "cfi.scaled")

#### By past therapy experience

## Individual groups

# Had therapy in the past
pastTherapy <- cfa(model = yeps.model.6, 
               data = yeps.cfa,
               group = "therapyP",
               group.label = "1",
               estimator = "WLSMV", 
               ordered = names(yeps.cfa))
summary(object = pastTherapy,
        fit.measures = TRUE,
        standardized = TRUE)
# No past therapy
noPastTherapy <- cfa(model = yeps.model.6, 
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
therapyP.configural.fit <- cfa(model = yeps.model.6, 
                          data = yeps.cfa,
                          group = "therapyP",
                          estimator = "WLSMV", 
                          ordered = names(yeps.cfa))
fitMeasures(therapyP.configural.fit, fit.indices)
# Weak fit
therapyP.weak.fit <- cfa(model = yeps.model.6, 
                          data = yeps.cfa,
                          group = "therapyP",
                          estimator = "WLSMV", 
                          ordered = names(yeps.cfa),
                          group.equal = c("loadings"))
fitMeasures(therapyP.weak.fit, fit.indices)

# Strong fit

# Strict fit

## Structural Invariance

# Add latent variance constraint

# Add latent means constraint


## Scaled-shifted chi-square difference test
lavTestLRT(therapyP.configural.fit, therapyP.weak.fit, therapyP.strong.fit, 
           therapyP.var.fit, therapyP.means.fit, method = "satorra.bentler.2010")



### By current therapy involvement 

## Individual groups

# Had therapy in the past
pastTherapy <- cfa(model = yeps.model.6, 
                   data = yeps.cfa,
                   group = "therapyP",
                   group.label = "1",
                   estimator = "WLSMV", 
                   ordered = names(yeps.cfa))
summary(object = pastTherapy,
        fit.measures = TRUE,
        standardized = TRUE)
# No past therapy
noPastTherapy <- cfa(model = yeps.model.6, 
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
therapyP.configural.fit <- cfa(model = yeps.model.6, 
                               data = yeps.cfa,
                               group = "therapyP",
                               estimator = "WLSMV", 
                               ordered = names(yeps.cfa))
fitMeasures(therapyP.configural.fit, fit.indices)
# Weak fit
therapyP.weak.fit <- cfa(model = yeps.model.6, 
                         data = yeps.cfa,
                         group = "therapyP",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings"))
fitMeasures(therapyP.weak.fit, fit.indices)

# Strong fit

# Strict fit

## Structural Invariance

# Add latent variance constraint

# Add latent means constraint



### By diagnosis status

## Individual groups

# Has been given psychiatric diagnosis
yesDiagnosis <- cfa(model = yeps.model.6, 
                   data = yeps.cfa,
                   group = "diagnosis",
                   group.label = "1",
                   estimator = "WLSMV", 
                   ordered = names(yeps.cfa))
summary(object = yesDiagnosis,
        fit.measures = TRUE,
        standardized = TRUE)
# No past therapy
noDiagnosis <- cfa(model = yeps.model.6, 
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
diagnosis.configural.fit <- cfa(model = yeps.model.6, 
                               data = yeps.cfa,
                               group = "therapyP",
                               estimator = "WLSMV", 
                               ordered = names(yeps.cfa))
fitMeasures(therapyP.configural.fit, fit.indices)
# Weak fit
diagnosis.weak.fit <- cfa(model = yeps.model.6, 
                         data = yeps.cfa,
                         group = "therapyP",
                         estimator = "WLSMV", 
                         ordered = names(yeps.cfa),
                         group.equal = c("loadings"))
fitMeasures(therapyP.weak.fit, fit.indices)

# Strong fit

# Strict fit

## Structural Invariance

# Add latent variance constraint

# Add latent means constraint


