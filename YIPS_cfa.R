#### College Student Mental Health Survey: YIPS CFA ####

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

## Subset YIPS items and demographics
yips.cfa <- data.frame(
  csmhs[,c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP",
           "therapyAny", "Diagnosis",
           "YIPS1", "YIPS2", "YIPS3", "YIPS4", "YIPS5", 
           "YIPS6", "YIPS7", "YIPS8", "YIPS9", "YIPS10")])
# Give variables meaningful names
setnames(x = yips.cfa,
         old = c("id_code", "Sex", "Race", "College", "TherapyC", "TherapyP", 
                 "therapyAny", "Diagnosis",
                 "YIPS1", "YIPS2", "YIPS3", "YIPS4", "YIPS5", 
                 "YIPS6", "YIPS7", "YIPS8", "YIPS9", "YIPS10"), 
         new = c("id", "sex", "race", "college", "therapyC", "therapyP", 
                 "therapyAny", "diagnosis",
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
yips.fit <- cfa(model = yips.model, 
                estimator = "WLSMV",
                ordered = names(yips.cfa),
                data = yips.cfa)
# Print fit summary
summary(object = yips.fit,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit)



## Specify the model with covariance between items 2 and 6
## NOTE: This model showed acceptable fit for all indices but chi-squared
yips.model.2 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy'

## Fit the model as a CFA
yips.fit.2 <- cfa(model = yips.model.2, 
                estimator = "WLSMV",
                ordered = names(yips.cfa),
                data = yips.cfa)
# Print fit summary
summary(object = yips.fit.2,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.2)


#### Continue adding covariance parameters until chi-squared p > .05

## Specify the model with added covariance between items 7 and 10
yips.model.3 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains'

## Fit the model as a CFA
yips.fit.3 <- cfa(model = yips.model.3, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.3,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.3)



## Specify the model with added covariance between items 3 and 5
yips.model.4 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense'

## Fit the model as a CFA
yips.fit.4 <- cfa(model = yips.model.4, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.4,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.4)



## Specify the model with added covariance between items 2 and 9
yips.model.5 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely'

## Fit the model as a CFA
yips.fit.5 <- cfa(model = yips.model.5, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.5,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.5)



## Specify the model with added covariance between items 1 and 10
yips.model.6 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains'

## Fit the model as a CFA
yips.fit.6 <- cfa(model = yips.model.6, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.6,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.6)



## Specify the model with added covariance between items 3 and 10
yips.model.7 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains'

## Fit the model as a CFA
yips.fit.7 <- cfa(model = yips.model.7, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.7,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.7)



## Specify the model with added covariance between items 3 and 7
yips.model.8 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips7_panicLoseControl'

## Fit the model as a CFA
yips.fit.8 <- cfa(model = yips.model.8, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.8,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.8)




## Specify the model with added covariance between items 1 and 9
yips.model.9 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips7_panicLoseControl
yips1_nervousAfraid ~~ yips9_worthlessLonely'

## Fit the model as a CFA
yips.fit.9 <- cfa(model = yips.model.9, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.9,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.9)



## Specify the model with added covariance between items 5 and 10
yips.model.10 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips7_panicLoseControl
yips1_nervousAfraid ~~ yips9_worthlessLonely
yips5_uncomfortableTense ~~ yips10_achesPains'

## Fit the model as a CFA
yips.fit.10 <- cfa(model = yips.model.10, 
                  estimator = "WLSMV",
                  ordered = names(yips.cfa),
                  data = yips.cfa)
# Print fit summary
summary(object = yips.fit.10,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.10)



## Specify the model with added covariance between items 4 and 6
yips.model.11 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips7_panicLoseControl
yips1_nervousAfraid ~~ yips9_worthlessLonely
yips5_uncomfortableTense ~~ yips10_achesPains
yips4_bothered ~~ yips6_moodyGrumpy'

## Fit the model as a CFA
yips.fit.11 <- cfa(model = yips.model.11, 
                   estimator = "WLSMV",
                   ordered = names(yips.cfa),
                   data = yips.cfa)
# Print fit summary
summary(object = yips.fit.11,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.11)



## Specify the model with added covariance between items 9 and 10
yips.model.12 <- '
## Specify latent structure
yips =~ yips1_nervousAfraid + yips2_tiredDrained + 
        yips3_relaxSettleDown + yips4_bothered +
        yips5_uncomfortableTense + yips6_moodyGrumpy +
        yips7_panicLoseControl + yips8_noEnjoyment + 
        yips9_worthlessLonely + yips10_achesPains
## Specify covariance terms
yips2_tiredDrained ~~ yips6_moodyGrumpy
yips7_panicLoseControl ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips5_uncomfortableTense
yips2_tiredDrained ~~ yips9_worthlessLonely
yips1_nervousAfraid ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips10_achesPains
yips3_relaxSettleDown ~~ yips7_panicLoseControl
yips1_nervousAfraid ~~ yips9_worthlessLonely
yips5_uncomfortableTense ~~ yips10_achesPains
yips4_bothered ~~ yips6_moodyGrumpy
yips9_worthlessLonely ~~ yips10_achesPains'

## Fit the model as a CFA
yips.fit.12 <- cfa(model = yips.model.12, 
                   estimator = "WLSMV",
                   ordered = names(yips.cfa),
                   data = yips.cfa)
# Print fit summary
summary(object = yips.fit.12,
        fit.measures = TRUE,
        standardized = TRUE)

## Inspect modification indices
modindices(yips.fit.12)





