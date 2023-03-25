setwd("~/University/5A/BU425/Whitepaper")

install.packages("car")
library(car)
source('./mylib2.r')
library(MASS)

# BU425 Whitepaper

#Things to add: 
  # extensive details about the dataset 
    # dataset 
  # decision tree
  # cook's distance
  # VIF
  # training and test data sets
# outliers
# pair graphs
# basic stats on factor variable -> bar graph => unsure how to 
# clustering -> log regression and accounted for clustering 
  

# Read the data

PatientData <- read.csv("ONPatients.csv", header=TRUE)
View(PatientData)

# Checking the data upload
head(PatientData)
names(PatientData)
dim(PatientData)

# Look at the data
# Examining some basic pairwise plots (but remember multivariate problem) -> export this to folder

# Examining the pairwise plots of the variables that show relationships amongst each other
pairs(~?..PatientID + AgeGroup + Gender + TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon + DiseaseType + InterventionType + AnaesthesiaType + SCU + DaysHospitalized + AcuteCareDays + ALCDays + Readmittance,data=PatientData[sample(nrow(PatientData),1000),],main="Plots")
pairs(~TransferTo + DischargeDispositon + DaysHospitalized + AcuteCareDays + ALCDays,data=PatientData[sample(nrow(PatientData),1000),],main="Plots")
pairs(~DaysHospitalized + AcuteCareDays + ALCDays,data=PatientData[sample(nrow(PatientData),1000),],main="Pairwise Plots", col = "red", pch = 22)


# Slight positive correlation between DaysHospitalized and AcuteCareDays

## Create factor variables for categorical variables 
PatientData$AgeGroup <- as.factor(PatientData$AgeGroup)
PatientData$Gender <- as.factor(PatientData$Gender)
PatientData$TransferFrom <- as.factor(PatientData$TransferFrom)
PatientData$AdmitCode <- as.factor(PatientData$AdmitCode)
PatientData$EmergencyCode <- as.factor(PatientData$EmergencyCode)
PatientData$TransferTo <- as.factor(PatientData$TransferTo)
PatientData$DischargeDispositon <- as.factor(PatientData$DischargeDispositon)
PatientData$DiseaseType <- as.factor(PatientData$DiseaseType)
PatientData$InterventionType <- as.factor(PatientData$InterventionType)
PatientData$AnaesthesiaType <- as.factor(PatientData$AnaesthesiaType)
PatientData$SCU <- as.factor(PatientData$SCU)
PatientData$Readmittance <- as.factor(PatientData$Readmittance)

attach(PatientData)

# runs basic summary statistics on the numerical variables
percentage <- prop.table(table(PatientData$AgeGroup)) * 100
cbind(freq=table(PatientData$AgeGroup), percentage=percentage)
 
percentage <- prop.table(table(PatientData$Gender)) * 100
cbind(freq=table(PatientData$Gender), percentage=percentage)

percentage <- prop.table(table(PatientData$TransferFrom)) * 100
cbind(freq=table(PatientData$TransferFrom), percentage=percentage)

percentage <- prop.table(table(PatientData$AdmitCode)) * 100
cbind(freq=table(PatientData$AdmitCode), percentage=percentage)

percentage <- prop.table(table(PatientData$EmergencyCode)) * 100
cbind(freq=table(PatientData$EmergencyCode), percentage=percentage)

percentage <- prop.table(table(PatientData$TransferTo)) * 100
cbind(freq=table(PatientData$TransferTo), percentage=percentage)

percentage <- prop.table(table(PatientData$DischargeDispositon)) * 100
cbind(freq=table(PatientData$DischargeDispositon), percentage=percentage)

percentage <- prop.table(table(PatientData$DiseaseType)) * 100
cbind(freq=table(PatientData$DiseaseType), percentage=percentage)

percentage <- prop.table(table(PatientData$InterventionType)) * 100
cbind(freq=table(PatientData$InterventionType), percentage=percentage)

percentage <- prop.table(table(PatientData$AnaesthesiaType)) * 100
cbind(freq=table(PatientData$AnaesthesiaType), percentage=percentage)

percentage <- prop.table(table(PatientData$SCU)) * 100
cbind(freq=table(PatientData$SCU), percentage=percentage)

percentage <- prop.table(table(PatientData$Readmittance)) * 100
cbind(freq=table(PatientData$Readmittance), percentage=percentage)

basic.stats(PatientData$DaysHospitalized)
basic.stats(PatientData$AcuteCareDays)
basic.stats(PatientData$ALCDays)

# No Missing Numerical Data

## 80% of the sample size
smp_size <- floor(0.80 * nrow(PatientData))
## set the seed to make your partition reproducible
train_ind <- sample(seq_len(nrow(PatientData)), size = smp_size)
train <- PatientData[train_ind, ]
test <- PatientData[-train_ind, ]

# Backwards elimination - Run the Logistic Regression
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + InterventionType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

Imodel.back <- stepAIC(Imodel, direction = "backward")
summary(Imodel.back)

Imodel <- glm(Readmittance ~ DaysHospitalized + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + InterventionType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
Imodel.back <- stepAIC(Imodel, direction = "backward")
summary(Imodel.back)
# <none> so no more variables will be dropped

#end of backwards elimination
# ------------------------------------

# Run the Logistic Regression
ReadmitModel <- glm(Readmittance ~ DaysHospitalized + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + InterventionType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = test)

# Summary of the Model
summary(ReadmitModel)
logLik(ReadmitModel)

cat("\nTest of Overall Fit of the Model\n")
cat("\nDifference in Deviance: ")
ReadmitModel$null.deviance - ReadmitModel$deviance
cat("\nDegrees of Freedom of the Difference: ")
ReadmitModel$df.null - ReadmitModel$df.residual
cat("\nChiSquare test for the Overall Model\n")
dchisq(ReadmitModel$null.deviance-ReadmitModel$deviance, ReadmitModel$df.null-ReadmitModel$df.residual)
cat("\n\n")

# examine residuals
cat("\n\nExamine Statistics of Residuals")
univariate.stats(resid(ReadmitModel))

# check for normality
cat("\n\nGenerate a Histogram of Standardized Residuals\n")
stdres <- rstandard(ReadmitModel)
hist(stdres, plot=TRUE, main="Main Model")

# Use Normality Plot
cat("\n\nGenerate a Normality Plot of Residuals\n")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Main Model")
qqline(stdres)


# check for outliers
cat("\n\nCooks Distance - Unusual Observations\n")
trm <- terms(ReadmitModel)
degfree <- length(attributes(trm)$term.labels) + attributes(trm)$intercept
cd <- cooks.distance(ReadmitModel)
pcd <- pf(cd,degfree,length(cd))
if (length(pcd[pcd>0.15]) > 0) {
  ds <-which(pcd > 0.15)
  print(ID[ds]) # =>  not working
}


ReadmitNum <- rep(0, length(test$Readmittance))
ReadmitNum[test$Readmittance == "Yes"] <- 1

cat("\nOverall Model Error Rate\n")
# overall error rate
predStatus <- rep(0,length(ReadmitNum))
predStatus[ReadmitModel$fitted > 0.5] <- 1
err = abs(predStatus - ReadmitNum)
sum(err)/length(err)

cat("\nTable of Predicted vs Actual\n")
conf_matrix <- table(predStatus, ReadmitNum)
conf_matrix

sensitivity <- (conf_matrix[1,1])/(conf_matrix[1,1] + conf_matrix[2,1])
sensitivity
specificity <- (conf_matrix[2,2])/(conf_matrix[1,2] + conf_matrix[2,2])
specificity
Accuracy <- (conf_matrix[1,1]+ conf_matrix[2,2])/(conf_matrix[1,1] + conf_matrix[1, 2]+conf_matrix[2,1]+conf_matrix[2, 2])
Accuracy

cor.test(ReadmitNum, predStatus)
library(pROC)
roc_obj <- roc(ReadmitNum, predStatus)
auc(roc_obj)
auc(ReadmitNum, predStatus)

plot(roc(ReadmitNum, predStatus, direction="<"), print.auc=TRUE, lwd=3, legacy.axes= TRUE, xlab= "False Positive Precentage", ylab= "True Positive Percentage", main="ROC Curve")

plot(roc(ReadmitNum, predStatus), )
