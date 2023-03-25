setwd("~/University/5A/BU425/Whitepaper")

library(car)
source('./mylib2.r')

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
pairs(~ï..PatientID + AgeGroup + Gender + TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon + DiseaseType + InterventionType + AnaesthesiaType + SCU + DaysHospitalized + AcuteCareDays + ALCDays + Readmittance,data=PatientData[sample(nrow(PatientData),1000),],main="Plots")
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

# drop InterventionType
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Special Rehab
train$TransferFrom[train$TransferFrom == 'Special Rehab'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Diseases of the Genitourinary System
train$DiseaseType[train$DiseaseType == 'Diseases of the Genitourinary System'] <- NA
droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AnaesthesiaType == 'Spinal
train$AnaesthesiaType[train$AnaesthesiaType == 'Spinal'] <- NA
x <- droplevels(train$AnaesthesiaType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AgeGroup == '35-39 yrs'
train$AgeGroup[train$AgeGroup == '35-39 yrs'] <- NA
x <- droplevels(train$AgeGroup)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DischargeDispositon == 'Long Term Care Transfer
train$DischargeDispositon[train$DischargeDispositon == 'Long Term Care Transfer'] <- NA
x <- droplevels(train$DischargeDispositon)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Congenital Malformations
train$DiseaseType[train$DiseaseType == 'Congenital Malformations'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferTo == 'General Rehab
train$TransferTo[train$TransferTo == 'General Rehab'] <- NA
x <- droplevels(train$TransferTo)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AgeGroup == '30-34 yrs'
train$AgeGroup[train$AgeGroup == '30-34 yrs'] <- NA
x <- droplevels(train$AgeGroup)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Pregnancy
train$DiseaseType[train$DiseaseType == 'Pregnancy'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Chronic Care 
train$TransferFrom[train$TransferFrom == 'Chronic Care'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AnaesthesiaType == 'General
train$AnaesthesiaType[train$AnaesthesiaType == 'General'] <- NA
x <- droplevels(train$AnaesthesiaType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop EmergencyCode == 'Emergency
train$EmergencyCode[train$EmergencyCode == 'Emergency'] <- NA
x <- droplevels(train$EmergencyCode)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Outpatient
train$TransferFrom[train$TransferFrom == 'Outpatient'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Home Care
train$TransferFrom[train$TransferFrom == 'Home Care'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Day Surgery
train$TransferFrom[train$TransferFrom == 'Day Surgery'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Body Injury
train$DiseaseType[train$DiseaseType == 'Body Injury'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferTo == 'Ambulatory Care
train$TransferTo[train$TransferTo == 'Ambulatory Care'] <- NA
x <- droplevels(train$TransferTo)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Diseases of the Digestive System
train$DiseaseType[train$DiseaseType == 'Diseases of the Digestive System'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferTo == 'Unclassified
train$TransferTo[train$TransferTo == 'Unclassified'] <- NA
x <- droplevels(train$TransferTo)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DischargeDispositon == 'Inpatient Transfer
train$DischargeDispositon[train$DischargeDispositon == 'Inpatient Transfer'] <- NA
x <- droplevels(train$DischargeDispositon)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DischargeDispositon == 'Other Transfer
train$DischargeDispositon[train$DischargeDispositon == 'Other Transfer'] <- NA
x <- droplevels(train$DischargeDispositon)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Burns and Corrosions
train$DiseaseType[train$DiseaseType == 'Burns and Corrosions'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Diseases of the Ear and Mastoid Process
train$DiseaseType[train$DiseaseType == 'Diseases of the Ear and Mastoid Process'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferTo
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Unclassified
train$TransferFrom[train$TransferFrom == 'Unclassified'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DischargeDispositon+DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop Discharge disposition
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType + SCU, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop SCU
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Endocrine, Nutritional, and Metabolic Diseases
train$DiseaseType[train$DiseaseType == 'Endocrine, Nutritional, and Metabolic Diseases'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'General Rehab
train$TransferFrom[train$TransferFrom == 'General Rehab'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Mental and Behaviour Disorders
train$DiseaseType[train$DiseaseType == 'Mental and Behaviour Disorders'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AnaesthesiaType == 'Epidural
train$AnaesthesiaType[train$AnaesthesiaType == 'Epidural'] <- NA
x <- droplevels(train$AnaesthesiaType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop AgeGroup == '25-29 yrs
train$AgeGroup[train$AgeGroup == '25-29 yrs'] <- NA
x <- droplevels(train$AgeGroup)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Infectious and Parasitic Diseases
train$DiseaseType[train$DiseaseType == 'Infectious and Parasitic Diseases'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Health Status Factors
train$DiseaseType[train$DiseaseType == 'Health Status Factors'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Nursing Home
train$TransferFrom[train$TransferFrom == 'Nursing Home'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Ambulatory Care
train$TransferFrom[train$TransferFrom == 'Ambulatory Care'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop TransferFrom == 'Aged Home
train$TransferFrom[train$TransferFrom == 'Aged Home'] <- NA
x <- droplevels(train$TransferFrom)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop Admitcode
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop DiseaseType == 'Diseases of the Nervous System
train$DiseaseType[train$DiseaseType == 'Diseases of the Nervous System'] <- NA
x <- droplevels(train$DiseaseType)
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)

# drop ALC Days
Imodel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + AgeGroup +Gender+ TransferFrom + EmergencyCode + DiseaseType + AnaesthesiaType, family=binomial(link='logit'), data = train)
summary(Imodel)
# ------------------------------------




#end of backwards elimination

# Run the Logistic Regression
ReadmitModel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DiseaseType + InterventionType, family=binomial(link='logit'), data = test)

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


ReadmitModel <- glm(Readmittance ~ DaysHospitalized + AcuteCareDays + ALCDays + AgeGroup +Gender+ TransferFrom + AdmitCode + EmergencyCode + TransferTo + DiseaseType + InterventionType, family=binomial(link='logit'), data = test)

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



