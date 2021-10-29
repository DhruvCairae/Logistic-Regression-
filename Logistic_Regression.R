## ---- Read and transform dataset----------------------------------------
library(dplyr)
file19<-read.csv("dataset.csv", header = T)
head(file19)
churn<-file19[-c(1:2)]
churn<-na.omit(churn)
install.packages('fastDummies')
library('fastDummies')
churn <- dummy_cols(churn, select_columns = c('gender','Partner','Dependents','PhoneService','MultipleLines',
'InternetService','OnlineSecurity','OnlineBackup','DeviceProtection','TechSupport','StreamingTV','StreamingMovies',
'PaperlessBilling'),remove_selected_columns = TRUE)

churn_1<-subset(churn,select = c(SeniorCitizen,tenure,MonthlyCharges,TotalCharges,gender_Male,Partner_Yes,Dependents_Yes,
PhoneService_Yes,MultipleLines_Yes,InternetService_DSL,InternetService_No,OnlineSecurity_Yes,OnlineBackup_Yes,DeviceProtection_Yes,TechSupport_Yes,StreamingTV_Yes,
StreamingMovies_Yes,PaperlessBilling_Yes))
churn_1$creditcard<- ifelse(churn$PaymentMethod=="Credit card (automatic)", 1, 0)                            
churn_1$banktransfer<- ifelse(churn$PaymentMethod=="Bank transfer (automatic)", 1, 0)
churn_1$ec<- ifelse(churn$PaymentMethod=="Electronic check", 1, 0)
churn_1$monthlycontract<- ifelse(churn$Contract=="Month-to-month", 1, 0)
churn_1$annual<- ifelse(churn$Contract=="One year", 1, 0)
churn_1$Churn_Yes<- ifelse(churn$Churn=="Yes", 1, 0)

                             
cor(churn_1,churn_1$Churn_Yes)
cor(churn_1)


#Linear Discriminant and Logistic Regression Model: Based on correlations, churn is strongly correlated with tenure, monthly charges, 
#internet service availability, usage of electronic checks as a payment method and whether the contract is monthly.

churn_2<-subset(churn_1,select=c(tenure,MonthlyCharges,InternetService_No,ec,monthlycontract,Churn_Yes))

churn_2$InternetService_No<-as.factor(churn_2$InternetService_No)
churn_2$ec<-as.factor(churn_2$ec)
churn_2$monthlycontract<-as.factor(churn_2$monthlycontract)
churn_2$Churn_Yes<-as.factor(churn_2$Churn_Yes)

set.seed(1000)
ind<-sample(2,nrow(churn_2),replace=TRUE,prob=c(0.6,0.4))
training<-churn_2[ind==1,]
testing<-churn_2[ind==2,]
library(MASS)
linear<-lda(Churn_Yes~.,training)
linear
p<-predict(linear,training)
ldahist(data=p$x[,1],g=training$Churn_Yes)

p1<-predict(linear,training)$class
tab<-table(Predicted=p1,Actual=training$Churn_Yes)
tab

p2<-predict(linear,testing)$class
tab1<-table(Predicted=p2,Actual=testing$Churn_Yes)
tab1

install.packages('caTools')
library(caTools)
sr<-sample.split(churn_2,SplitRatio = 0.8)
sr
train<-subset(churn_2,sr==TRUE)
test<-subset(churn_2,sr==FALSE)

model<-glm(Churn_Yes~tenure+MonthlyCharges+InternetService_No+ec+monthlycontract,data = train,family = 'binomial')
summary(model)
res<-predict(model,test,type = 'response')
res
res<-predict(model,train,type = 'response')
res

confmatrix<-table(Actual_Value=train$Churn_Yes,Predicted_Value=res>0.5)
confmatrix
(confmatrix[1,1]+confmatrix[2,2])/sum(confmatrix)

install.packages('epiDisplay')
library(epiDisplay)
logistic.display(model)



#Question3

# Divide all the customers into 3 categories namely Low, Medium and High 
# using the variable "TotalCharges".  Let us call them "Customer Value Segments". 
# Build prediction models to predict the category/ Value Segment.
# Comment on the profile of the customers in each category/ Value Segment. 
# Identify appropriate strategies to shift customers from each value segment 
# to the next higher segment.



segments<-file19[-c(1:2)]
segments<-na.omit(segments)

segments$cvs<- ifelse(segments$TotalCharges<418,"Low",ifelse(segments$TotalCharges>3797,"High","Medium"))
segments$cvs<-as.factor(segments$cvs)
segments$SeniorCitizen<-as.factor(segments$SeniorCitizen)
segments<-segments[-c(18:19)]

segments$MultipleLines<-as.character(segments$MultipleLines)
segments[ segments == "No phone service" ] <- "No"
segments$MultipleLines<-as.factor(segments$MultipleLines)

segments$OnlineSecurity<-as.character(segments$OnlineSecurity)
segments[ segments == "No internet service" ] <- "No"
segments$OnlineSecurity<-as.factor(segments$OnlineSecurity)

segments$OnlineBackup<-as.character(segments$OnlineBackup)
segments[ segments == "No internet service" ] <- "No"
segments$OnlineBackup<-as.factor(segments$OnlineBackup)

segments$DeviceProtection<-as.character(segments$DeviceProtection)
segments[ segments == "No internet service" ] <- "No"
segments$DeviceProtection<-as.factor(segments$DeviceProtection)

segments$TechSupport<-as.character(segments$TechSupport)
segments[ segments == "No internet service" ] <- "No"
segments$TechSupport<-as.factor(segments$TechSupport)

segments$TechSupport<-as.character(segments$TechSupport)
segments[ segments == "No internet service" ] <- "No"
segments$TechSupport<-as.factor(segments$TechSupport)

segments$StreamingTV<-as.character(segments$StreamingTV)
segments[ segments == "No internet service" ] <- "No"
segments$StreamingTV<-as.factor(segments$StreamingTV)

segments$StreamingMovies<-as.character(segments$StreamingMovies)
segments[ segments == "No internet service" ] <- "No"
segments$StreamingMovies<-as.factor(segments$StreamingMovies)


set.seed(577)
ind<-sample(2,nrow(segments),replace=TRUE,prob=c(0.8,0.2))
training<-segments[ind==1,]
testing<-segments[ind==2,]
library(MASS)
linear<-lda(cvs~.,training)
linear
p<-predict(linear,training)
ldahist(data=p$x[,1],g=training$cvs)

p1<-predict(linear,training)$class
tab<-table(Predicted=p1,Actual=training$cvs)
tab

p2<-predict(linear,testing)$class
tab1<-table(Predicted=p2,Actual=testing$cvs)
tab1


# Q4. Create an overall survival curve using the Tenure variable.
# Use Kaplan-Meier method. 


## ---------------------------------------------------------------------------------
# Loading Required Packages for Survival Analysis
library(readr)
library("survival")
library("survminer")
options(scipen = 5)


## ---------------------------------------------------------------------------------
#Loading the data file
g <- read_csv("C:/Users/PankhuriManish/Desktop/SA4 Assignment/file19.csv")
g$Churn_Yes <- ifelse(g$Churn == "Yes", 1,0)


## ---------------------------------------------------------------------------------
# Checking if the data set has any Null Values
lapply(g,function(x) { length(which(is.na(x)))})


## ---------------------------------------------------------------------------------
# Overall survival curve using the Tenure variable.
# Using Kaplan-Meier Method
fit <- survfit(Surv(tenure,Churn_Yes) ~ 1, data = g)
summary(fit, times = c(1:7,8*c(1:9)))


## ---------------------------------------------------------------------------------
ggsurvplot(fit,
           conf.int = TRUE, 
           risk.table = TRUE,
           ggtheme = theme_light())



# Q5. Create separate survival curves for different categories of customers (for example, Gender). 
# Comment on the differences in these survival curves.

## ---------------------------------------------------------------------------------
# Converting categorical various variables are being treated as continuous into categoricals.

g$SeniorCitizen <- factor(g$SeniorCitizen)
g$Gender <- factor(g$gender)
g$Partner <- factor(g$Partner)
g$Dependents <- factor(g$Dependents)
g$PhoneService <- factor(g$PhoneService)
g$PaymentMethod <-factor(g$PaymentMethod)
g$MultipleLines <- factor(g$MultipleLines)
g$InternetService <- factor(g$InternetService)
g$Contract <- factor(g$Contract)
g$PaperlessBilling <- factor(g$PaperlessBilling)
g$OnlineSecurity <- factor(g$OnlineSecurity)
g$OnlineBackup <- factor(g$OnlineBackup)
g$DeviceProtection <- factor(g$DeviceProtection)
g$TechSupport <- factor(g$TechSupport)
g$StreamingTV <- factor(g$StreamingTV)
g$StreamingMovies <- factor(g$StreamingMovies)


## ---------------------------------------------------------------------------------

fit_SeniorCitizen <- survfit(Surv(tenure,Churn_Yes) ~ SeniorCitizen, data=g)

summary(fit_SeniorCitizen, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_SeniorCitizen,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ SeniorCitizen, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_Gender <- survfit(Surv(tenure,Churn_Yes) ~ Gender, data=g)

summary(fit_Gender, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_Gender,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ Gender, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_Partner <- survfit(Surv(tenure,Churn_Yes) ~ Partner, data=g)

summary(fit_Partner, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_Partner,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ Partner, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_Dependents <- survfit(Surv(tenure,Churn_Yes) ~ Dependents, data=g)

summary(fit_Dependents, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_Dependents,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ Dependents, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_PhoneService <- survfit(Surv(tenure,Churn_Yes) ~ PhoneService, data=g)

summary(fit_PhoneService, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_PhoneService,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ PhoneService, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_PaymentMethod <- survfit(Surv(tenure,Churn_Yes) ~ PaymentMethod, data=g)

summary(fit_PaymentMethod, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_PaymentMethod,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ PaymentMethod, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_MultipleLines <- survfit(Surv(tenure,Churn_Yes) ~ MultipleLines, data=g)

summary(fit_MultipleLines, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_MultipleLines,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ MultipleLines, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_InternetService <- survfit(Surv(tenure,Churn_Yes) ~ InternetService, data=g)

summary(fit_SeniorCitizen, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_InternetService,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ InternetService, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_Contract <- survfit(Surv(tenure,Churn_Yes) ~ Contract, data=g)

summary(fit_Contract, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_Contract,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ Contract, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_PaperlessBilling <- survfit(Surv(tenure,Churn_Yes) ~ PaperlessBilling, data=g)

summary(fit_PaperlessBilling, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_PaperlessBilling,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ PaperlessBilling, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_OnlineSecurity <- survfit(Surv(tenure,Churn_Yes) ~ OnlineSecurity, data=g)

summary(fit_OnlineSecurity, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_OnlineSecurity,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ OnlineSecurity, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_OnlineBackup <- survfit(Surv(tenure,Churn_Yes) ~ OnlineBackup, data=g)

summary(fit_OnlineBackup, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_OnlineBackup,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ OnlineBackup, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_DeviceProtection <- survfit(Surv(tenure,Churn_Yes) ~ DeviceProtection, data=g)

summary(fit_DeviceProtection, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_DeviceProtection,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ DeviceProtection, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_TechSupport <- survfit(Surv(tenure,Churn_Yes) ~ TechSupport, data=g)

summary(fit_TechSupport, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_TechSupport,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ TechSupport, rho=0, data = g)


## ---------------------------------------------------------------------------------

fit_StreamingTV <- survfit(Surv(tenure,Churn_Yes) ~ StreamingTV,
                           data=g[which(g$StreamingTV != "No internet service"),])

summary(fit_StreamingTV, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_StreamingTV,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ StreamingTV, rho=0,
         data=g[which(g$StreamingTV != "No internet service"),])


## ---------------------------------------------------------------------------------

fit_StreamingMovies <- survfit(Surv(tenure,Churn_Yes) ~ StreamingMovies,
                               data=g[which(g$StreamingMovies != "No internet service"),])

summary(fit_StreamingMovies, times = c(1:7,8*c(1:9)))

#Kaplan-Meier plot

ggsurvplot(fit_StreamingMovies,
           conf.int = TRUE,
           conf.int.style = "step", 
           risk.table = "abs_pct",
           risk.table.col = "strata",
           pval = TRUE,
           surv.median.line = "hv",
           pval.method = TRUE,
           ggtheme = theme_classic())

# Calculating p - values to see if the difference between the two groups is significant
survdiff(Surv(tenure,Churn_Yes) ~ StreamingMovies, rho=0,
         data=g[which(g$StreamingMovies != "No internet service"),])


# 6. Build Cox's Hazard model using appropriate explanatory variables.
# Comment on the coefficients of the model.

## ---------------------------------------------------------------------------------
cox_model <- coxph(Surv(tenure, Churn_Yes)~ gender +
                     SeniorCitizen +
                     Partner +
                     Dependents +
                     PhoneService +
                     MultipleLines +
                     InternetService +
                     OnlineBackup +
                     OnlineSecurity +
                     DeviceProtection +
                     TechSupport +
                     StreamingTV +
                     StreamingMovies +
                     Contract +
                     PaperlessBilling +
                     PaymentMethod
                   , data = g)

summary(cox_model)


## ---------------------------------------------------------------------------------
cox_modelb <- coxph(Surv(tenure, Churn_Yes)~ Partner +
                      MultipleLines +
                      OnlineBackup +
                      OnlineSecurity +
                      DeviceProtection +
                      TechSupport +
                      Contract +
                      StreamingMovies +
                      PaperlessBilling +
                      PaymentMethod
                    , data = g)

summary(cox_modelb)