# Telco Customer Churn

# Objective: Predict behavior to retain customers. You can analyze all relevant customer 
#            data and develop focused customer retention programs.

# Data: Each row represents a customer, each column contains customer's attributes described 
#       on the column Metadata. The raw data contains 7043 rows (customers) and 21 columns (features).

# The "Churn" column is our target.

# Description: 
#       customerID: Customer ID
#       gender: Customer gender (female, male)
#       SeniorCitizen: Whether the customer is a senior citizen or not (1,0)
#       Partner: Whether the customer has a partner or not (Yes, No)
#       Dependents: Whether the customer has dependents or not (Yes, No)
#       tenure: Number of months the customer has stayed with the company
#       PhoneService: Whether the customer has a phone service or not (Yes, No)
#       MultipleLines: Whether the customer has multiple lines or not (Yes, No, No phone service)
#       InternetService: Customer's internet service provider (DSL, Fiber optic, No)
#       OnlineSecurity: Whether the customer has online security or not (Yes, No, No internet service)
#       OnlineBackup: Whether the customer has online backup or not (Yes, No, No internet service)
#       DeviceProtection: Whether the customer has device protection or not (Yes, No, No internet service)
#       TechSupport: Whether the customer has tech support or not (Yes, No, No internet service)
#       StreamingTV: Whether the customer has streaming TV or not (Yes, No, No internet service)
#       StreamingMovies: Whether the customer has streaming movies or not (Yes, No, No internet service)
#       Contract: The contract term of the customer (Month-to-month, One year, Two year)
#       PaperlessBilling: Whether the customer has paperless billing or not (Yes, No)
#       PaymentMethod: The customer's payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic))
#       MonthlyCharges: The amount charged to the customer monthly
#       TotalCharges: The total amount charged to the customer
#       Churn: Whether the customer churned or not (Yes or No)
       

setwd("C:/Users/SEYOUNG/Desktop/Jung/")
getwd()

library(readr)      # read_csv
library(dplyr)      # glimpse, 
library(tidyr)
library(caTools)    # sample.split
library(ggplot2)    # ggplot


##################################
########   Data Cleaning  ########
##################################
telcodata <- read_csv("telcodata.csv")
head(telcodata, 10)
tail(telcodata, 10)
glimpse(telcodata)

## Check if there are missing data. And if the number of missing data is small, we will delete the corresponding rows.
MVinfo <- apply(is.na(telcodata), 2, which)
withoutMV <- telcodata[-MVinfo$TotalCharges, ]

## In order for us to analyze data efficiently, all the variables except for customerID, SeniorCitizen, tenure, MonthlyCharges, and TotalCharges should be coerced to factor variables. 
cols <- c(1, 3, 6, 19, 20)
withoutMV[,-cols] <- data.frame(apply(withoutMV[-cols], 2, as.factor))
summary(withoutMV)

## Coerce the SeniorCitizen variable to a factor. 1: Yes, 2: No
withoutMV <- withoutMV %>% mutate(SeniorCitizen = ifelse(SeniorCitizen == 0, "No", "Yes"))
withoutMV$SeniorCitizen <- as.factor(withoutMV$SeniorCitizen)


## Since the first column (customerID) is useless when analyzing data, we will get rid of it
cleandata <- withoutMV[, 2:21]
summary(cleandata)


## Check the quantile of tenure and create a new column that classifies the elements by years
quantile(cleandata$tenure, probs=seq(0, 1, by=0.1))

cleandata <- mutate(cleandata, Year=ifelse(tenure %in% 1:12, "0-1", 
                         ifelse(tenure %in% 13:24, "1-2", 
                           ifelse(tenure %in% 25:36, "2-3", 
                             ifelse(tenure %in% 37:48, "3-4", 
                               ifelse(tenure %in% 49:60, "4-5", 
                                 ifelse(tenure %in% 61:72, "5-6", "6-7")))))))
cleandata$Year <- as.factor(cleandata$Year)


## Since we are interested in prediction, we will split the dataset into training and test sets
set.seed(12345)
sample <- sample.split(cleandata, SplitRatio=0.7)
train_data <- subset(cleandata, sample==TRUE)
test_data <- subset(cleandata, sample==FALSE)






##############################################
########   Exploratory Data Analysis  ########
##############################################

attach(cleandata)

## EDA Boxplot of Monthly Charges vs. Churn
par(mfrow=c(1,1))
means <- aggregate(MonthlyCharges ~ Churn, cleandata, mean)
Category_Monthly <- ggplot(data=cleandata, aes(x=Churn, y=MonthlyCharges, fill=Churn)) + geom_boxplot() + 
     stat_summary(fun.y=mean, geom="point", shape=15, size=2) +
     geom_text(data=means, aes(label=round(MonthlyCharges,3), y=MonthlyCharges -3.0)) +
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Monthly + labs(y="Monthly Charges ($)", title="Boxplot of Monthly Charges vs. Churn"))




## EDA Barplot of Customer Tenure (Year)
Category_Year <- ggplot(data = cleandata, aes(Year, fill=Year)) + 
     geom_bar(fill="steelblue") +
     geom_text(stat='count', aes(label=..count..), vjust=1.6, color="white", size=3.5) +
     theme_minimal() + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Year + labs(x="Tenure (Year)", y="Count", title="Customer Tenure"))




## Below graph is similar to the one on Kaggle
## ggplot(as.data.frame(table(Churn, gender)), aes(x=Churn, y=Freq, fill=gender)) + geom_bar(stat="identity")

plot(table(Churn, gender)/nrow(cleandata))
