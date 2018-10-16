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
library(reshape2)   # melt


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

par(mfrow=c(1,1))


# EDA Barplot of Gender vs. Churn
subDF1 <- data.frame(cleandata$gender, cleandata$Churn)
Category_Gender <- ggplot(subDF1, aes(cleandata$gender, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Gender + labs(y="Proportion", x="Gender", title="Barplot of Gender vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of SeniorCitizen vs. Churn
subDF2 <- data.frame(cleandata$SeniorCitizen, cleandata$Churn)
Category_SeniorCitizen <- ggplot(subDF2, aes(cleandata$SeniorCitizen, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_SeniorCitizen + labs(y="Proportion", x="Senior Citizen", title="Barplot of Senior Citizen vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of Partner vs. Churn
subDF3 <- data.frame(cleandata$Partner, cleandata$Churn)
Category_Partner <- ggplot(subDF3, aes(cleandata$Partner, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Partner + labs(y="Proportion", x="Partner", title="Barplot of Partner vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of Dependents vs. Churn
subDF4 <- data.frame(cleandata$Dependents, cleandata$Churn)
Category_Dependents <- ggplot(subDF4, aes(cleandata$Dependents, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Dependents + labs(y="Proportion", x="Dependents", title="Barplot of Dependents vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of PhoneService vs. Churn
subDF5 <- data.frame(cleandata$PhoneService, cleandata$Churn)
Category_PhoneService <- ggplot(subDF5, aes(cleandata$PhoneService, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_PhoneService + labs(y="Proportion", x="Phone Service", title="Barplot of Phone Service vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of MultipleLines vs. Churn
subDF6 <- data.frame(cleandata$MultipleLines, cleandata$Churn)
Category_MultipleLines <- ggplot(subDF6, aes(cleandata$MultipleLines, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_MultipleLines + labs(y="Proportion", x="Multiple Lines", title="Barplot of Multiple Lines vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of InternetService vs. Churn
subDF7 <- data.frame(cleandata$InternetService, cleandata$Churn)
Category_InternetService <- ggplot(subDF7, aes(cleandata$InternetService, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_InternetService + labs(y="Proportion", x="Internet Service", title="Barplot of Internet Service vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of OnlineSecurity vs. Churn
subDF8 <- data.frame(cleandata$OnlineSecurity, cleandata$Churn)
Category_OnlineSecurity <- ggplot(subDF8, aes(cleandata$OnlineSecurity, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_OnlineSecurity + labs(y="Proportion", x="Online Security", title="Barplot of Online Security vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of OnlineBackup vs. Churn
subDF9 <- data.frame(cleandata$OnlineBackup, cleandata$Churn)
Category_OnlineBackup <- ggplot(subDF9, aes(cleandata$OnlineBackup, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_OnlineBackup + labs(y="Proportion", x="Online Backup", title="Barplot of Online Backup vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of DeviceProtection vs. Churn
subDF10 <- data.frame(cleandata$DeviceProtection, cleandata$Churn)
Category_DeviceProtection <- ggplot(subDF10, aes(cleandata$DeviceProtection, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_DeviceProtection + labs(y="Proportion", x="Device Protection", title="Barplot of Device Protection vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of TechSupport vs. Churn
subDF11 <- data.frame(cleandata$TechSupport, cleandata$Churn)
Category_TechSupport <- ggplot(subDF11, aes(cleandata$TechSupport, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_TechSupport + labs(y="Proportion", x="Tech Support", title="Barplot of Tech Support vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of StreamingTV vs. Churn
subDF12 <- data.frame(cleandata$StreamingTV, cleandata$Churn)
Category_StreamingTV <- ggplot(subDF12, aes(cleandata$StreamingTV, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_StreamingTV + labs(y="Proportion", x="Streaming TV", title="Barplot of Streaming TV vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of StreamingMovies vs. Churn
subDF13 <- data.frame(cleandata$StreamingMovies, cleandata$Churn)
Category_StreamingMovies <- ggplot(subDF13, aes(cleandata$StreamingMovies, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_StreamingMovies + labs(y="Proportion", x="Streaming Movies", title="Barplot of Streaming Movies vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of Contract vs. Churn
subDF14 <- data.frame(cleandata$Contract, cleandata$Churn)
Category_Contract <- ggplot(subDF14, aes(cleandata$Contract, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_Contract + labs(y="Proportion", x="Contract", title="Barplot of Contract vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of PaperlessBilling vs. Churn
subDF15 <- data.frame(cleandata$PaperlessBilling, cleandata$Churn)
Category_PaperlessBilling <- ggplot(subDF15, aes(cleandata$PaperlessBilling, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_PaperlessBilling + labs(y="Proportion", x="Paperless Billing", title="Barplot of Paperless Billing vs. Churn") +
           scale_fill_discrete(name="Churn"))


# EDA Barplot of PaymentMethod vs. Churn
subDF16 <- data.frame(cleandata$PaymentMethod, cleandata$Churn)
Category_PaymentMethod <- ggplot(subDF16, aes(cleandata$PaymentMethod, (..count..)/sum(..count..))) + 
     geom_bar(aes(fill=cleandata$Churn), position="dodge") + 
     theme(plot.title = element_text(hjust = 0.5))
print(Category_PaymentMethod + labs(y="Proportion", x="Payment Method", title="Barplot of Payment Method vs. Churn") +
           scale_fill_discrete(name="Churn"))


## EDA Boxplot of Monthly Charges vs. Churn
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



