Telco Customer Churn Prediction
================
> Understanding telco customer behavior to predict whether they will churn or not


### Introduction

According to [this article](https://www.bizjournals.com/albany/news/2019/04/11/number-of-businesses-in-the-united-states.html#:~:text=But%20with%20more%20than%2032.5,to%20where%20people%20conduct%20business.), there are "more than 32.5 million businesses in the world". No one can doubt that the key to success for businesses is to have a more solid and wider customer base. Therefore, their main interest is to have a better customer service for their existing and future customers. And this is absolutely more important to telecom companies such as AT&T and Verizon, because the market is already saturated. Hence, they should first retain their customers not to lose to their competitors. To do so, they should first understand customer behavior and make programs that satisfy their needs accurately. 

-----
### Objective

In this project, I will build a classification model to predict whether a Telco customer will churn or not. This analysis will help companies better understand their  customers so they can figure out what they actually need to retain them. To accomplish this, I will implement a logistic regression model. You can get more information about the algorithm [here](https://en.wikipedia.org/wiki/Logistic_regression). 

This is part of a group project where we introduce the concept of logistic regression and how logistic regression can be used in practice. This group is comprised of five graduate students. Our group made this [Youtube playlist](https://www.youtube.com/playlist?list=PLGmy0B-4mUItp8wku-Mil8iBGIp1u0JIV) for the project. I was in charge of data cleaning, EDA, and model building. You can find my portion [here](https://www.youtube.com/watch?v=d7LDoWV-xjk&list=PLGmy0B-4mUItp8wku-Mil8iBGIp1u0JIV&index=5&t=0s). 

#### About the Data

The Telco Customer Churn dataset is provided by the [IBM Analytics Communities](https://www.kaggle.com/blastchar/telco-customer-churn)
- This dataset is comprised of 7,043 of customers' information and 21 features such as gender, tenure, etc. 
- The target variable is "Churn". 
- Detailed descriptions of the features can be found in the link above. 


#### Why use logistic regression for this project?

Logistic regression is used when a target variable is dichotomous. Since our target variable is yes/no, meaning they churned/not churned, this algorithm is one of our options to solve this problem. And logistic regression is relatively simple and the outputs are easier to interpret compared to other classification algorithms. Since this is my first data science project to share, I will choose this simple algorithm to demonstrate how this is used for this classification problem. I will be using a threshold value of 0.5 for evaluations. The scores above this threshold will be classified as positive, and vice versa. 


-----

### Conclusion 

If we use a cutoff point of 0.5 and just guess without any statistical models, we will get roughly 73% right, because 73% of customers from our dataset churned. However, we will be approximately 80% correct if we use our second logistic regression model that has only selected features rather than using the entire given features. And this increased accuracy will help the Telco company predict whether a customer will churn or not more accurately, and this analysis will be helpful for them when they develop their business strategy to retain their customers.




-----
### Versions

For this project, I use

  - R version 4.0.0
  - cmake version 3.18.1
  - Visual Studio 2019 version 16.6
  - Git version 2.28.0

-----

### Contact Information

  - If you have any questions, feel free to email me at
    <sjung.stat@gmail.com>
  - You can find my LinkedIn profile
    [here](https://www.linkedin.com/in/sjung-stat/)
