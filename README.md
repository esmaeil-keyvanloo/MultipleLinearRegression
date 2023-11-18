# MultipleLinearRegression

#' 
#' #### Example exercise: Number of trips to 273 traffic zones in Lisbon Metropolitan Area (LMA) in 2010.
#' 
#' **Your task**: estimate a multiple linear regression model that predicts the Average number of 
trips during the day (per person) (“atrip”) of the Lisbon Metropolitan Area (LMA).
#' 
#' #### Variables:
#' 
#' * `DCBD`: Distance to CBD (specified by the three main employment locations:Saldanha, Av. Columbano Bordalo Pinheiro, and Parque das Nações) (km);
#' * `atrip`: Average number of trips during the day (per person);
#' * `AvCar`: Car or car and/or motorcycle daily availability;
#' * `ATTD`:Average average total distance traveled during the day per person (km);  
#' * `ATTT`: Average Total time spent traveling during the day (minutes); 
#' * `Work`:Percentage of interviews that referred “Work” as the main trip purpose within the trip chain;
#' * `Shop`:Percentage of interviews that referred “Shopping” as the main trip purpose within the trip chain;
#' * `Family`:Percentage of interviews that referred “Family” related purpose as the main trip purpose within the trip chain;
#' * `Personal`:Percentage of interviews that referred “Personal” reasons as the main trip purpose within the trip chain;
#' * `aChildren`:Average number of children in the households;
#' * `ElderRetired`:Number of elder or retired persons in the households;
#' * `TCPass`:Percentage of interviews holding a Public Transport Pass;
#' * `AcessMetroResid`:Distance to the closest subway or suburban rail stations near the residence (meters);
#' * `ParkingOrigin`:Indicator (between 0 and 1) that indicates the parking pressure close to the residential area. Higher values indicate that the availability of parking places near home is scarce;
#' 
#' ## Let's begin!
#' 
#' ##### Import Libraries
library(readxl) #Library used to import excel files
library(tidyverse) # Pack of most used libraries
library(skimr) # Library used for providing a summary of the data
library(DataExplorer) # Library used in data science to perform exploratory data analysis
library(corrplot) # Library used for correlation plots
library(car) # Library used for testing autocorrelation (Durbin Watson)
library(olsrr) # Library used for testing multicollinearity (VIF, TOL, etc.)

#' 
#' ##### Import dataset
dataset <- read_excel("Data/TDM_LinearRegression_HA_data_LMA1.xls")
class(dataset)

#' 
#' ##### Transform the dataset into a dataframe
df <- data.frame(dataset)

#' 
#' ##### Show summary statistics
skim(df)
summary(df)

#' 
#' ## Multiple Linear Regression
#' Equation with `atrip` as the dependent variable:  
#' 
#' #### Checking assumptions
#' Before running the model, you need to check if the assumptions are met.
#' 
#' ##### Linear relation
#' For instance, let's take a look if the independent variables have linear relation with the dependent variable.
#' 
par(mfrow=c(2,3)) #set plot area as 2 rows and 3 columns
plot(x = df$atrip, y = df$DCBD, xlab = "atrip", ylab = "DCBD")  
plot(x = df$atrip, y = df$AHS, xlab = "atrip", ylab = "ATTD")  
plot(x = df$atrip, y = df$SI, xlab = "atrip", ylab = "ATTT")  
plot(x = df$atrip, y = df$SRI, xlab = "atrip", ylab = "Work")  
plot(x = df$atrip, y = df$UI, xlab = "atrip", ylab = "Shop")
plot(x = df$atrip, y = df$UI, xlab = "atrip", ylab = "Family")
plot(x = df$atrip, y = df$UI, xlab = "atrip", ylab = "Personal")

#' 
#' Or you could execute a pairwise scatterplot matrix, that compares every variable with each other: 
#' 
pairs(df[,1:6], pch = 19, lower.panel = NULL)

#' 
#' 
#' ##### Normal distribution of the dependent variable
#' Check if the dependent variable is normally distributed. 
#' If the sample is smaller than 50 observations, use Shapiro-Wilk test: 
#' 
shapiro.test(df$atrip)

#' 
#' If not, use the Kolmogorov-Smirnov test
#' 
ks.test(df$atrip, "pnorm", mean=mean(df$atrip), sd = sd(df$atrip))

#' 
#' > **Note:** Regarding the warning that appears in the Kolmogorov-Smirnov test 
#' "ties should not be present for the Kolmogorov-Smirnov test", what most likely happened is that this test 
#' is only reliable with continuous variables.   
#' 
#' Although `atrip` is a continuous variable, the big sample size (n=273) makes it likely to have repeated values. 
#' Consequently, the test considers `atrip` as a categorical variable. 
#' Therefore, this is another evidence, that for small samples it is more appropriate to use the Shapiro-Wilk Test.  
#' The null hypothesis of both tests is that the distribution is normal. 
#' Therefore, for the distribution to be normal, the pvalue > 0.05 and you should not reject the null hypothesis.
#' 
#' ### Multiple linear regression model
#' 
model <- lm(atrip ~ DCBD + ATTD + ATTT + Work + Shop + Family + Personal, data = df)
summary(model)

#' 
#' **Assessing the model**:
#' 
#' 1. First check the **pvalue** and the **F statistics** of the model to see if there is any statistical relation 
#' between the dependent variable and the independent variables. 
#' If pvalue < 0.05 and the F statistics > Fcritical = 2,39, then the model is statistically acceptable.  
#' 
#' 2. The **R-square** and **Adjusted R-square** evaluate the amount of variance that is explained by the model. 
#' The difference between one and another is that the R-square does not consider the number of variables.
#' If you increase the number of variables in the model, the R-square will tend to increase which can lead to overfitting. 
#' On the other hand, the Adjusted R-square adjust to the number of independent variables. 
#'  
#' 3. Take a look a the **t-value** and the Pr(>|t|). 
#' If the t-value > 1,96 or Pr(>|t|) < 0,05, then the IV is statistically significant to the model.
#'    
#' 4. To analyze the **estimates** of the variables, you should first check the **signal** 
#' and evaluate if the independent variable has a direct or inverse relationship with the dependent variable. 
#' It is only possible to evaluate the **magnitude** of the estimate if all variables are continuous and standardized 
#' or by calculating the elasticities. The elasticities are explained and demonstrated in chapter 4. 
#' 
#' 
#' ##### Residuals
#' Let's see how do the residuals behave by plotting them.  
#' 
#' * **Residuals vs Fitted:** This plot is used to detect non-linearity, heteroscedasticity, and outliers. 
#' * **Normal Q-Q:** The quantile-quantile (Q-Q) plot is used to check if the dependent variable follows a normal distribution.
#' * **Scale-Location:** This plot is used to verify if the residuals are spread equally (homoscedasticity) or not 
#' (heteroscedasticity) through the sample. 
#' * **Residuals vs Leverage:** This plot is used to detect the impact of the outliers in the model. 
#' If the outliers are outside the Cook-distance, this may lead to serious problems in the model. 
#' 
#' Try analyzing the plots and check if the model meets the assumptions. 
par(mfrow=c(2,2))
plot(model)

#' 
#' 
#' ##### Autocorrelation
#' Execute the Durbin-Watson test to evaluate autocorrelation of the residuals
durbinWatsonTest(model)

#' 
#' > **Note:** In the Durbin-Watson test, values of the D-W Statistic vary from 0 to 4. 
#' If the values are from 1.8 to 2.2 this means that there is no autocorrelation in the model. 
#' 
#' ##### Multicollinearity
#' Calculate the VIF and TOL to test for multicollinearity.
#' 
ols_vif_tol(model)

#' 
#' > **Note:** Values of VIF > 5, indicate multicollinearity problems.
#' 
#' Calculate the Condition Index to test for multicollinearity
ols_eigen_cindex(model)

#' 
#' > **Note:** Condition index values > 15 indicate multicollinearity problems, 
#' and values > 30 indicate serious problems of multicollinearity.
#' 
#' To test both simultaneously, you can run the code below:
ols_coll_diag(model)

