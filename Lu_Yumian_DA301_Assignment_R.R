## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact

###############################################################################

# Assignment template

## Scenario
## You are a data analyst working for Turtle Games, a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points (Week 1)
## - how useful are remuneration and spending scores data (Week 2)
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns (Week 3)
## - what is the impact on sales per product (Week 4)
## - the reliability of the data (e.g. normal distribution, Skewness, Kurtosis)
##     (Week 5)
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales (Week 6).

################################################################################

# Week 4 assignment: EDA using R

## The sales department of Turtle games prefers R to Python. As you can perform
## data analysis in R, you will explore and prepare the data set for analysis by
## utilising basic statistics and plots. Note that you will use this data set 
## in future modules as well and it is, therefore, strongly encouraged to first
## clean the data as per provided guidelines and then save a copy of the clean 
## data for future use.

# Instructions
# 1. Load and explore the data.
##  - Remove redundant columns (Ranking, Year, Genre, Publisher) by creating 
##      a subset of the data frame.
##  - Create a summary of the new data frame.
# 2. Create plots to review and determine insights into data set.
##  - Create scatterplots, histograms and boxplots to gain insights into
##      the Sales data.
##  - Note your observations and diagrams that could be used to provide
##      insights to the business.
# 3. Include your insights and observations.

###############################################################################

# 1. Load and explore the data

# Import the tidyverse library.
library(tidyverse)

# Import 'turtle_sales' CSV file as 'tsdata'
tsdata <- read.csv(file.choose(), header=T)

# Print the data frame.
tsdata
View(tsdata)

# Create a new data frame from a subset of the sales data frame.
# Remove unnecessary columns. Change 'Product' data type to Categorical. 
tsdata1 <- select(tsdata, -Ranking, -Year, -Genre, -Publisher)
tsdata2 <- mutate(tsdata1,
                 Product = as.factor(Product))
# View the data frame.
view(tsdata2)
typeof(tsdata2)
str(tsdata2)
# View the descriptive statistics.
summary(tsdata2)

################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
qplot(Product, Global_Sales,data=tsdata2)
qplot(EU_Sales, NA_Sales,data=tsdata2)

## 2b) Histograms
# Create histograms.
qplot(Global_Sales, data=tsdata2)
qplot(NA_Sales, data=tsdata2)
qplot(EU_Sales, data=tsdata2)

## 2c) Boxplots
# Create boxplots.
qplot(Global_Sales, Platform, data=tsdata2, geom='boxplot')
qplot(Global_Sales, Product, data=tsdata2, geom='boxplot')
###############################################################################

# 3. Observations and insights

## Your observations and insights here ......
# They sell on average more games in North America than in Europe.
# North America sales and EU sales data look positively correlated but there are
# few outliners.
# Global sales and Product code shows that smaller product codes have higher 
# sales - is this a coincident?
# The platform that sold more games on average are Wii and SNES but the range is
# very wide.


###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and maniulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data

# View data frame created in Week 4.
view(tsdata2)

# Check output: Determine the min, max, and mean values.
summary(tsdata2)

# View the descriptive statistics.
# Compute the ones that are not included in the Summary above
tsdata3 <- select(tsdata2, c('Global_Sales',
                                   'NA_Sales',
                                   'EU_Sales'))
salesvar <- sapply(tsdata3,var)
salessd <- sapply(tsdata3,sd)

salesvar
salessd

###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sum_sales <- tsdata2 %>% group_by(Product) %>%
  summarise(sum_glbsales=sum(Global_Sales),
            sum_NAsales=sum(NA_Sales),
            sum_EUsales=sum(EU_Sales),
            .groups='drop')

# View the data frame.
sum_sales

# Explore the data frame.
summary(sum_sales)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.
qplot(Product, sum_glbsales,data=sum_sales)
qplot(Product, sum_NAsales,data=sum_sales)
qplot(Product, sum_EUsales,data=sum_sales)

# Create histograms.
qplot(sum_glbsales,data=sum_sales)
qplot(sum_NAsales,data=sum_sales)
qplot(sum_EUsales,data=sum_sales)

# Create boxplots.
qplot(sum_glbsales, Product, data=sum_sales, geom='boxplot')
qplot(sum_NAsales, Product, data=sum_sales, geom='boxplot')
qplot(sum_EUsales, Product, data=sum_sales, geom='boxplot')


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plot for global, NA and EU sales respectively.
qqnorm(tsdata2$Global_Sales)
qqline(tsdata2$Global_Sales)

qqnorm(tsdata2$NA_Sales)
qqline(tsdata2$NA_Sales)

qqnorm(tsdata2$EU_Sales)
qqline(tsdata2$EU_Sales)

## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
install.packages('moments') 
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(tsdata2$Global_Sales)
shapiro.test(tsdata2$NA_Sales)
shapiro.test(tsdata2$EU_Sales)
# All the p-values are significantly smaller than 0.5, which suggest the sales
# data are not normally distributed. This is supported by the plots earlier. 

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(tsdata2$Global_Sales) 
kurtosis(tsdata2$Global_Sales)

skewness(tsdata2$NA_Sales) 
kurtosis(tsdata2$NA_Sales)

skewness(tsdata2$EU_Sales) 
kurtosis(tsdata2$EU_Sales)
# The sales data are all very skewed and have fatter tails than a normal 
# distribution.

## 3d) Determine correlation
# Determine correlation.
round (cor(tsdata3),
       digits=2)
# Global sales and NA_Sales are highly positively correlated. While NA sales
# and EU sales are also positively correlated, the correlation is less strong.


###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.

# After going through the above analysis, I believe the most useful information
# we could get for this part of the analysis, using sum of sales and product id,
# is the importance of NA sales v.s.EU sales per product. 
# In my view, there are other important data to explore e.g. sales per genre

# To plot a stacked barchart:
# First, use melt() to convert data frame from wide to long format, keeping only
# NA and EU sales observations

library(reshape2)
sum_sales2 <- subset(sum_sales, select = -c(sum_glbsales))
long_df <- melt(sum_sales2, id='Product')

# View long data frame
long_df

# Rename columns in long_df and drop global sales 
names(long_df) <- c('Product', 'Region', 'Total_Sales_Value')
long_df

# Plotting share of sales per region per product id using barplot
ggplot(long_df, aes(x=Product, y=Total_Sales_Value, fill=Region)) + 
  geom_bar(stat = "identity") +
  theme_minimal()+
  ggtitle("Total Sales (£m) per Product by Region")


###############################################################################

# 5. Observations and insights
# Your observations and insights here...

# Due to the large number of products, visualising total sales per product using
# boxplots were not very useful.

# Scatterplots of the sum of sales per productd revealed that, similar to what we observed earlier, the smaller 
# the product id the larger the total sales. 
# This could be that the smaller product ids were older products that had been 
# on the market for longer. Therefore we may want to subset the
# data into groups of product ids, or find more information about the number of
# years they had been offered for. 

# All the normality tests suggest that the sales
# data are not normally distributed. This is supported by the plots earlier.
# They are very skewed with fat tails suggesting significant outliners.

# Global sales and NA_Sales are highly positively correlated. While NA sales
# and EU sales are also positively correlated, the correlation is less strong.

###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################

# 1. Load and explor the data
# View data frame created in Week 5.
sum_sales

# Determine a summary of the data frame.
summary(sum_sales)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.
model1 <- lm(sum_sales$sum_glbsales ~ sum_sales$sum_NAsales)
model2 <- lm(sum_sales$sum_glbsales ~ sum_sales$sum_EUsales)
model3 <- lm(sum_sales$sum_NAsales ~ sum_sales$sum_EUsales)

summary(model1)
summary(model2)
summary(model3)
# They all have statistically significant x coefficients - i.e. the sales 
# columns have positive correlations. R-sq of model 3 is the lowest at 
# 0.38, while model 1 and 2 have quite high R-sq at 0.84 and 0.72 respectively. 
# This suggests that EU sales is a poor explanatory variable for NA sales. 
par(mfrow=c(3, 1))
qqnorm(residuals(model1))
qqline(residuals(model1), col='red')

qqnorm(residuals(model2))
qqline(residuals(model2), col='blue')

qqnorm(residuals(model3))
qqline(residuals(model3), col='green')
# The residual plots suggest there are less extreme data on the left-hand tail
# but more extreme data on the right-hand tail, than a normal distribution.

## 2b) Create a plot (simple linear regression)
# Basic visualisation.
# Arrange plot with the par(mfrow) function.
par(mfrow=c(3, 1))

# Compare the plots of the three models.
plot(sum_sales$sum_NAsales,sum_sales$sum_glbsales)
abline(coefficients(model1), col='red')

plot(sum_sales$sum_EUsales,sum_sales$sum_glbsales)
abline(coefficients(model2), col='blue') 

plot(sum_sales$sum_EUsales,sum_sales$sum_NAsales)
abline(coefficients(model3), col='green') 

# Notice there are quite a few outliners in all three plots,which means a few
# products sold significantly better than the rest across geographies. 

###############################################################################

# 3. Create a multiple linear regression model
# Select only numeric columns from the original data frame.
sum_sales3 <- subset(sum_sales, select = -c(Product))

# To find correlations
cor(sum_sales3)

# Install and import the psych package.
install.packages('psych')
library(psych)

# Use the corPlot() function.
corPlot(sum_sales3, cex=2)

# Multiple linear regression model.
mlr = lm(sum_glbsales ~ sum_NAsales+sum_EUsales, data=sum_sales3)
summary(mlr)

# Highly significant P values and high R-sq of 0.97.
###############################################################################

# 4. Predictions based on given values
# Compare with observed values for a number of records.

#Build a dataframe with the given NA and EU values and predicted global sales

NAtest <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EUtest <- c(23.8, 1.56, 0.65, 0.97, 0.52)
NAEUtest <- data.frame(NAtest, EUtest)

colnames(NAEUtest) <- c("sum_NAsales", "sum_EUsales")

NAEUtest$glbsales_predicted<-predict(mlr,
        newdata=NAEUtest)

NAEUtest

# Merge the new df with the original sum_sales df to compare the predcited vs.
# observed global sales values

library(dplyr)

ModelvsObs <- left_join(NAEUtest, sum_sales, by=c('sum_NAsales', 'sum_EUsales'))

ModelvsObs
###############################################################################

# 5. Observations and insights
# Your observations and insights here...
# The multiple regression model appears to have the highest R-sq and 
# statistically significant co-efficient. The predicted global sales values are 
# also close to the observed values. This suggests that the multiple regression
# model does a good job in predicting global sales from NA and EU sales data.


###############################################################################
###############################################################################




