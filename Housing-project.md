---
title: "Housing Project; predicting  rural house prices based on square footage using linear regression"
author: "Oleksandra Uralska"
date: "2024-01-12"
output: 
  html_document:
    toc: true
    toc_float: 
      collapled: false
      smooth_scroll: false 
    theme: sandstone 
    highlight: tango
    keep_md: true
---
* In this project, we'll develop a simple regression model to predict house prices in the rural area based on their square footage.
* **Goal:** The sole objective of this project is to demonstrate competence in coding with R and navigating RStudio.
* **Data sourse:** We're going to use this [synthetic dataset](https://www.kaggle.com/datasets/muhammadbinimran/housing-price-prediction-data), which includes housing data from 1950 to 2021. I selected this dataset based on positive reviews and its well-organized structure.
* **Data format:** Attributes include *price, square footage, year built, number of bedrooms and bathrooms, and type of neighborhood.*
* **Packages used:** For this project, I use the following packages: *janitor, readxl, dplyr, ggplot2, gridExtra, tibble, ggridges, modelr, broom, knitr, kableExtra*. I load them without displaying the code.


## Exploring and preprocessing the data

First, I export the (already downloaded) dataset from Excel.


```r
excel_file <- "C:\\Learning R\\Data for analysis\\housing_price_dataset.xlsx"
sheet_names <- excel_sheets(excel_file)
print(sheet_names)
my_data <- read_excel(excel_file, sheet = "housing_price_dataset"); str(my_data)
```

  The following tables are the contents of the exported data frame and its summary statistics. 
  

```r
kable(head(my_data), caption = paste("The exported housing data in a table format ", "(total number of rows: ", nrow(my_data), ")")) %>%
   kable_styling(full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>The exported housing data in a table format  (total number of rows:  50000 )</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> SquareFeet </th>
   <th style="text-align:right;"> Bedrooms </th>
   <th style="text-align:right;"> Bathrooms </th>
   <th style="text-align:left;"> Neighborhood </th>
   <th style="text-align:right;"> YearBuilt </th>
   <th style="text-align:right;"> Price </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 2126 </td>
   <td style="text-align:right;"> 4 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Rural </td>
   <td style="text-align:right;"> 1969 </td>
   <td style="text-align:right;"> 215355.3 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2459 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Rural </td>
   <td style="text-align:right;"> 1980 </td>
   <td style="text-align:right;"> 195014.2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1860 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Suburb </td>
   <td style="text-align:right;"> 1970 </td>
   <td style="text-align:right;"> 306891.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2294 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> Urban </td>
   <td style="text-align:right;"> 1996 </td>
   <td style="text-align:right;"> 206786.8 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2130 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:left;"> Suburb </td>
   <td style="text-align:right;"> 2001 </td>
   <td style="text-align:right;"> 272436.2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 2095 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> Suburb </td>
   <td style="text-align:right;"> 2020 </td>
   <td style="text-align:right;"> 198208.8 </td>
  </tr>
</tbody>
</table>

```r
summary(my_data)
```

```
##    SquareFeet      Bedrooms       Bathrooms     Neighborhood      
##  Min.   :1000   Min.   :2.000   Min.   :1.000   Length:50000      
##  1st Qu.:1513   1st Qu.:3.000   1st Qu.:1.000   Class :character  
##  Median :2007   Median :3.000   Median :2.000   Mode  :character  
##  Mean   :2006   Mean   :3.499   Mean   :1.995                     
##  3rd Qu.:2506   3rd Qu.:4.000   3rd Qu.:3.000                     
##  Max.   :2999   Max.   :5.000   Max.   :3.000                     
##    YearBuilt        Price       
##  Min.   :1950   Min.   :-36588  
##  1st Qu.:1967   1st Qu.:169956  
##  Median :1985   Median :225052  
##  Mean   :1985   Mean   :224827  
##  3rd Qu.:2003   3rd Qu.:279374  
##  Max.   :2021   Max.   :492195
```

  In order to prepare the data for the upcoming analysis, I address several issues that I've noticed while analyzing two tables. Using the *clear_names()* function from the 'janitor' package, I reformatted column names for convenience. I converted the rows of the 'neighbourhood' column into factor variables and removed the column(s) containing negative prices, which likely resulted from errors during data collection. Additionally, I filtered out rows with NA values. 
  
  I prefer to always keep the original dataset as a separate object. This ensures that, in case of errors or mistakes during data manipulations, the original data can be easily reused without the need to retrieve it from an external source. In the code below, a new object named 'data' is created, containing the original dataset with the highlighted issues addressed.
  

```r
data <- clean_names(my_data)
data$neighborhood <- as.factor(data$neighborhood)
data <- data %>% 
  filter(data$price > 0)
data <- na.omit(data)
```

## Comparing houses in rural, suburban, and urban areas 

  For this project, we're interested in the rows that correspond to "Rural" factor. Before separating those rows, however, let's study the other two factors in a little more detail. 


```r
ggplot(data, aes(neighborhood)) + geom_bar() + ggtitle("The number of houses in each neighbourhood")
```

![](Housing-project_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
  The histogram reveals an approximately equal distribution of datapoints across each factor in our dataset.
  
  Our prime interest is the relationship between the house price and its square footage. Let's see how this relationship varies depending on the location of the house.
  

```r
ggplot(data, aes(x = square_feet, y = price, 
                 color = neighborhood, shape = neighborhood)) +
  geom_point(alpha = 0.3) 
```

![](Housing-project_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
  
  This plot illustrates the multivariate relationship among neighborhood type, house square footage, and price. Its purpose is to answer questions such as whether the houses located in rural areas have higher square footage and lower prices then those in urban areas, etc. The red circles represent the houses located in the rural area, and their respective price and square footage parameters are reflected on the axes. Similarly, the green triangles represent houses in suburban area, and blue squares - those in urban area. 
  
  As we can see, there's no discernible distinction among the three factors, which leads me to a thought that our synthetic dataset may not accurately capture the characteristics of the target population. This lack of distinction could also suggest that the results of our linear regression might be quite similar for all three factors, although making such assumptions with a large dataset entails risks.
  
  In any case, we're now going to focus on the houses located in the rural areas. 
  
## Predicting  rural house prices based on square footage using linear regression

We will conduct a simple linear regression analysis with the house price being a dependent variable, and the square footage - an independent variable. The goal is to determine the intercept, the coefficient (aka slope term), and, of course, to see if our model of predicting the house prices is valid with the help of our to-be-defined test set.
    
We're going to use the following equation:
    
<font size = '3'>"Y = β~0~ + β~1~X + ϵ"</font>

**Where:** 
  
* Y = house price
  
* X = house square footage
  
* β~0~ = intercept
  
* β~1~ = coefficient representing the linear relationship
  
* ϵ = a random error term 

   
### Preparing the data

  To begin with, we'll create a new data table by excluding rows where the factor is not "Rural". In addition, I'll remove the column containing the factor name since it now contains the same character in every row.
  

```r
rural1 <- data %>%
  filter(neighborhood == "Rural") %>%
  mutate(neighborhood = factor(neighborhood))
rural1 <- rural1 %>%
  select(-neighborhood)
```

  Exploratory analysis typically involves the use of a training set to discover potential relationships, while a test set is employed to evaluate the effectiveness of the discovered relationships. In the next code chunk, I set a seed to 103 for reproducibility of my analysis, and use the conventional 60/40 split, where I train my model on 60% of my data and test it on the remaining 40%.
  

```r
set.seed(103)
sample <- sample(c(TRUE, FALSE), nrow(rural1), replace = T, prob = c(0.6,0.4))
train_data <- rural1[sample, ]
test_data <- rural1[!sample, ]
```

  
  With the help of lm() function, I produce the best-fit linear relationship for the data which minimizes the least-squares criterion.


```r
mod <- lm(price ~ square_feet, data = train_data)
tidy_sum <- tidy(mod)
round_tidy_sum <- tidy_sum %>%
  mutate_if(is.numeric, round, digits = 2)
print(round_tidy_sum)
```

```
## # A tibble: 2 × 5
##   term        estimate std.error statistic p.value
##   <chr>          <dbl>     <dbl>     <dbl>   <dbl>
## 1 (Intercept)  26124.    1818.        14.4       0
## 2 square_feet     99.0      0.87     113.        0
```
  
  The results suggest that our p-value is very close to zero, which means that the values of the intercept and the coefficient are statistically significant. The value of the intercept estimate is not practically meaningful in the context of out data (there’s no house with 0 square feet). The coefficient for square feet, which equals 99.03, is more insightful. It tells us that, on average, each additional square foot leads to an increase of \$99.03 in price. 
  
  With these insights, our model takes the form of:
  
<font size = '3'>"Y = 26124.1 + 99.03X + ϵ"</font>
  
  Next, I access the 95% confidence interval by using confint() function. 
  

```r
round(confint(mod), digits = 2)
```

```
##                2.5 %   97.5 %
## (Intercept) 22561.24 29686.95
## square_feet    97.32   100.75
```
  
  It suggests that, with a 95% confidence, an increase of 1 square feet in the house area raises its price by \$97 – \$101. 
  
### Asessing the model visually

  It is important not only to comprehend quantitative metrics related to our model accuracy, but also to access our model visually for more useful insights.


```r
ggplot(rural1, aes(x = square_feet, y = price, )) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm') +
  geom_smooth(se = FALSE, color = 'red') +
  ggtitle("Vizualization of the linear regression model")
```

![](Housing-project_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
  
  On this plot, the linearity of our model (blue line) and a non-linear LOESS model are compared, which helps ensure that our modeling approach aligns with the structure of the training data. Besides, it helps us to decide whether the linear regression is appropriate for our dataset. Clearly, the red and blue lines align very closely, so the linear model adequately captures the underlying relationship between our variables.
  
  Next, we create the residuals vs. fitted values plot, and our primary goal is to find out if the model is linear and if the error terms have constant variance. 
  

```r
mod_res <- augment(mod, train_data)
ggplot(mod_res, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")
```

![](Housing-project_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
  
  
  Judging by the blue line produced, we can conclude that the assumption of linearity is very accurate. Also, looking at the general shape of our residuals, we don’t see any gaps, a funnel, etc., which suggests that the spread of the residuals is consistent across the range of predicted values, in other words, that the assumption of homoscedasticity is accurate.

  Now, let’s check if we have any outliers and if the residuals are spread equally along ranges of predictors. The first graph is the same as “Residual vs. fitted values” graph, except for the y-axis. 


```r
res1 <- ggplot(mod_res, aes(.fitted, .std.resid)) +
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Standartized residuals vs \nfitted")
res2 <- ggplot(mod_res, aes(.fitted, sqrt(.std.resid))) +
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Scale-location plot")
grid.arrange(res1, res2, nrow = 1)
```

![](Housing-project_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


  The first graph shows us deviations of residuals by 1-4 standard deviations (standard deviations are indicated on the y-axis). If we consider a residual that deviates from the predicted value by over 3 standard deviations an outlier, then I can count around *12 outliers*. The plot on the right is needed to to double-check the assumption of homoscedasticity, and better highlight any patterns in the variance. We see a horizontal line and, once again, no clear patter of the residuals, which confirms our claim that the error terms have constant variance.

  As I mentioned, we seem to have some outliers. Let’s determine how much influence these outliers have on our regression line by using Cook's Distance plot and residuals vs. leverage plot. These plots help us find influential data points which, when removed, might significantly alter our regression line.


```r
par(mfrow=c(1, 2))
plot(mod, which = 4, id.n = 12)
plot(mod, which = 5, id.n = 12, alpha = 0.2)
```

![](Housing-project_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

  We definitely see a few outstanding points which are both to the upper right of our Residuals vs. Leverage plot. However, the extent of their influence is uncertain. To assess this, I generate a new tibble, excluding points with high Cook’s distances, and construct a new residuals plot with the outliers removed. This plot will be displayed alongside our original residuals plot for comparison.


```r
trsh <- 4/ length(cooks.distance(mod))
filtr_data <- train_data %>%
  filter(cooks.distance(mod) <= trsh)
resid_plot <- ggplot(mod_res, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted, with influential points")
mod_cook <- lm(price ~ square_feet, data = filtr_data) 
mod_res_cook <- augment(mod_cook, filtr_data)
cook_res_plot <- ggplot(mod_res_cook, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point(alpha = 0.2) +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted, influential points removed")
grid.arrange(resid_plot, cook_res_plot, nrow = 1)
```

![](Housing-project_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

  We see that the residuals plot without outliers (the second graph) narrows at the beginning and end but widens in the center, in other words, it has a "funnel" pattern. It is an undesirable situation because our new data violates the assumption of homoscedasticity. Apparently, the outliers played a significant role in stabilizing the variance of our model. Since the assumption of homoscedasticity is crucial in linear regression analysis, I will stick to the original data that includes outliers.

  Now, let’s access the normality of the residuals by building a Q-Q plot. If the points are falling directly on the diagonal line, we can say that the residuals are normally distributed.
  

```r
qq_plot <- qqnorm(mod_res$.resid)
qq_plot <- qqline(mod_res$.resid)
```

![](Housing-project_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
  
  The normal Q-Q plot looks great, so our data follows normal distribution.

### Accessing model accuracy

  Next, let’s analyze how accurate the model is.  To begin with, we can access RSE (an estimate of a standard deviation of a random error term) using *sigma()* function. 
  

```r
sigma(mod)
```

```
## [1] 50072.13
```
    RSE of our model is 50159.9, which means that the actual house price might deviate from our predicted price by up to \$50,159.9. To find out if this is a significant number, we calculate the percentage error: RSE/mean price. 
  

```r
sigma(mod)/mean(rural1$price)
```

```
## [1] 0.2233276
```
  
  It equals to 22.3% which is a fair result. For more certainty, further analysis is needed. Another way to measure the fitness of our model is to find the R^2^ statistic. In our case, R^2^ statistic is the proportion of variation in price that can be explained by the area of the house, in square feet. I calculate it using rsquare() function.


```r
rsquare(mod, data = train_data)
```

```
## [1] 0.5639178
```

  It equals to 56%, which means that 56% of variability in price is explained by the variation in square feet of a house. It is a rather moderate result, so I will further test the accuracy of the model by accessing f-statistic value and the p-value (both are found on the bottom of the model summary table).
  

```r
summary(mod)
```

```
## 
## Call:
## lm(formula = price ~ square_feet, data = train_data)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -179631  -34518     -63   33801  227403 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 2.612e+04  1.818e+03   14.37   <2e-16 ***
## square_feet 9.903e+01  8.746e-01  113.23   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 50070 on 9914 degrees of freedom
## Multiple R-squared:  0.5639,	Adjusted R-squared:  0.5639 
## F-statistic: 1.282e+04 on 1 and 9914 DF,  p-value: < 2.2e-16
```
  
  F-statistic value is 1.266 * 10^0.4^, while our p-value is 2.210 * 10^-16^. The results of our tests suggest that the accuracy of the produced model is moderate. 
  
### Making predictions 

  Finally, let’s make predictions on the house price based on its square feet. To begin with, I’ll access how the model performs on making predictions against our test data set (40 % of the initial data set). 
  

```r
test <- test_data %>% 
  select(-bedrooms, -bathrooms, -year_built) %>%
  add_predictions(mod)
kable(head(test), caption = 
        paste("Actual vs. Predicted price data in the test frame", nrow(test), ")")) %>%
   kable_styling(full_width = FALSE)
```

<table class="table" style="width: auto !important; margin-left: auto; margin-right: auto;">
<caption>Actual vs. Predicted price data in the test frame 6752 )</caption>
 <thead>
  <tr>
   <th style="text-align:right;"> square_feet </th>
   <th style="text-align:right;"> price </th>
   <th style="text-align:right;"> pred </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 1459 </td>
   <td style="text-align:right;"> 77493.93 </td>
   <td style="text-align:right;"> 170612.0 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1021 </td>
   <td style="text-align:right;"> 110408.67 </td>
   <td style="text-align:right;"> 127235.9 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1747 </td>
   <td style="text-align:right;"> 228683.23 </td>
   <td style="text-align:right;"> 199133.2 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1600 </td>
   <td style="text-align:right;"> 256725.98 </td>
   <td style="text-align:right;"> 184575.5 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1564 </td>
   <td style="text-align:right;"> 163053.39 </td>
   <td style="text-align:right;"> 181010.4 </td>
  </tr>
  <tr>
   <td style="text-align:right;"> 1897 </td>
   <td style="text-align:right;"> 304004.69 </td>
   <td style="text-align:right;"> 213988.1 </td>
  </tr>
</tbody>
</table>

  The second column provides the actual prices already present in our dataset. The third column shows the price values predicted by our model. If we had no price data available in our test data frame, this is what prices the model would assign to the corresponding square footage.

  To find out if the predictions are accurate, I compute the mean squared prediction error for my test and training data.
  

```r
mse_test <- test_data %>% 
  add_predictions(mod) %>%
  summarise(MSE = mean((price - pred)^2))
mse_train <- train_data %>% 
  add_predictions(mod) %>%
  summarise(MSE = mean((price - pred)^2))
cat('MSE for test data: ', mse_test$MSE, 
    '\nMSE for train data: ', mse_train$MSE, '.\n',
    all.equal(mse_test, mse_train))
```

```
## MSE for test data:  2529908469 
## MSE for train data:  2506712903 .
##  Component "MSE": Mean relative difference: 0.00916854
```
  
  As we can see, the difference is not that significant, which indicates that the model is performing similarly on both the training and test datasets. It is also a sign that the model is not biased towards either the test nor training data, and that is can be described as stable, in a sense that it is not very sensitive to variations in input data

