# BIOS625HW3
# Linear Regression Package

The package enables fitting a Linear Regression model for a continous response variable against both continous and/or categorical covariates

## Description

Given an dataframe with N rows and p columns, we fit a Linear regression model on a chosen response variable from the dataframe and any set of n covariates. 

The model is given by:
$Y_i = X_1 + X_2 + ... + X_n + error_i$

The package can then be used to fit a Linear Regression model estimated with the method of least squares and display the results

## Dependencies

* R
* data.table, tibble, stringr, fastDummies, bench

## Functions

*lm_blog* :
*lm_blog* takes an equation and a dataframe as arguments to fit a linear regression model represented by the equation.
The dataframe should contain the response variable and the covariates present in the equation as column names.The covariates are treated as continous or categorical. *lm_blog* automatically converts any non numeric data time into dummy variables using one-got encoding.
lm_blog then fits the model using the method of least squares

*sig_i* :
Checks the significance of p-values of the coefficient estimates from the fitted model and returns an array, where each element is the p value appended with a symbol denoting it's significant

*summary_blog* :
Displays the result summaries of the results of a linear regression model fitted using *lm_blog*

## Executing program
Given a dataframe *df* with N rows and P columns
Let column Y be the response variable and X1 & X2 be covariates. 

The linear regression model is fitted using
lr <- lm_blog(Y ~ X1 + X2,df)

The results table is generated using
summary_blog(lr)

## Authors

Aditya Jalin
