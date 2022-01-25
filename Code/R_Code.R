#Author: Suryateja Chalapati

#Importing Libraries
library(readxl)
library(data.table)
library(stargazer)

#Setting the Working Directory and Importing the Dataset
setwd("C:/Users/surya/Downloads")

rc_stores <- read_excel("RetailChain.xlsx", sheet = "stores")
names(rc_stores) <- tolower(colnames(rc_stores))
rc_products <- read_excel("RetailChain.xlsx", sheet = "products")
names(rc_products) <- tolower(colnames(rc_products))
rc_transactions <- read_excel("RetailChain.xlsx", sheet = "transactions")
names(rc_transactions) <- tolower(colnames(rc_transactions))
#attach(rc_stores)
#attach(rc_products)
#attach(rc_transactions)

#NA Values Column-Wise & Pre-Processing
sapply(rc_transactions, function(x) sum(is.na(x)))

colSums(is.na(rc_transactions))
rc_transactions <- rc_transactions[complete.cases(rc_transactions), ]
str(rc_transactions)

#Remove Oral Hygiene Products
ohp_temp <- rc_products[rc_products$category == "ORAL HYGIENE PRODUCTS", ]
rc_transactions <- rc_transactions[!(rc_transactions$upc %in% ohp_temp$upc), ]

#Extracting Year, Month & Week Number
rc_transactions$year <- format(rc_transactions$week_end_date, "%Y")
rc_transactions$month <- format(rc_transactions$week_end_date, "%B")
rc_transactions$month <- as.factor(rc_transactions$month)
rc_transactions$month <- relevel(rc_transactions$month, "January")
#rc_transactions$weeknum <- strftime(rc_transactions$week_end_date, format = "%V")

library(lubridate)
rc_transactions$weeknum <- (interval(min(rc_transactions$week_end_date), rc_transactions$week_end_date) %/% weeks(1)) + 1
rc_transactions$spend <- log(rc_transactions$spend + min(rc_transactions$spend) + 1)

#Factorizing Variables
rc_transactions$store_num <- as.factor(rc_transactions$store_num)
rc_transactions$upc <- as.factor(rc_transactions$upc)
rc_transactions$feature <- as.factor(rc_transactions$feature)
rc_transactions$display <- as.factor(rc_transactions$display)
rc_transactions$tpr_only <- as.factor(rc_transactions$tpr_only)

#Data Visualizations
hist(rc_transactions$spend)
hist(log(rc_transactions$spend))
hist(rc_transactions$units)
hist(log(rc_transactions$units))
hist(rc_transactions$hhs)
hist(log(rc_transactions$hhs))

hist(rc_transactions$visits)
hist(log(rc_transactions$visits))

hist(rc_transactions$price)
hist(log(rc_transactions$price))

#Correlations
rct_corr <- rc_transactions[, c(4:9)]

#library(PerformanceAnalytics)
#chart.Correlation(rct_corr)

library(corrplot)
rct_corplot <- cor(rct_corr)
corrplot(rct_corplot, method = "number", number.cex= 0.7)

#Q1
#Multi-Level Analysis
library(lme4)
library(arm)
any(is.na(log(rc_transactions$spend)))
any(is.nan(log(rc_transactions$spend)))
any(is.infinite(log(rc_transactions$spend)))

spend_model <- lmer(spend ~ log(visits) + feature + display + tpr_only + price + 
                      year + weeknum + (1 | store_num), data = rc_transactions)
units_model <- lmer(log(units) ~ log(visits) + feature + display + tpr_only + price + 
                      year + weeknum + (1 | store_num), data = rc_transactions)
hhs_model <- lmer(log(hhs) ~ log(visits) + feature + display + tpr_only + price + 
                    year + weeknum + (1 | store_num), data = rc_transactions)

#Stargazer
stargazer(spend_model, units_model, hhs_model, type="text", single.row=TRUE)

#Q2
#Merging Data Frames
step <- merge(rc_transactions, rc_products, by = "upc")
merged_rc <- merge(step, rc_stores, by.x = "store_num", by.y = "store_id")
str(merged_rc)
merged_rc <- merged_rc[complete.cases(merged_rc), ]

merged_rc$category <- as.factor(merged_rc$category)
merged_rc$city <- as.factor(merged_rc$city)
merged_rc$state <- as.factor(merged_rc$state)
merged_rc$segment <- as.factor(merged_rc$segment)

#Multi-Level Analysis Based on Category & Segment
spend_model2 <- lmer(spend ~ log(visits) + feature + display + tpr_only + price + 
                      year + weeknum + segment + category + (1 | store_num), data = merged_rc)
units_model2 <- lmer(log(units) ~ log(visits) + feature + display + tpr_only + price + 
                      year + weeknum + segment + category + (1 | store_num), data = merged_rc)
hhs_model2 <- lmer(log(hhs) ~ log(visits) + feature + display + tpr_only + price + 
                    year + weeknum + segment + category + (1 | store_num), data = merged_rc)

#Stargazer
stargazer(spend_model2, units_model2, hhs_model2, type="text", single.row=TRUE)

#Q3
#Five Most Price Elastic & Five Least Price Elastic
elasticity_model <- glm(units ~ price + upc, data = rc_transactions, family = poisson(link = log))  
summary(elasticity_model)

coeff <- data.frame(elasticity_model$coefficients)
coeff$upc <- row.names(coeff)
row.names(coeff) <- NULL
colnames(coeff) <- c("standing", "UPC")
coeff$UPC <- as.character(coeff$UPC)
coeff$UPC <- gsub("^.{0,3}", "", coeff$UPC)
coeff <- coeff[3:43,]
coeff$UPC <- as.numeric(coeff$UPC)
coeff <- merge(coeff, rc_products, by.x = "UPC", by.y = "upc")
coeff <- coeff[order(coeff$standing, decreasing = TRUE), ]
row.names(coeff) <- NULL
head(coeff,5)
tail(coeff,5)

#Assumptions
#Multicollinearity
library(car)
vif(spend_model2)
vif(units_model2)
vif(hhs_model2)

#Autocorrelation (Independence)
#Durbin-Watson Test
library(DHARMa)
testTemporalAutocorrelation(spend_model2, time = NULL)
testTemporalAutocorrelation(units_model2, time = NULL)
testTemporalAutocorrelation(hhs_model2, time = NULL)
