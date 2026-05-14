# WORKBOOK ASSIGNMENT - 10 :


# Install Required Packages
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")

# Load Libraries
library(readr)
library(dplyr)
library(ggplot2)


# IMPORT DATA
data <- read.csv("C:/Users/SDBI/Downloads/Mall_Customers.csv")


# CHECK DATA

head(data)

str(data)

summary(data)


# REMOVE MISSING VALUES
data <- na.omit(data)


# USE COLUMN POSITIONS

# Column 3 = Age
# Column 4 = Income
# Column 5 = Spending Score

Age <- as.numeric(data[[3]])

Income <- as.numeric(data[[4]])

Spending <- as.numeric(data[[5]])


# HISTOGRAM

hist(Income,
     col = "lightblue",
     main = "Income Distribution")

# BOXPLOT

boxplot(Spending,
        col = "pink",
        main = "Spending Score")

# SCATTER PLOT

plot(Income,
     Spending,
     col = "blue",
     pch = 19,
     main = "Income vs Spending")

# PIE CHART

gender_count <- table(data[[2]])

pie(gender_count,
    col = c("pink","lightblue"),
    main = "Gender Distribution")

# BARPLOT

barplot(gender_count,
        col = "orange",
        main = "Gender Count")

# CORRELATION

cor(Income,
    Spending)

# CORRELATION TEST

cor.test(Income,
         Spending)

# T TEST

t.test(Age,
       mu = 35)

# CHI SQUARE TEST

chisq.test(gender_count)


# FINAL CONCLUSION :

# The dataset was successfully analyzed using R programming.
# Data cleaning, visualization, and statistical analysis were performed.
# Correlation analysis showed a relationship between income and spending score.
# Graphs helped understand customer behavior and spending patterns.
# Overall, meaningful insights were obtained from the customer dataset.