# WORKBOOK ASSIGNMENT - 7 :

# Sample Dataset for Workbook Assignment - 7

analysis_data <- data.frame(
  
  CustomerID = 1:20,
  
  Age = c(
    22,25,28,30,35,
    40,45,27,32,29,
    31,26,38,41,24,
    36,33,42,23,37
  ),
  
  Income = c(
    35000,42000,50000,62000,58000,
    75000,82000,47000,54000,49000,
    61000,45000,70000,85000,39000,
    67000,56000,90000,36000,72000
  ),
  
  SpendingScore = c(
    65,70,75,60,55,
    50,45,72,68,74,
    58,77,52,40,80,
    57,63,38,82,48
  ),
  
  Region = c(
    "North","South","East","West","North",
    "South","East","West","North","South",
    "East","West","North","South","East",
    "West","North","South","East","West"
  ),
  
  Purchase = c(
    "Yes","No","Yes","Yes","No",
    "Yes","No","Yes","No","Yes",
    "Yes","No","Yes","No","Yes",
    "No","Yes","Yes","No","Yes"
  )
)

print(analysis_data)

install.packages("writexl")
library(writexl)
library(readxl)

write_xlsx(analysis_data, "analysis_data.xlsx")

data <- read_excel("analysis_data.xlsx")



# Q1) Retail Income vs Spending Strategy

# 1. Calculate Pearson correlation between Income and SpendingScore. Interpret the result.  
correlation <- cor(data$Income,
                   data$SpendingScore,
                   method = "pearson",
                   use = "complete.obs")
correlation


#  2. Test significance of the correlation. 
cor_test <- cor.test(data$Income,
                      data$SpendingScore,
                      method = "pearson")
cor_test

# 3. Generate a normal distribution using Income and compare with actual data.
hist(data$Income,
     probability = TRUE)

x <- seq(min(data$Income),
         max(data$Income),
         length = 100)

y <- dnorm(x,
           mean = mean(data$Income),
           sd = sd(data$Income))

lines(x, y, col = "red")

# 4. Perform a one-sample t-test to check if average income is ₹50,000. 
t.test(data$Income,
       mu = 50000)

# 5. Test whether purchase success rate differs from 50% using binomial test. 
success <- sum(data$Purchase == "Yes")
total <- nrow(data)

binom.test(success,
           total,
           p = 0.5)



# Q2) Customer Segmentation Strategy

# 1. Compute correlation matrix of Age, Income, SpendingScore.
cor_matrix <- cor(data[, c("Age",
                           "Income",
                           "SpendingScore")],
                  use = "complete.obs")
cor_matrix

# 2. Identify strongest relationship.
cor(data$Income,
    data$SpendingScore)

# 3. Generate normal distribution for SpendingScore.
hist(data$SpendingScore,
     probability = TRUE)

x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean = mean(data$SpendingScore),
           sd = sd(data$SpendingScore))

lines(x, y, col = "red")

# 4. Perform t-test for SpendingScore = 60.
t.test(data$SpendingScore,
       mu = 60)

# 5. Use Kruskal-Wallis test to compare SpendingScore across regions.
kruskal.test(SpendingScore ~ Region,
             data = data)



# Q3) Financial Risk Assessment

#1. Analyze correlation between Income and SpendingScore.
cor(data$Income,
    data$SpendingScore)

#2. Test statistical significance.
cor.test(data$Income,
         data$SpendingScore)

#3. Generate Poisson distribution for number of risky customers.
risk <- rpois(100,
              lambda = 5)

hist(risk)

#4. Perform t-test on Income = ₹40,000.
t.test(data$Income,
       mu = 40000)

#5. Perform binomial test assuming 60% safe customers.
safe <- sum(data$Purchase == "Yes")

total <- nrow(data)

binom.test(safe,
           total,
           p = 0.60)



#Q4) Product Demand Forecasting

# 1. Compute Spearman correlation between Age and Income.
cor(data$Age,
    data$Income,
    method = "spearman")

# 2. Explain monotonic relationship if any.
cor.test(data$Age,
         data$Income,
         method = "spearman")

# 3. Generate binomial distribution for purchase events.
purchase <- rbinom(100,
                   size = 10,
                   prob = 0.5)

hist(purchase)

# 4. Perform t-test on Age = 35.
t.test(data$Age,
       mu = 35)

# 5. Use chi-square goodness-of-fit for Region distribution.
region_table <- table(data$Region)

chisq.test(region_table)



# Q5) Customer Satisfaction Modeling

# 1. Correlate Income and SpendingScore using Pearson.
cor(data$Income,
    data$SpendingScore)

# 2. Validate using Spearman correlation.
cor(data$Income,
    data$SpendingScore,
    method = "spearman")

# 3. Generate normal distribution for SpendingScore.
hist(data$SpendingScore,
     probability = TRUE)

x <- seq(min(data$SpendingScore),
         max(data$SpendingScore),
         length = 100)

y <- dnorm(x,
           mean = mean(data$SpendingScore),
           sd = sd(data$SpendingScore))

lines(x, y, col = "blue")

# 4. Perform t-test for mean satisfaction = 55.
t.test(data$SpendingScore,
       mu = 55)

# 5. Apply Wilcoxon test between Age and SpendingScore.
wilcox.test(data$Age,
            data$SpendingScore)



# Q6) Sales Probability Analysis

# 1. Convert Purchase into binary and correlate with Income.
data$PurchaseBinary <- ifelse(data$Purchase == "Yes",
                              1,
                              0)

cor(data$PurchaseBinary,
    data$Income)

# 2. Generate binomial distribution for purchase trials.
trials <- rbinom(100,
                 size = 20,
                 prob = 0.6)

hist(trials)

# 3. Perform binomial test for observed success rate.
success <- sum(data$PurchaseBinary)

total <- nrow(data)

binom.test(success,
           total,
           p = 0.5)

# 4. Perform t-test on Income = ₹55,000.
t.test(data$Income,
       mu = 55000)

# 5. Apply chi-square test for purchase distribution.
purchase_table <- table(data$Purchase)

chisq.test(purchase_table)



# Q7) Strategic Business Decision Model

# 1. Compute correlation matrix among all numeric variables.
numeric_data <- data[, sapply(data, is.numeric)]

cor(numeric_data)

# 2. Identify strongest predictor of SpendingScore.
cor(data$Income,
    data$SpendingScore)

# 3. Generate Poisson distribution for simulated demand.
demand <- rpois(100,
                lambda = 8)

hist(demand)

# 4. Perform t-test on Income benchmark ₹60,000.
t.test(data$Income,
       mu = 60000)

# 5. Perform Kruskal-Wallis test for Age across regions. 
kruskal.test(Age ~ Region,
             data = data)

