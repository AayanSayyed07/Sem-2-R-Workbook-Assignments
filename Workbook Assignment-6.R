# WORKBOOK ASSIGNMENT - 6 :


# Sample Customer Dataset

customer_data <- data.frame(
  
  CustomerID = 1:20,
  
  CustomerName = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas",
    "Anjali","Kunal","Meera","Sanjay","Riya",
    "Aditya","Komal","Nikhil","Simran","Manoj"
  ),
  
  Gender = c(
    "Male","Male","Female","Female","Male",
    "Female","Male","Female","Female","Male",
    "Female","Male","Female","Male","Female",
    "Male","Female","Male","Female","Male"
  ),
  
  Region = c(
    "North","West","South","East","North",
    "West","South","East","North","West",
    "South","East","North","West","South",
    "East","North","West","South","East"
  ),
  
  Purchase = c(
    "Laptop","Mobile","Tablet","Shoes","Watch",
    "Laptop","Mobile","Tablet","Shoes","Watch",
    "Mobile","Laptop","Watch","Tablet","Shoes",
    "Mobile","Laptop","Watch","Tablet","Shoes"
  ),
  
  Amount = c(
    55000,25000,18000,4000,7000,
    60000,27000,15000,5000,8000,
    22000,58000,9000,17000,4500,
    30000,62000,8500,16000,4800
  )
)

print(customer_data)

# Load Dataset
write.csv(customer_data, "customer_data.csv", row.names = FALSE)

sales_data_csv <- read.csv("customer_data.csv")


# Q1) Sales Performance Analysis

# a) Total sales by region
aggregate(Amount ~ Region, sales_data_csv, sum)

# b) Average sales per gender
aggregate(Amount ~ Gender, sales_data_csv, mean)

# c) Frequency distribution of purchase
table(sales_data_csv$Purchase)

# d) Cross-tab: Region vs Purchase
table(sales_data_csv$Region, sales_data_csv$Purchase)

# e) Chi-square test
chisq.test(table(sales_data_csv$Region, sales_data_csv$Purchase))



# Q2) Customer Segmentation 

# a) Count customers by region
table(sales_data_csv$Region)

# b) Average purchase by region using tapply
tapply(sales_data_csv$Amount, sales_data_csv$Region, mean)

# c) Frequency of gender
table(sales_data_csv$Gender)

# d) Cross-tab: Gender vs Region
table(sales_data_csv$Gender, sales_data_csv$Region)

# e) Chi-square test
chisq.test(table(sales_data_csv$Gender, sales_data_csv$Region))



# Q3) Purchase Pattern Analysis 

# a) Total purchase amount
sum(sales_data_csv$Amount)

# b) Row-wise sum using apply
numeric_data <- data.frame(sales_data_csv$Amount)
apply(numeric_data, 1, sum)

# c) Frequency of purchase
table(sales_data_csv$Purchase)

# d) Cross-tab: Purchase vs Region
table(sales_data_csv$Purchase, sales_data_csv$Region)

# e) Chi-square test
chisq.test(table(sales_data_csv$Purchase, sales_data_csv$Region))



# Q4) High-Value Customer Analysis 

# a) Filter customers with Amount > 5000
high_value <- sales_data_csv[sales_data_csv$Amount > 5000, ]
high_value

# b) Mean purchase of filtered group
mean(high_value$Amount)

# c) Frequency of region (filtered)
table(high_value$Region)

# d) Cross-tab: Gender vs Purchase (filtered)
table(high_value$Gender, high_value$Purchase)

# e) Chi-square test
chisq.test(table(high_value$Gender, high_value$Purchase))



# Q5) Business Decision Analysis

# a) Total revenue
sum(sales_data_csv$Amount)

# b) Average revenue per region
aggregate(Amount ~ Region, sales_data_csv, mean)

# c) Frequency distribution of regions
table(sales_data_csv$Region)

# d) Cross-tab: Region vs Purchase
table(sales_data_csv$Region, sales_data_csv$Purchase)

# e) Statistical inference
chisq.test(table(sales_data_csv$Region, sales_data_csv$Purchase))

