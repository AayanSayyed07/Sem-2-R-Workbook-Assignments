# WORKBOOK ASSIGNMENT - 8 :

sales <- data.frame(
  
  ProductID = 1:10,
  
  Product = c(
    "Laptop","Mobile","Tablet","Shoes","Watch",
    "Bag","Camera","Headphones","Printer","Mouse"
  ),
  
  Category = c(
    "Electronics","Electronics","Electronics",
    "Fashion","Fashion",
    "Accessories","Electronics","Electronics",
    "Electronics","Accessories"
  ),
  
  Quantity = c(
    40,60,35,80,25,
    55,20,75,30,90
  ),
  
  Price = c(
    50000,25000,18000,3000,7000,
    2000,45000,2500,12000,800
  )
)
print(sales)

# Dataset: sales.xlsx (Sheet: sales)
library(readxl)
library(dplyr)

# Load Dataset
write.csv(sales, "sales.csv", row.names = FALSE)
sales_data <- read.csv("sales.csv")



# Q1) A Retail Sales Analysis (Dataset: sales.csv)

# a) Load the sales dataset and display the first 10 records.
head(data, 10)

# b) Calculate total revenue for each product.
sales$Revenue <- sales$Quantity * sales$Price

aggregate(Revenue ~ Product,
          sales,
          sum)

# c) Find the top 5 products with highest revenue.
top_products <- sales %>%
  group_by(Product) %>%
  summarise(Total_Revenue = sum(Revenue)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(5)
top_products

# d) Filter sales where quantity sold is greater than 50.
high_quantity_sales <- sales %>%
  filter(Quantity > 50)
high_quantity_sales

# e) Find average product price by category.
avg_price_by_category <- sales %>%
  group_by(Category) %>%
  summarise(Average_Price = mean(Price))
avg_price_by_category

# Q2) The E-Commerce Customer Analysis (Dataset: customers.csv) 

customers <- data.frame(
  
  CustomerID = 1:10,
  
  Name = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  Gender = c(
    "Male","Male","Female","Female","Male",
    "Female","Male","Female","Female","Male"
  ),
  
  Age = c(
    22,35,28,45,52,
    24,31,60,41,19
  ),
  
  PurchaseAmount = c(
    12000,25000,18000,30000,15000,
    9000,22000,35000,27000,8000
  )
)
print(customers)

library(readxl)
library(dplyr)

# Load Dataset
write.csv(customers, "customers.csv", row.names = FALSE)
customer_data <- read.csv("customers.csv")

# a) Load the customers dataset and display the first 10 records.
head(customer_data, 10)

# b) Calculate average purchase amount.
avg_purchase <- mean(customer_data$PurchaseAmount)
avg_purchase

# c) Find customers above average purchase amount.
above_avg <- customer_data %>%
  filter(PurchaseAmount > avg_purchase)
above_avg

# d) Count number of male and female customers.
gender_count <- table(customer_data$Gender)
gender_count

# e) Create age groups: Youth (<25), Adult (25–50), Senior (>50)
sales_data$AgeGroup <- ifelse(customer_data$Age < 25,
                         "Youth",
                         ifelse(customer_data$Age <= 50,
                                "Adult",
                                "Senior"))
head(data)

# f) Find top 10 highest spending customers.
top_customers <- customer_data %>%
  arrange(desc(PurchaseAmount)) %>%
  head(10)
top_customers



# Q3) The Banking Transaction Analysis (Dataset: transactions.csv)

transactions <- data.frame(
  
  TransactionID = 1:10,
  
  AccountName = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  Type = c(
    "Deposit","Withdrawal","Deposit","Withdrawal","Deposit",
    "Deposit","Withdrawal","Deposit","Withdrawal","Deposit"
  ),
  
  Amount = c(
    15000,5000,22000,12000,18000,
    25000,7000,30000,4000,11000
  )
)
print(transactions)

# Load Dataset
write.csv(transactions, "transactions.csv", row.names = FALSE)
transactions_data <- read.csv("transactions.csv")

# a) Load the transactions dataset and display the first 10 records.
head(transactions_data, 10)

# b) Calculate total deposits.
total_deposits <- transactions_data %>%
  filter(TransactionType == "Deposit") %>%
  summarise(Total_Deposit = sum(Amount))
total_deposits

# c) Calculate total withdrawals.
total_withdrawals <- transactions_data %>%
  filter(TransactionType == "Withdrawal") %>%
  summarise(Total_Withdrawal = sum(Amount))
total_withdrawals

# d) Find accounts with transactions above 10000.
high_transactions <- transactions_data %>%
  filter(Amount > 10000)
high_transactions

# e) Calculate average transaction amount.
avg_transaction <- mean(transactions_data$Amount)
avg_transaction

# f) Count number of transactions by type.
transaction_count <- table(transactions_data$TransactionType)
transaction_count



# Q4) The Healthcare Patient Dataset (patients.csv)

patients <- data.frame(
  
  PatientID = 1:10,
  
  Name = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  Age = c(
    25,61,45,32,70,
    55,28,66,48,73
  ),
  
  BloodPressure = c(
    120,150,145,130,160,
    138,125,170,142,155
  ),
  
  Temperature = c(
    36.5,37.5,38.0,36.8,39.0,
    37.2,36.7,38.5,37.8,39.2
  )
)
print(patients)

# Load Dataset
write.csv(patients, "patients.csv", row.names = FALSE)
patients_data <- read.csv("patients.csv")

# a) Load the patient’s dataset and display the first 10 records.
head(patients_data, 10)

# b) Find patients with high blood pressure (>140).
high_bp <- patients_data %>%
  filter(BloodPressure > 140)
high_bp

# c) Detect patients with fever (>37°C).
fever_patients <- patients_data %>%
  filter(Temperature > 37)
fever_patients

# d) Calculate average patient age.
avg_age <- mean(patients_data$Age)
avg_age

# e) Find maximum and minimum blood pressure.
max_bp <- max(patients_data$BloodPressure)
min_bp <- min(patients_data$BloodPressure)

max_bp
min_bp


# f) Count number of patients above 60 years.
senior_patients <- patients_data %>%
  filter(Age > 60)

nrow(senior_patients)



# Q5) The Student Performance Dataset (students.csv)

students <- data.frame(
  
  StudentID = 1:10,
  
  Name = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  Subject = c(
    "Math","Science","English","Math","Science",
    "English","Math","Science","English","Math"
  ),
  
  Marks = c(
    85,72,91,67,38,
    95,44,81,29,76
  )
)
print(students)

# Load Dataset
write.csv(students, "students.csv", row.names = FALSE)
students_data <- read.csv("students.csv")

# a) Load the students dataset and display the first 10 records.
head(students_data, 10)

# b) Find students who scored above 80.
high_scorers <- students_data %>%
  filter(Marks > 80)
high_scorers

# c) Calculate average marks by subject.
avg_marks <- students_data %>%
  group_by(Subject) %>%
  summarise(Average_Marks = mean(Marks))
avg_marks

# d) Find top scoring student.
top_student <- students_data %>%
  arrange(desc(Marks)) %>%
  head(1)
top_student

# e) Find students who failed (Marks < 40).
failed_students <- students_data %>%
  filter(Marks < 40)
failed_students

# f) Count number of students in each subject.
subject_count <- table(students_data$Subject)
subject_count



# Q6) The Social Media Analytics Dataset (posts.csv)

posts <- data.frame(
  
  PostID = 1:10,
  
  Likes = c(
    120,340,560,80,620,
    150,90,700,450,60
  ),
  
  Comments = c(
    20,45,60,10,80,
    25,15,95,40,8
  ),
  
  Shares = c(
    15,30,55,5,70,
    20,8,90,35,4
  )
)
print(posts)

# Load Dataset
write.csv(posts, "posts.csv", row.names = FALSE)
posts_data <- read.csv("posts.csv")

# a) Load the posts dataset and display the first 10 records.
head(posts_data, 10)

# b) Calculate total engagement per post.
posts_data$Total_Engagement <- posts_data$Likes +
  posts_data$Comments +
  posts_data$Shares

head(posts_data)

# c) Find posts with engagement > 500.
high_engagement <- posts_data %>%
  filter(Total_Engagement > 500)
high_engagement

# d) Find most liked post.
most_liked <- posts_data %>%
  arrange(desc(Likes)) %>%
  head(1)
most_liked

# e) Calculate average engagement.
avg_engagement <- mean(posts_data$Total_Engagement)
avg_engagement

# f) Find posts with low engagement (<100).
low_engagement <- posts_data %>%
  filter(Total_Engagement < 100)
low_engagement

