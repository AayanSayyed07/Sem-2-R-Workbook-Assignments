# WORKBOOK ASSIGNMENT - 3 :


set.seed(123)
sales_data <- data.frame(
  OrderID = 1:200,
  CustomerName =
    sample(c("Rahul","Amit","Priya","Neha","Arjun","Sneha","Rohit","Kiran"),200,replace=TRUE),
  City = sample(c("Pune","Mumbai","Delhi","Bangalore","Hyderabad"),200,replace=TRUE),
  Product = sample(c("Laptop","Mobile","Tablet","Shoes","Watch"),200,replace=TRUE),
  Category = sample(c("Electronics","Fashion"),200,replace=TRUE),
  Quantity = sample(1:10,200,replace=TRUE),
  Price = sample(seq(500,50000,500),200,replace=TRUE),
  Discount = sample(c(0,5,10,15,20),200,replace=TRUE),
  PaymentMethod = sample(c("Cash","Card","UPI"),200,replace=TRUE)
)



# Q1) Problems on Recoding Variables, Sorting and New Variable Creation

# a) Create OrderSize (Small, Medium, Large)
sales_data$Revenue <- sales_data$Quantity * sales_data$Price
sales_data$DiscountAmount <- sales_data$Revenue * sales_data$Discount/100
sales_data$FinalAmount <- sales_data$Revenue - sales_data$DiscountAmount 

# b) Sort by Quantity descending
sorted_quantity <- sales_data[order(-sales_data$Quantity), ]
print(sorted_quantity)

# c) Sort by City and Revenue
sorted_cit_revenue <- sales_data[order(sales_data$City, sales_data$Revenue), ]
print(sorted_cit_revenue)  

# d) Create Tax column
sales_data$Tax <- sales_data$FinalAmount * 0.18
print(sales_data$Tax)

# e) Create TotalBill column 
sales_data$TotalBill <- sales_data$FinalAmount + sales_data$Tax
print(sales_data$TotalBill)

head(sales_data)



# Q2) Problems on dplyr select() and filter()

# a) Select CustomerName and Product
select(sales_data, CustomerName, Product)

# b) Select numeric columns
select(sales_data, where(is.numeric))

# c) Remove Discount column
select(sales_data, -Discount)

# d) Filter Pune customers
filter(sales_data, City == "Pune")

# e) Filter multiple conditions (Price > 20000 and Quantity > 5)
filter(sales_data, Price > 20000 & Quantity > 5)



#Q3) Problems on Using mutate() and arrange()

install.packages("dplyr")
library(dplyr)

# a) Create Tax variable 
sales_data <- sales_data %>%
  mutate(Tax = FinalAmount * 0.18)
print(sales_data$Tax)

# b) Create Profit variable 
sales_data <- sales_data %>%
  mutate(Profit = FinalAmount * 0.20)
print(sales_data$Profit)

# c) Create OrderCategory
sales_data <- sales_data %>%
  mutate(OrderCategory = ifelse(Revenue > 30000, "High", 
                              ifelse(Revenue > 10000, "Medium", "Low")))
print(sales_data$OrderCategory)

# d) Create DiscountCategory
sales_data <- sales_data %>%
  mutate(DiscountCategory = ifelse(Discount > 10, "High Discount", "Low Discount"))
print(sales_data$DiscountCategory)

# e) Create PaymentType variable
sales_data <- sales_data %>%
  mutate(PaymentType = PaymentMethod)
print(sales_data$PaymentMethod)

# f) Sort by Revenue descending by using arrange() 
arrange(sales_data, desc(Revenue))



# Q4) Problems on Using group_by() and summarise()

# a) Total revenue by city
total_revenue_city <- sales_data%>%
  group_by(City)%>%
  summarise(TotalRevenue = sum(Revenue))
print(total_revenue_city)

# b) Average price by product
average_price_product <- sales_data%>%
  group_by(Product)%>%
  summarise(AveragePrice = mean(Price))
print(average_price_product)

# c) Total quantity sold by product
total_quantity_product <- sales_data%>%
  group_by(Product)%>%
  summarise(TotalQuantity = sum(Quantity))
print(total_quantity_product)

# d) Maximum revenue by city
max_revenue_city <- sales_data%>%
  group_by(City)%>%
  summarise(MaxRevenue = max(Revenue))
print(max_revenue_city)

# e) Minimum revenue by product
min_revenue_product <- sales_data%>%
  group_by(Product)%>%
  summarise(MinRevenue = min(Revenue))
print(min_revenue_product)



# Q5) Problems on Customer Analysis


# a) Count orders per customer
orders_per_customer <- sales_data%>%
  group_by(CustomerName)%>%
  summarise(OrderCount = n())
print(orders_per_customer)

# b) Total revenue per customer
revenue_per_customer <- sales_data%>%
  group_by(CustomerName)%>%
  summarise(TotalRevenue = sum(Revenue))
print(revenue_per_customer)

# c) Average order value
average_order_value <- sales_data%>%
  group_by(CustomerName)%>%
  summarise(TotalRevenue = mean(Revenue))
print(average_order_value)

# d) Top customer
top_customer <- sales_data%>%
  group_by(CustomerName)%>%
  summarise(TotalRevenue = sum(Revenue))%>%
  arrange(desc(TotalRevenue))%>%
  head(1)
print(top_customer)

# e) Sort customers by revenue
sorted_customers <- sales_data%>%
  group_by(CustomerName)%>%
  summarise(TotalRevenue = sum(Revenue))%>%
  arrange(desc(TotalRevenue))
print(sorted_customers)

