# WORKBOOK ASSIGNMENT - 9 :

oop_customer_data <- data.frame(
  
  CustomerID = 1:10,
  
  Name = c(
    "Rahul","Amit","Priya","Neha","Arjun",
    "Sneha","Rohit","Kiran","Pooja","Vikas"
  ),
  
  Age = c(
    22,35,28,45,31,
    26,40,38,29,50
  ),
  
  Income = c(
    35000,50000,42000,65000,48000,
    39000,72000,58000,45000,80000
  ),
  
  Membership = c(
    "Silver","Gold","Silver","Platinum","Gold",
    "Silver","Platinum","Gold","Silver","Platinum"
  ),
  
  SpendingScore = c(
    60,75,68,82,70,
    55,90,78,66,95
  )
)

print(oop_customer_data)



# Q1) Customer Object Modeling (S3 System)

# 1. Create an S3 class "Customer" using dataset rows.
customer <- list(
  Name = "Rahul",
  Age = 30,
  Income = 55000,
  Membership = "Gold"
)

class(customer) <- "Customer"
customer

# 2. Assign attributes like Age, Income, Membership.
customer$Age
customer$Income
customer$Membership

# 3. Create a custom print.Customer() method.
print.Customer <- function(obj)
{
  cat("Customer Details\n")
  cat("------------------\n")
  cat("Name :", obj$Name, "\n")
  cat("Age :", obj$Age, "\n")
  cat("Income :", obj$Income, "\n")
  cat("Membership :", obj$Membership, "\n")
}
print(customer)

# 4. Extract and display customer details using object reference
customer$Name
customer$Age
customer$Income
customer$Membership

# 5. Modify one attribute and observe behavior.
customer$Membership <- "Platinum"
print(customer)



# Q2) Advanced Customer Class (S4 System)

# 1. Define an S4 class "Customer" with slots (Age, Income, Membership).
setClass(
  "Customer",
  slots = list(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)

# 2. Create objects from dataset rows.
customer1 <- new(
  "Customer",
  Age = 35,
  Income = 75000,
  Membership = "Gold"
)
customer1

# 3. Write a show() method. 
setMethod(
  "show",
  "Customer",
  function(object)
  {
    cat("Customer Details\n")
    cat("------------------\n")
    cat("Age :", object@Age, "\n")
    cat("Income :", object@Income, "\n")
    cat("Membership :", object@Membership, "\n")
  }
)
customer1



# Q3) Object Referencing and Copy Behavior

# 1. Assign dataset to new variable.
data_copy <- customer_data

# 2. Modify one column in copied dataset. 
data_copy$Income <- data_copy$Income + 5000
head(data_copy)

# 3. Check if original dataset changes.
head(oop_customer_data)

# 4. Use identical() to compare objects.
identical(oop_customer_data, data_copy)



# Q4) Memory Optimization Study

# 1. Create large object from dataset. 
large_data <- rbind(customer_data,
                    customer_data,
                    customer_data,
                    customer_data,
                    customer_data)
head(large_data)


# 2. Check memory usage using object.size()
object.size(large_data)

# 3. Remove unnecessary objects using rm().
rm(large_data)

# 4. Run garbage collection. 
gc()



# Q5) List-Based Customer Object System 

# 1. Convert dataset into list of customers. 
customer_list <- split(data, seq(nrow(data)))
customer_list[[1]]

# 2. Assign S3 class to each list element. 
customer_list <- lapply(customer_list, function(x)
{
  class(x) <- "Customer"
  return(x)
})
class(customer_list[[1]])

# 3. Access nested values
customer_list[[1]]$Name
customer_list[[1]]$Age

# 4. Modify one customer's data.
customer_list[[1]]$Income <- 80000
customer_list[[1]]

# 5. Print structure using str().
str(customer_list)

# Q6) Class Inspection and Documentation

# 1. Check class of dataset.
class(customer_data)

# 2. Use str() to inspect structure.
str(customer_data)

# 3. Extract Attributes
attributes(customer_data)

# 4. Use summary() for overview.
summary(customer_data)



# Q7) S4 Slot Manipulation

# 1. Create S4 class "SpendingProfile"
setClass(
  "SpendingProfile",
  slots = list(
    SpendingScore = "numeric",
    Income = "numeric"
  )
)
getClass("SpendingProfile")

# 2. Add slots for SpendingScore and Income
setClass(
  "SpendingProfile",
  slots = c(
    SpendingScore = "numeric",
    Income = "numeric"
  )
)

# 3. Create object using dataset values.
profile1 <- new(
  "SpendingProfile",
  SpendingScore = 78,
  Income = 65000
)
profile1

# 4. Access slots using @. 
profile1@SpendingScore
profile1@Income

# 5. Modify slot values and validate.
profile1@SpendingScore <- 90
profile1@Income <- 75000
profile1



# Q8) Advanced Data Structure Integration

# 1. Create nested list containing dataset and summary
sales_summary <- summary(data)
company_data <- list(
  Dataset = data,
  Summary = sales_summary
)
company_data

# 2. Access inner elements.
company_data$Dataset
company_data$Summary

# 3. Modify nested structure.
company_data$CompanyName <- "Retail Analytics Pvt Ltd"
company_data

# 4. Apply class to nested object. 
class(company_data) <- "CompanyData"
class(company_data)

# 5. Display structure using str()
str(company_data)



# Q9) Object-Oriented Data Analysis

# 1. Convert dataset into S3 object.
customer_object <- data
class(customer_object) <- "CustomerData"
class(customer_object)

# 2. Create function to calculate average income
average_income <- function(obj)
{
  mean(obj$Income)
}

# 3. Apply function to object.
average_income(customer_object)

# 4. Extend object with new attribute.
attr(customer_object, "Source") <- "Customer Survey Data"
attributes(customer_object)

# 5. Validate object consistency
str(customer_object)

class(customer_object)

sum(is.na(customer_object))



# Q10) Enterprise-Level Object System Design

# 1. Create both S3 and S4 versions of dataset.
# Create S3 Object
s3_customer <- data
class(s3_customer) <- "CustomerS3"
class(s3_customer)

# S4 Object Creation
setClass(
  "CustomerS4",
  slots = list(
    Age = "numeric",
    Income = "numeric",
    Membership = "character"
  )
)

# Create S4 Object
s4_customer <- new(
  "CustomerS4",
  Age = 35,
  Income = 65000,
  Membership = "Gold"
)
s4_customer

# 2. Compare flexibility vs strictness. 
s3_customer$new_column <- "Flexible"

# 3. Implement method for summary statistics.
# S3 Summary Method
summary.CustomerS3 <- function(object)
{
  cat("Average Income :", mean(object$Income), "\n")
  cat("Average Age :", mean(object$Age), "\n")
}
summary(s3_customer)

# S4 Summary Method
setMethod(
  "summary",
  "CustomerS4",
  function(object)
  {
    cat("Customer Summary\n")
    cat("----------------\n")
    cat("Age :", object@Age, "\n")
    cat("Income :", object@Income, "\n")
    cat("Membership :", object@Membership, "\n")
  }
)
summary(s4_customer)

