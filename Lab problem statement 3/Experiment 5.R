#Step 1: Install required packages

install.packages(c(
  "tidyverse",
  "jsonlite",
  "readxl",
  "DBI",
  "RSQLite"
))

# Load required libraries
library(tidyverse)
library(jsonlite)
library(readxl)
library(DBI)
library(RSQLite)

getwd()

#Step 2: Read the files
retail <- read_excel("Online Retail.xlsx")

head(retail)
dim(retail)
str(retail)
colnames(retail)

# Step 3: Check missing values

colSums(is.na(retail))

# Check duplicate rows
sum(duplicated(retail))

# Check invalid quantities
sum(retail$Quantity <= 0, na.rm = TRUE)

# Check invalid unit prices
sum(retail$UnitPrice <= 0, na.rm = TRUE)

# Step 4: Clean the dataset

retail_clean <- retail %>%
  filter(
    !is.na(CustomerID),
    !is.na(Description),
    Quantity > 0,
    UnitPrice > 0
  ) %>%
  distinct()

# Check dimensions after cleaning
dim(retail_clean)

# Calculate Revenue
retail_clean <- retail_clean %>%
  mutate(
    Revenue = Quantity * UnitPrice
  )

# View cleaned data
head(retail_clean)

# Verify missing values
colSums(is.na(retail_clean))

# Verify invalid quantities
sum(retail_clean$Quantity <= 0)

# Verify invalid prices
sum(retail_clean$UnitPrice <= 0)

# Verify duplicates
sum(duplicated(retail_clean))

# Final dimensions
dim(retail_clean)

# Create Revenue attribute
retail_clean <- retail_clean %>%
  mutate(
    Revenue = Quantity * UnitPrice
  )

# Check the result
head(retail_clean)

# Check dimensions
dim(retail_clean)

# Create transactions dataset
transactions <- retail_clean %>%
  select(
    InvoiceNo,
    StockCode,
    CustomerID,
    Quantity,
    InvoiceDate
  )

# Export to CSV
write_csv(transactions, "transactions.csv")

# Check
head(transactions)

# Create products dataset
products <- retail_clean %>%
  select(
    StockCode,
    Description,
    UnitPrice
  ) %>%
  distinct(StockCode, .keep_all = TRUE)

# Export to JSON
write_json(
  products,
  "products.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

# Check
head(products)

install.packages("writexl")

library(writexl)

# Create customers dataset
customers <- retail_clean %>%
  select(
    CustomerID,
    Country
  ) %>%
  distinct(CustomerID, .keep_all = TRUE)

# Export to Excel
write_xlsx(customers, "customers.xlsx")

# Check
head(customers)

# Step 6: Import the three data sources

transactions <- read_csv("transactions.csv")

products <- fromJSON("products.json")

customers <- read_excel("customers.xlsx")

# Inspect the datasets
head(transactions)
head(products)
head(customers)

# Check dimensions

dim(transactions)
dim(products)
dim(customers)

# Check key columns

colnames(transactions)
colnames(products)
colnames(customers)

# Step 8: Integrate the three datasets

# Join transactions with products
sales_with_products <- transactions %>%
  left_join(products, by = "StockCode")

# Join customer information
retail_final <- sales_with_products %>%
  left_join(customers, by = "CustomerID")

# Check final dimensions
dim(retail_final)

# View the integrated data
head(retail_final)

# Check unmatched product records
sum(is.na(sales_with_products$Description))

# Check unmatched customer records
sum(is.na(retail_final$Country))

# Check missing values in final dataset
colSums(is.na(retail_final))

# Step 9: Calculate Revenue in the integrated dataset

retail_final <- retail_final %>%
  mutate(
    Revenue = Quantity * UnitPrice
  )

# Check
head(retail_final)

# Check dimensions
dim(retail_final)

# Total sales revenue

total_revenue <- retail_final %>%
  summarise(
    Total_Revenue = sum(Revenue)
  )

# Display total revenue in a readable format

cat(
  "Total Sales Revenue: £",
  format(round(total_revenue$Total_Revenue, 2),
         big.mark = ",",
         nsmall = 2),
  "\n"
)

total_revenue

# Top 5 products by revenue

top_products <- retail_final %>%
  group_by(StockCode, Description) %>%
  summarise(
    Total_Revenue = sum(Revenue),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 5)

top_products

total_revenue

top_products

# Top 5 countries by revenue

top_countries <- retail_final %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue)) %>%
  slice_head(n = 5)

top_countries

# Display revenue with commas

top_countries %>%
  mutate(
    Total_Revenue = round(Total_Revenue, 2)
  )

# Top 5 customers by total purchase value

top_customers <- retail_final %>%
  group_by(CustomerID) %>%
  summarise(
    Total_Purchase_Value = sum(Revenue),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Purchase_Value)) %>%
  slice_head(n = 5)

top_customers

# Calculate total purchase value for every customer

customer_value <- retail_final %>%
  group_by(CustomerID) %>%
  summarise(
    Total_Purchase_Value = sum(Revenue),
    .groups = "drop"
  )

# Examine the distribution

summary(customer_value$Total_Purchase_Value)

# Calculate useful percentiles

quantile(
  customer_value$Total_Purchase_Value,
  probs = c(0.25, 0.50, 0.75),
  na.rm = TRUE
)

# Step 15: Classify customers based on purchase value

customer_value <- customer_value %>%
  mutate(
    Customer_Category = case_when(
      Total_Purchase_Value <= 317.835 ~ "Low Value",
      Total_Purchase_Value <= 703.570 ~ "Medium Value",
      Total_Purchase_Value <= 1744.907 ~ "High Value",
      Total_Purchase_Value > 1744.907 ~ "Premium"
    )
  )

# View classified customers
head(customer_value)
# Count customers in each category

customer_value %>%
  count(Customer_Category)

# Revenue contribution by customer category

customer_category_summary <- customer_value %>%
  group_by(Customer_Category) %>%
  summarise(
    Number_of_Customers = n(),
    Total_Revenue = sum(Total_Purchase_Value),
    Average_Purchase_Value = mean(Total_Purchase_Value),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue))

customer_category_summary

# Complete country performance analysis

country_analysis <- retail_final %>%
  group_by(Country) %>%
  summarise(
    Total_Revenue = sum(Revenue),
    Number_of_Transactions = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Revenue))

country_analysis

# Highest and lowest revenue markets

head(country_analysis, 5)

tail(country_analysis, 5)

# Step 16: Create SQLite database

con <- dbConnect(
  SQLite(),
  "retail_sales.db"
)

# Check connection
dbListTables(con)

# Store final dataset in SQLite

dbWriteTable(
  con,
  "retail_sales",
  retail_final,
  overwrite = TRUE
)

# Check tables
dbListTables(con)

# Check number of rows in SQLite table

dbGetQuery(
  con,
  "SELECT COUNT(*) AS total_rows FROM retail_sales"
)

# View first 5 records from SQLite

dbGetQuery(
  con,
  "SELECT * FROM retail_sales LIMIT 5"
)

# SQL Query 1: Top 5 customers by revenue

sql_top_customers <- dbGetQuery(
  con,
  "
  SELECT
    CustomerID,
    SUM(Revenue) AS Total_Revenue
  FROM retail_sales
  GROUP BY CustomerID
  ORDER BY Total_Revenue DESC
  LIMIT 5
  "
)

sql_top_customers

# SQL Query 2: Total revenue by country

sql_country_revenue <- dbGetQuery(
  con,
  "
  SELECT
    Country,
    SUM(Revenue) AS Total_Revenue
  FROM retail_sales
  GROUP BY Country
  ORDER BY Total_Revenue DESC
  "
)

sql_country_revenue

# Close SQLite    connection

dbDisconnect(con)