
# Load libraries
# Do not load any additional libraries other than what is provided in this template
library(odbc) # odbc
library(DBI) # DBI
library(lubridate)
library(tidyverse) # tidyverse

# Connection string info
# Already completed, just run the code block
# Everyone uses the same SQL credentials
driver_name <- "ODBC Driver 13 for SQL Server"
server_name <- "uwc-sqlserver.clients.uw.edu"
database_name <- "AdventureWorks2016CTP3" 
user_id <- "sqlstudentreader"
password <- "PA6aX2gAhe4hE!ru$6atru"

# Connect to the database
# Store connection in conn variable
conn <- dbConnect(odbc::odbc(), 
                  driver = driver_name, 
                  server = server_name, 
                  database = database_name,
                  uid = user_id,
                  pwd = password)

# Print the connection object
print(conn)

## Business Question 1: Which business entity has the highest sales quota? 


# Get Sales.SalesPerson
sql_select <- "SELECT * FROM Sales.SalesPerson"
df_sales_person <- conn %>% 
   dbGetQuery(sql_select)

# Get Sales.SalesPersonQuotaHistory
sql_select <- "SELECT * FROM Sales.SalesPersonQuotaHistory"
df_sales_quota <- conn %>% 
   dbGetQuery(sql_select)

# Glimpse results
glimpse(df_sales_person)
glimpse(df_sales_quota)

# Use semi-join by BusinessEntityID to check and filter if all BusinessEntityID is in both tables
df_semi_join <- df_sales_quota %>% semi_join(df_sales_person,"BusinessEntityID") %>% 
   glimpse()

df_semi_join 

chart1 <- df_semi_join  %>% mutate(year = year(QuotaDate), business_entity = as.factor(BusinessEntityID))  %>% 
    group_by(business_entity, year)  %>% 
    summarise(sum = sum(SalesQuota), mean = mean(SalesQuota, na.rm = TRUE))  %>% 
    arrange(desc(sum))

chart1

chart1  %>% 
ggplot() +
    geom_col(aes(y = sum, x = year, fill = business_entity), position = "dodge") +
    labs(title = "Sales Quota over Time", sub = "2011 to 2014") +
    scale_fill_manual(values = c("azure4", "azure3", "darkseagreen4","lightskyblue4", "lightsalmon", "lightpink4",
                                "aquamarine4", "rosybrown4", "rosybrown3", "rosybrown2", "rosybrown1", "thistle1",
                                "thistle2", "thistle3","thistle4", "darkorange4", "plum2"))

sql_select <- "SELECT * 
    FROM Person.Person"
df_person_person <- conn %>% 
   dbGetQuery(sql_select) 


df_person_person  %>%
    filter(BusinessEntityID == 289)  %>% 
    mutate(full_name = str_c(full_name = FirstName, MiddleName, LastName, sep =" "))  %>% 
    select(BusinessEntityID, PersonType, full_name)

## Question 2: What is the stardard cost of each product? 
## Question 3: How much is the mark up for each product

# Get Production.Product
sql_select <- "SELECT ProductID, Name
    FROM Production.Product"
df_production_product <- conn %>% 
   dbGetQuery(sql_select)

# Get Production.ProductCostHistory
sql_select <- "SELECT ProductID, StandardCost
    FROM Production.ProductCostHistory"
df_production.product_cost<- conn %>% 
   dbGetQuery(sql_select)


# Get Sales.SalesOrderDetail

sql_select <- "SELECT ProductID, UnitPrice
    FROM Sales.SalesOrderDetail"
df_sales.order_detail<- conn %>% 
   dbGetQuery(sql_select)

# Glimpse results

glimpse(df_production_product)
glimpse(df_production.product_cost)
glimpse(df_sales.order_detail)

# Count the distinct rows for each product id of the three tables 

df_production_product %>% 
   distinct(ProductID)%>% 
   nrow()

df_production.product_cost %>% 
   distinct(ProductID)%>% 
   nrow()

df_sales.order_detail %>% 
   distinct(ProductID)%>% 
   nrow()

# Use inner join so that only the product id with SalesOrderDetails.UnitPrice will show up
df_inner_join_sales_product_cost  <- df_sales.order_detail %>% 
    inner_join(df_production.product_cost,"ProductID")  %>% 
    inner_join(df_production_product,"ProductID")   %>% 
    mutate(profit = UnitPrice - StandardCost)
  

df_inner_join_sales_product_cost %>% 
    head(50)

df_inner_join_sales_product_cost %>% 
    ggplot(aes(x = StandardCost, y = UnitPrice)) +
    geom_jitter(alpha = 0.01) +
    geom_smooth(method = 'lm', colour = "darkseagreen4") +
    labs(title = "Standard Cost vs Unit Price")

df_summary  <- data_frame <- df_inner_join_sales_product_cost  %>% 
    group_by(ProductID) %>% 
    summarise(price_mean = mean(UnitPrice),
             cost_mean = mean(StandardCost),
             profit_mean = mean(profit),
             price_max = max(UnitPrice),
             cost_max = max(StandardCost),
             profit_max = max(profit))  %>% 
    arrange(desc(profit_max)) 

df_summary

 df_summary_filtered <- df_summary  %>% filter(profit_max >= 1406.975)  %>% 
    mutate(ProductID = as.factor(ProductID))  %>% 
    arrange(desc(profit_mean))

 df_summary_filtered

df_inner_join_sales_product_cost  %>% 
    filter (ProductID == c("771","772","773","774","775","776","777",
                                              "778","749","750","751","752","753"))  %>% 
    select(ProductID, Name)

df_summary_filtered %>% 
ggplot(aes(x = ProductID, y = profit_mean, fill = cost_mean)) +
    geom_col() +
    scale_fill_gradient(high = "darkseagreen4", low = "darkseagreen3") +
    labs(title = "Profit and Cost")

dbDisconnect(conn)
