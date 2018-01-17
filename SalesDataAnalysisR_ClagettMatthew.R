# Title: SALES DATASET ANALYSIS
# Author: Matthew Clagett

# 1 Year-over-Year comparison by region, product line, customer segment
# 2 Bookings distribution (please use "quote" column) for each region, product line, customer segment
# 3 Sales person performance comparison
# 4 Composition of each sales person's portfolio (i.e. what product line they sell and how much of their annual sales is for each particular product line)

# Using XLConnect to load the xlsx file
library(XLConnect)
library(dplyr)
library(ggplot2)

wb <- loadWorkbook("Sales Data.xlsx", create = TRUE)
sales <- (wb[1])

### 1 YOY Comparisons
# By Region
# Loaded a second dataset that matches country to each region that they are in
# source: https://github.com/clagett/ISO-3166-Countries-with-Regional-Codes

edasubset <- sales[c(4,6,8,12,20,22)]
View(edasubset)
colnames(sales)
regionmatch <- read.csv("all.csv")
View(regionmatch)


# Merged sets to get the regions, realizing that there was misnamed
m <- merge(edasubset, regionmatch, by.x="Country", by.y="name", all.x=TRUE)
ggplot(m, aes(x=region, y = Annualized.Booking)) + 
     geom_bar(aes(fill = Fiscal.Year), stat = "identity", position = "dodge")
View(m)

# Going to manually add in region
us <- m$Country == "United States"
m$region[us] <- "Americas"
uk <- m$Country == "United Kingdom"
m$region[uk] <- "Europe"
mol <- m$Country == "Moldova, Republic Of"
m$region[mol] <- "Europe"
slv <- m$Country == "Slovakia (Slovak Republic)"
m$region[slv] <- "Europe"
tw <- m$Country == "Taiwan"
m$region[tw] <- "Asia"
tt <- m$Country == "Trinidad And Tobago"
m$region[tt] <- "Americas"
bol <- m$Country == "Bolivia"
m$region[bol] <- "Americas"

ggplot(m, aes(x=region, y = Annualized.Booking)) + 
     geom_bar(aes(fill = Fiscal.Year), stat = "identity", position = "dodge") +
     scale_fill_brewer(palette="Oranges") +
     scale_y_continuous(labels = scales::comma)

# Year-over-year comparison by product line
prodline <- sales[c(6, 12, 22)]
ggplot(prodline, aes(x= Product.Line, y = Annualized.Booking)) + 
     geom_bar(aes(fill = Fiscal.Year), stat = "identity", position = "dodge") +
     scale_fill_brewer(palette="Oranges")

# By customer segment - 
cust <- sales[c(8, 12, 22)]
ggplot(cust, aes(x= Customer.Segment, y = Annualized.Booking)) + 
     geom_bar(aes(fill = Fiscal.Year), stat = "identity", position = "dodge") +
     coord_flip() + 
     scale_fill_brewer(palette="Oranges")


### 2 Bookings distribution (please use "quote" column) for each region, product line, customer segment
# Regional
colnames(m)
regional <- m[c(5, 11)]
ggplot(regional, aes(x=region, y=Quote)) + 
     geom_boxplot() + 
     scale_y_continuous(labels = scales::comma) + 
     ylim(0,150000)

# Product Line
colnames(sales)
prodline2 <- sales[c(6, 20)]
ggplot(prodline2, aes(x=Product.Line, y=Quote)) + 
            geom_boxplot() + 
     scale_y_continuous(labels = scales::comma) + 
     ylim(0,150000)

# Customer Segment
# Ran into outliers, so created graphs with and without limits
cust2 <- sales[c(8,20)]
ggplot(cust2, aes(x=Customer.Segment, y=Quote)) + 
     geom_boxplot() + 
     theme(axis.text.x = element_text(size  = 8, angle = 35, hjust = 1, vjust = 1)) + 
     scale_y_continuous(labels = scales::comma) +
     ylim(0,1000000)

cust2 <- cust2 %>% mutate(LogQuote = log(Quote))
head(cust2)


### 3 Sales Person performance comparison
salespeople <- sales[c(18,22)]

# Distribution of sales sizes
ggplot(salespeople, aes(x=Sales.Person, y=Annualized.Booking)) + 
     geom_boxplot() + 
     theme(axis.text.x = element_text(size  = 8, angle = 35, hjust = 1, vjust = 1)) + 
     scale_y_continuous(labels = scales::comma) +
     ylim(0,1000000)

# Comparing total sales by each salesperson
totalsales <- salespeople %>% group_by(Sales.Person) %>% summarise_each(funs(sum))
ggplot(totalsales, aes(x=Sales.Person, y = Annualized.Booking)) +
     geom_bar(aes(fill = Sales.Person), stat = "identity", position = "dodge") + 
     theme(axis.text.x = element_text(size  = 8, angle = 35, hjust = 1, vjust = 1)) + 
     scale_y_continuous(labels = scales::comma) +
     ylab("Total Sales")


### 4 Composition of each sales person's portfolio (i.e. what product line they sell and how much of their annual sales is for each particular product line)
# 100 chart
salesport <- sales[c(6,18,22)]
ggplot(salesport, aes(x=Sales.Person, y = Annualized.Booking)) + 
     geom_bar(aes(fill = Product.Line), stat = "identity", position = "fill") + 
     scale_fill_brewer(palette="Spectral") + 
     coord_flip()

