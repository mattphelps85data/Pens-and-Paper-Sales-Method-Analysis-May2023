
# Pens and Paper Sales Method Analysis


## Tech

**Database:** R

**Database Tool:** RStudio, Excel

**Visualization Tool:** Tableau


## Description

Six weeks ago, a new line of office stationery was launched.  The sales team has tested three different sales strategies for this, targeted email and phone calls, as well as combination of the two.
A recommendation of which sales method to continue with should be given after analysis.

## Sales Department Requested Analysis

How many customers were there for each approach?

What does the spread of the revenue look like overall? And for each method? 

Was there any difference in revenue over time for each of the methods? 

Are there any other differences between the customers in each group? 

Based on the data, which method should we continue to use? 


## Data Validation and Cleaning 

```bash
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tibble)
library(tidyr)


product_sales <- read.csv("product_sales.csv")

#Data Validation
summary(product_sales)
str(product_sales)
sum(is.na(product_sales$week) == TRUE)
summary(product_sales$week)
str(product_sales$week)

sum(is.na(product_sales$sales_method) == TRUE)
summary(product_sales$sales_method)
str(product_sales$sales_method)
product_sales$sales_method <- tolower(product_sales$sales_method)
product_sales$sales_method[product_sales$sales_method == "em + call"] <- 'email + call'


sum(is.na(product_sales$customer_id) == TRUE)
summary(product_sales$customer_id)
str(product_sales$customer_id)
product_sales %>%
  add_count(customer_id) %>%
  filter(n>1) %>%
  distinct()


sum(is.na(product_sales$nb_sold) == TRUE)
summary(product_sales$nb_sold)
str(product_sales$nb_sold)


sum(is.na(product_sales$revenue) == TRUE)
summary(product_sales$revenue)
str(product_sales$revenue)
product_sales %>%
  filter(is.na(product_sales$revenue))
product_sales <- product_sales %>%
  group_by(nb_sold) %>%
  mutate_at(vars(revenue), ~replace_na(., mean(., na.rm = TRUE)))
product_sales$revenue <- round(product_sales$revenue, 2)


sum(is.na(product_sales$years_as_customer) == TRUE)
summary(product_sales$years_as_customer)
str(product_sales$years_as_customer)
product_sales <- product_sales %>%
  filter(years_as_customer < 40)
str(product_sales)


sum(is.na(product_sales$nb_site_visits) == TRUE)
summary(product_sales$nb_site_visits)
str(product_sales$nb_site_visits)

sum(is.na(product_sales$state) == TRUE)
summary(product_sales$state)
str(product_sales$state)
product_sales$state <- as.factor(product_sales$state)
levels(product_sales$state)

str(product_sales)

write.csv(product_sales, "product_sales_clean.csv")
```

## Exploratory Analysis

```bash
# Task 2: Exploratory Analysis-------------------------------------------

#How many customers were there for each approach
ggplot(product_sales,
       aes(x = factor(sales_method,
                      level = c("email", "call", "email + call")))) +
  geom_bar(fill = "#063970") +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5,
                colour = "white")+
  theme_minimal() +
  labs(title = "Number of Customers for Each Sales Method",
       y = "Number of Customers", x = "Sales Method")

# What does spread of revenue look like overall?
ggplot(product_sales, aes(x = 1, y = revenue)) +
  geom_boxplot()

ggplot(product_sales, aes(revenue)) +
  geom_histogram(bins = 250)

# What does spread of revenue look like for each method?
ggplot(product_sales, aes(x = sales_method, y = revenue)) +
  geom_boxplot()

ggplot(product_sales, aes(revenue, fill = sales_method)) +
  geom_histogram(bins = 250)


# Difference in revenue over time for each sales method
ggplot(product_sales, aes(x = week, y = revenue)) +
  geom_col(width = 0.5) +
  facet_wrap(~ sales_method, nrow = 3)

ggplot(product_sales, aes(x = week, y = revenue, fill = sales_method)) +
  geom_col(width = 0.5)


# Graphics showing multi variables
ggplot(product_sales, aes(x = nb_sold, y = revenue, color = sales_method)) +
  geom_point()

ggplot(product_sales, aes(x = nb_sold, y = revenue, fill = sales_method)) +
  geom_col()

ggplot(product_sales, aes(x = sales_method, y = revenue)) +
  geom_boxplot()

ggplot(product_sales, aes(x = sales_method, y = revenue)) +
  geom_col()

ggplot(product_sales, aes(x = years_as_customer, y = nb_site_visits)) +
  geom_point() +
  coord_flip()

ggplot(product_sales, aes(x = years_as_customer, y = revenue)) +
  geom_col()

ggplot(product_sales, aes(x = nb_site_visits, y = revenue)) +
  geom_col()

product_sales %>%
  group_by(state) %>%
  summarize(total_revenue = sum(revenue)) %>%
  ggplot() +
  geom_col(aes(x = reorder(state, total_revenue),y = total_revenue, fill = state),
           stat = "identity", show.legend = FALSE) +
  coord_flip()


product_sales %>%
  group_by(state) %>%
  summarize(total_revenue = sum(revenue)) %>%
  filter(total_revenue > 50000) %>%
  ggplot() +
  geom_col(aes(x = reorder(state, total_revenue),y = total_revenue, fill = state),
           stat = "identity", show.legend = FALSE) +
  coord_flip()

product_sales %>%
  filter(state %in% c("California", "Texas", "New York",
                      "Florida", "Illinois", "Pennsylvania", "Ohio")) %>%
  ggplot(aes(x = state, y = revenue, fill = sales_method)) +
  geom_col(position = "fill")


product_sales %>%
  group_by(state) %>%
  summarize(total_revenue = sum(revenue)) %>%
  filter(total_revenue < 5000) %>%
  ggplot() +
  geom_col(aes(x = reorder(state, total_revenue),y = total_revenue, fill = state),
           stat = "identity", show.legend = FALSE) +
  coord_flip()


product_sales %>%
  filter(state %in% c("New Hampshire", "South Dakota", "Rhode Island",
                      "Alaska", "Montana", "Delaware", "Wyoming",
                      "North Dakota", "Vermont")) %>%
  ggplot(aes(x = state, y = revenue, fill = sales_method)) +
  geom_col(position = "fill")

ggplot(product_sales, aes(x = week, y = revenue)) +
  geom_boxplot()

ggplot(product_sales, aes(x = week, y = nb_sold)) +
  geom_boxplot()

ggplot(product_sales, aes(x = nb_site_visits, y = revenue)) +
  geom_smooth()

ggplot(product_sales, aes(x = nb_site_visits, y = nb_sold)) +
  geom_smooth()

ggplot(product_sales, aes(x = nb_site_visits, y = revenue,
                          color = sales_method)) +
  geom_point(position = "jitter", alpha = 0.5)
```
## Recommendations
Based on the data, my recommendation would be to continue using the "e-mail + call" sales approach. The data tells us that the "e-mail + call" sales approach:
 - has the most positive revenue growth.
 - is the only sales approach that increases in customers weekly.
 - brings in more revenue per customer site visit.
 - brings in more revenue per customer order.
 - accounted for 32% of total revenue despite this approach being used on only 17% of the customers.
For initial launch, "e-mail" only sales approach is great, but it is not sustainable.
For continued sales success and revenue growth, the "e-mail + call" sales approach is the recommended sales approach.
I would recommend targeting customers that have been with us 6 years or less, as they spend more and accounted for about 75% of all of our revenue.
Further data collection of success / fail of sale for customers contacted could provide further insights.

## Acknowledgements

 - [@DataCamp](https://www.datacamp.com/)

## Authors

- [@mattphelps85data](https://github.com/mattphelps85data)


[![Matthew-1.png](https://i.postimg.cc/W4rdD6fp/Matthew-1.png)](https://postimg.cc/t1qqwPj8)

