library(dplyr)
library(ggplot2)
library(tidyverse)
library(tibble)
library(tidyr)


product_sales <- read.csv("product_sales.csv")

#Data Validation
summary(product_sales)
str(product_sales)

# The dataset contains 15000 rows and 8 columns before cleaning and validation.
# I have validated the entire dataset. The following describes how each column
# was validated:

# week: There are no missing values in the week data set. There are six weeks in the
#data set, matching how many weeks ago the new line of office stationery was 
#launched. No cleaning is needed.
sum(is.na(product_sales$week) == TRUE)
summary(product_sales$week)
str(product_sales$week)
#product_sales$week <- as.factor(product_sales$week)
#levels(product_sales$week)

# sales_method: There are no missing values in the sales_method dataset. Validated
#by checking for character data type, same as the description. I converted all characters to
#lower case. I then converted "em + call" to "email + call" throughout the column.
# The dataset now has three sales methods: "email", "call", and "email + call".
sum(is.na(product_sales$sales_method) == TRUE)
summary(product_sales$sales_method)
str(product_sales$sales_method)
#need to change all to lower 
product_sales$sales_method <- tolower(product_sales$sales_method)
#need to change em + call to email + call
product_sales$sales_method[product_sales$sales_method == "em + call"] <- 'email + call'


# customer_id: There are 15000 unique values in the customer_id dataset. Validated
# by checking character type and unique rows. Data matches detail requirements.
# There are no missing values in the customer_id dataset. No cleaning is needed.
sum(is.na(product_sales$customer_id) == TRUE)
summary(product_sales$customer_id)
str(product_sales$customer_id)
product_sales %>%
  add_count(customer_id) %>%
  filter(n>1) %>%
  distinct()

# nb_sold: Validated by checking for numeric data type. The nb_sold data set has
# no missing values and matches the detail requirements. No cleaning is needed.
sum(is.na(product_sales$nb_sold) == TRUE)
summary(product_sales$nb_sold)
str(product_sales$nb_sold)


# revenue: Validated by checking for numeric data type. There were 1074 missing values in the revenue data set.
# This was cleaned by taking the mean of the revenue by nb_sold groups and 
# applying the mean revenue of each nb_sold group to the appropriate missing
# values. I chose to apply the mean of each nb_sold group, rather than the
# entire data set, as the revenue was strongly correlated with increasing nb_sold values. I 
# then rounded the dataset to two decimal places.
sum(is.na(product_sales$revenue) == TRUE)
summary(product_sales$revenue)
str(product_sales$revenue)
product_sales %>%
  filter(is.na(product_sales$revenue))
product_sales <- product_sales %>%
  group_by(nb_sold) %>%
  mutate_at(vars(revenue), ~replace_na(., mean(., na.rm = TRUE)))
product_sales$revenue <- round(product_sales$revenue, 2)


# years_as_customer: The years_as_customer data set was validated by checking for
# numeric datatype. I removed two rows of data that exceeded the maximum years
# one could be a customer. Now the data contains only values from 0 to 39. 
sum(is.na(product_sales$years_as_customer) == TRUE)
summary(product_sales$years_as_customer)
str(product_sales$years_as_customer)
product_sales <- product_sales %>%
  filter(years_as_customer < 40)
str(product_sales)

# nb_site_visits: The nb_site_visits data was validated by checking for numeric
# datatype. The nb_site_visits data set has no missing values and matches the 
# detail requirements. No cleaning is needed.
sum(is.na(product_sales$nb_site_visits) == TRUE)
summary(product_sales$nb_site_visits)
str(product_sales$nb_site_visits)


# state: The state data was validated by checking for character datatype. The
# state data set has no missing values and matches the detail requirements as
# there are 50 different states represented in the dataset. No cleaning is needed.
sum(is.na(product_sales$state) == TRUE)
summary(product_sales$state)
str(product_sales$state)
product_sales$state <- as.factor(product_sales$state)
levels(product_sales$state)

str(product_sales)

write.csv(product_sales, "product_sales_clean.csv")



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

