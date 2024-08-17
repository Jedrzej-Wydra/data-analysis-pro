library(readr)
library(skimr)
library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
dataset <- read_csv("https://s3.amazonaws.com/talent-assets.datacamp.com/product_sales.csv")
skim(dataset)
head(dataset)
dataset.na <- na.omit(dataset)
skim(dataset.na)

hist(dataset[is.na(dataset$revenue),]$nb_site_visits)
hist(dataset.na$nb_site_visits)
hist(dataset$nb_site_visits)

#cleaning data in sales_method
dataset <- dataset %>%
  mutate(sales_method = recode(sales_method, "email" = "Email", "em + call" = "Email + Call", .default = sales_method))
#cleaning data in revenue
dataset.na <- na.omit(dataset)

dataset$sales_method %>% table() / nrow(dataset)
dataset.na$sales_method %>% table() / nrow(dataset.na)
dataset[is.na(dataset$revenue),]$sales_method %>% table() / nrow(dataset[is.na(dataset$revenue),])

skim(dataset.na)
#proportions is categorical variable before and after removing rows with missing
#values are almost identical 

boxplot(dataset.na$years_as_customer)
Sys.time() - as.Date("1984-01-01")
difftime(Sys.time(), as.Date("1984-01-01"), unit = "days") / 365

sum(dataset.na$years_as_customer >= 40)

data <- dataset.na %>% filter(years_as_customer < 40)

skim(data)
boxplot(data$years_as_customer)

data %>% filter(week > 6)

data %>%
  count(sales_method) %>%
  arrange(desc(n)) %>%
  ggplot(aes(reorder(sales_method, -n), n, fill = sales_method)) + 
    geom_col(show.legend = FALSE) + 
    xlab("sales method") +
    ylab("count") +
    ggtitle("Number of customers by sales strategy") +
    scale_y_continuous(breaks = seq(0, 7000, 1000)) +
    theme_bw() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank()) 
    

ggplot(data, aes(x = sales_method, y = revenue, fill = sales_method)) +
  geom_boxplot(show.legend = FALSE) +
  coord_flip() +
  xlab("sales method") +
  ggtitle("Spread of revenue for each strategy") +
  theme(panel.grid.minor.x = element_blank()) +
  theme_bw()

ggplot(data, aes(x = revenue)) +
  geom_histogram(fill = "#F8666D", bins = 15) +
  xlab("sales method") +
  ggtitle("Spread of revenue (overall)") +
  theme_bw()

ggplot(data, aes(x = revenue, fill = sales_method)) +
  geom_histogram(bins = 15) +
  xlab("revenue") +
  labs(fill = "") +
  ggtitle("Distribution of revenue (overall)") +
  theme_bw()

ggplot(data, aes(y = revenue, x = 0)) +
  geom_boxplot() +
  labs(fill = "") +
  ggtitle("Spread of revenue (overall)") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

data %>%
  group_by(sales_method) %>%
  summarise(total = sum(revenue)) %>%
  ungroup() %>%
  ggplot(aes(reorder(sales_method, -total), total, fill = sales_method)) + 
  geom_col(show.legend = FALSE) + 
  xlab("sales method") +
  ylab("total revenue") +
  ggtitle("Total revenue for each strategy") +
  scale_y_continuous(breaks = seq(0, 700000, 100000), labels = scales::comma) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()) 



data %>%
  group_by(week, sales_method) %>%
  summarise(total = sum(revenue)) %>%
  ungroup() %>%
  ggplot(aes(week, total, color = sales_method)) + 
  geom_line() + 
  xlab("sales method") +
  ylab("total revenue") +
  labs(color = "") +
  ggtitle("Total revenue for each strategy over time") +
  scale_x_continuous(breaks = 1:6,) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()) 




data %>%
  group_by(week, sales_method) %>%
  summarise(total = sum(revenue), n = n()) %>%
  ungroup() %>%
  ggplot(aes(week, n, color = sales_method)) + geom_line()

data %>%
  group_by(week, sales_method) %>%
  summarise(total = sum(revenue), n = n(), avg = mean(revenue)) %>%
  ungroup() %>%
  ggplot(aes(week, avg, color = sales_method)) + geom_line()

data %>%
  group_by(state) %>%
  summarise(total = sum(revenue)) %>%
  ggplot(aes(x = state, y = total)) + geom_col() + coord_flip()

best_states <- c("Califirnia", "Texas", "New York", "Florida")

data %>%
  filter(state %in% best_states) %>%
  group_by(week, sales_method) %>%
  summarise(total = sum(revenue), n = n()) %>%
  ungroup() %>%
  ggplot(aes(week, total, color = sales_method)) + geom_line()

data %>% count(sales_method) %>% arrange(desc(n)) %>% mutate(n = n / sum(n))
data %>% filter(state %in% best_states) %>% count(sales_method) %>% arrange(desc(n)) %>% mutate(n = n / sum(n))

data %>%
  group_by(week, sales_method) %>%
  summarise(total = sum(revenue), n = n(), avg = mean(revenue), total_sales = sum(nb_sold)) %>%
  ungroup() %>%
  ggplot(aes(week, total_sales, color = sales_method)) + geom_line()

data %>%
  mutate(years = cut(years_as_customer, c(0,6,40), include.lowest = TRUE))%>%
  group_by(sales_method, years) %>%
  summarise(total = sum(revenue), avg = mean(revenue)) %>%
  ggplot(aes(years, total, fill = sales_method)) +
  geom_col(position = "dodge")

data %>%
  group_by(sales_method) %>%
  summarise(average = mean(revenue), total = sum(revenue), n = n()) %>%
  arrange(desc(total)) %>%
  mutate(time_cost = c(0, 10, 30) + c(1, 1, 0), time = n * time_cost / 60) %>%
  select(-average)









