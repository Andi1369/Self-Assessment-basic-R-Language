library(dplyr)
library(tidyverse)
df = read.csv("Dataset_superstore_simple.csv")
#customer_id dengan sales paling besar
data_a = select(df,c(customer_id,sales))
data_a %>% group_by(customer_id) %>% summarise(total_sales = sum(sales))
data_a = data_a [order(data_a$total_sales, decreasing = TRUE),]
#total profit dari masing-masing sub_category pada category 'Office Supplies'
data_b = select(df,c(category,sub_category,profit))
data_b = filter(data_b,category=='Office Supplies')
data_b = data_b %>% group_by(category,sub_category) %>% summarise(total_profit = sum(profit))
view(data_b)
#order yang menghasilkan profit rugi
data_c = select(df,c(order_id,profit))
data_c = data_c %>% group_by(order_id) %>% summarise(profit = sum(profit))
data_c = filter(data_c,profit < 0)
view(data_c)
nrow(data_c)
summary(data_c)
#3 customer_id dengan total sales terbanyak
data_d = select(df,c(customer_id,sales))
data_d = filter(data_d,customer_id %in% c("JE-16165","KH-16510","AD-10180"))
data_d = data_d %>% group_by(customer_id) %>% summarise(total_sales = sum(sales))
data_d = data_d [order(data_d$total_sales, decreasing = TRUE),]
view(data_d)
#membuat data frame yearly_sales
df$order_date = as.Date(df$order_date)
df$order_year = as.Date(cut(df$order_date, breaks = 'year'))
view(df)
yearly_sales = df %>% group_by(order_year) %>% summarise(total_sales = sum(sales),n_customer=n(),total_profit=sum(profit))
yearly_sales = yearly_sales[order(yearly_sales$total_sales, decreasing = TRUE),]
view(yearly_sales)
#membuat scatterplot sales vs profit
new_data = select(df,c(order_year,sales,profit))
new_data = filter(new_data,order_year == c("2014-01-01","2015-01-01"))
view(new_data)
library(ggplot2)
ggplot(new_data ,aes(x=sales ,y=profit))+geom_point(aes(color= order_year))+geom_smooth(method ='lm')+labs(title = 'Sales vs Profit')
# membuat barchart
data_x = select(df, c(customer_id,sales,order_year))
data_x = filter(data_x ,order_year=='2015-01-01')
view(data_x)
data_x = data_x %>% group_by(customer_id) %>% summarise(total_sales=sum(sales))
view(data_x)
data_x = data_x[order(data_x$total_sales, decreasing = TRUE),]
view(data_x)
data_x = head(10)
view(data_x)
data_x = select(df, c(customer_id,sales,order_year))
data_x = filter(data_x ,order_year=='2015-01-01')
data_x = data_x %>% group_by(customer_id) %>% summarise(total_sales=sum(sales))
data_x = data_x[order(data_x$total_sales, decreasing = TRUE),]
data_x = head(data_x,10)
view(data_x)
data_y = select(df, c(customer_id,profit))
data_y = data_y %>% group_by(customer_id) %>% summarise(total_profit = sum(profit))
view(data_y)
data_z = inner_join(data_x,data_y)
View(data_z)
ggplot(data_z ,aes(x=customer_id,y=total_profit))+geom_bar(stat = 'identity',fill='yellow')
