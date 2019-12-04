# Read Datafile####

library(readxl)
Sales_Blackwell <- read_excel("Sales Blackwell.xlsx", 
                              sheet = "Existing Product List")
View(Sales_Blackwell)
str(Sales_Blackwell)
summary(Sales_Blackwell)
#Visualize Data####
library(ggplot2)
library(gcookbook)
library(dplyr)

#plot sum of sales volume per category####
Sum_Blackwell <- Sales_Blackwell %>% group_by(Product_Category)%>%
        summarize(Total_Sales = sum(Sales_Volume))

ggplot(Sum_Blackwell, aes(x = reorder(Product_Category, -Total_Sales), 
                y = Total_Sales)) +
  geom_col()
  

       