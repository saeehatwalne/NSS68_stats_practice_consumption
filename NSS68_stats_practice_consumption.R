#Saee Hatwalne
#Statistics with R
# 2023

setwd("/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR")
library(tidyverse)
library(haven)
library(ggplot2)
library(dplyr)
library(tidyr)
install.packages("ggpubr")
library(ggpubr)
library(hexbin)
library(corrplot)
library(igraph)

rm(list=ls())

#Q1) Download the datasets and save as .RData

block5_3 <- read_dta("/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR/Block_5_3_Time disposition during the week ended on.dta")
save(block5_3, file="/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR/Block_5_3_Time disposition during the week ended on.RData")
load("Block_5_3_Time disposition during the week ended on.RData")
block8 <- read_dta("/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR/Block_8_Household consumer expenditure.dta")
save(block8, file="/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR/Block_8_Household consumer expenditure.RData")
load("Block_8_Household consumer expenditure.RData")

#Q2) Renaming columns and  top 15 categories

#NOTE:I have taken 30 days consumption for my expenditures (throughout the assignment)
block8 <- block8 %>% 
  rename("consumption_categories" = "Item_Group_Srl_No")
#another way to do this
#colnames(block8)[20] <- "consumption_categories"
category_totals <- block8 %>%
  group_by(consumption_categories) %>%
  summarize(Total_Expenditure = sum(Value_of_Consumption_Last_30_Day, na.rm = TRUE)) %>%
  arrange(desc(Total_Expenditure))
#categories 40,23,39, 38 are subtotals and not separate categories as per data description, hence removed them
category_totals <- subset(category_totals, !(consumption_categories %in% c(40, 23, 39,38)))
#taking the top 15 categories
category_df <- head(category_totals, 15)
#renaming
category_df[category_df == "01"] <- "cereals"
category_df[category_df == "13"] <- "fuel_light"
category_df[category_df == "03"] <- "milk"
category_df[category_df == "19"] <- "conveyance"
category_df[category_df == "06"] <- "vegetables"
category_df[category_df == "22"] <- "medical"
category_df[category_df == "18"] <- "consumer_services"
category_df[category_df == "08"] <- "food_nonveg"
category_df[category_df == "11"] <- "food_other"
category_df[category_df == "05"] <- "edible_oil"
category_df[category_df == "20"] <- "rent"
category_df[category_df == "02"] <- "pulses"
category_df[category_df == "12"] <- "intoxicants"
category_df[category_df == "07"] <- "fruits_nuts"
category_df[category_df == "16"] <- "toilet_articles"
#my interpretation:
#cereals, fuel, milk are some of the most important daily usage items every household needs
#so it is consistent to get these items as top most expenditures in households
#conveyance and transportation is also a crucial category since vehicles etc. are also costly purchases for a household

#Q3)
#Q3a) categories and their proportions
#making a column with relative proportions of expenditure
category_df1 <- category_df %>%
  add_column(relative_prop = category_df$Total_Expenditure)
sum <- sum(category_df1$relative_prop)
category_df1 <- category_df1 %>% 
  mutate(relative_prop = Total_Expenditure/sum)
#bar graph and custom colors/borders
ggplot(category_df1, aes(x = consumption_categories, y = relative_prop)) +
  geom_col(fill = "blue", color = "red")+
  geom_col(data = category_df1[1:3,], fill = c("magenta", "seagreen", "yellow"), color = "red") +
  theme(axis.text.x = element_text(angle=45,hjust = 1))

#bar graphs in descending order with gradient colors
library(RColorBrewer)
display.brewer.all()
category_df1$color_variable <- seq_along(category_df1$consumption_categories)

ggplot(category_df1, aes(x = reorder(consumption_categories, -relative_prop), y = relative_prop, fill = color_variable)) +
  geom_bar(stat = "identity") +
  scale_fill_gradientn(colours = colorRampPalette(c("blue", "red"))(length(unique(category_df1$color_variable)))) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Relative proportion of consumption categories in India",
       x = "Consumption categories",
       y = "Relative proportion")
  # scale_x_discrete(limits = category_df1$consumption_categories) +
  # scale_fill_identity()

#Q3b) Contingency table for categories 1,2,3,4 and four given states
#contingency table1
category_df2 <- block8 %>%
  filter(State==10 | State == 23 | State == 28 | State == 33)
category_df2$State <- as.numeric(category_df2$State)
category_df2$consumption_categories <- as.numeric(category_df2$consumption_categories)
contingency_df <- as.data.frame(with(category_df2, table(consumption_categories, State)))
contingency_df <- spread(contingency_df, State, Freq)
#renaming states
contingency_df <- contingency_df %>% 
  rename("BR" = "10")
contingency_df <- contingency_df %>% 
  rename("MP" = "23")
contingency_df <- contingency_df %>% 
  rename("AP" = "28")
contingency_df <- contingency_df %>% 
  rename("TN" = "33")
#making a spare dataframe, need it for (Q3d) and (Q3e)
contingency_df3 <- contingency_df
#total
contingency_df <- contingency_df %>% 
  mutate(total =contingency_df$BR + contingency_df$MP + contingency_df$AP + contingency_df$TN)

#proportions
contingency_df <- contingency_df %>% 
  mutate(BR = BR/total)  
contingency_df <- contingency_df %>% 
  mutate(MP = MP/total)     
contingency_df <- contingency_df %>% 
  mutate(AP = AP/total)     
contingency_df <- contingency_df %>% 
  mutate(TN = TN/total)     
contingency_df <- contingency_df %>% 
  rename("N" = "total")    
contingency_df <- contingency_df %>% 
  mutate(total =contingency_df$BR + contingency_df$MP + contingency_df$AP + contingency_df$TN)

#First finding out total proportions for each category
contingency_df$totalprop <- contingency_df$N/sum(contingency_df$N)
contingency_df <- contingency_df %>%
  arrange(desc(totalprop))
#removing the subtotal categories
contingency_df <- subset(contingency_df, !(consumption_categories %in% c(40, 23, 39,38)))

#Q3c) Stacked bar graph

#Finding out categories with highest total proportion in these 4 states
contingency_df1 <- contingency_df
#contingency table result
contingency_df1 <- head(contingency_df1, 4)
#making stacked bar graph with these four highest proportion categories
ggplot(contingency_df1, aes(x = reorder(consumption_categories, BR), y = BR)) +
  geom_col(fill = "thistle", colour = "blue") +
  xlab("consumption categories")+
  ylab("Prop in BR")
ggplot(contingency_df1, aes(x = reorder(consumption_categories, MP), y = MP)) +
  geom_col(fill = "plum1", colour = "blue") +
  xlab("consumption categories")+
  ylab("Prop in MP")
ggplot(contingency_df1, aes(x = reorder(consumption_categories, AP), y = AP)) +
  geom_col(fill = "slateblue2", colour = "blue") +
  xlab("consumption categories")+
  ylab("Prop in AP")
ggplot(contingency_df1, aes(x = reorder(consumption_categories, TN), y = TN)) +
  geom_col(fill = "orchid4", colour = "blue") +
  xlab("consumption categories")+
  ylab("Prop in TN")
contingency_df1 <- gather(contingency_df1, state, proportion, BR, MP, AP, TN)
plot1 <- ggplot(contingency_df1, aes(x = consumption_categories, y = proportion, fill = state)) +
  geom_col()
plot1
#almost all proportions are similar in number, hence the similar categories in stacked bar graph

#reducing top 14 categories and clubbing rest together as 15th category
#first 14 + rest all clubbed together
contingency_df$consumption_categories <- as.character(contingency_df$consumption_categories)
contingency_df$consumption_categories[contingency_df$totalprop < 0.031550592 ] <- "Others"
contingency_df$consumption_categories[contingency_df$N == 19513 ] <- "Others"
contingency_df2 <- contingency_df %>%
  group_by(consumption_categories) %>%
  summarise(
    sumBR = sum(BR),
    sumMP = sum(MP),
    sumAP = sum(AP),
    sumTN = sum(TN)
  )
#category with highest proportion in BR
#(sorted the data in the dataframe view iself - options for arranging in ascending or descending order)
#from the contingency table we can see that
#category 2 i.e. pulses represents the highest proportion in Bihar
#category 9 i.e. sugar represents the highest proportion in Madhya Pradesh
#category 3 i.e. milk represents the highest proportion in Madhya Pradesh
#category 11 i.e. other food items represents the highest proportion in Tamil Nadu

#my interpretation:
#pulses, milk and sugar are relatively expensive items
#but still come under necessary daily items, hence, their consumption proportion being more
#is but natural. Additionally, demand for these items w.r.t prices would be inelastic

#Q3d)
#line graph for 15 top categories versus total expenditure
plot2<- ggplot(category_df, aes(x=consumption_categories, y=Total_Expenditure, group=1)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle=45,hjust = 1))
plot2

#Q3e)
#line graph for all states versus total expenditure
state_df <- block8 %>%
  group_by(State) %>%
  summarize(Total_Expenditure2 = sum(Value_of_Consumption_Last_30_Day, na.rm = TRUE)) %>%
  arrange(desc(Total_Expenditure2))

ggplot(state_df, aes(x=State, y=Total_Expenditure2, group=1)) + 
  geom_line() +
  theme(axis.text.x = element_text(angle=90,hjust = 1))

#Q3f) In part (c)
#removing the legend
plot1 + guides(fill = FALSE)
#change legend position
plot1 + theme(legend.position = "bottom")
#change state names in legend
#I had already renamed it as per RTO abbreviations, so changed to full names, hope that's okay!
plot1 + scale_fill_discrete(labels = c("Andhra Pradesh", "Bihar", "Madhya Pradesh", "Tamil Nadu"))
#change order of state names in legend
plot1 + scale_fill_discrete(limits = c("AP", "MP", "BR", "TN"))

#Q3g) in part (d)
#swapping axes
plot2 + coord_flip()
#removing grid lines
plot2 + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())

#Q4) Mergings
#NOTE: I am just taking the required columns from both datasets for merging
#because my computer is hanging if I start to merge entire datasets together
block8copy <- block8 %>%
  select(HHID, Value_of_Consumption_Last_30_Day, State)
block5_3copy <- block5_3 %>%
  select(HHID, Wage_and_Salary_Earnings_Total, State)

# Q4a) MEAN CONSUMPTION AND WAGES AT HH LEVEL
block8meanHH <- block8copy %>%
  group_by(HHID) %>%
  summarize(avg_consump = mean(Value_of_Consumption_Last_30_Day, na.rm=TRUE))
block8meanHH <- na.omit(block8meanHH)

block5_3meanHH <- block5_3copy %>%
  group_by(HHID) %>%
  summarize(avg_wages = mean(Wage_and_Salary_Earnings_Total, na.rm=TRUE))
block5_3meanHH <- na.omit(block5_3meanHH)

merged_df1 <- merge(block8meanHH,block5_3meanHH, by.x="HHID", by.y = "HHID" )
ggplot(merged_df1, aes(x= avg_wages, y= avg_consump)) + geom_point() + 
  geom_smooth(method = lm)
#my interpretation:
#this regression line clearly shows that average consumption is positively correlated with wages
#although it doesn't mean causation, association can be infered from the graph

#Q4b) MEAN CONSUMPTION AND WAGES AT STATE LEVEL
block8mean_state <- block8copy %>%
  group_by(State) %>%
  summarize(avg_consump = mean(Value_of_Consumption_Last_30_Day, na.rm=TRUE))
block8mean_state <- na.omit(block8mean_state)

block5_3mean_state <- block5_3copy %>%
  group_by(State) %>%
  summarize(avg_wages = mean(Wage_and_Salary_Earnings_Total, na.rm=TRUE))
block5_3mean_state <- na.omit(block5_3mean_state)

merged_df2 <- merge(block8mean_state,block5_3mean_state, by.x="State", by.y = "State" )
ggplot(merged_df2, aes(x= avg_wages, y= avg_consump)) + geom_point() + 
  geom_smooth(method = lm)
#my interpretation:
#this regression line clearly shows that avg consumption is positively correlated with wages
#if average income increases, this means that average consumption is also seen to be greater

#Q4c) TOTAL CONSUMPTION AND WAGES AT STATE LEVEL
block8total_state <- block8copy %>%
  group_by(State) %>%
  summarize(total_consump = sum(Value_of_Consumption_Last_30_Day, na.rm=TRUE))
block8total_state <- na.omit(block8total_state)

block5_3total_state <- block5_3copy %>%
  group_by(State) %>%
  summarize(total_wages = sum(Wage_and_Salary_Earnings_Total, na.rm=TRUE))
block5_3total_state <- na.omit(block5_3total_state)

merged_df3 <- merge(block8total_state,block5_3total_state, by.x="State", by.y = "State" )
ggplot(merged_df3, aes(x= total_wages, y= total_consump)) + geom_point() + geom_smooth(method = lm)
#my interpretation:
#this regression line clearly shows that consumption is positively correlated with wages
#the regression line has a positive slope
#theoretically, it is true that with increase in income, consumption should also go up

# Q4d) correlation matrix at HH level
corr_df1 = merged_df1 %>%
  select(avg_consump, avg_wages)
mycorr = cor(corr_df1)
round(mycorr, digits=3)
corrplot(mycorr)
#my interpretation:
#large dark blue circle denoting 100% correlation because every variable will be 100% correlated with itself
#smaller light blue circle showing correlation between avg wages and avg consumption, which is less than 100%
#this is consistent with the positively sloping regression line we got earlier

#Q4e) adding regression line equation and R square in Q4 (a)
ggplot(merged_df1, aes(x= avg_wages, y= avg_consump)) + geom_point() + 
  geom_smooth(method = lm) +
  stat_regline_equation(label.x = 3, label.y = 20) +
  stat_cor(label.y = 40000)

# Q5) Network graphs
#NOTE: all libraries installed at the beginning only
#NOTE: Please zoom and see the network diagram for better view

VandeAPU <- graph(c("Ashoka","Hampi", "Ashoka","Mysuru", "Ashoka","Kaveri Poompuhar", "Ashoka","Kaveri Sangama", "Kaveri Poompuhar","Kaveri Sangama", "Mysuru","Hampi"))
plot(VandeAPU, edge.arrow.size = 0.5, loop.size = 2, vertex.color = "black", vertex.frame.color = "hotpink", vertex.label.dist = 4)

