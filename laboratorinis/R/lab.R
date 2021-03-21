library(tidyverse)
library(readr)

# Reading data
df_org <-read_csv("https://raw.githubusercontent.com/Marius-Arlauskas/KTU-duomenu-vizualizacija/main/laboratorinis/data/lab_sodra.csv")

# Filter my precious data

df<-df_org %>% filter(ecoActCode==467300)


# 1 Avarage wage histogram

?geom_histogram
ggplot(df, aes(x=avgWage))+
  geom_histogram(fill="darkslategrey", alpha=0.8, col="black", bins = 50)+
  labs(title = "Average wage of employees (economic activity code: 467300)",
       x="Average Wage, euros", y="Count") + theme_bw()

dev.print(file="plot1.png", device=png, width=780, height= 880)


# 2 
# Months extraction from date

df <- df %>% mutate(month_value=as.integer(substr(month, 5 ,7)))

# Getting top companies names 

topcomps<-df %>% 
  group_by(name) %>% 
  slice_max(avgWage, n=1) %>% 
  ungroup() %>%
  top_n(avgWage, n=5) %>% 
  select(name)

top5 <- df %>% filter(name %in% topcomps$name)

# 2nd plot

ggplot(top5, aes(x=month_value, y=avgWage, col=name))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:12,limits=c(1,12))+
  theme_bw()+
  labs(title= "Average wage of employees by month",
       x="Month", y="Average Wage")

dev.print(file="plot2.png", device=png, width=1180, height= 880)

# 3nd plot
top5 %>%  group_by(name) %>% top_n(numInsured,n=1) %>% distinct(name,numInsured) %>%
  ggplot(aes(x = (reorder(name,-numInsured)), y = numInsured, fill=name))+
  geom_col()+
  labs(title= "# of employees insured",
       x="Company", y="# of emplyees") +
  theme_bw()+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

dev.print(file="plot3.png", device=png, width=1580, height= 880)

