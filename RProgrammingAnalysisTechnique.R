# Loading the dataset into rstudio
library(readr) # To read the csv file
telecom_customer_churn <- read_csv("C:/Users/Ms. Ann/OneDrive - Amity University/telecom_customer_churn.csv")

# Exploring the structure of the dataset
View(telecom_customer_churn)
head(telecom_customer_churn, 10)
tail(telecom_customer_churn, 10)
summary(telecom_customer_churn)
str(telecom_customer_churn)

# Data Cleaning:Checking and handling missing values.
table(is.na(telecom_customer_churn))

#Checking For Missing Values
library(descriptr)#package to check for missing values
ds_screener(telecom_customer_churn)

# Handling Missing Values
telecom_customer_churn[is.na(telecom_customer_churn$`Avg Monthly Long Distance Charges`), "Avg Monthly Long Distance Charges"]<- 0
table(is.na(telecom_customer_churn$`Avg Monthly Long Distance Charges`))

medML<-median(telecom_customer_churn$`Multiple Lines`, na.rm=TRUE)
print(medML)
telecom_customer_churn[is.na(telecom_customer_churn$`Multiple Lines`), "Multiple Lines"]<- medML
table(is.na(telecom_customer_churn$`Multiple Lines`))

telecom_customer_churn[is.na(telecom_customer_churn$`Internet Type`), "Internet Type"]<- "None"
table(is.na(telecom_customer_churn$`Internet Type`))

telecom_customer_churn[is.na(telecom_customer_churn$`Avg Monthly GB Download`), "Avg Monthly GB Download"]<- 0
table(is.na(telecom_customer_churn$`Avg Monthly GB Download`))

medOS<-median(telecom_customer_churn$`Online Security`, na.rm=TRUE)
print(medOS)
telecom_customer_churn[is.na(telecom_customer_churn$`Online Security`), "Online Security"]<- medOS
table(is.na(telecom_customer_churn$`Online Security`))

medOB<-median(telecom_customer_churn$`Online Backup`, na.rm=TRUE)
print(medOB)
telecom_customer_churn[is.na(telecom_customer_churn$`Online Backup`), "Online Backup"]<- medOB
table(is.na(telecom_customer_churn$`Online Backup`))

telecom_customer_churn[is.na(telecom_customer_churn$`Device Protection Plan`), "Device Protection Plan"]<- "No"
table(is.na(telecom_customer_churn$`Device Protection Plan`))

telecom_customer_churn[is.na(telecom_customer_churn$`Premium Tech Support`), "Premium Tech Support"]<- "No"
table(is.na(telecom_customer_churn$`Premium Tech Support`))

telecom_customer_churn[is.na(telecom_customer_churn$`Streaming Movies`), "Streaming Movies"]<- "No"
table(is.na(telecom_customer_churn$`Streaming Movies`))

telecom_customer_churn[is.na(telecom_customer_churn$`Streaming Music`), "Streaming Music"]<- "No"
table(is.na(telecom_customer_churn$`Streaming Music`))

telecom_customer_churn[is.na(telecom_customer_churn$`Unlimited Data`), "Unlimited Data"]<- "No"
table(is.na(telecom_customer_churn$`Unlimited Data`))

telecom_customer_churn[is.na(telecom_customer_churn$`Streaming TV`), "Streaming TV"]<- "No"
table(is.na(telecom_customer_churn$`Streaming TV`))

telecom_customer_churn[is.na(telecom_customer_churn$`Churn Category`), "Churn Category"]<- "None"
table(is.na(telecom_customer_churn$`Churn Category`))

drop(telecom_customer_churn$`Customer ID`)

telecom_customer_churn[is.na(telecom_customer_churn$`Churn Reason`), "Churn Reason"]<- "None"
table(is.na(telecom_customer_churn$`Churn Reason`))

save(telecom_customer_churn, file = "telecom_customer_churn.RData")
write.csv(telecom_customer_churn, file = "telecom_customer_churn_cleaned.csv")

# To check for outliers
library(readr)
Outlier_set <- read_csv("C:/Users/Ms. Ann/Downloads/telecom_customer_churn_cleaned_outlier set 2.csv")

sum(is.na(Outlier_set))

boxplot(Outlier_set)

summary(Outlier_set)

data1<- Outlier_set$`Number of Dependents`
length(data1)
bench<-0+1.5*IQR(data1)
bench
data1<data1[data1<bench]
data1[data1>bench]
data1[data1>bench]<-bench
summary(data1)
boxplot(data1)

data2<- Outlier_set$`Number of Referrals`
length(data2)
bench<-3+1.5*IQR(data2)
bench
data2<data2[data2<bench]
data2[data2>bench]
data2[data2>bench]<-bench
summary(data2)
boxplot(data2)

data3<- Outlier_set$`Total Refunds`
length(data3)
bench<-0+1.5*IQR(data3)
bench
data3<data3[data3<bench]
data3[data3>bench]
data3[data3>bench]<-bench
summary(data3)
boxplot(data3)

data4<- Outlier_set$`Total Extra Data Charges`
length(data4)
bench<-0+1.5*IQR(data4)
bench
data4<data4[data4<bench]
data4[data4>bench]
data4[data4>bench]<-bench
summary(data4)
boxplot(data4)

data5<- Outlier_set$`Total Long Distance Charges`
length(data5)
bench<-1191.10+1.5*IQR(data5)
bench
data5<data5[data5<bench]
data5[data5>bench]
data5[data5>bench]<-bench
summary(data5)
boxplot(data5)

data6<- Outlier_set$`Total Revenue`
length(data6)
bench<-4801.15+1*IQR(data6)
bench
data6<data6[data6<bench]
data6[data6>bench]
data6[data6>bench]<-bench
summary(data6)
boxplot(data6)

Outlier_set$`Number of Dependents`<-data1
Outlier_set$`Number of Referrals`<-data2
Outlier_set$`Total Refunds`<-data3
Outlier_set$`Total Extra Data Charges`<-data4
Outlier_set$`Total Long Distance Charges`<-data5
Outlier_set$`Total Revenue`<-data6

Outlier_set2=subset(Outlier_set,Age<=80 & `Number of Dependents`<=0 & `Zip Code`<=96150 & Latitude<=41.96 & Longitude<=-114.2 & `Number of Referrals`<=7.5 & `Tenure in Months`<=72 & `Avg Monthly Long Distance Charges`<=49.99 & `Monthly Charge`<=118.75 & `Total Charges`<=8684.8 & `Total Refunds`<=0 & `Total Extra Data Charges`<=0 & `Total Long Distance Charges`<=2871.93 & `Total Revenue`<=8996.68)
boxplot(Outlier_set2)
summary(Outlier_set2)

data5<- Outlier_set$`Total Long Distance Charges`
length(data5)
bench<-1016.98+1.5*IQR(data5)
bench
data5<data5[data5<bench]
data5[data5>bench]
data5[data5>bench]<-bench
summary(data5)
boxplot(data5)

data4<- Outlier_set$`Total Charges`
length(data4)
bench<-3317.2+1.5*IQR(data4)
bench
data4<data4[data4<bench]
data4[data4>bench]
data4[data4>bench]<-bench
summary(data4)
boxplot(data4)

Outlier_set$`Total Long Distance Charges`<-data5
Outlier_set$`Total Charges`<-data4

Outlier_set3=subset(Outlier_set2,Age<=80 & `Number of Dependents`<=0 & `Zip Code`<=96150 & Latitude<=41.96 & Longitude<=-114.2 & `Number of Referrals`<=7.5 & `Tenure in Months`<=72 & `Avg Monthly Long Distance Charges`<=49.99 & `Monthly Charge`<=118.75 & `Total Charges`<=8396.9  & `Total Refunds`<=0 & `Total Extra Data Charges`<=0 & `Total Long Distance Charges`<=2697.81  & `Total Revenue`<=8996.68)
boxplot(Outlier_set3)
summary(Outlier_set3)

data5<- Outlier_set3$`Total Long Distance Charges`
length(data5)
bench<-991.8 +1.5*IQR(data5)
bench
data5<data5[data5<bench]
data5[data5>bench]
data5[data5>bench]<-bench
summary(data5)
boxplot(data5)

data4<- Outlier_set3$`Total Charges`
length(data4)
bench<-3263.6+1.5*IQR(data4)
bench
data4<data4[data4<bench]
data4[data4>bench]
data4[data4>bench]<-bench

boxplot(data4)

Outlier_set3$`Total Long Distance Charges`<-data5
Outlier_set3$`Total Charges`<-data4

Outlier_set4=subset(Outlier_set3,Age<=80 & `Number of Dependents`<=0 & `Zip Code`<=96150 & Latitude<=41.96 & Longitude<=-114.2 & `Number of Referrals`<=7.5 & `Tenure in Months`<=72 & `Avg Monthly Long Distance Charges`<=49.99 & `Monthly Charge`<=118.75 & `Total Charges`<=7635.8  & `Total Refunds`<=0 & `Total Extra Data Charges`<=0 & `Total Long Distance Charges`<=2397.0  & `Total Revenue`<=8996.68)
boxplot(Outlier_set4)

save(Outlier_set4, file = "Outlier_set4.RData")
write.csv(Outlier_set4, file = "Outlier_set4.csv")


#Exploratory Data Analysis
df<-telecom_customer_churn_totally_cleaned
#Demographics
ggplot(df, aes(x=Gender,fill=Gender))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")
ggplot(df, aes(x=Married,fill=`Customer Status`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")
hist(df$Age, main="Age Group of Customers", xlab="Ages", ylab="Frequency", col="cyan")

#Major Churn Elements
hist(df$`Total Charges`, xlab="Total Charges", ylab="Frequency", col="cyan")
hist(df$`Tenure in Months`, main="Tenure in Months", xlab="Tenure", ylab="Frequency", col="cyan")
boxplot(df$`Total Charges`)
boxplot(df$`Tenure in Months`)

summary(df$`Tenure in Months`)

#Product Analysis
box1<-ggplot(df, aes(x=`Phone Service`, y=`Total Revenue`))+
  geom_line(color='indianred3',linewidth=1)+ geom_smooth()+
  labs(title="Revenue By Phone Service",
       x="Phone service",y="Revenue")
theme_minimal()

box2<-ggplot(df, aes(x=`Internet Service`, y=`Total Revenue`))+
  geom_line(color='indianred3',size=1)+ geom_smooth()+
  labs(title="Revenue By Internet Service",
       x="Internet service",y="Revenue")
theme_minimal()

library(cowplot)
plot_grid(box1,box2,Labels="Auto")

box3<-ggplot(df, aes(x=`Online Security`,fill=`Online Security`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")

box4<-ggplot(df, aes(x=`Online Backup`,fill=`Online Backup`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")

box5<-ggplot(df, aes(x=`Device Protection Plan`,fill=`Device Protection Plan`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")

box6<-ggplot(df, aes(x=`Premium Tech Support`,fill=`Premium Tech Support`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")

box7<-ggplot(df, aes(x=`Payment Method`,fill=`Payment Method`))+
  geom_bar(width=.60)+scale_fill_brewer(palette="Dark2")

plot_grid(box3,box4,box5,box6,box7,labels="")

library(ggplot2)
library(lessR)
library(magrittr)
library(dplyr)
#Churn Analysis
(churn.base <- df%>% 
    group_by(`Customer Status`)%>% 
    count(`Customer Status`)%>% 
    mutate(perc = n/nrow(df) * 100)%>% 
    rename(customers = n))
churnR<-data.frame(churn=df$`Customer Status`)
PieChart(churn,hole=0, values='%', data=churnR,
         fill=c('blue','green','orange'), main="Churn Rate")
##Based on gender
ggplot(df,aes(x=df$Gender,fill=`Customer Status`))+
  geom_bar()+labs(title="Gender-wise Customer Status",x="Gender")
##Based on contract
ggplot(df,aes(x=df$Contract,fill=`Customer Status`))+
  geom_bar()+labs(title="Contract-wise Customer Status",x="Contract type")

#Tenure Analysis
ggplot(df,aes(x=`Tenure in Months`,color=`Customer Status`))+
  geom_freqpoly(size=2)

#Total Charges Analysis
library(ggplot2)
ggplot(df)+
  geom_boxplot(aes(x=`Customer Status`,y=`Total Charges`, fill=`Customer Status`))
ggplot(df)+ 
  geom_boxplot(aes(x=`Customer Status`,y=`Monthly Charge`, fill=`Customer Status`))
#calculating average total charge for churned and non-churned customers
df%>%
  group_by(`Customer Status`)%>%
  summarize(mean(`Total Charges`))





























