#NFL november survey


#install.packages("readxl") 
options(max.print=999999)
library(readxl)

data1<-read_excel("nfl_nov.xlsx", sheet = 1)
data1<-read.csv("nfl_feb.csv")
data1<-read.csv("nfl_june.csv")

summary(data1)
#Information about data
nrow(data1) #no of rows
ncol(data1) #no of cols
class(data1) #type of data
dim(data1) #dimension of data
str(data1) #display structure of datith categorical data
library(dplyr)
#count<-table(data1$`Level of interest in NFL`)
#Check multicollinearity of question 10

myvars<-c("Q10_1","Q10_2","Q10_3","Q10_4","Q10_5","Q10_6","Q10_7","Q10_8","Q10_9","Q10_10","Q10_11") #Feb

myvars<-c("Q10_1","Q10_2","Q10_3","Q10_4","Q10_5","Q10_6","Q10_7","Q10_8","Q10_9","Q10_10","Q10_11","Q10_12") #June

myvars<-c("Q10_1",	"Q10_2",	"Q10_3",	"Q10_4",	"Q10_5",	"Q10_6",	"Q10_7",	"Q10_8",	"Q10_9",	"Q10_10",	"Q10_11",	"Q10_12", "Q10_13",	"Q10_14")#Nov
corr_data<-data1[myvars]

#correlation matrix
corr_matrix<-cor(corr_data)
round(corr_matrix, 2)
write.csv(corr_matrix,"corr_matrix_june.csv")
#bucket data options
corr_data<-replace(corr_data,corr_data==2,1)
corr_data<-replace(corr_data,corr_data==3,2)
corr_data<-replace(corr_data,corr_data==4,3)
corr_data<-replace(corr_data,corr_data==5,3)
corr_data<-replace(corr_data,corr_data==6,4)
#corr_matrix2<- cor(corr_data)

#write.csv(corr_matrix2,"corr_matrix2.csv")

#Hypothesis testing: chi square

corr_data<-cbind(corr_data,data1$Q3_5)
colnames(corr_data)[15]<-"Q3_5" #nov
colnames(corr_data)[13]<-"Q3_5" #june
colnames(corr_data)[12]<-"Q3_5" #Feb

#Bucket data for Q3
corr_data$Q3_5<-replace(corr_data$Q3_5,corr_data$Q3_5 ==2,1)
corr_data$Q3_5<-replace(corr_data$Q3_5,corr_data$Q3_5 ==3,2)
corr_data$Q3_5<-replace(corr_data$Q3_5,corr_data$Q3_5 ==4,2)
corr_data$Q3_5<-replace(corr_data$Q3_5,corr_data$Q3_5 ==5,NA)
corr_data$Q3_5<-replace(corr_data$Q3_5,corr_data$Q3_5 ==6,3)
write.csv(corr_data,"corr_data.csv")

#install.packages("MASS")
library(MASS)
tl<-table(corr_data$Q3_5,corr_data$Q10_2)
tl
chisq.test(corr_data$Q3_5,corr_data$Q10_11)

#Ordinal logistic regression

corr_data$Q3_5<-factor(corr_data$Q3_5)
model <- polr(corr_data$Q3_5~ corr_data$Q10_1+corr_data$Q10_2+corr_data$Q10_3+corr_data$Q10_4+corr_data$Q10_5+corr_data$Q10_6+corr_data$Q10_7+corr_data$Q10_8+corr_data$Q10_9+corr_data$Q10_10
             +corr_data$Q10_11,Hess=TRUE) #feb
model <- polr(corr_data$Q3_5~ corr_data$Q10_1+corr_data$Q10_2+corr_data$Q10_3+corr_data$Q10_4+corr_data$Q10_5+corr_data$Q10_6+corr_data$Q10_7+corr_data$Q10_8+corr_data$Q10_9+corr_data$Q10_10
              +corr_data$Q10_11+corr_data$Q10_12,Hess=TRUE) #june
model <- polr(corr_data$Q3_5~ corr_data$Q10_1+corr_data$Q10_2+corr_data$Q10_3+corr_data$Q10_4+corr_data$Q10_5+corr_data$Q10_6+corr_data$Q10_7+corr_data$Q10_8+corr_data$Q10_9+corr_data$Q10_10
              +corr_data$Q10_11++corr_data$Q10_12+corr_data$Q10_13+corr_data$Q10_14,Hess=TRUE) #nov
summary(model) 
#Convert odd-ratios to probabilities 
ctable <- coef(summary(model))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
# confidence intervals
ci <- confint(model)
exp(coef(model))

## coeff(OR) and CI
exp(cbind(OR = coef(model), ci))

# For opinion elites 
oe_data<-subset(data1,SAMPLE == 1)
#myvars<-c("Q10_1","Q10_2","Q10_3","Q10_4","Q10_5","Q10_6","Q10_7","Q10_8","Q10_9","Q10_10","Q10_11")
oe_data1<-oe_data[myvars]
#correlation matrix
#corr_matrix<-cor(oe_data)
#round(corr_matrix, 2)
#write.csv(corr_matrix,"corr_matrix_june_oe.csv")

#Bucket data options

oe_data1<-replace(oe_data,oe_data==2,1)
oe_data1<-replace(oe_data,oe_data==3,2)
oe_data1<-replace(oe_data,oe_data==4,3)
oe_data1<-replace(oe_data,oe_data==5,3)
oe_data1<-replace(oe_data,oe_data==6,4)
#corr_matrix2<- cor(oe_data)


#Bucket data for Q3
oe_data1<-cbind(oe_data1,oe_data$Q3_5)
colnames(oe_data1)[15]<-"Q3_5"#Nov
colnames(oe_data1)[13]<-"Q3_5" #june
colnames(oe_data1)[12]<-"Q3_5" #Feb
oe_data1$Q3_5<-replace(oe_data1$Q3_5,oe_data1$Q3_5 ==2,1)
oe_data1$Q3_5<-replace(oe_data1$Q3_5,oe_data1$Q3_5 ==3,2)
oe_data1$Q3_5<-replace(oe_data1$Q3_5,oe_data1$Q3_5 ==4,2)
oe_data1$Q3_5<-replace(oe_data1$Q3_5,oe_data1$Q3_5 ==5,NA)
oe_data1$Q3_5<-replace(oe_data1$Q3_5,oe_data1$Q3_5 ==6,3)




#Regression model
oe_data1$Q3_5<-factor(oe_data1$Q3_5)
model <- polr(oe_data1$Q3_5~ oe_data1$Q10_1+oe_data1$Q10_2+oe_data1$Q10_3+oe_data1$Q10_4+oe_data1$Q10_5+oe_data1$Q10_6+oe_data1$Q10_7+oe_data1$Q10_8+oe_data1$Q10_9+oe_data1$Q10_10
              +oe_data1$Q10_11,Hess=TRUE) #feb
model <- polr(oe_data1$Q3_5~ oe_data1$Q10_1+oe_data1$Q10_2+oe_data1$Q10_3+oe_data1$Q10_4+oe_data1$Q10_5+oe_data1$Q10_6+oe_data1$Q10_7+oe_data1$Q10_8+oe_data1$Q10_9+oe_data1$Q10_10
              +oe_data1$Q10_11+oe_data1$Q10_12,Hess=TRUE) #june

summary(model) 
#write.csv(corr_matrix2,"corr_matrix2.csv")


#Convert odd-ratios to probabilities 
ctable <- coef(summary(model))
ctable

p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
# confidence intervals
ci <- confint(model)
exp(coef(model))

## coeff(OR) and CI
exp(cbind(OR = coef(model), ci))

