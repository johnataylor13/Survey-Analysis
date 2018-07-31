###Analysis of NFL dataset
## Demographic difference between avid and casual fans
#install.packages("readxl") 
options(max.print=999999)
library(readxl)

data1<-read_excel("avid-casual.xlsx", sheet = 1)
summary(data1)
#Information about data
nrow(data1) #no of rows
ncol(data1) #no of cols
class(data1) #type of data
dim(data1) #dimension of data
str(data1) #display structure of datith categorical data
library(dplyr)
count<-table(data1$`Level of interest in NFL`)
#Hypothesis testing: chi square
#install.packages("MASS")
library(MASS)
tl<-table(data1$`Level of interest in NFL`,data1$D11)
tl
chisq.test(data1$`Level of interest in NFL`, data1$`Relationship status`, correct=FALSE)
#Logistic model using lm for avid and casual

model <- glm(data1$`Level of interest in NFL`~ data1$Gender + data1$State + data1$Voter + data1$Age + data1$Race
               +data1$Employment + data1$`Employment industry`+data1$`Household Income`+data1$Son_age+data1$`Interest in sports`+data1$`Coached a sports team`+data1$`Religious service frequency`
             +data1$`Kids play sports`+data1$`Social media engagement`+data1$`Area of residence`)
summary(model) 
plot(model)


#install.packages("corrplot")
#library(corrplot)
#corrplot(data1, method ="number")

#to calculate the predictive values of fandom
pred<-predict(model) 

# attach it with the dataframe 
pred1<- data.frame(pred) 
pred1
data2<-data.frame(data1)
final_data<-cbind(data2,pred1)

#write the final data to csv and save it. 
write.csv(pred1,"linear_output.csv")
#visualization

#colors <- c("lightgrey","lightgreen","lightyellow","lightblue","lavender","mistyrose")
#colors<-c("darkred","darkblue","darkgreen")
colors<-c("grey","darkgreen")
#regions <- c("Northeast","Midwest","South","west")
gender<-c("Male","Female")
#binary<-c("Yes","No")
#Education<-c("Less than 9th grade	","9th to 12th grade, no diploma","High School Graduate","Some college or associate degree","Bachelor’s degree","Advanced degree")
#media<-c("Several times a week","About once a day","Every few weeks","Once a week","Less often","Never")
#area<-c("Urban","Suburban","Rural")
#pol<-c("Republican","Democrat","Independent")
#race<-c("Caucasian or White","African American or Black","Hispanic/Latino","Asian","American Indian/Native American","Other")
#data1$freqs <- as.numeric(as.character(data1$freqs))
fans<-c("Avid","Casual")
counts <- table(data1$`Level of interest in NFL`,data1$Voter)
c<-data.matrix(counts)
c
#f1<-count(data1$Education)
#m<-as.vector(f1)
#f2<-count(data1$`Level of interest in NFL`)
#t<-table(data1$`Level of interest in NFL`)
#as.data.frame.matrix(f2)
counts
#barplot(c)
perc<-c("61.3%","38.6%","43.4%","56.5%")
perc<-data.frame(perc)
perc
bp<-barplot(counts, 
            main ="NFL fans and Gender", 
            xlab = "Gender", 
            ylab= "NFL fans",
            names.arg=c("Avid","casual"), 
            col = colors,
            beside = TRUE)
legend("top",cex=0.7,gender, fill = colors)
 #text(x = Education, y = data1$`freq(edu)`, label = data1$`freq(edu)`, pos = 3, cex = 0.8, col = "red")
 #legend = rownames(f1)
text(bp, 0, round(perc,1),cex=0.8,pos=3) 

# col=c("grey","darkgreen","darkred","darkblue") 
       


#Cluster analysis using Kmodes
#install.packages("klaR")
#library(klaR)
#cluster.results <-kmodes(data2, 2, iter.max = 10, weighted = FALSE )
cluster.results
summary(data1)