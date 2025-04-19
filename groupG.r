library(corrplot)
library(RColorBrewer)
library(report)
library(car)

#import dataset then
mydata<- lead
#Read in the dataset and analyze the read‐in dataset using str
str(mydata)
#1.descriptive statistics:
#convert the numeric variables in id into charachter, and in area, sex, lead type and exposure into factor
mydata$Id=as.character(mydata$Id)
mydata$Area<- factor(mydata$Area, labels=c("Paso", "Texas", "USA"))
mydata$Sex<-factor(mydata$Sex,labels=c("male", "female"))
mydata$Lead_type<-factor(mydata$Lead_type, labels = c("type1","type2"))
mydata$Exposed<- factor(mydata$Exposed, labels=c("yes","no"))
#Summarize the data and calculate the following: mean, median, minimum, maximum, first and third quartile (for each variable). 
summary(mydata)
#IQ
mean(mydata$Iqf ,na.rm=TRUE)
median(mydata$Iqf,na.rm=TRUE)
min(mydata$Iqf,na.rm=TRUE)
max(mydata$Iqf,na.rm=TRUE)
quantile(mydata$Iqf,na.rm=TRUE,c(0.25,0.75))
#Ld72
mean(mydata$Ld72 ,na.rm=TRUE)
median(mydata$Ld72,na.rm=TRUE)
min(mydata$Ld72,na.rm=TRUE)
max(mydata$Ld72,na.rm=TRUE)
quantile(mydata$Ld72,na.rm=TRUE,c(0.25,0.75))
#Ld73
mean(mydata$Ld73 ,na.rm=TRUE)
median(mydata$Ld73,na.rm=TRUE)
min(mydata$Ld73,na.rm=TRUE)
max(mydata$Ld73,na.rm=TRUE)
quantile(mydata$Ld73,na.rm=TRUE,c(0.25,0.75))

#For the categorical variable existing, calculate a frequency table 
table(mydata$Area)
table(mydata$Sex)
table(mydata$Lead_type)
table(mydata$Exposed)
#Calculate the correlation coefficient (MAXWT and Ld72) and (MAXWT and Ld73)
#default method (pearson)
Maxfwt_72<-cor(mydata$MAXFWT,mydata$Ld72, use="complete.obs")
Maxfwt_72
Maxfwt_73<-cor(mydata$MAXFWT,mydata$Ld73, use="complete.obs")
Maxfwt_73
#correlation using Spearman
cor(mydata$MAXFWT,mydata$Ld72, use="complete.obs", method="spearman")
cor(mydata$MAXFWT,mydata$Ld73, use="complete.obs",method="spearman")

#to view 
cormat=cor(mydata[,c("MAXFWT","Ld72","Ld73")], use="complete.obs")
corrplot(cormat, method="color", col=brewer.pal(n=10, name="PuOr"),
         tl.col="black",tl.srt=50, main="relation between MAXFWT, LD72 and LD73")

#Graphics
# a bar chart of a categorical variable for the gender
barplot(table(mydata$Sex), xlab = "sex", ylab = "frequency", main = " children gender", col = 4, ylim = c(0,70))
# bar chart graph with mean MAXWT in  males and females 
barplot(tapply(mydata$MAXFWT,list(sex=mydata$Sex),mean,na.rm= T), xlab = "sex", ylab="mean", main="Mean MAXWT", ylim=c(0,60), col=5)
# histogram of “age”
hist(mydata$Age,xlab="Age",main="Distribution of Age", ylim=c(0,30),col=6)
#histogram of “MAXWT
hist(mydata$MAXFWT,xlab="MAXFWT",main="Distribution of MAXFWT")
#scatterplot of  Ld72 and MAXWT, with the regression lines for each gender
#1st draw the plot with male sex
plot(MAXFWT[Sex=="male"]~Ld72[Sex=="male"], data=mydata, xlab="LD72", ylab = "MAXFWT", main="scatterplot")
#adding points with female sex
points(MAXFWT[Sex=="female"]~Ld72[Sex=="female"], data=mydata, col=2)
#drawing the regression line
abline(lm(mydata$MAXFWT[mydata$Sex=='male']~mydata$Ld72[mydata$Sex=="male"]))
abline(lm(mydata$MAXFWT[mydata$Sex=='female']~mydata$Ld72[mydata$Sex=="female"]), col=2)
# boxplot of age  and a separate boxplots per Ld72 and per Ld73 (as.factors).
boxplot(Age ~ Ld72, data = mydata, main = "Boxplot of Age by Ld72", xlab = "Ld72", ylab = "Age") 
boxplot(mydata$Age, main= "boxplot for age") 

par(mfrow=c(1,3))
boxplot(mydata$Age, main="age")
boxplot(mydata$Ld72, main="Ld72")
boxplot(mydata$Ld73, main="Ld73")

#Outlier detection
# age
age_outliers<-boxplot(mydata$Age,plot=FALSE)$out
age_outliers # no outliers detected
boxplot(mydata$Age, main= "Age")
#IQ
IQ_outliers<-boxplot(mydata$Iqf,plot=FALSE)$out
IQ_outliers
#to get the row containing the outliers
mydata[mydata$Iqf%in% IQ_outliers,]
boxplot(mydata$Iqf, main="IQ")
#Ld72
Ld72_outliers<-boxplot(mydata$Ld72,plot=FALSE)$out
Ld72_outliers
mydata[mydata$Ld72%in% Ld72_outliers,]
boxplot(mydata$Ld72, main="ld72")
#Ld73
Ld73_outliers<- boxplot(mydata$Ld73, plot= FALSE)$out
Ld73_outliers
mydata[mydata$Ld73%in%Ld73_outliers,]
boxplot(mydata$Ld73, main="Ld73")
#Totyrs:
totyrs_outliers<- boxplot(mydata$Totyrs,plot=FALSE)$out
totyrs_outliers
mydata[mydata$Totyrs%in%totyrs_outliers,]
boxplot(mydata$Totyrs, main ="TOTyrs")
#MAXFWT
MAXFWT_outliers<- boxplot(mydata$MAXFWT, plot=FALSE)$out
MAXFWT_outliers
mydata[mydata$MAXFWT%in%MAXFWT_outliers,]
boxplot(mydata$MAXFWT, main= "MAXFWT")

#checking the normality:
#ld72
hist(mydata$Ld72,main="Ld72 histogram" , xlab = "Ld72")
qqnorm(mydata$Ld72, main="Ld72")
qqline(mydata$Ld72)
shapiro.test(mydata$Ld72)
#not normally distributed

#ld73
hist(mydata$Ld73, main="Ld73 histogram" , xlab = "Ld73")
qqnorm(mydata$Ld73, main = "Ld73")
qqline(mydata$Ld73)
shapiro.test(mydata$Ld73)
#not normally distributed

#Totyrs
hist(mydata$Totyrs, main="lead exposure", xlab = "years")
qqnorm(mydata$Totyrs, main= "lead exposure in years")
qqline(mydata$Totyrs)
shapiro.test(mydata$Totyrs)
#reject the null hypothesis,not normally distributed

#MAXFWT
hist(mydata$MAXFWT, main="MAXFWT", xlab = "MAXFWT")
qqnorm(mydata$MAXFWT, main= "MAXFWT")
qqline(mydata$MAXFWT)
shapiro.test(mydata$MAXFWT)
#not normally distributed

#checking homoscedasticity:
var.test(mydata$Ld72~mydata$Exposed, data = mydata)
leveneTest(MAXFWT~interaction(Lead_type,Sex), data=mydata)

#CI:
#CI= mean +- z*SE
#MAXFWT in males
male<-mydata[mydata$Sex=="male",]$MAXFWT
#calculate the mean
male.mean<- mean(male, na.rm = TRUE)
male.mean
#Sample size
m<- na.omit(male)
n1<-length(m)
#calculate standard deviation:
male.sd<- sd(male, na.rm = TRUE)
#SE
male_SE<- male.sd / sqrt(n1)
male_SE

#90% interval
rt.value1 <- round(male.mean + 1.65*male_SE,3)
lt.value1 <- round(male.mean - 1.65*male_SE,3)

print(paste("90% confidence interval =",lt.value1,"_", rt.value1))

#95%interval
rt.value2 <- round(male.mean + 1.96*male_SE,3)
lt.value2 <- round(male.mean - 1.96*male_SE,3)
print(paste("95% confidence interval =",lt.value2,"_", rt.value2))
#99%interval
rt.value3 <- round(male.mean + 2.58*male_SE,3)
lt.value3 <- round(male.mean - 2.58*male_SE,3)
print(paste("99% confidence interval =",lt.value3,"_", rt.value3))

#MAXFWT in females
female<- mydata[mydata$Sex=="female",]$MAXFWT
f<- na.omit(female)
#calculate the mean
female.mean<- mean(f, na.rm = TRUE)
female.mean
#Sample size
n2<- length(f)
n2
#calculate standard deviation:
female.sd<- sd(f, na.rm = TRUE)
#SE
female_SE<- female.sd / sqrt(n2)
female_SE

#90% interval
rt.valuef1 <- round(female.mean + 1.65*female_SE,3)
lt.valuef1 <- round(female.mean - 1.65*female_SE,3)

print(paste("90% confidence interval =",lt.valuef1,"_", rt.valuef1))

#95%interval
rt.valuef2 <- round(female.mean + 1.96*female_SE,3)
lt.valuef2 <- round(female.mean - 1.96*female_SE,3)
print(paste("95% confidence interval =",lt.valuef2,"_", rt.valuef2))
#99%interval
rt.valuef3 <- round(female.mean + 2.58*female_SE,3)
lt.valuef3 <- round(female.mean - 2.58*female_SE,3)
print(paste("99% confidence interval =",lt.valuef3,"_", rt.valuef3))

# Hypothesis testing:

#1- MAXWT is different between male vs female
#assuming normality and equal variance the test of choice is 2 sample t.test
t.test(MAXFWT~Sex, data=mydata, var.equal=TRUE)
#no significant difference
#do not reject the null
#checking for normality:
hist(male,main="MAXFWT in males" , xlab=" MAXFWT")
qqnorm(male, main="MAXFWT in males")
qqline(male)
shapiro.test(male)
shapiro.test(mydata[mydata$Sex=="male",]$MAXFWT)
#reject the null hypothesis, not normally distributed
hist(female, main = "MAXFWT in females", xlab= "MAXFWT")
qqnorm(female, main="MAXFWT in females")
qqline(female)
shapiro.test(female)
#no enough evidence to reject the null hypothesis, normally distributed
#check for equal variance:
boxplot(MAXFWT~Sex, data=mydata)
leveneTest(MAXFWT~Sex, data=mydata)
#no enough evidence to reject the null hypothesis, equal variance

#t.test isless accurate because the male data is not normally distributed
#the test of choice is 
wilcox.test(MAXFWT~Sex, data=mydata)

#no enough evidence to reject the null hypothesis,no significant difference

#2-hypothesis that MAXWT is “lower” in the group receiving Ld72 > 40  compared to the control Ld72 <=40
exposed_group<- mydata$MAXFWT[mydata$Ld72>40]
exposed_group
control_group<- mydata$MAXFWT[mydata$Ld72<= 40]
control_group

welch_test<-t.test(exposed_group,control_group,var.equal = FALSE)
welch_test
#reject ehe null hypothesis there is a true difference
#test for  normal distribution 
shapiro.test(control_group)
shapiro.test(exposed_group)
#both are not normally distributed
#testing for heteroscedasiticy 
boxplot(control_group,exposed_group)
var.test(control_group,exposed_group)
#no enough data to reject the null hypothesis, equal variance
wilcox.test(control_group,exposed_group)
#reject the null hypothesis, there is a significant difference

#3-We hypothesis that MAXWT is different between the different Lead types with the different genders  (i.e. 4 groups male_leadtype1, male_leadtype2, female_leadtype1, female_leadtype2)
#assuming normality and homoscedasticity the test of choice is Anova
Twoway_AnovaModel11 <- aov(mydata$MAXFWT ~ mydata$Lead_type+lead$Sex, data=mydata)
summary(Twoway_AnovaModel11)
#dividing the data into 4 groups:
male_leadtype1 <- (mydata[mydata$Lead_type=="type1" & mydata$Sex=="male",]$MAXFWT)
male_leadtype2 <- (mydata[mydata$Lead_type=="type2" & mydata$Sex=="male",]$MAXFWT)
female_leadtype1 <- (mydata[mydata$Lead_type=="type1" & mydata$Sex=="female",]$MAXFWT)
female_leadtype2 <- (mydata[mydata$Lead_type=="type2" & mydata$Sex=="female",]$MAXFWT)

library(ggstatsplot)
library(ggplot2)
#Verify equality of variances 
plot(MAXFWT~interaction(Lead_type, Sex), data =mydata)

ggplot(mydata) +
  aes(x = Lead_type, y = MAXFWT, color = 1, fill= Sex) +
  geom_boxplot()

leveneTest(MAXFWT~interaction(Lead_type,Sex), data=mydata)
#no enough evidence to reject the null hypothesis, equal variance

#testing for normality:
shapiro.test(mydata[mydata$Lead_type=="type1" & mydata$Sex=="male",]$MAXFWT)
#reject H0l, not normally distributed
shapiro.test(mydata[mydata$Lead_type=="type1" & mydata$Sex=="female",]$MAXFWT)
#no enough evidence to reject the null, normally distributed
shapiro.test(mydata[mydata$Lead_type=="type2" & mydata$Sex=="male",]$MAXFWT)
##reject H0, not normally distributed
shapiro.test(mydata[mydata$Lead_type=="type2" & mydata$Sex=="female",]$MAXFWT)
#no enough evidence to reject the null, normally distributed

#assuming normality and homoscedasticity the test of choice is Anova
MAXFWT_anova1<-aov(MAXFWT~Lead_type+Sex, data =mydata)
#summary(MAXFWT_anova1)
report(MAXFWT_anova1)
#Posthoc analysis (with Tukey correction)
posthoc1<-TukeyHSD(MAXFWT_anova1) 
posthoc1
plot(posthoc1)


MAXFWT_anova2<-aov(MAXFWT~Lead_type*Sex, data =mydata)
summary(MAXFWT_anova2)
report(MAXFWT_anova2)

posthoc2<-TukeyHSD(MAXFWT_anova2) 
posthoc2
plot(posthoc2)
#there is a significant difference between type2.male-type1.male, type1.female-type2.male
#as the data not normally distributed the test of choice is Kruskal wallis:
kruskal.test(MAXFWT~ interaction(Sex, Lead_type), data=mydata)
library(dunn.test)
pairwise.t.test(mydata$MAXFWT,interaction(mydata$Sex,mydata$Lead_type),p.adjust.method = "bonferroni")
 
#linear model
#plot a graph for the data
plot( mydata$Ld72, mydata$MAXFWT, xlab = "Ld72", ylab="MAXFWT", 
      main="linear regression model")
#draw the line
abline(H2)
#test the regression
H2<- lm( MAXFWT~Ld72, data=mydata)
summary(H2)
#Q2. confidence interval:
confint(H2)

#Q3.bonus
plot( mydata$Ld73, mydata$MAXFWT)
#test the regression
bonus<- lm( MAXFWT~Ld73, data=mydata)
summary(bonus)
#draw the line
abline(bonus)
#prediction
predict.lm(bonus, newdata = data.frame(Ld73=100))
#Estimating the average MAXWT reduction for with increasing the lead concentration (Lead73) to 100 μg /100 ml (bonus)
#the avarege reduction = intercept - predicted MAXFWT when Ld73 =100 (64.36-24.98= ~39.38)
