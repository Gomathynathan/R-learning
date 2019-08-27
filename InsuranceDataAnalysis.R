insurance<-read.csv("insurance.csv",header=TRUE)
attach(insurance)
library(tree)
library(ggplot2)
library(RColorBrewer)
library("MPV")

##Smoker-yes:1,no:0

##PLOTS
nrow(insurance)###number of observation in the data is 1338.
names(insurance)###variables in the insurance data.
#"age","sex","bmi","children", "smoker","region","charges"
summary(insurance$age)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#18.00   27.00   39.00   39.21   51.00   64.00 
summary(insurance$sex)
#female   male 
#662    676 
summary(insurance$bmi)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.96   26.30   30.40   30.66   34.69   53.13 
summary(insurance$bmi[insurance$smoker=="yes"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#17.20   26.08   30.45   30.71   35.20   52.58 
summary(insurance$bmi[insurance$smoker=="no"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#15.96   26.32   30.35   30.65   34.43   53.13 
summary(insurance$children)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   1.000   1.095   2.000   5.000 
summary(insurance$smoker)
#no  yes 
#1064  274 
summary(insurance$region)
#northeast northwest southeast southwest 
#324       325       364       325 
summary(insurance$charges)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1122    4740    9382   13270   16640   63770 
summary(insurance$charges[insurance$region=="northeast"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1695    5194   10058   13406   16687   58571
summary(insurance$charges[insurance$region=="northwest"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1621    4720    8966   12418   14712   60021
summary(insurance$charges[insurance$region=="southeast"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1122    4441    9294   14735   19526   63770 
summary(Insurance$charges[insurance$region=="southwest"])
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1242    4751    8799   12347   13463   52591 
#.....................................................................#
###GRAPHICAL SUMMARY##
##pdf("graph.pdf")
par(oma=rep(1,4),mar=c(4,4,4,4))
par(mfrow=c(1,1))
##a. histogram for age and boxplot(age vs gender)####
hist(insurance$age,border = "red",ann=FALSE,axes = FALSE,col="LightGreen")
axis(1,col.axis="gray50",col.ticks = "gray50",
     at = seq.int(10,75,by=5))
axis(2,col.axis="gray50",col.ticks = "gray50",
     at=seq.int(0,250,by=10))
title(main = "AGE DISRTIBUTION",xlab = "AGE",ylab = "FREQUENCY")

boxplot(insurance$age ~ insurance$sex,horizontal=TRUE,col=c("Cyan","Blue"),
        main= "Age distribution of gender",
        xlab="AGE",ylab="GENDER")
##b.histogram for bmi and boxplot(bmi vs smoker)##
par(mfrow=c(1,3))
hist(insurance$bmi,col="cornflowerblue",border = "aquamarine3",
     ann=FALSE,axes = FALSE,breaks = c(0,seq.int(5,60,by=5)),
     right = FALSE)
axis(1,col.axis="gray50",col.ticks = "gray50",
     at = seq.int(0,60,by=5))
axis(2,col.axis="gray50",col.ticks = "gray50",
     at = seq.int(0,700,by =50))
title(main = "BMI DISRTIBUTION",xlab = "BMI",ylab = "FREQUENCY")
boxplot(insurance$bmi ~ insurance$smoker,horizontal=TRUE,col=c("tan","salmon4"),
        main= "BMI distribution of smoker",
        xlab="BMI",ylab="SMOKING")
plot(insurance$bmi,insurance$charges,col=c("Yellow","Brown"),main = "Charge Distribution Vs BMI",xlab = "BMI",ylab = "Charges")
par(mfrow=c(1,1))
##charges Vs Sex:
p_1<-ggplot(data=insurance, aes(x=sex, y=charges))
p_1+geom_boxplot(fill=c("Yellow","Blue"),outlier.colour = c("Cyan"))+ggtitle("Charge distributiion-Genderwise")
summary(insurance$charges[insurance$sex=="male"])
summary(insurance$charges[insurance$sex=="female"])
##c.histogram for charges and boxplot(charges vs region)
hist(insurance$charges,border = "aquamarine3", col = "pink",
     ann=FALSE,axes = FALSE, right = FALSE)
axis(1,col.axis="gray50",col.ticks = "gray50",
     at = seq.int(1000,60000,by=5000))
axis(2,col.axis="gray50",col.ticks = "gray50",
     at = seq.int(0,500,by =50))
title(main = "Charge Distribution",xlab = "CHARGE",ylab = "FREQUENCY")
p22<-ggplot(insurance,aes(region,charges))
p22+geom_boxplot(fill=c("Yellow","red4","Seagreen","royalblue"))+ggtitle("Charge Distribtuion-Regionwise")


##Boxplot for Charges Vs Smoking habits:

p1 <- ggplot(insurance, aes(smoker, charges)) + geom_point()
p1+geom_boxplot(fill=c("Tan","Violet")) +ggtitle("Charge Disrtibtuion by Smoking Habit")

##Boxplot of Charges Vs Children:
boxplot(charges~children,col=c("Blue","Cyan","Purple","Yellow","Brown","Orange"),main="Charge Distribution Vs Dependent Children",xlab="Number of Dependent Children",ylab="Charges")
#......................HYPOTHESIS TESTING...........................#
#....Testing the factors are independent using chisquare test...#

#[1]. Determine whether smoking habit differ among men and women.
# Ho: Gender and smoking habit are independent.
# H1: They are not independent.
table(insurance$sex,insurance$smoker)
#       no yes
#female 547 115
#male   517 159
chisq.test(insurance$sex,insurance$smoker)
#Pearson's Chi-squared test with Yates' continuity correction
#data:  Insurance$sex and Insurance$smoker
#X-squared = 7.3929, df = 1, p-value = 0.006548
#Small pvalue shows the evidence that there is difference 
#between men and women smoking behaviour.
#................Two sample t test...............
##Hypothesis test for smoking has significance on insurance charges
t.test(charges~smoker,data = insurance)
#Result:The smoking has significance effect on insurace charges.
#............ANOVA:comparisons of more than two groups. A t.test is
# performed to determine whether there exist any difference in 
# the population means for two groups...............

#[1] Check the boxplot for "charge distribution for region"
#in  graphical summary.....................................
# To test whether on average,do the four region in the data have the 
# same amount of insurance charge.
anovainsurance <- aov(insurance$charges~insurance$region)
summary(anovainsurance)
#TABLE FROM R..................................................
#Df     Sum Sq   Mean Sq F value  Pr(>F)                      .
#Insurance$region    3 1.3008e+09 433586560  2.9696 0.03089 * .
#Residuals        1334 1.9477e+11 146007093                   .
#..............................................................
# pvalue from the table = 0.03089
# From the above one way anova test the table shows that 
#at 0.05 significance level atleast two regions 
#are having different means.
TukeyHSD(anovainsurance)
# Tukey's multiple comparison in ANOVA helps to test the existence of
#difference in means in each pair of regions........

#.TABLE FROM R OUTPUT.Tukey multiple comparisons of means..........
#95% family-wise confidence level                                 .
#$`Insurance$region`                                              .
#diff         lwr        upr     p adj                            .
#northwest-northeast  -988.8091 -3428.93434 1451.31605 0.7245243  .
#southeast-northeast  1329.0269 -1044.94167 3702.99551 0.4745046  .
#southwest-northeast -1059.4471 -3499.57234 1380.67806 0.6792086  .
#southeast-northwest  2317.8361   -54.19944 4689.87157 0.0582938  .
#southwest-northwest   -70.6380 -2508.88256 2367.60656 0.9998516  .
#southwest-southeast -2388.4741 -4760.50957  -16.43855 0.0476896  .
#..................................................................
#For southwest and southeast regions, the insurance charges differ
#on average at alpha = 0.05
##Correaltion matrix &heatmap
insurance1<-insurance
insurance1$sex<-factor(insurance1$sex,levels = c("female","male"),labels = c(0,1))
insurance1$sex<-as.numeric(insurance1$sex)
insurance1$region<-as.numeric(factor(insurance1$region,levels = c("northeast","northwest","southeast","southwest"),labels = c(0:3)))
insurance1$smoker<-as.numeric(factor(insurance1$smoker,levels = c("no","yes"),labels = c(0,1)))
cor(insurance1)
my_palette <- colorRampPalette(c("white", "green", "red"))(n = 10)

heatmap(cor(insurance1),col=my_palette,main="Correlation Heatmap")
##Correlation heat map shows there exist strong correlation between smoking status and charges
##Plot of charges vs Age
par(mfrow=c(1,1))
  plot(insurance$age,insurance$charges)
  ##Regression Fit:
  ##NonSmoker regression:
  samNsmoker<-insurance[which(insurance$smoker=="no"),]
  attach(samNsmoker)
  plot(age,charges,main="Age distibution of Medical Charges-Non-smoker",col=c("Seagreen"),xlab = "Age",ylab = "Charges")
  
  ##Backward selection method to select the predictors for Non-smoker Regression Analysis with significant level=0.05:
  fit.Nsmoker<-lm(charges~age+bmi+children+factor(region)+sex,data=samNsmoker)
  library(leaps)
  drop1(fit.Nsmoker,charges~age+bmi+children+factor(region)+sex,test="F")
  ##Since the P value of "BMI" is high(0.439265),we drop the predictor-"BMI"
  fit.2<-lm(charges~age+children+factor(region)+sex,data=samNsmoker)
  drop1(fit.Nsmoker,charges~age+children+factor(region)+sex,test="F")
  ##Since the P value of "sex" is high(0.062204),we drop the predictor-"sex"
  
  fit.3<-fit.2<-lm(charges~age+children+factor(region),data=samNsmoker)
  drop1(fit.Nsmoker,charges~age+children+factor(region),test="F")
  ##Since all the P values are less than 0.05,we reatin the predictors to fit the model
  ##The final model of a Non-smoker:
  fit.finalNsmoker<-lm(charges~age+children+factor(region),data=samNsmoker)
  summary(fit.finalNsmoker)
  
  ##Residual plots-Non-smoker:
  par(mfrow=c(1,2),mai=c(0.42,0.42,0.2,0.22),font.main=3)
  plot(residuals(fit.Nsmokerfinal),main = "Residual Plot-Non-smoker")
  qqnorm(residuals(fit.Nsmokerfinal),main="QQ Plot-Residuals(Non-Smoker)")
  qqline(residuals(fit.Nsmokerfinal))
  ##From the residual QQ plot, the outliers clearly shows that though being a non-smoker,the 
  ##individuals are charged more might be because of other ailments for which we dont have much data to discuss on
  
  ##Smoker regression:
  samsmoker<-insurance[which(insurance$smoker=="yes"),]
  attach(samsmoker)
  par(mfrow=c(1,3),mai=c(5,4,5,4),mar=c(5.1,4.1,4.1,2.1),oma=c(0,0,1,0))
  plot(age,charges,main = "Smoker",xlab = "Age",ylab = "Charges",col=c("Violet","Green"),pch=3)
  smokerobesse<-samsmoker[samsmoker$bmi>30,]
  smokerbminormal<-samsmoker[samsmoker$bmi<=30,]
  plot(smokerbminormal$age,smokerbminormal$charges,main="Normal BMI Smoker",xlab = "Age",ylab = "Charges",col="Navyblue")
  plot(smokerobesse$age,smokerobesse$charges,main = "Obesse Smoker",xlab="Age",ylab="Charges",col="Red")
  title("Charge Distribution Vs Age",outer = TRUE)
  ##The assumption that the residuals are normally distributed and const variance are satisfied
  
  ##Smoker-Normal BMI:
  nrow(insurance[(insurance$smoker=="yes"),])
  summary(insurance[(insurance$smoker=="yes"),])
  
  nrow(insurance[(insurance$smoker=="yes"&insurance$children>0),])
  ##Normal BMI smoker regression:
  ##Backward selection method to select the predictors for Normal BMI-smoker MEdical charges Prediction with significant level=0.05:
  fit.SmNl<-lm(charges~age+bmi+children+factor(region)+sex,data=smokerbminormal)
  drop1(fit.SmNl,charges~age+bmi+children+factor(region)+sex,test="F")
  ##Since the P value of "Region" is high(0.9318664 ),we drop the predictor-"Region"
  fit.SmNl1<-lm(charges~age+bmi+children+sex,data=smokerbminormal)
  drop1(fit.SmNl,charges~age+bmi+children+sex,test="F")
  ##Since the P value of "sex" is high(0.8527664),we drop the predictor-"sex"
  
  fit.SmNl3<-lm(charges~age+bmi+children,data=smokerbminormal)
  drop1(fit.SmNl,charges~age+bmi+children,test="F")
  ##Since the P value of "Children" is high(0.5191392),we drop the predictor-"Children"
  
  fit.SmNl1<-lm(charges~age+bmi,data=smokerbminormal)
  drop1(fit.SmNl,charges~age+bmi,test="F")
  ##Since all the the predictors have P avlues less than 0.05,we retian the predictors-"Age" 
  ##and "BMI" in fitting the model to predict the medical chrages of a normal BMI smoker.
  ##The final model of a Normal BMI Smoker is given by:
  fit.final_Normal_Smoker<-lm(charges~age+bmi,data=smokerbminormal)
  summary(fit.final_Normal_Smoker)
  ##Residual plots-Normal BMI smoker
  par(mfrow=c(1,2),mai=c(0.42,0.42,0.2,0.22),font.main=3)
  
  plot(residuals(fit.finalsmokernormal),main = "Residual Plot")
  
  qqnorm(residuals(fit.smokerbminormal),main = "Residual QQ Plot")
  qqline(residuals(fit.smokerbminormal))
  title("Smoker with Normal BMI",outer = TRUE)
  ##The assumption that the residuals are normally distributed and const variance are satisfied
  
  ##Obesse BMI Smoker regression:
  summary(smokerobesse)
  nrow(smokerobesse[smokerobesse$children>0,])
  
  #Backward Selection method to select the predictors for Obese Smoker MEdical charge prediction with significant level=0.05:
  fit.SmOb<-lm(charges~age+bmi+children+factor(region)+sex,data=smokerobesse)
  drop1(fit.SmOb,charges~age+bmi+children+factor(region)+sex,test="F")
  ##Since the P value of "Children" is high(0.7057),we drop the predictor-"Children"
  
  fit.SmOb1<-lm(charges~age+bmi+factor(region)+sex,data=smokerobesse)
  drop1(fit.SmOb,charges~age+bmi+factor(region)+sex,test="F")
  ##Since the P value of "Region" is high(0.3242 ),we drop the predictor-"Region"
  
  fit.SmOb2<-lm(charges~age+bmi+sex,data=smokerobesse)
  drop1(fit.SmOb,charges~age+bmi+sex,test="F")
  ##Since all the the predictors have P avlues less than 0.05,we retian the predictors-"Age" 
  ##and "BMI" in fitting the model to predict the medical chrages of an Obese smoker.
  ##The final model of a Obese Smoker is given by:
  
  fit.final_Obese_Smoker<-lm(charges~age+bmi,data=smokerobesse)
  summary(fit.final_Obese_Smoker)
  
  ##Residual plots-Obese Smoker:
  par(mfrow=c(1,2),mai=c(0.42,0.42,0.2,0.22),font.main=3)
    plot(residuals(fit.finalsmokerobese),main = "Residual Plot")
    qqnorm(residuals(fit.finalsmokerobese),main = "Residual QQ Plot")
  qqline(residuals(fit.finalsmokerobese))
  title("Obese Smoker",outer = TRUE)
  ##The assumption that the residuals are normally distributed and const variance are satisfied
  
  ##CART Analysis:
  tree1 <- tree(charges~age+bmi+children+(region)+sex+smoker, data = insurance)
  plot(tree1)
  text(tree1)
  title("Decision Tree")
  tree1
  summary(tree1)
  
