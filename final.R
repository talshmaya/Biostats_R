survey_data<-read.csv('survey.csv')

summary(survey_data)

attach(survey_data)

# source the functions created
source('Functions1.R')

#  Run your final model to get your parameter estimates

hbp.label<-c('Yes','No')
HD.label<-c('Yes','No')
GENDER.label<-c('Male', 'Female')
smk.label<-c('Yes','No')
eth.label<-c('Other','Other','Other','Other','Latino','White')
HBP <- as.factor(hbp.label[HBP])

DRINK <- as.factor(HD.label[DRINK])
GENDER <- as.factor(GENDER.label[GENDER])
SMOKE <- as.factor(smk.label[SMOKE])
ETHNICITY<-as.factor(eth.label[survey_data$ETHNICITY])

BMI <- (WT/(HT^2))*703
#survey_data$HD <- as.factor(HD.label[HD])
#survey_data$GENDER <- as.factor(GENDER.label[GENDER])
#survey_data$SMOKE <- as.factor(smk.label[SMOKE])
#survey_data$ETHNICITY<-as.factor(eth.label[survey_data$ETHNICITY])

attach(survey_data)

means.grp(WT,HD)
means.grp(BMI,PA)
means.grp(BMI,HBP)
means.grp(BMI,SMOKE)
means.grp(BMI>25,HD)
boxplot(BMI~PA)
boxplot(BMI~HD)

cor(cbind(WT,BMI,AGE,PA,HT,ETHNICITY,GENDER,SMOKE),use='pairwise.complete.obs')
cor.test(WT,AGE)$p.value
cor.test(WT,PA)$p.value
cor.test(WT,HT)$p.value
#cor.test(WT,GENDER)$p.value
#cor.test(WT,SMOKE)$p.value
#1c
cor(cbind(BMI,PA,ETHNICITY,GENDER,SMOKE),use='pairwise.complete.obs')
cor.test(WT,HT)$p.value
cor.test(BMI,as.numeric(PA))$p.value

# Plot HD vs. AGE.
library('graphics')
sunflowerplot(x=WT,y=HD,xlim=c(100,350),xlab='WEIGHT',yaxt='n',ylab='Heart Disease',seg.col='black')
axis(2,at=c(1,2),labels=c('HD','no HD'),las=1)
#same with BMI
sunflowerplot(x=BMI,y=HD,xlim=c(10,70),xlab='BMI',yaxt='n',ylab='Heart Disease',seg.col='black')
axis(2,at=c(1,2),labels=c('HD','no HD'),las=1)

# Make PA and SMOKE factors and assign value labels to them.
survey_data$PA<-as.factor(survey_data$PA)
survey_data$SMOKE<-as.factor(survey_data$SMOKE)
PA.label<-c('Low','High')
SMOKE.label<-c('No','Yes')
survey_data$PA<-as.factor(PA.label[survey_data$PA])
survey_data$SMOKE<-as.factor(SMOKE.label[survey_data$SMOKE])

#---------------------------------------------------------------------------
#  Fit the basic logistic regression model to the HD/WT relationship
#---------------------------------------------------------------------------
u.model1<-glm(relevel(as.factor(HD),'2')~WT,family='binomial'(link='logit'),data=survey_data)
# Provides the Maximum Likelihood Estimates
summary(u.model1)
# Performs a Likelihood Ratio Test
anova(u.model1,test='LRT')
# Computes the ORs and 95% CIs
exp(cbind(OR = coef(u.model1), confint(u.model1)))

# Plot the fitted trend line, and compare it to the observed survey_data distribution
plot(x=WT,y=HD,pch='o',ylim=c(1,2),xlim=c(100,350),xlab='WT',yaxt='n',ylab='Heart Disease')
points(x=WT,y=u.model1$fitted.values)
axis(2,at=c(1,2),labels=c('HD','no HD'))


#---------------------------------------------------------------------------------
#  Create new variables to contain the sample probability, odds, & log-odds that
#  describe whether a man will have HD
#---------------------------------------------------------------------------------
# Create 'binned up' version of WT to represent a man's WT paegory;
WT2<-round(WT/5)-8
#AGE2<-round(WT/5)-8

# Estimate probability a man will have HD, by averaging sample values of
# outcome variable HD, within each paegory of his WT.  Output results;
PHD<-means.grp(HD,as.factor(WT2))$mean

# Transform sample probabilities into odds and log-odds statistics, 
# within each paegory of the man's WT;
#  Eliminate WT paegories in which PHD is either 0 or 1;
PHD<-PHD[PHD!=1]

# Estimate probability that a man will NOT have HD;
NPHD<-1-PHD

# Estimate sample odds that a man will have HD vs. No HD;
ODDSHD<-PHD/(1-PHD)

# Estimate sample log-odds (logits) that a man will have HD vs. not;
LOG_ODDSHD<-log(-1*ODDSHD)

# Check linearity of relationship between log-odds of HD & predictors;
plot(0:38,LOG_ODDSHD,xlab='WT_pa',ylim=c(0,2))
points(loess.smooth(0:38,LOG_ODDSHD,span=1),type='l')


#-----------------------------------------------------------------------
#  Start the survey_data analysis
#-----------------------------------------------------------------------
# Descriptive Statistics
descript(BMI)
freq(HD)
freq(PA)
freq(SMOKE)
# Bivariate Analysis
round(exp(cbind(OR = coef(u.model1)*5, confint(u.model1)*5)),2)[2,]
#high physical activity
freq(HD[PA==3:7])[2,]
#low physical activity
freq(HD[PA==1:2])[2,]
freq(HD[BMI>30])[2,]
freq(HD[BMI<25])[2,]
freq(HD[SMOKE=='Yes'])
freq(HD[SMOKE=='No'])

PA.label<-c('Low','Low','High','High','High','High','High')
survey_data$PA<-as.factor(PA.label[survey_data$PA])
SMOKE.label<-c('No','Yes')
survey_data$SMOKE<-as.factor(SMOKE.label[survey_data$SMOKE])
#low pa
u.model2<-glm(relevel(as.factor(HD),'2')~relevel(PA,'Low'),family='binomial'(link='logit'),data=survey_data)
#not smoking
u.model3<-glm(relevel(as.factor(HD),'2')~relevel(SMOKE,'No'),family='binomial'(link='logit'),data=survey_data)
round(exp(cbind(OR = coef(u.model2), confint(u.model2))),2)[2,]
round(exp(cbind(OR = coef(u.model3), confint(u.model3))),2)[2,]

#-----------------------------------------------------------------------
#  Model building
#-----------------------------------------------------------------------
a.model1<-glm(relevel(as.factor(HD),'2')~WT,family='binomial'(link='logit'),data=survey_data)
a.model2<-glm(relevel(as.factor(HD),'2')~WT+relevel(PA,'Low'),family='binomial'(link='logit'),data=survey_data)
a.model3<-glm(relevel(as.factor(HD),'2')~WT+relevel(PA,'Low')+relevel(SMOKE,'No'),family='binomial'(link='logit'),data=survey_data)
a.model4<-glm(relevel(as.factor(HD),'2')~WT*relevel(PA,'Low')+relevel(SMOKE,'No'),family='binomial'(link='logit'),data=survey_data)

summary(a.model1)
round(exp(cbind(OR=coef(a.model1),confint(a.model1))),3)
summary(a.model2)
round(exp(cbind(OR=coef(a.model2),confint(a.model2))),3)
summary(a.model3)
round(exp(cbind(OR=coef(a.model3),confint(a.model3))),3)
summary(a.model4)
round(exp(cbind(OR=coef(a.model4),confint(a.model4))),3)[4,]

########################################
########################################


survey_data.sorted<-survey_data[order(survey_data$WT),]

attach(survey_data.sorted)

#test pa for low and high activity 
summary(WT[PA=='Low'])
summary(WT[PA=='High'])
#divide heights by median
summary(WT[HT==42:66])
summary(WT[HT==66:77])

a.model4<-glm(relevel(as.factor(HD),'2')~WT*relevel(PA,'Low')+relevel(SMOKE,'No'),family='binomial'(link='logit'),data=survey_data)
#a.model44<-glm(relevel(as.factor(HD),'2')~WT*relevel(PA,'High')+relevel(SMOKE,'No'),family='binomial'(link='logit'),data=survey_data)
summary(a.model4)

install.packWTs('multcomp')
library('multcomp')

c1<-matrix(c(0,0,1,0,75),1)
t1<-glht(a.model4, linfct=c1)
summary(t1)
exp(confint(t1)$confint)[1,]

c2<-matrix(c(1,0,0,1,50),1)
t2<-glht(a.model4, linfct=c2)
summary(t2)
exp(confint(t2)$confint)[1,]

c3<-matrix(c(0,0,1,1,0),1)
t3<-glht(a.model4, linfct=c3)
summary(t3)
exp(confint(t3)$confint)[1,]

c4<-matrix(c(1,1,1,0,70),1)
t4<-glht(a.model4, linfct=c4)
summary(t4)
exp(confint(t4)$confint)[1,]

# Proportion of SMOKE
summary(SMOKE)/length(SMOKE)


#logit.c.1<--4.4295+0.05299*AGE-2.09677*1+0.6184*0.60+0.09049*1*AGE
#logit.c.0<--4.4295+0.05299*AGE-2.09677*0+0.6184*0.60+0.09049*0*AGE
# PLot the fitted probability versus WT by PA controlling SMOKE
logit.c.1<--2.4295+0.07*WT-1.888*1+0.2184*0.6083+0.02049*1*WT
logit.c.0<--2.4295+0.07*WT-1.888*0+0.2184*0.6083+0.02049*0*WT
pHD.c.1<- 1/(1+exp(-logit.c.1))
pHD.c.0<- 1/(1+exp(-logit.c.0))

plot(x=WT,y=pHD.c.1,type='l',ylim=c(0.7,1.3),xlim=c(50,350),xlab='WT',ylab='Fitted Probability of HD',col='red')
lines(x=WT,y=pHD.c.0,type='l',ylim=c(0.7,1.3),xlim=c(50,350))

# ------------------------------------------------------------------------------------
#  Refitting the final model & conducting interesting general linear hypothesis tests
# ------------------------------------------------------------------------------------
# Two men, one w/ high and one w/ low PAecholamine level, WT is 75
# (1,75,1,H,75*1) vs. (1,75,0,H,75*0)
delta.a<-matrix(c(0,0,1,0,75),1)
test1<-glht(a.model4, linfct=delta.a)
summary(test1)

# Two men, one w/ high and one w/ low PAecholamine level, WT is 41
# (1,41,1,H,41*1) vs. (1,41,0,H,41*0)
delta.b<-matrix(c(0,0,1,0,150),1)
test2<-glht(a.model4, linfct=delta.b)
summary(test2)

# Two men, one w/ high PA & 41 years old, one w/ low PA & 75 years old
# (1,75,0,H,75*0) vs. (1,41,1,H,41*1)
delta.c<-matrix(c(0,34,-1,0,-300),1)
test3<-glht(a.model4, linfct=delta.c)
summary(test3)

# ----------------------------------------
# Goodness-of-Fit Tests and Measures
# ----------------------------------------
# R-Squared
R2<- 1-(a.model4$deviance/a.model4$null.deviance);R2

# Likelihood Ratio Test
LRT<-cbind(LRT=a.model4$null.deviance-a.model4$deviance,
           df=a.model4$df.null-a.model4$df.residual,
           p.value=1-pchisq(a.model4$null.deviance-a.model4$deviance,a.model4$df.null-a.model4$df.residual))
LRT

# Hosmer and Lemeshow Test
# install.packWTs('ResourceSelection')
# library(ResourceSelection)

# hl<-hoslem.test(a.model4$y, fitted(a.model4),g=10);hl
# cbind(hl$observed,hl$expected)

# # Plot the ROC Curve
# fitted<-predict(a.model4,type='response')
# roc.curve<-roc(a.model4$y~a.model4$fitted.values)
# plot(roc.curve)
# colAUC(roc.curve)

# # Plot the Sensitivities and Specificities for classifiPAion
# plot(x=roc.curve$thresholds,y=roc.curve$sensitivities,type='l',ylim=c(0,1),xlim=c(0,1),ylab='Sensitivity',xlab='Probability Level')
# points(x=roc.curve$thresholds,y=roc.curve$specificities,type='l',col='red')
# mtext(side=4,text='Specificity',srt=45,col='red')

# Question #4

u.model8<-glm(relevel(as.factor(HD),'1')~BMI,family='binomial'(link='logit'),data=survey_data)
# Provides the Maximum Likelihood Estimates
summary(u.model8)
# Performs a Likelihood Ratio Test
anova(u.model8,test='LRT')
# Computes the ORs and 95% CIs
exp(cbind(OR = coef(u.model8), confint(u.model8)))
