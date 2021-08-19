library(tidyverse)
library(lubridate)
library(plotly)
library(dplyr)
library(fitdistrplus)
library(rmarkdown)
library(reshape2)
library(ggplot2)

insurance <- read.csv('C:/Users/meera/Desktop/US_Health_Insurance.csv', header = TRUE, sep = ',')
str(insurance)
total<-count(insurance)
total

#*****************************QUESTION 1.a****************************************************************************
# Scatter Plot:Meets_BMI VS Age

Meets_BMI <- insurance %>% mutate(bmi_group = case_when(
  bmi >= 0  & bmi <= 18.5 ~ 'Underweight',
  bmi > 18.5  & bmi < 25 ~ 'Normal',
  bmi >= 25  ~ 'Overweight'))
Meets_BMI

ggplot(data = Meets_BMI) + 
  geom_point(mapping = aes(x = age, y = charges, color = bmi_group)) +
  geom_hline(yintercept = 30000)

#******************************QUESTION 1.b********************************************************************
BMI_Underweight <- subset(insurance,insurance$bmi<18.5 )
freq_Underweight<-nrow(BMI_Underweight)
freq_Underweight

BMI_Normal <- subset(insurance,(insurance$bmi>=18.5 & insurance$bmi<25))
freq_Normal <- nrow(BMI_Normal)
freq_Normal

BMI_Overweight <- subset(insurance,(insurance$bmi>=25 & insurance$bmi<30))
freq_Overweight <- nrow(BMI_Overweight)
freq_Overweight

BMI_Obese <- subset(insurance,(insurance$bmi>=30))
freq_Obese <- nrow(BMI_Obese)
freq_Obese

Normal_Smoker <- subset(BMI_Normal,BMI_Normal$smoker=='yes')
freq_Normal_Smoker<-nrow(Normal_Smoker)
freq_Normal_Smoker

Overweight_Smoker <- subset(BMI_Overweight,BMI_Overweight$smoker=='yes')
freq_Overweight_Smoker <- nrow(Overweight_Smoker)
freq_Overweight_Smoker

Underweight_Smoker <- subset(BMI_Underweight,BMI_Underweight$smoker=='yes')
freq_Underweight_Smoker <- nrow(Underweight_Smoker)
freq_Underweight_Smoker 

Obese_Smoker <- subset(BMI_Obese,BMI_Obese$smoker=='yes')
freq_Obese_Smoker <- nrow(Obese_Smoker) 
freq_Obese_Smoker  

#dat <- data.frame(category= factor(c("Underweight","Normal","Overweight","Obese")), levels=c("Underweight","Normal","Overweight","Obese),count = c(freq_Underweight_Smoker ,freq_Normal_Smoker, freq_Overweight_Smoker,freq_Obese_Smoker))
dat <- data.frame(category= factor(c("Underweight","Normal","Overweight","Obese"), levels=c("Underweight","Normal","Overweight","Obese")),
                  count = c(freq_Underweight_Smoker,freq_Normal_Smoker, freq_Overweight_Smoker,freq_Obese_Smoker))
p <- ggplot(data=dat, aes(x=category, y=count)) +
  geom_bar(stat="identity",color="blue", fill=rgb(0.1,0.4,0.5,0.7))
p <- ggplotly(p)
p

#*****************************QUESTION 1.c****************************************************************************
#Line graph : Smoker VS age VS charges
ggplot(data = insurance) +
  geom_line(mapping = aes(x = age, y = charges, color=smoker))

#****************************QUESTION 2*******************************************************
#Calculating joint frequency

data_age <- insurance %>% mutate(age_group = case_when(
  age >= 10  & age <= 19 ~ '>=10<20',
  age >= 20  & age <= 29 ~ '>=20<30',
  age >= 30  & age <= 39 ~ '>=30<40',
  age >= 40  & age <= 49 ~ '>=40<50',
  age >= 50  & age <= 59 ~ '>=50<60',
  age >= 60 ~ '>=60<70'))

data_age

age_freq <- data_age %>%
  group_by(age_group) %>%
  summarise(count = n()) %>%
  mutate(pmf = count/sum(count)) %>%
  mutate(cdf = cumsum(pmf))
age_freq

child_freq <- data_age %>%
  group_by(children) %>%
  summarise(count1 = n()) %>%
  mutate(pmf1 = count1/sum(count1)) %>%
  mutate(cdf1 = cumsum(pmf1))
child_freq

joint_freq <- outer(age_freq$count[1:6], child_freq$count1[1:6], FUN = "+")
rownames(joint_freq) <- age_freq$age_group[1:6]
colnames(joint_freq) <- child_freq$children[1:6]
joint_freq

joint_prob <- round(joint_freq/sum(joint_freq),5)
joint_prob

joint_df <- melt(joint_freq)
colnames(joint_df) <- c('age_group', 'children', 'frequency')
head(joint_df, 100)

#*******************************QUESTION 3*************************************************************
#correlation coefficient
cor(age_freq$count, child_freq$count1)
#cor(insurance$age, insurance$children)

#heatmap
ggplot(joint_df, aes(age_group,children, fill= frequency)) + 
  geom_tile() +
  scale_fill_gradient(low="white", high="blue")

#scatterplot
ggplot(data = joint_df,aes(x=age_group, y=children)) +
  geom_point(aes(color = frequency , size=frequency)) +
  labs(x = 'age_group', y = 'children') +
  scale_x_discrete("age_group", labels = as.character(joint_df$age_group),
                   breaks = joint_df$age_group) +
  scale_y_discrete("children", labels = as.character(joint_df$children),
                   breaks = joint_df$children)


#************************QUESTION 4*******************************************************************
insurance_sample <- sample_n(insurance, 1000)

reqd_row1 <- insurance %>%
  dplyr::filter(children==0 & region=='southwest')
count(reqd_row1)
reqd_row2 <- insurance %>%
  dplyr::filter(children>0 & region=='southwest')
reqd_row2
z_test2 = function(a, b, var_a, var_b){
  n.a = length(a)
  n.b = length(b)
  z = (mean(a) - mean(b)) / (sqrt((var_a)/n.a + (var_b)/n.b))
  return(z)
}

z_test2(reqd_row1$charges,reqd_row2$charges,var(reqd_row1$charges),var(reqd_row2$charges))

#Box plot
ggplot(data = reqd_row1) + 
  geom_boxplot(mapping = aes(y = charges))
ggplot(data = reqd_row2) + 
  geom_boxplot(mapping = aes(y = charges))


#*******************************QUESTION 5********************************************************************
reqd_row3 <- insurance %>%
  dplyr::filter(region=='southwest'& smoker=='yes')
prop1<-nrow(reqd_row3)
prop1

reqd_row4 <- insurance %>%
  dplyr::filter(region=='southeast'& smoker=='yes')
prop2<-nrow(reqd_row4)
prop2

pro<-prop.test(c(58,91),c(1338,1338))
pro

pro$p.value

#**************************QUESTION 6******************************************************************
#test ratio of proportion
var1 <- insurance %>% dplyr::select(age,region,charges) %>%
  dplyr::filter(age > 50)

count(var1)

var2 <- insurance %>% dplyr::select(age,region,charges) %>%
  dplyr::filter(age < 50)

count(var2)

#Using the function var.test to calculate the Fcalc
var.test(var1$age,var2$age,alternative = "two.sided",conf.level = 0.95)

#*******************************QUESTION 7***************************************************
#Continuous-gamma, norm,lnorm
ggplot(insurance, aes(bmi)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')

# descriptive statistics
descdist(insurance$bmi)

fit_n <- fitdist(insurance$bmi, "norm")
summary(fit_n)

# for lognormal
fit_ln <- fitdist(insurance$bmi, "lnorm")
summary(fit_ln)

#for gamma 
fit_g <- fitdist(insurance$bmi, "gamma")
summary(fit_g)

# goodness-of-fit plots : Gamma distribution
par(mfrow=c(2,2))
plot.legend <- c("gamma")
denscomp(list(fit_g), legendtext = plot.legend, xlab = '(x)', xlegend = 'topleft')
cdfcomp (list(fit_g), legendtext = plot.legend, xlab = '(x)')
qqcomp  (list(fit_g), legendtext = plot.legend, xlab = '(x)')
ppcomp  (list(fit_g), legendtext = plot.legend, xlab = '(x)')

#Children-Discrete Data - Negative binomial
ggplot(insurance, aes(children)) +
  geom_histogram(bins = 50, color = 'Black', fill = 'steelblue')

# get paramter estimates for negative binomial distribution
fit_nb <- fitdist(insurance$children, "nbinom",discrete = TRUE )
summary(fit_nb)

# get paramter estimates for poisson distribution
fit_p <- fitdist(insurance$children, "pois",discrete = TRUE )
summary(fit_p)

# goodness-of-fit tests
gofstat(list(fit_nb, fit_p))

#*************************QUESTION 8**************************************************
#ggplot(data = insurance) +
 # geom_line(mapping = aes(x = age, y = charges, color=Meets_BMI, linetype="dotted"))

#test ratio of proportion 
a <- insurance %>% dplyr::select(age,bmi,charges,smoker) %>%
  dplyr::filter(bmi>=18.5 & bmi<=24.9)

count(a)

b <- insurance %>% dplyr::select(age,bmi,charges,smoker) %>%
  dplyr::filter(bmi>0 & bmi<18.5 |bmi>24.9  )

count(b)

#Using the function var.test to calculate the Fcalc
var.test(a$age,b$age,alternative = "two.sided",conf.level = 0.95)

