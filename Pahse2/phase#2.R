
insurance <- read.csv('insurance.csv')
insurance <- data.frame(insurance)
count(insurance)

######### Q1 part a #############



# ct1 <- table(insurance$smoker, insurance$region, dnn=c("insurance","region"))
# ptable <- prop.table(ct1)
# ptable
# smokeyes <- ptable[2]+ptable[4]+ptable[6]+ptable[8]
# northeast <- ptable[1]+ptable[2]
# pe <- northeast - smokeyes
# 
# se <- sqrt((northeast*(1-northeast)/324)+(smokeyes*(smokeyes)/68))
# 
# low <- (pe)-1.96 * se
# up <- (pe)+1.96 * se
# ci <- c(low,up)
# ci


n_region <- table(insurance$region)
n_region
n_northeast <- n_region[1]
n_others <- n_region[2]+n_region[3]+n_region[4]
p_hat1 <- ptable[2]/(ptable[1]+ptable[2])
p_hat2 <- ptable[4]+ptable[6]+ptable[8]/(ptable[3]+ptable[4]+ptable[5]+ptable[6]+ptable[7]+ptable[8])


SE <- sqrt((p_hat1*(1-p_hat1)/n_northeast)+(p_hat2*(1-p_hat2)/n_others))

lower <- (p_hat1-p_hat2)-1.96 * SE
upper <- (p_hat1-p_hat2)+1.96 * SE
CI <- c(lower,upper)
CI

SE
CI


######### Q1 part b #############

# n_success1 <-n_region[1]*ptable[2]
# n_success2 <-(n_region[2]+n_region[3]+n_region[4])*(ptable[4]+ptable[6]+ptable[8])
# 
# p_pool <- (n_success1 + n_success2)/ (n_region[1]+n_region[2]+n_region[3]+n_region[4])
# p_pool
# 
# SE_pool <- sqrt(p_pool*(1-p_pool)*(1/n_northeast+1/n_others))
# SE_pool
# 
# point_estimate <- p_hat1 - p_hat2
# 
# z <- (point_estimate - 0) / SE_pool
# 
# pvalue <- 2*pnorm(-abs(z))
# pvalue

###### check sample size condition #####
ct1 <- table(insurance$smoker, insurance$region, dnn=c("insurance","region"))
ptable <- prop.table(ct1)
ptable
n_region <- table(insurance$region)
n_region

n_region[1]*ptable[2]
n_region[2]*ptable[4]
n_region[3]*ptable[6]
n_region[4]*ptable[8]


q1_b_table <- table(insurance$region, insurance$smoker)
chisq.test(q1_b_table)



######### Q2 #############

sample_smokers <- sample(insurance$smoker, size=14, replace = FALSE)
count_smokers <- sum(c(sample_smokers == 'yes'))
count_smokers
success_per_iter = c()
for(i in cbind(1:2000)){
  yes <- sum(sample(0:1,14,replace = TRUE))
  success_per_iter[i] <- yes
}
total_success <- sum(c(success_per_iter > 14-count_smokers)+c(success_per_iter < count_smokers))
success_rate <- total_success/2000
success_rate

######### Q3 part a #############
library(dplyr)

n_region <- prop.table(table(insurance$region))
n_region

sample_unbiased <- insurance[sample(nrow(insurance), 100), ]
prop_dist1 <- table(sample_unbiased$region)
prop_dist1
chisq.test(prop_dist1,n_region)

filter_by_sex <- filter(select(insurance,sex,region), insurance$sex == 'female')

sample_biased <- filter_by_sex[sample(nrow(filter_by_sex), 100), ]
prop_dist2 <- table(sample_biased$region)
prop_dist2
chisq.test(prop_dist2,n_region)


######### Q3 part b #############

q3_b_table <- table(insurance$region, insurance$sex)
chisq.test(q3_b_table)


######### Q4 part a #############

model_4_a <- lm(insurance$charges ~ insurance$age)
######### Q4 part b #############

summary(model_4_a)
######### Q4 part c #############

plot(x=insurance$age, y=insurance$charges, main = "scatter plot",
     xlab = "insurance$age", ylab = "insurance$charges",
     pch = 19, frame = TRUE)
abline(model_4_a, col = "blue", lwd=3)

######### Q4 part d1 #############

sample_4_d <- insurance[sample(nrow(insurance), 27), ]
model_4_d1 <- lm(charges ~ age , data = sample_4_d)
summary(model_4_d1)

2*pt(-abs(2.011), df=25)

######### Q4 part d2 #############


tstar <- abs(qt(0.025,df=25))
pointEstimate = 272.2
SE_b1 = 135.3

lower <- pointEstimate - tstar*SE_b1
upper <- pointEstimate + tstar*SE_b1
ci <- c(lower,upper)
ci

######### Q4 part d3 #############


model_4_d3 <- lm(charges ~ children+age ,data = sample_4_d)
summary(model_4_d3)

anova(model_4_d1)
anova(model_4_d3)

######### Q5 part a #############
## backward elimination##
model_5_a_1 <- lm(charges ~ age+sex+bmi+children+smoker+region, data = insurance)
summary(model_5_a_1)
model_5_a_2 <- lm(charges ~ age+bmi+children+smoker+region, data = insurance)
summary(model_5_a_2)
model_5_a_3 <- lm(charges ~ age+bmi+children+smoker, data = insurance)
summary(model_5_a_3)
## forward selection ##

model1 <- lm(charges ~ age, data = insurance)
model2 <- lm(charges ~ sex, data = insurance)
model3 <- lm(charges ~ bmi, data = insurance)
model4 <- lm(charges ~ children, data = insurance)
model5 <- lm(charges ~ smoker, data = insurance)
model6 <- lm(charges ~ region, data = insurance)
summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared
summary(model4)$adj.r.squared
summary(model5)$adj.r.squared
summary(model6)$adj.r.squared


model21 <- lm(charges ~ smoker + age, data = insurance)
model22<- lm(charges ~ smoker + sex, data = insurance)
model23 <- lm(charges ~ smoker + bmi, data = insurance)
model24 <- lm(charges ~ smoker + children, data = insurance)
model25 <- lm(charges ~ smoker + region, data = insurance)
summary(model21)$adj.r.squared
summary(model22)$adj.r.squared
summary(model23)$adj.r.squared
summary(model24)$adj.r.squared
summary(model25)$adj.r.squared


model31 <- lm(charges ~ smoker + age + sex, data = insurance)
model32 <- lm(charges ~ smoker + age + bmi, data = insurance)
model33 <- lm(charges ~ smoker + age + children, data = insurance)
model34 <- lm(charges ~ smoker + age + region, data = insurance)
summary(model31)$adj.r.squared
summary(model32)$adj.r.squared
summary(model33)$adj.r.squared
summary(model34)$adj.r.squared



model41 <- lm(charges ~ smoker + age + bmi + sex, data = insurance)
model42 <- lm(charges ~ smoker + age + bmi + children, data = insurance)
model43 <- lm(charges ~ smoker + age + bmi + region, data = insurance)
summary(model41)$adj.r.squared
summary(model42)$adj.r.squared
summary(model43)$adj.r.squared

model51 <- lm(charges ~ smoker + age + bmi + children + sex, data = insurance)
model52 <- lm(charges ~ smoker + age + bmi + children + region, data = insurance)
summary(model51)$adj.r.squared
summary(model52)$adj.r.squared


model61 <- lm(charges ~ smoker + age + bmi + children + region + sex, data = insurance)
summary(model61)$adj.r.squared

######### Q5 part b #############

library(caret)
data_ctrl <- trainControl(method = "cv", number = 5)
model_caret <- train(charges ~ age + bmi + children + smoker + sex + region, data = insurance,                        
                     trControl = data_ctrl, method = "lm",na.action = na.pass)               

model_caret
model_caret$finalModel

######### Q5 part c #############
##linearity
plot(model52$residuals ~ insurance$age)
plot(model52$residuals ~ insurance$bmi)
# plot(model52$residuals ~ insurance$smoker)
plot(model52$residuals ~ insurance$children)
# plot(model52$residuals ~ insurance$region)

## nearly normal residuals
hist(model52$residuals)
qqnorm(model52$residuals)
qqline(model52$residuals)

##constant variability
plot(model52$residuals, model52$fitted.values)

######### Q5 part d #############

library(GGally)
library(corrgram)
corrgram(insurance, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt) 
corrgram(insurance, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt) 

corrgram(insurance, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax) 
ggcorr(insurance, method = c("everything", "pearson"))

######### Q5 part e #############

summary(model52)$r.squared
summary(model52)$adj.r.squared

######### Q6 part a #############

model_6_a <- glm(as.factor(smoker) ~ charges+age+children,data = insurance,family = binomial(link = "logit"))
summary(model_6_a)


######### Q6 part c #############

filter_dataa <- select(insurance,age,children,smoker,charges)

library(pROC)

prob <- predict(model_6_a,type=c("response"))
ROC <- roc(filter_dataa$smoker ~ prob, data = filter_dataa)
plot(ROC)
auc(ROC)

######### Q6 part d #############

exp(cbind("Odds ratio" = coef(model_6_a), confint.default(model_6_a, level = 0.98)))

zstar <- abs(qnorm(0.01))
PE <- c(2.966e-04,-8.626e-02,-1.601e-01)
SE <- c( 1.938e-05, 1.063e-02,1.081e-01)

lower <- PE - zstar*SE
upper <- PE + zstar*SE

log_ci_charges <- c(lower[1],upper[1])
log_ci_age <-  c(lower[2],upper[2])
log_ci_children <-  c(lower[3],upper[3])
exp(log_ci_charges)
exp(log_ci_age)
exp(log_ci_children)

######### Q7 part a #############


model_7_a <- glm(as.factor(smoker) ~ charges+age+children+sex+bmi+region,data = insurance,family = binomial(link = "logit"))
summary(model_7_a)

######### Q7 part b #############

model_7_b <- glm(as.factor(smoker) ~ sex,data = insurance,family = binomial(link = "logit"))
summary(model_7_b)
oddsratio <- exp(0.3804)
oddsratio
p1 <- c()
p2 <- c()
j <- 1
for(i in seq(from=0, to=1, by=0.001)){
  p_smoke_female <-i
  p_smoke_male <- (oddsratio*p_smoke_female/(1-p_smoke_female))/(1+(oddsratio*p_smoke_female/(1-p_smoke_female)))
  p1[j] <- p_smoke_female
  p2[j] <- p_smoke_male
  
  j = j +1 
  
}

plot(p1,p2,xlim=c(0,1),ylim=c(0,1),xlab="p(smoker|female)",ylab="p(smoker|male)",main = "Odds Ratio Curve")
abline(c(0,0),c(1,1),col= 'blue', lwd = 2)


######### Q7 part c #############

model_7_c <- glm(as.factor(smoker) ~ charges,data = insurance,family = binomial(link = "logit"))
summary(model_7_c)

######### Q7 part d #############


predictTrain = predict(model_7_c, type="response")
summary(predictTrain)
tapply(predictTrain, insurance$smoker, mean)
sens <- c()
spec <- c()
j <- 1
for(i in seq(0,1,0.1)){
  table_7_d <- table(insurance$smoker, predictTrain > i)
  sens[j] <- table_7_d[4]/(table_7_d[4]+table_7_d[3])
  spec[j] <- table_7_d[1]/(table_7_d[2]+table_7_d[1])
  j = j+1
}
sens
spec



######### Q7 part e #############

u <- c()
threshold <- c()
sensitivity <- c()
specificity <- c()
j <- 1
for(i in seq(0,1,0.0001)){
  table_7_e <- table(insurance$smoker, predictTrain > i)
  
  sensitivity[j] <- table_7_e[4]/(table_7_e[4]+table_7_e[3])
  specificity[j] <- table_7_e[1]/(table_7_e[2]+table_7_e[1])
  u[j] <- table_7_e[1]+table_7_e[2]+(-50)*table_7_e[3]+5*table_7_e[4]
  threshold[j] <- i
  print(u[j])
  j = j+1
}

max_index <- which.max(u)
threshold[max_index]
plot(threshold,u,main='Utility curve')
sensitivity[max_index]
specificity[max_index]


######### Q8 #############

high_medical_cost <- numeric(nrow(insurance))

high_medical_cost[insurance$charges < median(insurance$charges)] <- FALSE
high_medical_cost[insurance$charges > median(insurance$charges)] <- TRUE

# mean(insurance$charges)
# median(insurance$charges)
# high_medical_cost
model_8 <- glm(factor(high_medical_cost) ~ age+sex+bmi+children+region+smoker,
               data = insurance,family = binomial)
summary(model_8)








