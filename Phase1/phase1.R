

insurance <- read.csv("insurance.csv")
insurance <- data.frame(insurance)

######## Q0 part c ####
any(is.na(insurance))

######## Q1 part a ####

qqnorm(insurance$bmi,col = "darkolivegreen4")


####### Q1 part b ####

library(RColorBrewer)
max(insurance$bmi)
min(insurance$bmi)
(max(insurance$bmi)-min(insurance$bmi)) / 4
min(insurance$bmi)+(max(insurance$bmi)-min(insurance$bmi)) / 4
min(insurance$bmi)+2*(max(insurance$bmi)-min(insurance$bmi)) / 4
min(insurance$bmi)+3*(max(insurance$bmi)-min(insurance$bmi)) / 4
g1 <- 0
g2 <- 0
g3 <- 0
g4 <- 0
for(i in 1:1338){
  if(insurance$bmi[i] <= 25.2525)
    g1 <- g1+1
  else if((insurance$bmi[i] > 25.2525) & (insurance$bmi[i] <= 34.545))
    g2 <- g2+1
  else if((insurance$bmi[i] > 34.545) & (insurance$bmi[i] <= 43.8775))
    g3 <- g3+1    
  else
    g4 <- g4+1
}
myPalette <- brewer.pal(4, "Set2") 
slices <- c(g1,g2,g3,g4)
lbls = c("G1","G2","G3","G4")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="")

pie(slices, labels = lbls,
    main="Pie Chart of Species\n" , border="white", col=myPalette) 

####### Q1 part c ####

hist(insurance$bmi, col = "indianred3" , border = "white" , breaks = 20)

####### Q1 part d ####

density = density(insurance$bmi)
plot(density, col = "indianred3" , lwd = 3)

####### Q1 part e ####

library(e1071)
skewness(insurance$bmi)

####### Q1 part f ####
mean(insurance$bmi)
var(insurance$bmi)
sd(insurance$bmi)

####### Q1 part g ####

boxplot(insurance$bmi,boxwex=0.5, border=c("midnightblue"), col=c("indianred3"), xlab = "insurance$bmi")
Q1 <- max(min(insurance$bmi), quantile(insurance$bmi, c(0.25)))
lower_whisker <- Q1 - 1.5 * IQR(insurance$bmi)  
Q3 <- min(max(insurance$bmi), quantile(insurance$bmi, c(0.75)))
upper_whisker <- Q3 + 1.5 * IQR(insurance$bmi)  
IQR(insurance$bmi)
Q1
Q3
lower_whisker 
upper_whisker                      
####### Q1 part h ####                    
library(dplyr)
# filter(insurance$bmi > upper_whisker)
filter(select(insurance, bmi), bmi > upper_whisker)

####### Q2 part a ####                    

count <- table(insurance$region)
barplot(count, col = "midnightblue", border = "yellow",lwd = 3 , main = "insurance$region", cex.names=2, axis.lty=1 )


####### Q2 part b ####  

count <- table(insurance$region)
count
count <- count[order(count,decreasing = TRUE)]
count
barplot(count, col = "midnightblue", border = "yellow",lwd = 3 , 
        main = "insurance$region", cex.names=1, horiz = TRUE,axis.lty=1)

####### Q2 part c ####

attach(insurance)
table(region)
detach(insurance)

####### Q2 part d ####

library(ggplot2)
ggplot(insurance, aes(x=region, y=bmi)) + geom_violin(aes(fill = insurance$region))

####### Q3 part a ####

ggplot(insurance, aes(x=charges, y=bmi)) + geom_point() #+ geom_smooth(method=lm)

####### Q3 part b ####

ggplot(insurance, aes(x=charges, y=bmi, color = insurance$smoker)) + geom_point() 

####### Q3 part c ####

cor.test(insurance$charges, insurance$bmi, method = c("pearson"))

####### Q3 part d ####

library(ggplot2)
library(ggExtra)
library(hexbin)
# bin <- hexbin(insurance$charges,insurance$bmi,xbins = 40)
# plot(bin)
p <-ggplot(insurance, aes(charges, bmi)) + stat_binhex() + geom_point()
p1 <- ggMarginal(p, type="histogram")
p1

####### Q3 part e ####

ggplot(insurance, aes(x=charges, y=bmi)) + geom_density_2d()

####### Q4 part a ####
library(GGally)
library(corrgram)
corrgram(insurance, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt) 
corrgram(insurance, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt) 

corrgram(insurance, order=TRUE, lower.panel=panel.ellipse,
         upper.panel=panel.pts, text.panel=panel.txt, diag.panel=panel.minmax) 
ggcorr(insurance, method = c("everything", "pearson"))

####### Q4 part c ####

library(ggcorrplot)
library(dplyr)
library(corrplot)
filter_data <- filter(select(insurance, age, bmi, children, charges))
corr <- cor(filter_data[c('age','bmi', 'children', 'charges')])
color <- colorRampPalette(c("blue","white","red"))(8)
corrplot(corr, method = "color", addCoef.col = "black", diag = FALSE, col = color)

####### Q4 part d ####

library("scatterplot3d")
filter_data <- filter(select(insurance, age, bmi, charges))
scatterplot3d(filter_data, main="3D Scatter Plot", pch = 16, highlight.3d = TRUE)

####### Q4 part e ####
library("reprex")

library("scatterplot3d")
colors <- c("darkseagreen4","darksalmon")
colors <- colors[as.numeric(as.factor(insurance$smoker))]
filter_data <- filter(select(insurance, age, bmi, charges))
scatterplot3d(filter_data, main="3D Scatter Plot", pch = 16, color = colors, type = "h")

####### Q5 part a ####

ct1 <- table(insurance$smoker, insurance$region, dnn=c("insurance","region"))
prop.table(ct1)

####### Q5 part b ####

library(RColorBrewer)
library(ggplot2)
insurance <-filter(select(insurance,smoker,region,charges))
ggplot(insurance, aes(x=smoker,y=charges,fill = region)) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_brewer(palette = "Set2")

####### Q5 part c ####

library(ggplot2)

ggplot(insurance, aes(x=smoker,y=charges,fill = region)) + 
  geom_bar(stat="identity", position = "fill") + scale_fill_brewer(palette = "Set2") 

####### Q5 part d ####

tbl <- xtabs(~region + smoker, insurance)
mosaicplot(tbl, main = "insurance data", shade = TRUE)


####### Q6 part a ####
library(Rmisc)
insurance <- read.csv("insurance.csv")
conf = CI(insurance$charges, ci = 0.98)
low <- conf["lower"]
up <- conf["upper"]
conf

####### Q6 part c ####

g1 <- insurance$charges[1:446]
g2 <- insurance$charges[447:892]
g3 <- insurance$charges[893:1338]
m1 <- mean(g1)
m2 <- mean(g2)
m3 <- mean(g3)
yy <- c(m1,m2,m3)
data <-c(g1,g2,g3)
data <- data.frame(x)
xx <- c(g1,g2,g3)
ggplot(data = data, aes(y=yy, x= xx)) + geom_bar(stat="identity", position = position_dodge())
+geom_errorbar(aes(ymin=low, ymax=up), width=0.5,position=position_dodge(0.9))

####### Q6 part d ####

library(Rmisc)
conf = CI(insurance$charges, ci = 0.98)
conf
sam <- sample(insurance[,7], 35, replace = FALSE)
xbar <- conf["mean"]
xbar
s <- sd(insurance$charges)
s
m <- mean(sam)
m
se <- s/sqrt(35)
sqrt(35)
se
z = (xbar-m)/se
z
pvalue <- 2*pnorm(-abs(z))
pvalue

####### Q6 part e ####

library(Rmisc)
conf = CI(insurance$charges, ci = 0.95)
conf
sam <- sample(insurance[,7], 35, replace = FALSE)
xbar <- conf["mean"]
s <- sd(insurance$charges)
m <- mean(sam)
se <- s/sqrt(35)
z = (xbar-m)/se
pvalue <- 2*pnorm(-abs(z))
pvalue

####### Q6 part f ####
q = qnorm(p=0.05,mean=m, sd=se, lower.tail=FALSE)
beta <- pnorm(q, mean=xbar, sd=se)
beta
####### Q6 part g ####
power <- 1- beta
power

power.t.test(n = 100 , sd = s , sig.level = 0.05, type = "one.sample", alternative = "two.sided" , d= 3)



####### Q7 ####

sample_bmi <- sample(insurance[,3], 25, replace = FALSE)
sample_charges <- sample(insurance[,7], 25, replace = FALSE)
xbar1 <- mean(sample_charges)
xbar2 <- mean(sample_bmi)
s1 <- sd(sample_charges)
s2 <- sd(sample_bmi)
se = sqrt(s1^2/25 + s2^2/25)
t = (xbar1-xbar2)/se
2*pt(-abs(t),24)

####### Q8 ####

boxplot(insurance$age)
boxplot(insurance$bmi)
boxplot(insurance$charges)
boxplot(insurance$children)

library(boot)
calc_med <- function(data = insurance$charges, i) {
  d <- median(insurance$charges[i])
  return(d)
}

results <- boot(data=insurance$charges, statistic=calc_med,R=2000)
results
plot(results)
boot.ci(results)
quantile(results$t, c(0.025, 0.975))



sd <- sd(results$t)
se <- sd/sqrt(2000)
med <- median(results$t)
lower <- med - 1.96 * se
upper <- med + 1.96 * se
confidence_interval <- c(lower, upper)
confidence_interval


####### Q9 ####

cor.test(insurance$children, insurance$charges, method = c("pearson"))

data <- filter(select(insurance,children, charges))
anovaa <- aov(children~charges, data =data)
summary(anovaa)





