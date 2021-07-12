library(ggplot2)
attach(df)

df <- read.csv(file = 'C:/Users/PC/Desktop/Height_Data.csv', sep = ";")
factor(df$Sport)
df %>%
  group_by(Sport) %>%
  skim(Height)


##  Do the heights of the players differ between the six sports? Conduct a global test.

#Assumpion 1: all assumptions are independent and collected in >2 independent cateorical groups 
# Label sport an set as categorical factors

df$Sport <- as.factor(df$Sport)

class(df$Sport)
#Assumption 2: dependent variable is continuous(Height)
#Assumption 3: Normal distributions of each group no major outliers

basketball <- subset(df,Sport=="basketball")
handball <- subset(df,Sport=="handball")
ice_hockey <- subset(df,Sport=="ice hockey")
soccer <- subset(df,Sport=="soccer")
volleyball <- subset(df,Sport=="volleyball")
water_polo <- subset(df,Sport=="water polo")

mean(basketball$Height)
mean(handball$Height)
mean(ice_hockey$Height)
mean(soccer$Height)
mean(volleyball$Height)
mean(water_polo$Height)

sd(basketball$Height)
sd(handball$Height)
sd(ice_hockey$Height)
sd(soccer$Height)
sd(volleyball$Height)
sd(water_polo$Height)

var(basketball$Height)
var(handball$Height)
var(ice_hockey$Height)
var(soccer$Height)
var(volleyball$Height)
var(water_polo$Height)

#Normality
qqnorm(basketball$Height,cex.lab=1.75,cex.axis=1.75,lwd="3",main='')
qqline(basketball$Height)

qqnorm(handball$Height,main="",cex.lab=1.75,cex.axis=1.75,lwd="3")
qqline(handball$Height)

qqnorm(ice_hockey$Height,main="",cex.lab=1.75,cex.axis=1.75,lwd="3")
qqline(ice_hockey$Height)

qqnorm(soccer$Height,main="",cex.lab=1.75,cex.axis=1.75,lwd="3")
qqline(soccer$Height)

qqnorm(volleyball$Heigh,main='',cex.lab=1.75,cex.axis=1.75,lwd="3")
qqline(volleyball$Height)

qqnorm(water_polo$Height,main="",cex.lab=1.75,cex.axis=1.75,lwd="3")
qqline(water_polo$Height)

ggplot(df, aes(x=Sport, y=Height, fill=Sport))+
  geom_boxplot() + xlab("Sport") + ylab("Height") + labs(fill = "Sport")+
  theme(legend.position = "none", plot.title = element_text(
    size=20,hjust = 0.5,face = "bold"),text = element_text(size=30)) 

# The 4 assumptions are met so we can run the one-way anova
summary(aov(Height ~ Sport, data = df))
# F value, The P value is much smaller than 0.05 which means
# that we can reject the null hypothesis and we conclude
# there is difference in height of players according to the
# type of sport but we dont know which sport height is longer
# or smaller than the other one 



## Are there pairwise differences between the heights of the players?


with(df,pairwise.t.test(Height,Sport,p.adjust.method = "none",pool.sd = TRUE))
# The p-value of the test is less than the significance level alpha = 0.05 (except 3 cases out of 15).
# We can conclude that one sport's average height is significantly different 
# from other sport’s average height with according p-value.


# The Bonferroni correction can be applied in the context.
# Here the correction is applied before we get the p-values
# The results are quite severe

with(df,pairwise.t.test(Height,Sport,p.adjust.method = "bonferroni",pool.sd = TRUE))

