#######################################
# Stats course lab assignment -- Part 5
# Pierre Dragicevic, Oct 2014
# Updated Sep 2015
#######################################

# (Optional question)
# The financial situation got better and the manager decided to keep all employees.
# Ten months later, you analyze the new data out of curiosity. The data is in
# sales-sequel.csv. Is this data consistent with a stable performance compared to
# sales.csv? Is it compatible with what you recommended before?

# -----> [quickly discuss here.]
# The data is nithern consistent with sales.csv nor compatible with my previous
# recommendation. In the new data set, seller 1,2,3,4,6's performances are close to
# each other according to the indicators while seller 5 outperforms them all.
###################################################################################

# (Very optional question -- only for the craziest ones)
# Verify that the data is approximately lognormal, and report exact confidence
# intervals on log-transformed data for the two datasets. Are there differences?

# -----> [put analysis & graphing code, and discussions here.]
#Verify
library(fitdistrplus)
library(logspline)
salesData2 <- read.table("sales-sequel.csv", header = TRUE, sep = ",")
dev.new(width=7, height=5)
#use descdist function get the graph to know the data close to which distribution
descdist(salesData2$Seller.1, discrete = FALSE)
descdist(salesData2$Seller.2, discrete = FALSE)
descdist(salesData2$Seller.3, discrete = FALSE)
descdist(salesData2$Seller.4, discrete = FALSE)
descdist(salesData2$Seller.5, discrete = FALSE)
descdist(salesData2$Seller.6, discrete = FALSE)
#then use fitdist function to verify if data is follow lognormal distribution or not.
fit.lnorm1 <- fitdist(salesData2$Seller.1, "lnorm")
fit.lnorm2 <- fitdist(salesData2$Seller.2, "lnorm")
fit.lnorm3 <- fitdist(salesData2$Seller.3, "lnorm")
fit.lnorm4 <- fitdist(salesData2$Seller.4, "lnorm")
fit.lnorm5 <- fitdist(salesData2$Seller.5, "lnorm")
fit.lnorm6 <- fitdist(salesData2$Seller.6, "lnorm")
#plot fitdist for each seller
dev.new(width=3, height=3)
plot(fit.lnorm1)
plot(fit.lnorm2)
plot(fit.lnorm3)
plot(fit.lnorm4)
plot(fit.lnorm5)
plot(fit.lnorm6)
#calculate aic to judge the differences between reality and discrete data 
fit.lnorm1$aic
fit.lnorm2$aic
fit.lnorm3$aic
fit.lnorm4$aic
fit.lnorm5$aic
fit.lnorm6$aic

#or  
library(boot)
#firstly,transformed all the data to log
salesData2<-log(salesData2)
samplemean <- function(x, d) {
  return(mean(x[d]))
}
bootstrap_samples1<- boot(data = salesData2$Seller.1, statistic = samplemean, R = 1000)
bootstrap_samples2<- boot(data = salesData2$Seller.2, statistic = samplemean, R = 1000)
bootstrap_samples3<- boot(data = salesData2$Seller.3, statistic = samplemean, R = 1000)
bootstrap_samples4<- boot(data = salesData2$Seller.4, statistic = samplemean, R = 1000)
bootstrap_samples5<- boot(data = salesData2$Seller.5, statistic = samplemean, R = 1000)
bootstrap_samples6<- boot(data = salesData2$Seller.6, statistic = samplemean, R = 1000)
dev.new(width=7, height=5)
#use descdist function get the graph to know the data close to which distribution
descdist(salesData2$Seller.1, discrete = FALSE)
descdist(salesData2$Seller.2, discrete = FALSE)
descdist(salesData2$Seller.3, discrete = FALSE)
descdist(salesData2$Seller.4, discrete = FALSE)
descdist(salesData2$Seller.5, discrete = FALSE)
descdist(salesData2$Seller.6, discrete = FALSE)
#plot PDF
plot(bootstrap_samples1)
plot(bootstrap_samples2)
plot(bootstrap_samples3)
plot(bootstrap_samples4)
plot(bootstrap_samples5)
plot(bootstrap_samples6)

#report exact confidence intervals
salesData1 <- read.table("sales.csv", header = TRUE, sep = ",")
salesData1<-log(salesData1)
bootstrapMeanCI <- function(data) {
	samplemean <- function(x, d) {
  return(mean(x[d]))
}
 point_Estimate<-samplemean(data)
	bootstrap_data<- boot(data = data, statistic = samplemean, R = 1000)
	bootci <- boot.ci(bootstrap_data, type = "bca")
	result<-c(point_Estimate,bootci$bca[4],bootci$bca[5])
return(result)
}
result11<-bootstrapMeanCI(salesData1$Seller.1)
result12<-bootstrapMeanCI(salesData1$Seller.2)
result13<-bootstrapMeanCI(salesData1$Seller.3)
result14<-bootstrapMeanCI(salesData1$Seller.4)
result15<-bootstrapMeanCI(salesData1$Seller.5)
result16<-bootstrapMeanCI(salesData1$Seller.6)
result21<-bootstrapMeanCI(salesData2$Seller.1)
result22<-bootstrapMeanCI(salesData2$Seller.2)
result23<-bootstrapMeanCI(salesData2$Seller.3)
result24<-bootstrapMeanCI(salesData2$Seller.4)
result25<-bootstrapMeanCI(salesData2$Seller.5)
result26<-bootstrapMeanCI(salesData2$Seller.6)
seller<-c("seller.1","seller.2","seller.3","seller.4","seller.5","seller.6")
results1<-data.frame(result11,result12,result13,result14,result15,result16)
results2<-data.frame(result21,result22,result23,result24,result25,result26)
results1<-data.frame(seller,t(results1))
results2<-data.frame(seller,t(results2))
colnames(results1) <- c("seller","mean","ci.lower","ci.upper")
colnames(results2) <- c("seller","mean","ci.lower","ci.upper")
library(ggplot2)
plotBarchart<- function(data) {
	ggplot(data = data, aes(x = seller, y = mean)) + geom_bar(stat = "identity", width = 0.5, fill = "#00000010") +theme_bw() +ylab("Mean support") + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + geom_pointrange(aes(ymin=ci.lower, ymax=ci.upper), size=0.9)
}
dev.new(width=3, height=3)
plotBarchart(results1)
plotBarchart(results2)
#CI graphs are in the Readme,for same dataset,the CI are always same.But for to different dataset the CIs are little different.
###################################################################################

# (Another very optional question for the craziest ones)
# What alternative method involving statistics would you have recommended to your
# manager to help him improve sales?
# Since the sale of a company is a long term behavior, what of concern is only the cumulation of a seller's total sale among a long period of time, but not the seller's probabilistic behavior at each day. Thus the setting of confidence interval could be removed and we only look at each seller's expectation. Besides, we may consider to use time-series analysis to predict each seller's performance in the future, which may help to decide wether the company should keep employ a particular seller or not.   