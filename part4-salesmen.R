#######################################
# Stats course lab assignment -- Part 4
# Pierre Dragicevic, Oct 2014
# Updated Sep 2015
#######################################

# The last part of this assignment consists in finishing analyzing the salesman
# dataset seen during the class. Remember that we were able to produce a bar chart
# with means, but could not figure out how much trust we should put in it. Adding
# confidence intervals will address the issue and we will be able to report to our
# manager.

# The data file is sales.csv.
# Tip: use the "head" instruction to get a textual preview on large tables.
# Feel free to reuse the code seen in the parts 1, 2 and 3. Since there are 6
# columns, the code may be a bit repetitive (and error-prone), so you can use the
# for instruction if you prefer, but you don't have to. Syntax is:
# for (i in seq(1, 6)) {
#	...
# }

# Note that for the poll datasets, people were on rows. Now they are in columns.
# Still, you should compute confidence intervals for each column separately, like
# before. Only the unit of statistical analysis has changed.
#
# For the poll, we needed to analyze people's responses for each of the two policies
# separately, and generalize them to the entire population of people. We say that
# the unit of analysis is the person. This is almost always the case in
# psychology and medical research. 
#
# For the salesmen scenario, the problem is different. We are only interested in those
# six specific salesmen, not the entire population of possible salesmen.
# We need to examine sales performances across days for each salesman separately,
# and generalize these performances to the entire population of all possible days.
# We say that the unit of analysis is the day.
#
# It is always very important to clearly identify the unit of analysis. The inferences
# made can be very different.

###################################################################################

# -----> [write the analysis and graphing code here]


library(boot)
library(ggplot2)

salesData <- read.table("sales.csv", header = TRUE, sep = ",")
#function to calculate each seller's performance
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
#use bootstrapMeanCI function get each seller's mean,ci.lower and ci.upper
result1<-bootstrapMeanCI(salesData$Seller.1)
result2<-bootstrapMeanCI(salesData$Seller.2)
result3<-bootstrapMeanCI(salesData$Seller.3)
result4<-bootstrapMeanCI(salesData$Seller.4)
result5<-bootstrapMeanCI(salesData$Seller.5)
result6<-bootstrapMeanCI(salesData$Seller.6)
#merge all the results to one data frame
results<-data.frame(result1,result2,result3,result4,result5,result6)
seller<-c("seller.1","seller.2","seller.3","seller.4","seller.5","seller.6")
results<-data.frame(seller,t(results))
colnames(results) <- c("seller","mean","ci.lower","ci.upper")
#plotting the confidence intervals using ggplot
plotBarchart<- function(data) {
	ggplot(data = data, aes(x = seller, y = mean)) + geom_bar(stat = "identity", width = 0.5, fill = "#00000010") +theme_bw()+ylab("Mean support") + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + geom_pointrange(aes(ymin=ci.lower, ymax=ci.upper), size=0.9)
}
dev.new(width=3, height=3)
plotBarchart(results)

###################################################################################

# -----> [write your interpretations and your conclusions here. What are you going to
# report to the manager?]
# we judge the performance of each seller according to the following 3 indicators: 
# the lower and upper bound of the confidence interval and the expectation. From the # data set, it is clear to see that: , 
# seller 6's performances are the highest for all the 3 indicators；
# seller 3's performances are the lowest for all the 3 indicators；
# for the sellers 1,2 and 5, there performances on each indicator are quite close to # each other, which are better than seller 4 but worse than seller 6. In conclusion, # we rank the total performance of the sellers as:
# seller 6 > seller 1,2, and 5 > seller 4 > seller 3