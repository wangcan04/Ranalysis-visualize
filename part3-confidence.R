#######################################
# Stats course lab assignment -- Part 3
# Pierre Dragicevic, Oct 2014
# Updated Sep 2015
#######################################

# OK, now to the real deal. First, we make sure all preceding instructions
# have been executed: the data is loaded, processed, etc.

# If you use R without RStudio, replace the path below with your own path.
# setwd("/Users/dragice/Dropbox/VA stats class/assignments")
# source("part2-barcharts.R")

# Feel free to close the plot windows from the previous exercise.

# We will now demonstrate how to compute a bootstrap confidence interval using
# a toy example. Bootstrap CIs are not standard so we need to install and load
# the package. Make sure you installed the package as explained previously for
# ggplot.

library(boot)

# The toy example from part 1: just twelve numerical observations.

observations <- c(10, 2, 3, 6, 7, 12, 32, 18, 3, 0.5, 77, 2)

# We have enough to compute a confidence interval. First, we need to specify the
# statistic on which we want to compute our confidence interval. Here, we're 
# interested in the mean. So we define a function that returns the mean. The
# second parameter is an index that is required by the bootstrapping function.
# Select the next three lines and execute them.

samplemean <- function(x, d) {
  return(mean(x[d]))
}

# Let us first compute the best guess, i.e., the point estimate.

pointEstimate <- samplemean(observations)
pointEstimate

# This is exactly the same as computing the mean:

mean(observations)

# Now we generate 1000 bootstrap samples

bootstrap_samples <- boot(data = observations, statistic = samplemean, R = 1000)
bootstrap_samples

# Not much to see in textual form here. But we can plot the 1000 bootstrap samples:

dev.new(width=7, height=5)
plot(bootstrap_samples)

# This shows the sampling distribution of the mean estimated from the bootstrap
# samples. The distribution is a bit skewed to the right, which is shown by
# the right plot: had the distribution been normal, the plot would have fit a
# straight line. But we really don't have to worry about those details. This was
# just to show that we're only half-way through the process. We still need to
# extract the confidence interval from this sampling distribution, by removing
# the 5% extreme values from the left and right tails of the sampling distribution.
# We will use the BCa method, which is an improved method over what we saw in
# class. BCa stands for "Bias Corrected and Accelerated". No need to know more,
# it just works.

bootci <- boot.ci(bootstrap_samples, type = "bca")
bootci

# That's it. We now have the confidence interval. We only need to extract the
# numbers from the bootci data structure using the following trick.

lowerBound <- bootci$bca[4]
upperBound <- bootci$bca[5]

# Now here is how to present the result textually, e.g., in a scientific paper.

cat("The mean is ", pointEstimate, ", CI [", lowerBound, ", ", upperBound, "]. Confidence intervals are 95% BCa bootstrap confidence intervals.", sep="")

# Given the size of the confidence interval, it is absurd to give numbers with
# 3 to 6 decimal places. This is better:

cat("The mean is ", round(pointEstimate), ", CI [", round(lowerBound), ", ", round(upperBound), "]. Confidence intervals are 95% BCa bootstrap confidence intervals.", sep="")

# Now as an exercise, write a helper function called "bootstrapCI" that takes as
# input a vector of numbers and returns the 95% BCa confidence interval around
# the mean as a vector of three numbers: the point estimate, the lower confidence
# limit and the upper confidence limit. This will help us get rid of all those
# unnecessary computation details. You can put the samplemean function declaration
# inside.

bootstrapMeanCI <- function(data) {
	# -----> [write the function content here]
	samplemean <- function(x, d) {
  return(mean(x[d]))
}
 point_Estimate<-samplemean(data)
	bootstrap_data<- boot(data = data, statistic = samplemean, R = 1000)
	bootci <- boot.ci(bootstrap_data, type = "bca")
	result<-c(point_Estimate,bootci$bca[4],bootci$bca[5])
return(result)
}

# To test your function:

bootstrapMeanCI(observations)

# This should display something similar to this (remember that bootstrapping is random):
# [1] 14.37500  6.62500 35.27708

# Now back to our poll. Remember the bar chart was like this:

dev.new(width=3, height=3)
barchart

# The table used to construct it was:

analyses

# And it was build from this one:

pollData

# Now as an exercise, create two vectors of two numbers each. One should contain
# the lower CI bounds for each of the two policies, and the other one should
# contain their upper CI bounds.

# -----> [write the instructions here]
result1<-bootstrapMeanCI(pollData$policy.1)
result2<-bootstrapMeanCI(pollData$policy.2)
ci.lower<-c(result1[2],result2[2])
ci.upper<-c(result1[3],result2[3])

# Now add two extra columns to the "analyses" table, called "ci.lower" and
# "ci.upper", that contain the confidence intervals for each policy.

# -----> [write the instructions here]
 analyses <- cbind(analyses,ci.lower,ci.upper)
# Plotting the confidence intervals is fairly easy using ggplot. Let us extend
# the bar chart code from part2. We will do three changes:
# - We encapsulate it in a function to be able to reuse it
# - We make bars semi-translucent
# - We add a call to geom_pointrange, which adds error bars.

plotBarchartWithCIs <- function(data) {
	ggplot(data = data, aes(x = policy, y = mean)) + geom_bar(stat = "identity", width = 0.5, fill = "#00000010") + theme_bw() + ylim(0, 10) + ylab("Mean support") + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank()) + geom_pointrange(aes(ymin=ci.lower, ymax=ci.upper), size=0.7)
}

# Now let's call the function on our updated "analyses" table.

dev.new(width=3, height=3)
plotBarchartWithCIs(analyses)

# Conclude. Will a policy be likely more popular than the other among the entire
# population? Do we need a more extensive poll?
# (optional question) What is a possible pitfall of this method?
# -----> [write your conclusions here]
#It seems that a policy is more popular than the other,but I guess If we have more extensive poll,we could get the conclusions closer to the reality.For this method,contingency maybe cause big influence to the final result,If all the interviewers are belonged to same kind of people,their opinions can't stand for all the persons.So we need to get more data to avoid this situation.