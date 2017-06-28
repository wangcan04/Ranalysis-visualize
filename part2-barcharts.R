#######################################
# Stats course lab assignment -- Part 2
# Pierre Dragicevic, Oct 2014
# Updated Sep 2015
#######################################

# We will load an existing data file. This is fairly easy.

# If you use RStudio, you can skip the group of comments below and jump to "Now load a csv file..."

# If you use R without RStudio, we first have to tell R where is our current working directory.
# Uncomment the last two lines and replace the path below by your own path.
# To find the path:
# - On Mac: from the finder, right-click on this R file and select "Get info...". The path appears in the info window. Copy it to the clipboard.
# - On Windows: from the explorer, shift-right-click on the folder containing this R file and choose "copy as path". Replace all backslashes by slashes.
#
#setwd("/Users/dragice/Dropbox/VA stats class/assignments")
#list.files()

# Now load a csv file as a table.

pollData <- read.table("poll.csv", header = TRUE, sep = ",")
pollData

# This table is different from the salesmen scenario seen during the lecture.
# It will allow us to practice with graphing and statistical tools first.

# Explanation: a policitian is considering two policies for reducing crime rate.
# She wants to get a first idea of the popularity of these two policies without
# attracting attention. As her statistician counselor, you randomly pick
# 10 people from the phone book and ask them to rate each policy on a scale
# from 0 to 10.

# Let us first display a scatterplot to show the range of values and how the two
# policies relate in terms of popularity.

plot(pollData$policy.1, pollData$policy.2)

# We can see that the two policies are not clearly correlated: some people prefer
# one, some prefer the other, some prefer both, and some prefer none. If we want,
# we can compute a correlation and verify it is quite low:

cor(pollData$policy.1, pollData$policy.2)

# However we will ignore correlations in this series of exercises because ideally,
# we should always provide correlations with confidence intervals (topic not
# covered here).

# Instead, we will calculate the mean popularity for each policy separately.
# Please provide the code for doing this.

# -----> [write the commands here]
 policy.1<-mean(pollData$policy.1)
 policy.2<-mean(pollData$policy.2)
# To better see the magnitude of the difference, we will display these means
# using a bar chart. We will first load ggplot, a library for more advanced
# charts than the ones natively supported by R.
# Try to execute the command below then immediately switch to the comments that
# follow.

library(ggplot2)

# You may see an error if the library is not installed. In this case, go to the R menu
# and install ggplot2 using the package installer. Click on "Get list" first.
# if the list is empty, select "Other repository" and type http://cran.univ-paris1.fr/ 
# Then type "ggplot2" in the search box. Select the package, check "Install
# dependencies", and click on "Install Selected". Wait until the packages are
# all installed and ignore error messages. Execute the instruction again:

library(ggplot2)

# Now you should see no error message.

# We will create a vector containing the two policy names
policy <- c("policy.1", "policy.2")

# Now please put the two mean popuralities in a vector named "mean", using a similar method

# -----> [write the command here]
mean <- c(policy.1, policy.2)
# Now create a new table from these two vectors, named "analyses"
# (refer to part1-intro.R on how to do)

# -----> [write the commands here]
 analyses <- data.frame(policy,mean)
# Now display the table's content:

analyses

# You should see this:
#     policy mean
# 1 policy.1  3.3
# 2 policy.2  7.1

# Now let's make a bar chart.

ggplot(data = analyses, aes(x = policy, y = mean)) + geom_bar(stat = "identity")

# The following command makes a bar chart with a white background.

ggplot(data = analyses, aes(x = policy, y = mean)) + geom_bar(stat = "identity") + theme_bw() 

# We will now customize the bar chart more extensively for a cleaner appearance.

# The following command:
# - makes thinner bars (width = 0.5)
# - changes the color of bars from black to gray (fill = "#999999")
# - sets the y range from 0 to 10 (ylim)
# - changes the y label (ylab)
# - removes the x axis title, ticks and grid, and removes y minor ticks (theme)
#   For other theme options type ?theme
barchart <- ggplot(data = analyses, aes(x = policy, y = mean)) + geom_bar(stat = "identity", width = 0.5, fill = "#999999") + theme_bw() + ylim(0, 10) + ylab("Mean support") + theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank())

# The chart is not displayed yet, because we only stored it into a variable.
# We now set the size of the plot window and display it.

dev.new(width=3, height=3)
barchart

# The two policies are obviously not equally popular, at least for those 10
# randomly chosen people. But how reliable is this data? With 10 people can we
# generalize to the entire population? To find out, open part3-confidence.R and
# follow the instructions.
