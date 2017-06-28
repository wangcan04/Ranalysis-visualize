#######################################
# Stats course lab assignment -- Part 1
# Pierre Dragicevic, Oct 2014
# Updated Sep 2015
#######################################

# Your assignment starts with this document. Please read the comments carefully.
# Feel free to edit this file by adding your own comments or commands.

# To start with, you will execute the line of code below. To do this,
# put your keyboard cursor somewhere on the line and hit Command+Enter (on Mac)
# or Ctrl+Enter (on Windows).

observations <- c(10, 2, 3, 6, 7, 12, 32, 18, 3, 0.5, 77, 2)

# now you should see the line of code echoed in the console. You just created a
# vector with the c (combine) command. Now execute the two lines of code below.

observations
mean(observations)

# This shows you the content of the variable "observations", then its mean.
# Below are 3 other similar commands. To execute them all at once, select
# the three lines and hit Command+Enter or Ctrl+Enter.
# Alternatively, you can copy and paste them to the console, and hit Enter.

median(observations)
min(observations)
max(observations)

# You can also type directly in the console. The function for computing the
# standard deviation is "sd". Please go in the console and compute the standard
# deviation for the data in "observations". The result should be 21.64289.

# From now on, please execute all instructions below using Command+Enter or 
# Control+Enter. Read the preceding comment first.

# Here is a simple way to plot the data:

plot(observations)

# The x axis shows the index of each observation (from 1 to 12), while the y axis
# shows their values. Now close the plot window to avoid too many open windows.
# The following command is equivalent to the previous one, but specifies explicitly
# that "observations" is the value for the parameter "x" of the plot function.
# When parameter names are not provided, the function assumes a default (here, x).

plot(x = observations)

# Now find out how to plot a line chart using the ?plot instruction. The question
# mark opens a help window on the given function.

# -----> [write the command here]
lines(observations)

# Now we will create another vector

times <- seq(from = 0, to = 110, by = 10)
times

# Check that the two vectors have the same length

length(observations)
length(times)

# Now we will create a table with these two vectors as columns

table <- data.frame(times, observations)
table

# Here is how to access the columns:

table$times
table$observations

# Or:

table[,1]
table[,2]

# And for rows:

table[1,]
table[12,]

# And individual cells:

table[1,1]
table[12,2]

# Now try this:

table$observations
log(table$observations)

# Some functions like log (logarithm) can be applied to entire vectors.
# This makes it easy to create new columns.

table$logged <- log(table$observations)
table

# Now plot the log of all observations as a function of time.

# -----> [write the command here]
plot(times,table$logged)

# You can also plot value distributions.

hist(table$logged)

# Good! We now know enough to switch to the second part of this assignment.
# Please open part2-barchart.R and follow the instructions.
# Leave this window open, you might need to refer back to it.
