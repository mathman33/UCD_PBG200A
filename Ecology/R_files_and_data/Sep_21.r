rm(list=ls()) # clear active memory

# load the data
sivan=read.csv("cranes-2013.csv")
# save the data to global variables
N = sivan$Count
year = sivan$Year

# Plot the data
plot(year,N,pch=4)

# Take the log of the data and attempt a linear regression
y = log(N)
savin = lm(y~year)

# Plot the linear model (exponentiated) on top of the data
lines(year,exp(savin$fitted.values))

# Print a summary (in the console, or stdout) of the linear regression.
summary(savin)
