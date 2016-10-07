load("McLaughlin.Rdata")

# Plot the population
plot(pop, type="b", log="y")

# Plot the next step predictors in red
points(pop$year[index+1],f(ns,ws),pch=21,bg="red")

# Get the indeces for the years before 1971.
index2 = which(SanJose$year<1971)
# Get bootstrapped data
out=simulator(SJtransformed[index2], T=500, reps=10, fl = TRUE)
# Plot it
matplot(out, type="l", log="y")