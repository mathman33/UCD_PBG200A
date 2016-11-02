#getting the San Jose data set (data thiefed)
load("../../R_files_and_data/McLaughlin.Rdata")

pre1971indices=which(SanJose$year<1971)
pos1970indices=which(SanJose$year>1970)

r0 = function(W){model$coeff[1]+model$coeff[3]*W^(-2)}

