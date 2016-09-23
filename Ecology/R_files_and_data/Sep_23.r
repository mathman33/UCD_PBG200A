#rm(list=ls())

# initial density (first elements in R start at 1, not 0... :(.... )
N_initial = 25.01

# parameters
r0 = 3 # Try various values to see stable equilibrium, 2-cycles, 4-cycles, etc... chaos
K = 100

# final time
T_final = 50

# Store the results of the simulations in a vector "N"
N = numeric(T_final)

# Set the first value to the first value...
N[1] = N_initial

for(t in 2:T_final){
  N[t] = N[t-1]*exp(r0*(1 - (N[t-1]/K)))
}

plot(N,type="b")
print(N)