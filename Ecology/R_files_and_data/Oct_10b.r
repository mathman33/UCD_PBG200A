# the crouse et al. model

A=matrix(c(0, 0, 0,0, 127, 4, 80,
           0.6747, 0.737, 0, 0, 0, 0, 0,
           0, 0.0486, 0.6610, 0, 0, 0, 0,
           0, 0, 0.0147, 0.6907, 0, 0, 0,
           0, 0, 0, 0.0518, 0, 0, 0,
           0, 0, 0, 0, 0.8091, 0, 0,
           0, 0, 0, 0, 0, 0.8091, 0.8089),7,byrow=TRUE)

# The age-structured version

A=matrix(0,55,55)
A[2,1]=0.6747
# small juvs
for (i in 2:8){
  A[i+1,i]=0.7857
}
# large juvs
for (i in 9:16){
  A[i+1,i]=0.6758}
# subadults
for (i in 17:22){
  A[i+1,i]=0.7425}
# novice breeders
A[1,23]=127
A[24,23]=0.8091
# 1st remigrants
A[25,24]=0.8091
A[1,24]=4
# matures
for (i in 25:54){
  A[i+1,i]=0.8091
  A[1,i]=80}
A[1,55]=80