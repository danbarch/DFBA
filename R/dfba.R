# Function based on code from p. 346

Mann_Whitney_U_Stats<-function(E, C){
  UE_vector<-rep(NA, length(E)) # UE counter
  UC_vector<-rep(NA, length(C)) # UC counter
  for (i in 1:length(E)){
    UE_vector[i]<-sum(E[i]>C)
  }
  for (j in 1:length(C)){
    UC_vector[j]<-sum(C[j]>E)
  }

  list(UE=sum(UE_vector),
       UC=sum(UC_vector))
}
