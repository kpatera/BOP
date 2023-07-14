# (Run) - Table2-Figure2 function #####
Table2out_fun<-function(SAE, a0=100,b0=1000000, x=seq(0,1,length=50000),n.sims=0,extra, ar=1, br=1){
  #plot(x,dbeta(x,shape1 = a0,shape2 = b0),type="l")
  overlap_out<-cbind(overlap_fun(SAE=SAE, a0 = a0,b0 = b0, method = "analytic",extra=Table2b)[,1],
                     overlap_fun(SAE=SAE, a0 = a0,b0 = b0, method = "analytic", sims=n.sims,extra=Table2b, ar=ar, br=br)[,1],
                     overlap_fun(SAE=SAE, a0 = a0,b0 = b0, method = "analytic",extra=Table2b)[,1])
  
  overlap_out<-data.frame(overlap_out)
  names(overlap_out)<-c("Approximate","Analytical","Integrated")
  return(overlap_out)
}
