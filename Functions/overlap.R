overlap_fun<-function(SAE,a0=1,b0=1000, method="approx", sims=1000, ar=1, br=1, extra){
  
  ##sims=10000
  NovDir_out<-matrix(NA,N,1)
  if(length(b0)==1){
    
    
    if(method=="approx"){
      for(i in 1:N){
        
        muX=a0/(a0+b0)
        aY=1+SAE[i,1]
        bY=1-SAE[i,1]+SAE[i,2]
        muY=aY/(aY+bY)
        
        enuX=a0*b0
        denX=(a0+b0)^2 *(a0+b0+1)
        sigma2X=enuX/denX
        
        enuY=aY*bY
        denY=(aY+bY)^2 *(aY+bY+1)
        sigma2Y=enuY/denY
        
        enu=muY-muX # Pr( Y > X)
        den=(sigma2X+sigma2Y)^(1/2)
        PR_EMprox=pnorm(enu/den)
        NovDir_out[i,]<-PR_EMprox
      }
      
    } # 
    
    if(method=="analytic"){
      for(i in 1:N){
        PR_EMna=sum(rbeta(sims,a0,b0)<
                      rbeta(sims,ar+SAE[i,1],br-SAE[i,1]+SAE[i,2]))/sims
        NovDir_out[i,]<-PR_EMna
      }
    } # 1 mirio * 10
    
    if(method=="overlap"){
      for(i in 1:N){
        step_temp=Betalap(a0,b0,
                          1+SAE[i,1],
                          1-SAE[i,1]+SAE[i,2],
                          0.5)
        NovDir_out[i,]<-step_temp$Noverlap*step_temp$Direction
      }
      
    } # elegxo to integrate # subdivisions
  }
  
  if(length(b0)>1){
    
    
    if(method=="approx"){
      for(i in 1:N){
        
        muX=a0/(a0+b0[i])
        aY=1+SAE[i,1]
        bY=1-SAE[i,1]+SAE[i,2]
        muY=aY/(aY+bY)
        
        enuX=a0*b0[i]
        denX=(a0+b0[i])^2 *(a0+b0[i]+1)
        sigma2X=enuX/denX
        
        enuY=aY*bY
        denY=(aY+bY)^2 *(aY+bY+1)
        sigma2Y=enuY/denY
        
        enu=muY-muX # Pr( Y > X)
        den=(sigma2X+sigma2Y)^(1/2)
        PR_EMprox=pnorm(enu/den)
        NovDir_out[i,]<-PR_EMprox
      }
      
    } # 
    
    if(method=="analytic"){
      for(i in 1:N){
        PR_EMna=sum(rbeta(sims,a0,b0[i])<
                      rbeta(sims,ar+SAE[i,1],br-SAE[i,1]+SAE[i,2]))/sims
        NovDir_out[i,]<-PR_EMna
      }
    } # 1 mirio * 10
    
    if(method=="overlap"){
      for(i in 1:N){
        step_temp=Betalap(a0,b0[i],
                          1+SAE[i,1],
                          1-SAE[i,1]+SAE[i,2],
                          0.5)
        NovDir_out[i,]<-step_temp$Noverlap*step_temp$Direction
      }
      
    }
  }
  
  NovDir_out<-data.frame(NovDir_out)
  names(NovDir_out)<-c("Pr_larger")
  #NovDir_out<-cbind(NovDir_out,median=Table2b$Median,LL=Table2b$LL,UL=Table2b$UL)
  #NovDir_out$se_range<-round(NovDir_out$median/(NovDir_out$UL-NovDir_out$LL),3)
  #NovDir_out$se_range_rel<-c(NovDir_out$se_range)/c(NA,NovDir_out$se_range[-length(NovDir_out$se_range)])
  return(NovDir_out)
}
