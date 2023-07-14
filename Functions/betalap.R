Betalap<-function(a1=1,b1=1,a2=1,b2=0.5,quant=0.5){
  Noverlap<-1-integrate(int_f2, 0, 1, s11=a1, s12=b1, s21=a2, s22=b2)$value
  p1=median(rbeta(10000,a1,b1))
  p2=median(rbeta(10000,a2,b2))
  temp_d<-p1-p2
  if(temp_d<0) {direction=-1}
  if(temp_d==0 ) {direction=0}
  if(temp_d>0) {direction=1}
  #if((p1!=0 | p2!=0)){direction=1}
  return(list(Noverlap=Noverlap,Direction=direction))
}