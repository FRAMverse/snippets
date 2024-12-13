library(deSolve)

LV=function(t,y,parms) {
  # This function is based on the Lotka-Volterra competition model
  #state variables:
  x1=y[1]; x2=y[2];
  # Parameters:
  r1=parms[1]; r2=parms[2]; a=parms[3]; b=parms[4]
  # Model:
  dx1= x1*(r1-x1-a*x2)
  dx2= x2*(r2-x2-b*x1)
  dY=c(dx1,dx2);
  return(list(dY));
}

times=seq(0,50,by=.2)
x0=c(x1=1.3,x2=.1)
parms=c(r1=1,r2=1,a=1,b=1)
out.lsoda=lsoda(x0,times,LV,parms)

matplot(out.lsoda[,1],out.lsoda[,2:3],type="l",xlab="time t",ylab="x1,x2",main="lsoda solutions");
plot(out.lsoda[,2:3],type='l',xlab='u',ylab='v',main="Starting at (.2,.1), time 0-200")