plot.odesol <-
function(object){
## variables
tms       = object$times
ODEmod    = object$ODEmod
f=function(x) {sqrt( sum(x^2)/length(x) )}
x.hat   = matrix(object$x.hat,nrow=length(tms))
sigma       = apply(object$data-x.hat,2,f)
#est.hat   = matrix(object$x.hat,nrow=length(tms))
yini        = object$data[1,]
names(yini) = object$varnames
tms1=seq(range(tms)[1],range(tms)[2],length=500)
est.hat   = matrix(ode(yini, tms1, ODEmod, object$par)[,-1],nrow=length(tms1))
x.data    = as.matrix(object$data)
#


## we calculate what we need to make the matrix of plots
nvars = dim(x.data)[2]
if(round(sqrt(nvars))!=sqrt(nvars))
{
nrows = round(sqrt(nvars))
ncols = round(sqrt(nvars))+1
}
if(round(sqrt(nvars))==sqrt(nvars))
{
nrows = sqrt(nvars)
ncols = sqrt(nvars)
}
par(mfrow = c(nrows, ncols))

##
## plot if the function is an output of RKHS
##
if(object$object == "rkhs"){
data      = as.matrix(object$data)
for (i in 1:nrows){
for (j in 1:ncols){
index = ncols*(i-1)+j
if(index <= nvars){

plot(range(tms1), range(c(est.hat,data[,index])),main=colnames(data)[index],xlab="Time",ylab="value",pch=1,col="black",type="n")
points(tms,data[,index],pch=1,col="black")
#plot(tms,data[,index],main=colnames(data)[index],xlab="Time",ylab="value",pch=1,col="black",ylim=c(min(data[,i])-0.2,max(data[,i])+0.2))
lines(tms1, est.hat[,index],lty=1,lwd=2,col="blue")
#points(tms, est.hat[tms,index],pch=20,col="blue")
}
}}
}
##
## plot if the function is an output of ci.boot
##
if(object$object == "ci"){
data      = as.matrix(object$data)
for (i in 1:nrows){
for (j in 1:ncols){
index = ncols*(i-1)+j
if(index <= nvars){
l1=est.hat[,index]+2*sigma[index]
l2=est.hat[,index]-2*sigma[index]
plot(range(tms), range(c(l1,l2,data[,index])),main=colnames(data)[index],lty=1,lwd=2,col="blue",xlab="Time",ylab="value",pch=20,type="n")
lines(tms1, l1,lwd=2,col="grey",lty=4)
lines(tms1, l2,lwd=2,col="grey",lty=4)
polygon(c(tms1,rev(tms1)), c(l1,rev(l2)), col = "grey",border='grey')
lines(tms1, est.hat[,index],lwd=3,col="red")
points(tms,data[,index],pch=20)
}
}}

}

}

