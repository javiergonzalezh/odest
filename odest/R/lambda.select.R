lambda.select <-
function(rkhs.obj,lambda.grid=seq(1,10^4,length=10),criterion="AIC",optimise=TRUE)
{
cat("Selecting optimal lambda...","\n")
data		=	rkhs.obj$data
f 		=	rkhs.obj$f
coef		=	rkhs.obj$coef
times		=	rkhs.obj$times
par		=	rkhs.obj$par
sigma		=	diag(rkhs.obj$Sigma)
ODEmod          = 	rkhs.obj$ODEmod
varnames	=	rkhs.obj$ODEmod

if(optimise==TRUE)
{
lambda.score=function(lambda)
{
res     	=       rkhs(data=data,f=f,coef=coef,times=times,lambda=lambda,par=par,sigma=sigma,varnames=varnames,ODEmod=ODEmod)
lambda.criteria(res,criterion=criterion)
}
result=optimise(lambda.score,interval=c(0,10^5))$minimum
} else {lam.cr=NULL
for(i in 1:length(lambda.grid))
{
lambda  	= 	lambda.grid[i]
res     	=       rkhs(data=data,f=f,coef=coef,times=times,lambda=lambda,par=par,sigma=sigma,varnames=varnames,ODEmod=ODEmod)
l               =  	lambda.criteria(res,criterion=criterion)
lam.cr  	=	c(lam.cr,l)
}
result=lambda.grid[which.min(lam.cr)]
}
return(result)

}

