ci.boot <-
function(rkhs.obj,nboot=10)
{
data		=	rkhs.obj$data
f		=	rkhs.obj$f
coef		=	rkhs.obj$coef
x.hat		=	rkhs.obj$x.hat
times		=	rkhs.obj$times
lambda	        =	rkhs.obj$lambda
par		=	rkhs.obj$par
Sigma	  	=	rkhs.obj$Sigma
sigma		=	diag(Sigma)
n		=	length(f)
x		=	matrix(x.hat,ncol=n)
x.hat    	= 	matrix(x.hat, ncol = n)
par.boot 	=	NULL
varnames	=	rkhs.obj$varnames
ODEmod		=	rkhs.obj$ODEmod
for(i in 1:nboot){
cat("bootstrap iteration=", i,"\n")
# generate data
y= NULL
for(j in 1:n) y=cbind(y,x[,j]+rnorm( length(times),0,sqrt(sigma[j]) ) )

# estimate the parameters
sol = rkhs(data=y,f,coef,times,lambda,par=par,sigma=sigma,varnames=varnames,ODEmod=ODEmod)
par.boot = rbind(par.boot,sol$par)
}
ci =  apply(par.boot,2,quantile,probs=c(0.025,0.975))
return(list(data=data,ci=ci,par=par,par.boot=par.boot,times=times, x.hat=x.hat,object='ci',varnames=varnames,ODEmod=ODEmod))
}

