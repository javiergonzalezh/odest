rkhs <-
function(data,f,coef,times,lambda,par,sigma=NULL,varnames,ODEmod)
{
n.eqs = length(f)   #number of equations
# smoothing the data
y.hat = NULL
for(i in 1:ncol(data))  y.hat  = cbind(y.hat, predict(smooth.spline(times,data[,i]),times)$y)
#browser()
if(is.null(sigma))
{
sigma = c()
for(i in 1:n.eqs)
{
m=smooth.spline(times,data[,i],cv=TRUE)
sigma[i] = max(sum((m$y-data[,i])^2)/(length(times)-m$df),0.2)
}
}
if(length(sigma)==1) Sigma=diag(sigma,n.eqs) else Sigma=diag(sigma)

res    = NULL
result = NULL
res    = optim(par=par,fn=loglik,method = c("BFGS"),lambda=lambda,y=data,y.hat=y.hat,times=times,f=f,coef=coef,Sigma=Sigma)

y.vec     = matrix(data,ncol=1)
y.hat.vec = matrix(y.hat,ncol=1)
n.eqs = length(f)
theta=res$par
# Kernel
K		=	K.block(theta,times,coef,n.eqs,lambda,length(times),Sigma)
Ktheta		=	K$Ktheta
P		=	K$P

Slambda			= Ktheta%*%solve(P)
## Errors of the estimation (for y.tilde)
alpha			= solve(P)%*%y.vec
x.hat			= Ktheta%*%alpha
result = list(par=theta,data=data,f=f,coef=coef,times=times,x.hat=x.hat,lambda=lambda,Sigma=Sigma,Slambda=Slambda,object='rkhs',varnames=varnames,ODEmod=ODEmod)
}

