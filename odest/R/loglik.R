loglik <-
function(theta,lambda,y,y.hat,times,coef,f,eq.num,Sigma){

nT 			= length(times)

Sigma.i			= diag(1/diag(Sigma),ncol(Sigma))
y.vec     = matrix(y,ncol=1)
y.hat.vec = matrix(y.hat,ncol=1)
n.eqs = length(f)


# Kernel
K	=	K.block(theta,times,coef,n.eqs=length(f),lambda,nT,Sigma)
Ktheta	=	K$Ktheta
P	=	K$P
# Transformation to make the system homogeneous (y.tilde)
y.tilde			= NULL
for(i in 1:n.eqs)	y.tilde= cbind( y.tilde, transform2homog(theta=theta,times=times,y=y[,i],y.hat=y.hat,eq=f[[i]],coef=coef[[i]] )[[2]] )
y.tilde.vec=matrix(y.tilde,ncol=1)
## Errors of the estimation (for y.tilde)
alpha			= solve(P)%*%y.tilde.vec
errors  		= y.tilde.vec-Ktheta%*%alpha
# (minus)penalized log-likelihood  (for y.tilde)
result = 1/2*sum(log(diag(Sigma))) + 1/2*t(errors)%*%(Sigma.i%x%diag(nT))%*%errors +(lambda/2)* t(alpha)%*%Ktheta%*%alpha
result=as.numeric(result)
if ( is.na(result)==TRUE || is.infinite(result)==TRUE ) {result=runif(1,min=1,max=2^63);cat("result=",result,"\n")}
return(result)
}

