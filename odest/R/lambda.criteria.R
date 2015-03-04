lambda.criteria <-
function(rkhs.obj,criterion="AIC")
{
sol		    = rkhs.obj
theta		    = rkhs.obj$par
Slambda	    = rkhs.obj$Slambda
y.hat		    = rkhs.obj$x.hat
Sigma  	    = rkhs.obj$Sigma
y		    = as.vector(rkhs.obj$data)
times		    = rkhs.obj$times
Sigma.i	    = diag(1/diag(Sigma),ncol(Sigma))



if (criterion=="AIC"){
# AIC like criterion
df	    = sum(diag(Slambda))
errors  = y-y.hat
lik    = -1/2*t(errors)%*%(Sigma.i%x%diag(length(times)))%*%errors
AIC	    = -2*lik+2*df
return(AIC)
}

if (criterion=="GCV"){
# generalized cross-validation
sse	= t(y-y.hat)%*%(y-y.hat)
GCV	= sse/sum(diag(Slambda-1)^2)
return(GCV)
}
}

