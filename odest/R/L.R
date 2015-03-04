L <-
function(theta,times,coef){
nT		= length(times)
D               = D(times=times)
L.coef		= coef(theta=theta)
L 		= matrix(0,ncol=nT,nrow=nT)
for (k in 1:length(L.coef) ){ L = L+L.coef[k]*D%^%(k-1)}
return( L )
}

