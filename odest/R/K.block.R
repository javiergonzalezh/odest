K.block <-
function(theta,times,coef,n.eqs,lambda,nT,Sigma)
{
Ktheta			= NULL
for(i in 1:n.eqs){	#browser()
K.new = K( theta=theta,times=times,coef=coef[[i]])
if(is.matrix(K.new)==FALSE ) { cat("one of the operators is NULL \n"); i=n.eqs+1;Ktheta=NULL} else {
if(i==1) Ktheta = K.new   else	Ktheta = adiag( Ktheta, K.new )
}
}
#cat(Ktheta,"\n")
if(is.matrix(Ktheta) == FALSE) 	{result = runif(1,min=1,max=2^63); cat("result=",result,"\n")} else {
P       	     = Ktheta + lambda*Sigma%x%diag(nT)
}
return(list(Ktheta=Ktheta,P=P))
}

