transform2homog <-
function(theta,times,y,y.hat,eq,coef){
F		= eq(y.hat, theta )
L      		= L(theta=theta,times=times,coef=coef)
cond		= rcond(L)
if(cond <.Machine$double.eps) 	Li=pseudoinverse (L)  else   Li=solve(L)
y.tilde 	= y - Li%*%F
return( list(F = F, y.tilde = y.tilde) )
}

