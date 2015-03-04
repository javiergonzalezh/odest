K <-
function(theta,times,coef){
Ltheta = L(theta=theta,times=times,coef=coef)
LtL     = t(Ltheta)%*%Ltheta
cond   = rcond(LtL)
if(cond <.Machine$double.eps)  {Ktheta=pseudoinverse(LtL)}  else Ktheta = solve(LtL)
#if(cond == 0)  {Ktheta = Inf}  else Ktheta = solve(t(Ltheta)%*%Ltheta)
return(Ktheta)

}

