D <-
function(times,sing=0){
nT			= length(times)
D.res			= rbind(cbind(0,diag(rep(1,nT-1))),0)+cbind(rbind(0,diag(rep(-1,nT-1))),0)
D.res[1,1]		= -1
# to avoid singularities
D.res[nT-1,nT]	= 1+sing
D.res[nT,nT]	= 1-sing
D.res			= diag(c(1/(times[2]-times[1]),1/(times[-(1:2)]-times[-((nT-1):nT)]),1/(times[nT]-times[nT-1])))%*%D.res
return(D.res)
}

