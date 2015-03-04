\name{rkhs}
\alias{rkhs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
rkhs.estimation
}
\description{
RKHS estimation of ODE system
}
\usage{
rkhs(data, f, coef, times, lambda, par, sigma = NULL,varnames,ODEmod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
  Matrix of observed data values.
}
  \item{f}{
List giving the right hand side of each equation.
}
  \item{coef}{
List giving the coefficients of differential operators for each equation.
}
  \item{times}{
Vector of observation times for the data.
}
  \item{lambda}{
Regularization parameter.
}

  \item{par}{
Initial values of parameters to be estimated.
}

  \item{sigma}{
If vector, it comprises variances of each state variable. If scalar, then variance of each state variable is assumed
 to be equal to given value. If NULL, it is estimated offline.
}
  \item{varnames}{
Vector that contains the names of state variables.
}
  \item{ODEmod}{
A function that defines ODE model.
}

}
\value{
A list with where the most important component is:

 \item{par}{Estimated parameters.}
 \item{data}{Matrix of observed data values.}
 \item{f}{List giving the right hand side of each equation.}
 \item{coef}{List giving the coefficients of differential operators for each equation.}
 \item{times}{Estimated parameters.}
 \item{x.hat}{Estimated state variables.}
 \item{lambda}{Regularization parameter.}
 \item{Sigma}{Diagonal matrix of variances for every state.}
 \item{Slambda}{Smoother matrix.}
 \item{object}{Set to 'rkhs' and indicates that the list is provided by function 'rkhs'.}
 \item{varnames}{Vector that contains the names of state variables.}
 \item{ODEmod}{A function that defines ODE model.}

}

\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

###########################
##FitzHugh-Nagumo example##
###########################
#setting up the system
f1 = function( x, theta )
{
f = matrix(-x[,1]^3/3+x[,2],ncol=1)
return(f)
}
f2 = function( x, theta )
{
f = matrix( -x[,1]+rep( theta[1],length(x[,1]) ) ,ncol=1)
return(f)
}
coef1 = function(theta)
{
return( c(-1,1/theta[3]) )
}
coef2 = function(theta)
{
return( c(theta[2],theta[3]) )
}
f=list(f1,f2)
coef = list(coef1,coef2)
#ODE
FHNmod <- function(Time, State, Pars) {
with(as.list(c(State, Pars)), {
dx <- c*(x-x^3/3 + y)
dy <- -1/c*(x-a+b*y)
return(list(c(dx, dy)))
})
}
#true parameters and initial conditions
pms = c(a = 0.2, b = 0.2, c = 3)
yini = c(x = -1, y = 1)
#sample size
n = 50
#level of noise
noise = 0.1
times = seq(0, 30, length = n ) ## sequence of t1,...,tn
out = ode(yini, times, FHNmod, pms) ## ODE solution
## Generate the data
x = out[,2:3]
y = as.matrix(cbind(out[,2] + rnorm(n,0,noise),out[,3] +rnorm(n,0,noise)))
#variable names and initial parameters
par=c(a=4, b=4, c=4)
varnames=c('x','y')
colnames(y)=varnames
# estimate with lambda=1
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=1,par=par,varnames=varnames,ODEmod=FHNmod)
res$par
#selection of lambda with AIC
lambda=lambda.select(res,optimise=FALSE)
##or slower option
#lambda=lambda.select(res)
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=lambda,par=par,
varnames=varnames,ODEmod=FHNmod)
res$par
plot.odesol(res)
res.boot = ci.boot(res,20)
res.boot$ci
plot.odesol(res.boot)


################################
##Simple linear system example##
################################
#setting up the system
coef1 = function(theta)
{
return(c(-theta,1) )
}
f1 = function(x, theta)
{
f = 0*x
return(f)
}
f=list(f1)
coef = list(coef1)
separable=NULL
#ODE
EXPmod <- function(Time, State, Pars) {
with(as.list(c(State, Pars)), {
dx <- theta * x
return(list(c(dx)))
})
}
#true parameters and initial conditions
pms = c(theta = -2)
yini = c(x = -1)
#sample size
n = 50
#level of noise
noise = 0.1
times = seq(0, 1, length = n ) ## sequence of t1,...,tm
out = ode(yini, times, EXPmod, pms) ## ODE solution
## Generate the data
x = out[,2]
y = as.matrix(cbind(out[,2] + rnorm(n,0,noise)))
#variable names and initial parameters
par=c(theta=1)
varnames=c('x')
colnames(y)=varnames
# estimate with lambda=1000
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=1000,par=par,
varnames=varnames,ODEmod=EXPmod)
res$par
#selection of lambda with AIC
lambda=lambda.select(res,optimise=FALSE)
##or slower option
#lambda=lambda.select(res)
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=lambda,par=par,
varnames=varnames,ODEmod=EXPmod)
res$par
# confidence intervals
res.boot = ci.boot(res,20)
res.boot$ci
# plot estimate of state variable
plot.odesol(res)
#plot confidence bands
plot.odesol(res.boot)

##########################
##Lotka-Volterra example##
##########################
#setting up the system
f1 = function( x, theta )
{
f = -theta[2]*matrix(x[,1]*x[,2],ncol=1)
return(f)
}
f2 = function( x, theta )
{
f = theta[4]*matrix(x[,1]*x[,2],ncol=1)
return(f)
}
System.f=list(f1,f2)
coef1 = function(theta)
{
return( c(-theta[1],1) )
}
coef2 = function(theta)
{
return( c(theta[3],1) )
}
f = list(f1,f2)
coef = list(coef1,coef2)
separable=NULL
#ODE
LVmod <- function(Time, State, Pars) {
with(as.list(c(State, Pars)), {
dx <- alpha * x - beta *x*y
dy <- -gamma*y + delta*x*y
return(list(c(dx, dy)))
})
}
#true parameters and initial conditions
pms = c(alpha = 0.2,beta = 0.35, gamma = 0.7, delta = 0.40 )
yini = c(x = 1, y = 2)
## sample size values
n = 50
## level of noise
noise = 0.1
times = seq(0, 30, length = n ) ## sequence of t1,...,tm
out = ode(yini, times, LVmod, pms) ## ODE solution
## Generate the data
x = out[,2:3]
y = as.matrix(cbind(out[,2] + rnorm(n,0,noise),out[,3] +rnorm(n,0,noise)))
#variable names and initial parameters
par=c(alpha = 4,beta = 4, gamma = 4, delta = 4 )
varnames=c('x','y')
colnames(y)=varnames
#estimate with lambda=1
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=1,par=par,
varnames=varnames,ODEmod=LVmod)
res$par
#selection of lambda with AIC
lambda=lambda.select(res,optimise=FALSE)
##or slower option
#lambda=lambda.select(res)
res=rkhs(data=y,f=f,coef=coef,times=times,lambda=lambda,par=par,
varnames=varnames,ODEmod=LVmod)
res$par
# confidence intervals
res.boot = ci.boot(res,20)
res.boot$ci
#plot estimates of state variables
plot.odesol(res)
#plot confidence bands
plot.odesol(res.boot)

}
        

