\name{ci.boot}
\alias{ci.boot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Estimating Confidence Intervals
}
\description{
Estimating Confidence Intervals
}
\usage{
ci.boot(rkhs.obj, nboot = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rkhs.obj}{
   List returned by rkhs function.
}
  \item{nboot}{
Number of bootstrap samples.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A list with the following components:
\item{data }{Original data.}
\item{ci }{Confidence intervals of parameters.}
\item{par}{Estimated parameters obtained by function 'rkhs'.}
\item{par.boot}{Bootstrapped parameters.}
\item{times}{Vector of observation times for the data.}
\item{x.hat}{Estimate of state variables obtained by function 'rkhs'.}
\item{object}{Set to 'ci' and indicates that the list is provided by function 'ci.boot'.}
\item{varnames}{Vector that contains the names of state variables.}
\item{ODEmod}{A function that defines ODE model.}
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

