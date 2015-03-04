\name{lambda.select}
\alias{lambda.select}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Selecting the regularization parameter with AIC or GCV
}
\description{
Selection of the regularization parameter
}
\usage{
lambda.select(rkhs.obj, lambda.grid=seq(1,10^4,length=10), 
criterion = "AIC", optimise = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rkhs.obj}{
	List returned by the function rkhs.
}
  \item{lambda.grid}{
	Grid of lambda values in which selection criterion is to be evaluated if optimise=FALSE.
}
  \item{criterion}{
Selection criterion. Defult value is  "AIC" (Akaike's Information Criterion) and the other option is "GCV" (Generalized Cross-Validation).
}
  \item{optimise}{
	Logical value that indicates if selection criterion should be optimised over all  positive real numbers. 
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns optimal value of lambda in terms of the criterion that is selected.
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

