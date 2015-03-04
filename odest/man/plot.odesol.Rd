\name{plot.odesol}
\alias{plot.odesol}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots state variables and confidence bands of state variables. Confidence bands are based on the variance estimated from the residuals. Note: Confidence bands can be very wide.
}
\description{
Plots estimated state variables if the object is returned by 'rkhs' function and confidence bands of state variables if the object is returnd by 'ci.boot' function.
}
\usage{
plot.odesol(object)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
List either returned by the function 'rkhs' or the function 'ci.boot'.
}
}




