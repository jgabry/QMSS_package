% Generated by roxygen2 (4.0.1): do not edit by hand
\name{firstD}
\alias{firstD}
\title{Compute first differences}
\usage{
firstD(var, group, df)
}
\arguments{
\item{var}{Variable to be first-differenced.}

\item{group}{Optional grouping variable (see 'Details').}

\item{df}{Optional data frame containing \code{var} and \code{group} (see 'Details').}
}
\value{
\code{firstD(var)} returns a first-differenced version of \code{var}.
\code{firstD(var,group)} returns a first-differenced version of \code{var} by \code{group}.
And \code{firstD(var,group,df)} returns a first-differenced version of \code{var} by \code{group},
where \code{var} and \code{group} are searched for in \code{df}. Note that the first value of
\code{firstD(var)} will be \code{NA} since there is no difference to compute. Similarly, for
\code{firstD(var,group)} and \code{firstD(var,group,df)} the first value for each group
will be \code{NA}.
}
\description{
Compute first differences
}
\details{
If \code{df} is specified then \code{group} must also be specified. So it is possible
to specify all three parameters, \code{var} and \code{group} only, or \code{var} by itself.
An example of when one might wish to omit both \code{group} and \code{df} is when using \code{firstD}
in conjunction with  \pkg{plyr}'s \code{\link[plyr]{ddply}} (see 'Examples'). If \code{df} is specified then it
should be sorted by \code{group} and, if necessary, a second variable (e.g. time) that orders the
observations of \code{var} in the appropriate sequence.
}
\examples{
# Specifying both group and df
df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10))
df$Xdiff <- firstD(X, id, df)
df

# Omitting df
id <- rep(1:3, each = 3)
X <- rpois(9, 10)
Xdiff <- firstD(X, id)

# Omitting group and df
\dontrun{
library(plyr)
df <- data.frame(id = rep(1:3, each = 3), X = rpois(9, 10), Y = rpois(9, 5))
ddply(df, "id", mutate, Xdiff = firstD(X), Ydiff = firstD(Y))
}
}
\author{
Jonah Gabry <jsg2201@columbia.edu>
}

