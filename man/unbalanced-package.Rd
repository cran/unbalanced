\name{unbalanced-package}
\alias{unbalanced-package}
\alias{unbalanced}
\docType{package}

\title{Techniques for unbalanced dataset}

\description{A dataset is said to be unbalanced when the class of interest (minority class) is much rarer than normal behaviour (majority class). The cost of missing a minority class is typically much higher that missing a majority class. Most learning systems are not prepared to cope with unbalanced data. 
The proposed strategies essentially belong to the following categories: sampling and distance-based. 
Sampling techniques up-sample or down-sample a class of observations. SMOTE generates synthetic minority examples. Distance based techniques use distances between input points to under-sample or to remove noisy and borderline examples.
}

\details{
\tabular{ll}{
Package: \tab unbalanced\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2014-01-27\cr
License: \tab GPL (>= 2)\cr
}
~~ An overview of how to use the package, including the most important functions ~~
}

\author{
Andrea Dal Pozzolo

Maintainer: Andrea Dal Pozzolo <dalpozz@gmail.com>
}

\references{
Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.
}

\keyword{unbalanced datasets, imbalanced learning}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

#balance the dataset
data<-ubBalance(X= input, Y=output, type="ubSMOTE", perc.over=300, perc.under=150, verbose=TRUE)
balancedData<-cbind(data$X,data$Y)
}
