\name{ubUnder}
\alias{ubUnder}

\title{Under-sampling}

\description{The function removes randomly some instances from the majority class in order to obtain a more balanced dataset}

\usage{ubUnder(X, Y, propMinClass = 50, w = NULL)}


\arguments{
 \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{propMinClass}{proportion of minority class wanted in the final dataset (propMinClass=40 means 40 minority instances every 100 instances)}
  \item{w}{weights used for sampling the majority class, if NULL all majority instances are sampled with equal weights}
}


\value{
The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variables}
  \item{id.rm}{index of instances removed}
}


\seealso{
\code{\link{ubBalance}}
}

\examples{
library(unbalanced)
data(ubIonosphere)
n<-ncol(ubIonosphere)
output<-ubIonosphere$Class
input<-ubIonosphere[ ,-n]

data<-ubUnder(X=input, Y= output, propMinClass = 40)
newData<-cbind(data$X, data$Y)
}