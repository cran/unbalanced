\name{ubBalance}
\alias{ubBalance}

\title{balance}

\description{The function implementes several techniques to re-balance or remove noisy instances in unbalanced datasets.}

\usage{ubBalance(X, Y, type = "ubSMOTE", ...)}

\arguments{
  \item{X}{the input variables of the unbalanced dataset.}
  \item{Y}{the response variable of the unbalanced dataset. 
          It must be a binary factor where the majority class is coded as 0 and the minority as 1.}
  \item{type}{the balancing technique to use ("ubOver", "ubUnder", "ubSMOTE", "ubOSS","ubCNN", "ubENN", "ubNCL", "ubTomek").}
  \item{\dots}{further arguments passed to the selected technique}
}


\details{The argument type can take the following values: "ubOver" (over-sampling), "ubUnder" (under-sampling), "ubSMOTE" (SMOTE), "ubOSS" (One Side Selection), "ubCNN" (Condensed Nearest Neighbor), "ubENN" (Edited Nearest Neighbor), "ubNCL" (Neighborhood Cleaning Rule), "ubTomek" (Tomek Link).}

\value{
  The function returns a list:
  \item{X}{input variables}
  \item{Y}{response variables}
  \item{id.rm}{index of instances removed if availble in the technique selected}
}
\references{
Dal Pozzolo, Andrea, et al. "Racing for unbalanced methods selection." Intelligent Data Engineering and Automated Learning - IDEAL 2013. Springer Berlin Heidelberg, 2013. 24-31.}


\seealso{
\code{\link{ubOver}}, \code{\link{ubUnder}}, \code{\link{ubSMOTE}}, \code{\link{ubOSS}}, \code{\link{ubCNN}}, \code{\link{ubENN}}, \code{\link{ubNCL}}, \code{\link{ubTomek}}
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