\name{addinteger}
\alias{addinteger}

\title{
Add an Integer Penalty to an Existing Distance Matrix
}
\description{
Takes an integer valued covariate, and adds a penalty proportional
to the difference in the integer values, with proportionality constant
penalty.
}
\usage{
addinteger(costmatrix, z, iscore, penalty = 1000)
}
\arguments{
  \item{costmatrix}{
An existing cost matrix with sum(z) rows and sum(1-z) columns.  The function checks the compatability of costmatrix, z and p; so, it may stop with an error if these are not of appropriate dimensions.  In particular, costmatrix may come from startcost().
}
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.
}
  \item{iscore}{
An vector of integers with length equal to length(z).
}
  \item{penalty}{
One positive number used to penalize mismatches for iscore.
}
}
\details{
If a treated and control individual differ on iscore in absolute value by dif, then the distance between them is increased by adding dif*penalty.
}
\value{
A penalized distance matrix.
}

\author{
Paul R. Rosenbaum
}

\examples{
data(binge)
# Select two treated and four controls from binge
d<-binge[is.element(binge$SEQN,c(109315,109365,109266,109273,109290,109332)),]
attach(d)
z<-1*(AlcGroup=="B")
names(z)<-d$SEQN
rbind(z,education)
dist<-startcost(z)
addinteger(dist,z,education,penalty=3)
detach(d)
rm(d,dist,z)
}

