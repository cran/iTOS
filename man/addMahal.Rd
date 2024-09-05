\name{addMahal}
\alias{addMahal}
\concept{Matching}
\concept{Mahalanobis distance}

\title{
Rank-Based Mahalanobis Distance Matrix
}
\description{
Adds a rank-based Mahalanobis distance to an exisiting distance matrix.
}
\usage{
addMahal(costmatrix, z, X)
}
\arguments{
  \item{costmatrix}{
An existing cost matrix with sum(z) rows and sum(1-z) columns.  The function checks the compatability of costmatrix, z and p; so, it may stop with an error if these are not of appropriate dimensions.  In particular, costmatrix may come from startcost().
}
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.
}
  \item{X}{
A matrix with length(z) rows containing covariates.
}
}
\details{
The rank-based Mahalanobis distance is defined in section 9.3 of Rosenbaum (2020).
}
\value{
A new distance matrix that is the sum of costmatrix and the rank-based Mahalanobis distances.
}
\references{
Rosenbaum, P. R. (2020) <doi:10.1007/978-3-030-46405-9> Design of Observational Studies (2nd Edition).  New York: Springer.

Rubin, D. B. (1980) <doi:10.2307/2529981> Bias reduction using Mahalanobis-metric matching. Biometrics, 36, 293-298.


}
\author{
Paul R. Rosenbaum
}

\examples{
data(binge)
# Select two treated and three controls from binge
d<-binge[is.element(binge$SEQN,c(109315,109365,109266,109273,109290)),]
z<-1*(d$AlcGroup=="B")
names(z)<-d$SEQN
attach(d)
x<-cbind(age,female)
detach(d)
rownames(x)<-d$SEQN
dist<-startcost(z)
z
x
dist
dist<-addMahal(dist,z,x)
dist
rm(z,x,dist,d)
}

