\name{startcost}
\alias{startcost}

\title{
Initialize a Distance Matrix.
}
\description{
Creates an distance matrix of zeros of dimensions compatible with the treatment indicator vector z.
}
\usage{
startcost(z)
}
\arguments{
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.  Although not strictly required, it is best that z has names that are the same as the names of the data frame dat that will be used in matching.
}
}
\value{
A matrix of zeros with sum(z) rows and sum(1-z) columns.  If z has names, then they become the row and column names of this matrix.
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
dist<-startcost(z)
dist
rm(z,dist,d)
}

