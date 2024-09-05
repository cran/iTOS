\name{addcaliper}
\alias{addcaliper}

\title{
Add a Caliper to an Existing Cost Matrix
}
\description{
For one covariate, adds a caliper to an existing cost matrix.
}
\usage{
addcaliper(costmatrix, z, p, caliper = NULL, penalty = 1000, twostep = TRUE)
}
\arguments{
  \item{costmatrix}{
An existing cost matrix with sum(z) rows and sum(1-z) columns.  The function checks the compatability of costmatrix, z and p; so, it may stop with an error if these are not of appropriate dimensions.  In particular, costmatrix may come from startcost().
}
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.
}
  \item{p}{
A vector with the same length as p.  The vector p is the covariate for which a caliper is needed.
}
  \item{caliper}{
Determines the type and length of the caliper.  The caliper becomes a vector cvex with length 2.  If is.null(caliper), then the caliper is +/- 0.2 times the standard deviation of p, namely
cvec = c(-.2,.2)*sd(p).  If caliper is a single number, then the caliper is +/- caliper, or cvec = c(-1,1)*abs(caliper).  If caliper is a vector of length 2, then an asymmetric caliper is used,
cvec = c(min(caliper),max(caliper)), where min(caliper) must be negative and max caliper must be positive.
}
  \item{penalty}{
Let I be the index of ith treated individual, 1,...,sum(z), and J be the index of the jth control, j=1,...,sum(1-z), so 1 <= I <= length(z) and so 1 <= J <= length(z).  The penality added to costmatrix[i,j] is 0 if cvec[1] <= p[I]-p[J] <= cvex[2].
}
  \item{twostep}{
If twostep is FALSE, then no action is taken.  If twostep is true, no action is take if 2 cvec[1] <= p[I]-p[J] <= 2 cvex[2], and otherwise costmatrix[i,j] is further increased by adding penalty.  In words, the penalty is doubled if p[I]-p[J] falls outside twice the caliper.
}
}
\details{
For discussion of directional calipers, see Yu and Rosenbaum (2019).
}
\value{
A penalized costmatrix.
}
\references{
Cochran, William G., and Donald B. Rubin. Controlling bias in observational studies: A review. Sankhya: The Indian Journal of Statistics, Series A 1973;35:417-446.

Yu, Ruoqi, and Paul R. Rosenbaum. <doi:10.1111/biom.13098> Directional penalties for optimal matching in observational studies. Biometrics 2019;75:1380-1390.
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
x<-data.frame(age,female)
detach(d)
rownames(x)<-d$SEQN
dist<-startcost(z)
z
x
dist

# Ten-year age caliper
addcaliper(dist,z,x$age,caliper=10,twostep=FALSE)

# Ten-year age caliper with twostep=TRUE
addcaliper(dist,z,x$age,caliper=10,twostep=TRUE)

# Same ten-year age caliper with twostep=TRUE
addcaliper(dist,z,x$age,caliper=c(-10,10))

# Asymmetric, directional age caliper with twostep=TRUE
addcaliper(dist,z,x$age,caliper=c(-2,10))
# Treated 109315 aged 30 is more than 2 years younger
# than control 109273 aged 36, 30-36<(-2), so
# row 109315 column 109273 is penalized, indeed
# double penalized, as 30-36<2*(-2)

rm(z,x,dist,d)
}

