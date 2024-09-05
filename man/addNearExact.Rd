\name{addNearExact}
\alias{addNearExact}

\title{
Add a Near-exact Penalty to an Exisiting Distance Matrix.
}
\description{
Add a Near-exact Penalty to an Exisiting Distance Matrix.
}
\usage{
addNearExact(costmatrix, z, exact, penalty = 1000)
}

\arguments{
  \item{costmatrix}{
An existing cost matrix with sum(z) rows and sum(1-z) columns.  The function checks the compatability of costmatrix, z and p; so, it may stop with an error if these are not of appropriate dimensions.  In particular, costmatrix may come from startcost().
}
  \item{z}{
A vector with z[i]=1 if individual i is treated or z[i]=0 if individual i is control.  The rows of costmatrix refer to treated individuals and the columns refer to controls.
}
  \item{exact}{
A vector with the same length as z.  Typically, exact take a small or moderate number of values.
}
  \item{penalty}{
One positive number.
}
}
\details{
If the ith treated individual and the jth control have different values of exact, then the distance between them in costmatrix is increased by adding penalty.
}
\value{
A penalized distance matrix.
}

\author{
Paul R. Rosenbaum
}
\note{
A sufficiently large penalty will maximize the number of individuals exactly matched for exact.  A smaller penalty will tend to increase the number of individuals matched exactly, without prioritizing one covariate over all others.

If the left distance matrix is penalized, it will affect pairing and balance; however, if the right distance matrix is penalized it will affect balance only, as in the near-fine balance technique of Yang et al. (2012).

Adding several near-exact penalties for different covariates on the right distance matrix implements a Hamming distance on the joint distribution of those covariates, as discussed in Zhang et al. (2023).

Near-exact matching for a nominal covariate is discussed and contrasted with exact matching in Sections 10.3 and 10.4 of Rosenbaum (2020).  Near-exact matching is always feasible, because it implements a constraint using a penalty.  Exact matching may be infeasible, but when feasible it may be used to speed up computations.  For an alternative method of
speeding computations, see Yu et al. (2020) who identify feasible constraints very quickly prior to matching with those constraints.
}
\references{
Rosenbaum, P. R. (2020) <doi:10.1007/978-3-030-46405-9> Design of Observational Studies (2nd Edition).  New York: Springer.

Yang, D., Small, D. S., Silber, J. H. and Rosenbaum, P. R. (2012)
<doi:10.1111/j.1541-0420.2011.01691.x> Optimal matching with minimal deviation from fine balance in a study of obesity and surgical outcomes. Biometrics, 68, 628-636. (Extension of fine balance useful when fine balance is infeasible. Comes as close as possible to fine balance. Implemented in makematch() by placing a large near-exact
penalty on a nominal/integer covariate x1 on the right distance matrix.)

Yu, R., Silber, J. H., Rosenbaum, P. R. (2020) <doi:10.1214/19-STS699> Matching Methods for Observational Studies Derived from Large Administrative Databases. Statistical Science, 35, 338-355.

Zhang, B., D. S. Small, K. B. Lasater, M. McHugh, J. H. Silber, and P. R. Rosenbaum (2023) <doi:10.1080/01621459.2021.1981337> Matching one sample according to two criteria in observational studies. Journal of the American Statistical Association, 118, 1140-1151.

Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H. and Rosenbaum, P. R. (2011) <doi:10.1198/tas.2011.11072> Matching for several sparse nominal variables in a case control study of readmission following surgery. The American Statistician, 65(4), 229-238.
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
addNearExact(dist,z,x$female)
addNearExact(dist,z,x$age<40,penalty=10)

# Combine several penalties
dist<-addNearExact(dist,z,x$female)
dist<-addNearExact(dist,z,x$age<40,penalty=10)
dist
dist<-addNearExact(dist,z,x$age<60,penalty=5)
dist
# This distance suggests pairing 109315-109266
# and 109365-109290
rm(z,x,dist,d)
}
