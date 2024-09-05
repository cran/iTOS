\name{makenetwork}
\alias{makenetwork}

\title{
Make the Network Used for Matching with Two Criteria
}
\description{
This function is of limited interest to most users, and is called by other functions in the package.  Makes the network used in the two-criteria matching method of Zhang et al. (2022).
}
\usage{
makenetwork(costL, costR, ncontrols = 1, controlcosts = NULL)
}
\arguments{
  \item{costL}{
The distance matrix on the left side of the network, used for pairing.
}
  \item{costR}{
The distance matrix on the right side of the network, used for balancing.
}
  \item{ncontrols}{
One positive integer, 1 for pair matching, 2 for matching two controls to each treated individual, etc.
}
  \item{controlcosts}{
An optional vector of costs used to penalize the control-control edges.
}
}
\details{
This function creates the network depicted in Figure 1 of Zhang et al. (2023).

A minimum cost flow in this network is found by passing net to callrelax() in the package 'rcbalance'.  If you use callrelax(), I strongly suggest you do this with solver set to 'rrelaxiv'.  The 'rrelaxiv' package has an academic license.  The 'rrelaxiv' package uses Fortran code from RELAX IV developed by Bertsekas and Tseng (1988, 1994) based on Bertsekas' (1990) auction algorithm.
}
\value{
\item{idtreated}{Row identifications for treated individuals}
\item{idcontrol}{Control identifications for control individuals}
\item{net}{A network for use with callrelax in the 'rcbalance' package.}
}
\references{
Bertsekas, D. P., Tseng, P. (1988) <doi:10.1007/BF02288322> The relax codes for linear minimum cost network flow problems. Annals of Operations Research, 13, 125-190.

Bertsekas, D. P. (1990) <doi:10.1287/inte.20.4.133> The auction algorithm for assignment and other network flow problems: A tutorial. Interfaces, 20(4), 133-149.

Bertsekas, D. P., Tseng, P. (1994)
<http://web.mit.edu/dimitrib/www/Bertsekas_Tseng_RELAX4_!994.pdf> RELAX-IV: A Faster Version of the RELAX Code for Solving Minimum Cost Flow Problems.

Zhang, B., D. S. Small, K. B. Lasater, M. McHugh, J. H. Silber, and P. R. Rosenbaum (2023) <doi:10.1080/01621459.2021.1981337> Matching one sample according to two criteria in observational studies. Journal of the American Statistical Association, 118, 1140-1151.
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
Ldist<-startcost(z)
Ldist<-addcaliper(Ldist,z,x$age,caliper=10,penalty=5)
Rdist<-startcost(z)
Rdist<-addNearExact(Rdist,z,x$female)
makenetwork(Ldist,Rdist)
}

