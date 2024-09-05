\name{iTOS-package}
\alias{iTOS-package}
\alias{iTOS}
\docType{package}
\title{
\packageTitle{iTOS}
}
\description{
\packageDescription{iTOS}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{iTOS}
\packageIndices{iTOS}
}
\author{
\packageAuthor{iTOS}

Maintainer: \packageMaintainer{iTOS}
}
\references{
Rosenbaum, Paul R. Introduction to the Theory of Observational Studies.  Manuscript, 2024.

Rosenbaum, P. R. (1989) <doi:10.2307/2290079> Optimal matching for observational studies. Journal of the American Statistical Association, 84, 1024-1032.

Rosenbaum, Paul R., Richard N. Ross, and Jeffrey H. Silber (2007)
<doi:10.1198/016214506000001059> Minimum distance matched sampling with fine balance in an observational study of treatment for ovarian cancer. Journal of the American Statistical Association 102, 75-83.

Rosenbaum, P. R. (2023a) <doi:10.1111/biom.13558> Sensitivity analyses informed by tests for bias in observational studies. Biometrics 79, 475-487.

Rosenbaum, P. R. (2023b) <doi:10.1111/biom.13921> A second evidence factor for a second control group. Biometrics, 79, 3968-3980.

Yang, D., Small, D. S., Silber, J. H. and Rosenbaum, P. R. (2012)
<doi:10.1111/j.1541-0420.2011.01691.x> Optimal matching with minimal deviation from fine balance in a study of obesity and surgical outcomes. Biometrics, 68, 628-636.

Zhang, B., D. S. Small, K. B. Lasater, M. McHugh, J. H. Silber, and P. R. Rosenbaum (2023) <doi:10.1080/01621459.2021.1981337> Matching one sample according to two criteria in observational studies. Journal of the American Statistical Association, 118, 1140-1151.
}

\keyword{ package }
\examples{
data(binge)
table(binge$AlcGroup)
data(aHDL)
table(aHDL$grp)
}
