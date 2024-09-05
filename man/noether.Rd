\name{noether}
\alias{noether}
\title{
Sensitivity Analysis Using Noether's Test for Matched Pairs
}
\description{
Computes a sensitivity analysis for treated-minus-control matched
pair differences in observational studies.
}
\usage{
noether(y, f = 2/3, gamma = 1, alternative = "greater")
}
\arguments{
  \item{y}{
A vector of treated-minus-control matched pair differences.
}
  \item{f}{
A nonnegative number strictly less than 1.  Suppose that there are I matched
pair differences, length(y)=I.  Rank the absolute pair differences from 1 to I
with average ranks for ties.  Noether's statistic looks at the roughly (1-f)I
pair differences with absolute ranks that are at least fI, and computes the
sign test from these fI pair differences.  With f=0, Noether's statistic
is the usual sign test statistic.  With f=2/3, Noether's statistic focuses
on the 1/3 of pairs with the largest absolute pair differences.  In his
article, Noether suggested f=1/3 for randomized matched pair differences
from a Normal distribution, but f=2/3 is better for sensitivity analyses in
observational studies.  Pair differences that are zero are not counted, but
this is uncommon for f=2/3.
}
  \item{gamma}{
A number greater than or equal to 1.  gamma is the sensitivity
parameter, where gamma=1 for a randomization test, and gamma>1
for a sensitivity analysis.
}
  \item{alternative}{
The possible alternatives are "greater", "less" or "two.sided";
however, "two.sided" is available only for gamma=1.
}
}
\details{
Noether's (1973) strengthens the sign test.  In a randomized experiment, it
increase power.  In an observational study, it increases design sensitivity
and the Bahadur efficiency of a sensitivity analysis.

Because the test has a binomial null distribution in both a randomized
experiment and in an observational study, Noether's test is used in a
number of problems in Introduction to the Theory of Observational Studies.

Noether's test is related to methods of Gastwirth (1966), Brown (1981), and
Markowski and Hettmansperger (1982).  Its properties in an observational study
are discussed Rosenbaum (2012, 2015).
}
\value{
\item{number.pairs }{Number of pairs used by Noether's statistic, roughly
fI.}
\item{positive.pairs }{Number of positive pair differences among used pairs.}
\item{pval }{P-value testing the null hypothesis of no treatment effect.
Obtained from the binomial distribution.}
}
\references{
Brown, B. M. (1981) <doi:10.1093/biomet/68.1.235> Symmetric quantile averages and related estimators. Biometrika, 68(1), 235-242.

Gastwirth, J. L. (1966) <doi:10.1080/01621459.1966.10482185> On robust procedures. Journal of the American Statistical Association, 61(316), 929-948.

Markowski, E. P. and Hettmansperger, T. P. (1982)
<doi:10.1080/01621459.1982.10477905> Inference based on simple rank step score statistics for the location model. Journal of the American Statistical Association, 77(380), 901-907.

Noether, G. E. (1973) <doi:10.1080/01621459.1973.10481411> Some simple distribution-free confidence intervals for the center of a symmetric distribution. Journal of the American Statistical Association, 68(343), 716-719.

Rosenbaum, P. R. (2012) <10.1214/11-AOAS508> An exact adaptive test with superior design sensitivity in an observational study of treatments for ovarian cancer.  Annals of Applied Statistics, 6, 83-105.

Rosenbaum, P. R. (2015) <doi:10.1080/01621459.2014.960968> Bahadur efficiency of sensitivity analyses in observational studies. Journal of the American Statistical Association, 110(509), 205-217.
}
\author{
Paul R. Rosenbaum
}
\note{
As noted in the Preface to Introduction to the Theory of Observational Studies,
Noether's statistic is used in a sequence of Problems that appear in various
chapters.
}

\examples{
set.seed(1)
y<-rnorm(1000)+.5
noether(y,f=0,gamma=3)
noether(y,f=2/3,gamma=3)
}

