\name{evalBal}
\alias{evalBal}

\title{
Evaluate Covariate Balance in a Matched Sample
}
\description{
The covariate balance in a matched sample is compared to the balance that
would have been obtained in a completely randomized experiment built from
the same people.  The existing matched sample is randomized to treatment or
control many times, and various measures of covariate balance are computed
from the one matched sample and the many randomized experiments.  The main
elements of the method are from Hansen and Bowers (2008), Pimentel et al. (2015, Table 1), and Yu (2021).
}
\usage{
evalBal(z, x, statistic = "s", reps = 1000, trunc = 0.2, nunique = 2, alpha = 0.05)
}
\arguments{
  \item{z}{
z is a vector, with z[i]=1 for treated and z[i]=0 for control
}
  \item{x}{
x is a matrix or a data-frame containing covariates with no NAs.  An error will result if length(z) does not equal the number of rows of x.
}
  \item{statistic}{
If statistic="t", the default two-sample t-test from the 'stats' package computes a two-sided P-value for each covariate in the matched sample and the many randomized  experiments.  If statistic="w", then the two-sample Wilcoxon rank sum test is used, as implemented in the 'stats' package.  If statistic="s", the two-sample Wilcoxon rank sum test is used for numeric covariates with more than nunique distinct values, or the chi-square test for a two-way table is used for factors and for numeric covariates with at most nunique distinct values.  The default value is nunique=2.
}
  \item{reps}{
A positive integer.  A total of reps randomized experiments are compared to the one matched sample.
}
  \item{trunc}{
For each simulated randomized experiment, a P-value is computed for each covariate.
Also computed is the statistic proposed by Zykin et al. (2002) defined as the
product of those covariate-specific P-values that do not exceed trunc.  This truncated product is not a P-value, but it is a statistic.  See Details.
}
  \item{nunique}{
See the option statistic="s" above.
}
  \item{alpha}{
For each simulated randomized experiment, a P-value is computed for each covariate.
Also computed is number of these P-values that are less than or equal to alpha.
}
}
\details{
Truncated Product:  For independent uniform P-values, Zaykin et al. (2002) derive a true P-value from the null distribution of their truncated product of P-values.  That null distribution can be computed using the truncatedP() function in the 'sensitivitymv' package; however, it is not used here, because the P-values for dependent covariates are not independent.  Rather, the actual randomization distribution of the truncated product is simulated.  Taking trunc=1 yields Fisher's statistic, namely the product of all of the P-values.
}
\value{
\item{test.name }{The name of the test used. }
\item{actual }{For each covariate, the usual two sample P-values comparing the distributions of
treated and control groups for each covariate in x.  Also, the minimum P-value,
the truncated product of P-values, and the number of P-values less than or equal to alpha -- none of these quantities is a P-value.}
\item{simBetter }{Comparison of the covariate imbalance in the actual
matched sample and the many simulated randomized experiment.  Of the reps
randomized experiments, how many were strictly better balanced than the
one matched observational study.}
\item{sim }{Details of the simulated randomized experiments.  }
}
\references{

Hansen, B. B., and Bowers, J. (2008) <doi:10.1214/08-STS254> Covariate balance in simple, stratified and clustered comparative studies. Statistical Science, 23, 219-236.

Pimentel, S. D., Kelz, R. R., Silber, J. H. and Rosenbaum, P. R. (2015)
<doi:10.1080/01621459.2014.997879> Large, sparse optimal matching with refined covariate balance in an observational study of the health outcomes produced by new surgeons. Journal of the American Statistical Association, 110, 515-527.

Yu, R. (2021) <doi:10.1111/biom.13098> Evaluating and improving a matched comparison of antidepressants and bone density. Biometrics, 77(4), 1276-1288.

Zaykin, D. V., Zhivotovsky, L. A., Westfall, P. H. and Weir, B. S. (2002)
<doi:10.1002/gepi.0042> Truncated product method of combining P-values. Genetic Epidemiology, 22, 170-185.
}
\author{
Paul R. Rosenbaum
}

\examples{
# Evaluate the balance in the bingeM matched sample.
# The more difficult control group, P, will be evaluated.
data(bingeM)
attach(bingeM)
xBP<-data.frame(age,female,education,bmi,waisthip,vigor,smokenow,bpRX,smokeQuit)
xBP<-xBP[bingeM$AlcGroup!="N",]
detach(bingeM)
z<-bingeM$z[bingeM$AlcGroup!="N"]

# In a serious evaluation, take reps=1000 or reps=10000.
# For a quick example, reps is set to reps=100 here.
set.seed(5)
balBP<-evalBal(z,xBP,reps=100)
balBP$test.name
# This says that age is compared using the Wilcoxon two-sample test,
# and female is compared using the chi-square test for a 2x2 table.
# Because the default, nunique=2, was used, education was evaluated
# using Wilcoxon's test; however, changing nunique to 5 would evaluate
# the 5 levels of education using a chi-square test for a 2x5 table.
balBP$actual
# In the matched sample, none of the 9 covariates has a P-value
# of 0.05 or less.  The smallest of the 9 P-values is .366, and
# their truncated product is 1, because, by definition, the truncated
# product is 1 if all of the P-values are above trunc.
apply(balBP$sim,2,median)
# In the simulated randomized experiments, the median of the 100
# P-values is close to 1/2 for all covariates.
balBP$simBetter
# Of the 100 simulated randomized experiments, only 3 were better
# balanced than the matched sample in terms of the minimum P-value,
# and none were better balanced in terms of the truncated product
# of P-values.
#
# There were too few controls in the P control group who smoked
# on somedays to match exactly for smokenow.  Nonetheless, only
# 13/100 randomized experiments were better balanced for smokenow.
#
# Now compare the binge group B to the combination of the two
# control groups.
attach(bingeM)
x<-data.frame(age,female,education,bmi,waisthip,vigor,smokenow,bpRX,smokeQuit)
detach(bingeM)
set.seed(5)
balAll<-evalBal(bingeM$z,x,reps=100,trunc=1)
balAll$actual
balAll$simBetter
# This time, Fisher's product of all P-values is used, with trunc=1.
# In terms of the minimum P-value and the product of P-values,
# none of the 100 randomized experiments is better balanced than the
# matched sample.
}

