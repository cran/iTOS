\name{binge}
\alias{binge}
\concept{Observational study}
\concept{Causal inference}
\concept{Matching}
\docType{data}
\title{
Binge Drinking and High Blood Pressure
}
\description{
These unmatched data are from NHANES, and they illustrate multivariate matching.
The matched version of the data is bingeM, and it was produced by the example in the documentation for the makematch() function.
}
\usage{data("binge")}
\format{
  A data frame with 4627 observations on the following 17 variables.
  \describe{
    \item{\code{SEQN}}{NHANES identification number}
    \item{\code{age}}{Age in years}
    \item{\code{ageC}}{Age cut into four categories, [20,30), [30,45), [45,60)
    and [60,Inf). }
    \item{\code{female}}{1=female, 0=male}
    \item{\code{educationf}}{Education in five categories: <9th grade, 9th-11th grade without a high school degree or equivalent, high school degree or equivalent, some college, at least a BA degree.  An ordered factor with levels \code{<9th} < \code{9-11} < \code{HS} < \code{SomeCol} < \code{>=BA}}
    \item{\code{education}}{The previous variable, educationf, as integer scores, 1-5.}
    \item{\code{bmi}}{BMI or body mass index.  A measure of obesity.}
    \item{\code{waisthip}}{Waist-to-hip ratio.  A measure of obesity.}
    \item{\code{vigor}}{1 if engages in vigorous activity, either in recreation or at work, 0 otherwise }
    \item{\code{smokenowf}}{Do you smoke now? \code{Everyday} < \code{SomeDays} < \code{No}}
    \item{\code{smokenow}}{The previous variable, smokenowf, as integer scores.}
    \item{\code{bpRX}}{1=currently taking medication to control high blood pressure, 0=other}
    \item{\code{smokeQuit}}{1=used to smoke regularly but quit, 0=other.  A current smoker and a never smoker both have value 0.}
    \item{\code{AlcGroup}}{An ordered factor with levels \code{B} < \code{N} < \code{P}}
    \item{\code{bpSystolic}}{Systolic blood pressure.  The average of up to three measurements.}
    \item{\code{bpDiastolic}}{Diastolic blood pressure.  The average of up to three measurements.}
    \item{\code{bpCombined}}{A combined measure of blood pressure.}
  }
}
\details{
This data set is intended to illustrate multivariate matching.  See the example
in the documentation for the makematch() function.

In the examples below, the simple B-N match in Section 4.3 of the iTOS book is
constructed.  It matches for the propensity score and nothing else.

bpCombined is the sum of two standardized measures, one for systolic blood pressure and one for diastolic blood pressure.  In the larger NHANES data set of individuals at least 20 years of age who are not pregnant, the median and the mad (=median absolute deviation from the median) were determined separately for systolic and diastolic blood pressure. The calculation used the median and mad functions in the 'stats' package, so the mad was by default scaled to resemble the standard deviation for a Normal distribution. bpCombined is the sum of two quantities: systolic blood pressure minus its median divided by its mad plus diastolic blood pressure minus its median divided by its mad.
}
\source{
The data are from the US National Health and Nutrition Examination Survey, NHANES 2017-March 2020 Pre-pandemic.  The 2017-2020 data were affected by COVID-19 and are not a survey.  The complete data are available from the CDC web page.  With minor differences, the data were used as an example in Rosenbaum (2023).
}
\references{
Rosenbaum, P. R. (2023) <doi:10.1111/biom.13921> A second evidence factor for a second control group. Biometrics, 79(4), 3968-3980.

Rosenbaum, P.R. and Rubin, D.B. (1985) <doi:10.2307/2683903> Constructing a control group using multivariate matched sampling methods that incorporate the propensity score. The American Statistician, 39(1):33-38.

US National Health and Nutrition Examination Survey, 2017-2020.  Atlanta: US Centers for Disease Control.
}
\examples{
data(binge)
attach(binge)
# A simple goal is to match each of the 206 binge drinkers
# to one control from each of the two control groups.
table(AlcGroup)
# The matching ratios are very different, 19-to-1 or 2.4 to 1.
table(AlcGroup)/206
# Before matching, the age distributions are very different.
boxplot(age~AlcGroup)
# NHANES does not ask every question of every person, often
# using either age or sex to determine what will be asked.
# If we match exactly for age categories that determine what
# will be asked, then both members of a matched pair will
# either have an answer or no answer, and this simplifies
# some analyses, while also beginning the work of controlling
# an important covariate.
table(AlcGroup,ageC)
# Age is going to be a problem for the P controls.  There
# are 29 bingers B under age 30, but only 19 past bingers P.
table(AlcGroup,smokenowf)
# Smoking is a challenge, too.  The 19-to-1 matching ratio
# for N controls drops to about 4 = 364/92 for everyday
# smokers, and from 2.43 to 1.44 = 133/92 for P controls.
# There are too few P controls who smoke SomeDays to
# match exactly with bingers B.
detach(binge)

####################################################
# This example produces the elementary match in
# Section 4.3 of the iTOS book.
# It matches binge drinkers (B) to never binge
# controls (N), matching just for the propensity
# score.  The match uses both a caliper and a fine
# balance constraint for the propensity score.
# Generally, one does not just match for the
# propensity score, but this is an example.
# See also the documentation for makematch.
library(iTOS)
data(binge)
#  Make the treatment indicator z for B versus N
z<-rep(NA,dim(binge)[1])
z[binge$AlcGroup=="B"]<-1
z[binge$AlcGroup=="N"]<-0

#  I find it convenient to write a small function
#  to create a match.  In that way, small
#  changes in the function can improve the
#  match by improving covariate balance.
#  It also documents the details of the match.
#  It also makes it possible to reproduce the match.

matchPropensity<-function(z,ncontrols=1){
  #
  # Some bookkeeping.
  # Select the B versus N part of the binge data
  dt<-binge[!is.na(z),]
  z<-z[!is.na(z)]
  # Sort data, placing treated (B) first,
  # and then ordered by SEQN.
  dt<-dt[order(1-z,dt$SEQN),]
  z<-z[order(1-z,dt$SEQN)]
  rownames(dt)<-dt$SEQN
  names(z)<-dt$SEQN

  #  Initialize the distance matrix on
  #  the left and right to zero distances
  left<-startcost(z)
  right<-startcost(z)

  #  Create the propensity score
  attach(dt)
  propmod<-glm(z~age+female+education+smokenow+smokeQuit+bpRX+
                 bmi+vigor+waisthip,family=binomial)
  p<-propmod$fitted.values

  # The left distance matrix is changed from zero
  # by adding a caliper on the propensity score.
  # The caliper is almost the one used in
  # Rosenbaum and Rubin (1985, American Statistician).
  # If two individuals differ on the propensity score
  # by more than 0.2 times the standard deviation of p,
  # the distance between them is increased from 0 to 10.
  # That was the caliper in Rosenbaum and Rubin (1985).
  # By default, addcaliper doubles the distance (to 20) at
  # 0.4 = 2 x 0.2 times the standard deviation of p.
  # Without this doubling feature, the caliper views
  # everyone who violates the 0.2 caliper as equivalent.
  left<-addcaliper(left,z,p,penalty=10)

  # The right distance matrix now adds a fine balance
  # constraint.  The variable (p>0.05)+(p>.1)+(p>.15)+(p>.2)
  # takes integer values from 0 to 4, taking steps up as
  # the propensity score increases.
  # The right distance matrix is 0 if two people fall in
  # the same category, is 1000 if they fall in adjacent
  # categories, 2000 if there if there is a category
  # between them, and so on.  Because 1000 is so much
  # larger than 10, the fine balance constraint takes
  # priority over the caliper.
  right<-addinteger(right,z,(p>0.05)+(p>.1)+(p>.15)+(p>.2))

  # Some more bookkeeping
  detach(dt)
  dt<-cbind(dt,z,p)

  # The big step: Use the distance matrices to make the match
  m<-makematch(dt,left,right,ncontrols=ncontrols)

  # Final bookkeeping
  m$mset<-as.integer(m$mset)
  treated<-m$SEQN[m$z==1]
  treated<-as.vector(t(matrix(treated,length(treated),ncontrols+1)))
  m<-cbind(m,treated)
  list(m=m,dt=dt)
}

# Call the function above to make the match
mProp<-matchPropensity(z)
m<-mProp$m

# Make Table 4.3 in the iTOS book.
t.test(m$age~m$z)$p.value
t.test(m$female~m$z)$p.value
t.test(m$education~m$z)$p.value
t.test(m$bmi~m$z)$p.value
t.test(m$waisthip~m$z)$p.value
t.test(m$vigor~m$z)$p.value
t.test(m$smokenow~m$z)$p.value
t.test(m$smokeQuit~m$z)$p.value
t.test(m$bpRX~m$z)$p.value
t.test(m$p~m$z)$p.value
dt<-mProp$dt
tr<-m$z==1
co<-m$z==0
un<-!is.element(dt$SEQN,m$SEQN)

vnames<-c("age","female","education","bmi","waisthip","vigor",
          "smokenow","smokeQuit","bpRX","p")
o<-matrix(NA,10,3)
pval<-rep(NA,10)
names(pval)<-vnames
rownames(o)<-vnames
colnames(o)<-c("Treated","Control","Unmatched")
for (i in 1:10){
  vname<-vnames[i]
  vm<-as.vector(m[,colnames(m)==vname])
  vdt<-as.vector(dt[,colnames(dt)==vname])
  o[i,1]<-mean(vm[tr])
  o[i,2]<-mean(vm[co])
  o[i,3]<-mean(vdt[un])
  pval[i]<-t.test(vm[tr],vm[co])$p.value
}
library(xtable)
o<-cbind(o,pval)
xtable(o,digits=c(NA,2,2,2,3))
m[5:6,]

}
\keyword{datasets}
