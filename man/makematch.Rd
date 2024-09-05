\name{makematch}
\alias{makematch}
\concept{Matching}
\concept{Two-criteria matching}
\concept{Propensity score}
\concept{Network optimization}

\title{
Two-Criteria Matching
}
\description{
Implements the method of Zhang et al (2023) <doi:10.1080/01621459.2021.1981337>.
As special cases, this includes: minimum distance (or optimal matching), matching
with fine balance or near-fine balance or refined balance; see Chapter 5 of the iTOS book or the references below.
}
\usage{
makematch(dat, costL, costR, ncontrols = 1, controlcosts = NULL,solver='rlemon')
}
\arguments{
  \item{dat}{
A data frame.  Typically, this is the entire data set.  Part of it will be
returned as a matched sample with some added variables.
}
  \item{costL}{
The distance matrix on the left side of the network, used for pairing.  This matrix would most often be made by adding distances to a zero distance matrix created by startcost(), for instance, using addMahal().  In section 5.4 of the book iTOS or
Figure 1 of Zhang et al. (2023), these are the costs on the left treated-control edges.
}
  \item{costR}{
The distance matrix on the right side of the network, used for balancing.  This matrix would most often be made by adding distances to a zero distance matrix created by startcost(), for instance, using addNearExact().  If you do not need a right distance matrix, then initialize it to zero using startcost() and do not add additional distances to its intial form.  In section 5.4 of the book iTOS or Figure 1 of Zhang et al. (2023), these are the costs on the right control-treated edges.
}
  \item{ncontrols}{
One positive integer, 1 for pair matching, 2 for matching two controls to each treated individual, etc.  When ncontrols=2 is feasible, it is often useful to compare the quality of the match obtained with ncontrols=1 and ncontrols=2.  In Figure 1 of Zhang et al. (2023), ncontrols determines the total flow that leaves the source and is collected by the sink as ncontrols times the number of treated individuals.
}
  \item{controlcosts}{
An optional vector of costs used to penalize the control-control edges.  For instance, one might penalize the use of controls with low propensity scores.  This is illustrated in the example for the B-P match.  In Figure 1 of Zhang et al. (2023), these are the costs on the central control-control edges.
}
  \item{solver}{
The solver must be either 'rlemon' or 'rrelaxiv'.  Both solvers find
a minimum cost flow in a network, but rlemon is public and rrelaxiv has
an academic license; so, rrelaxiv requires a separate instalation;
see details.
}
}
\details{
This function calls the callrelax() function in Samuel Pimentel's package
rcbalance.  There are two solvers for callrelax, namely rlemon and the
rrelaxiv code of Bertsekas and Tseng (1988).  The default is rlemon.
rrelaxiv may be the better solver,
but it has an academic license, so it is not distributed by CRAN and must
be downloaded separately from github at https://errickson.net/rrelaxiv/ or
<https://github.com/josherrickson/rrelaxiv/>; see the documentation for callrelax().
There is no need to install rrelaxiv: the package works without it.

In principle, it can happen that two different matches
have the same minimum cost, and in this case the two solvers might produce
different but equally good matched samples.  This is not a problem, but it
can come as a surprise.  It is unlikely to happen unless the distance matrix
has many tied distances.  Tied distances are rare in modern distance matrices, with many covariates, using modern ambitious matching techniques.
}
\value{
Returns a matched data set.  The matched rows of dat are returned with a new variable mset indicating the matched set.  The returned file is sorted by mset and z.
}
\references{

Bertsekas, D. P., Tseng, P. (1988) <doi:10.1007/BF02288322> The Relax codes for linear minimum cost network flow problems. Annals of Operations Research, 13, 125-190.

Bertsekas, D. P. (1990) <doi:10.1287/inte.20.4.133> The auction algorithm for assignment and other network flow problems: A tutorial. Interfaces, 20(4), 133-149.

Bertsekas, D. P., Tseng, P. (1994)
<http://web.mit.edu/dimitrib/www/Bertsekas_Tseng_RELAX4_!994.pdf> RELAX-IV: A Faster Version of the RELAX Code for Solving Minimum Cost Flow Problems.

Hansen, B. B. and Klopfer, S. O. (2006) <doi:10.1198/106186006X137047> "Optimal full matching and related designs via network flows". Journal of computational and Graphical Statistics, 15(3), 609-627. ('optmatch' package)

Hansen, B. B. (2007)
<https://www.r-project.org/conferences/useR-2007/program/presentations/hansen.pdf> Flexible, optimal matching for observational studies. R News, 7, 18-24. ('optmatch' package)

Pimentel, S. D., Kelz, R. R., Silber, J. H. and Rosenbaum, P. R. (2015)
<doi:10.1080/01621459.2014.997879> Large, sparse optimal matching with refined covariate balance in an observational study of the health outcomes produced by new surgeons. Journal of the American Statistical Association, 110, 515-527. (Introduces an extension of fine balance called refined balance that is implemented in Pimentel's package 'rcbalance'.  This can be implemented using makematch() by, say, placing on the right a very large near-exact penalty on a nominal/integer covariate x1, and a still large but smaller penalty on the nominal/integer covariate as.integer(factor(x1):factor(x2)), etc.)

Pimentel, S. D. (2016) "Large, Sparse Optimal Matching with R Package rcbalance" <https://obsstudies.org/large-sparse-optimal-matching-with-r-package-rcbalance/> Observational Studies, 2, 4-23. (Discusses and illustrates the use of Pimentel's 'rcbalance' package.)

Rosenbaum, P. R. and Rubin, D. B. (1985) <doi:10.1080/00031305.1985.10479383> Constructing a control group using multivariate matched sampling methods that incorporate the propensity score. The American Statistician, 39, 33-38. (This paper suggested
emphasizing the propensity score in a match, but also attempting to obtain a
close match for key covariates using a Mahalanobis distance.)

Rosenbaum, P. R. (1989) <doi:10.1080/01621459.1989.10478868> Optimal matching for observational studies.  Journal of the American Statistical Association, 84(408), 1024-1032. (Discusses and illustrates fine balance using minimum cost flow in a network in section 3.2.  This is implemented using makematch() by placing a large near-exact
penalty on a nominal/integer covariate x1 on the right distance matrix.)

Rosenbaum, P. R., Ross, R. N. and Silber, J. H. (2007) <doi:10.1198/016214506000001059> Minimum distance matched sampling with fine balance in an observational study of treatment for ovarian cancer. Journal of the American Statistical Association, 102, 75-83.

Rosenbaum, P. R. (2020) <doi:10.1007/978-3-030-46405-9> Design of Observational Studies (2nd Edition).  New York: Springer.

Yang, D., Small, D. S., Silber, J. H. and Rosenbaum, P. R. (2012)
<doi:10.1111/j.1541-0420.2011.01691.x> Optimal matching with minimal deviation from fine balance in a study of obesity and surgical outcomes. Biometrics, 68, 628-636. (Extension of fine balance useful when fine balance is infeasible. Comes as close as possible to fine balance. Implemented in makematch() by placing a large near-exact
penalty on a nominal/integer covariate x1 on the right distance matrix.)

Yu, Ruoqi, and P. R. Rosenbaum. <doi:10.1111/biom.13098> Directional penalties for optimal matching in observational studies. Biometrics 75, no. 4 (2019): 1380-1390.  (If a covariate is very out-of-balance, we should prefer a mismatch that works against the imbalance to an equally large mismatch that supports the imbalance.  This is illustrated with the propensity score in the example below.  Because a directional penalty tolerates a big mismatch in the good direction, it makes sense to place the penalty on the right distance matrix.)

Yu, R. (2023) <doi:10.1111/biom.13771> How well can fine balance work for covariate balancing? Biometrics.  79(3), 2346-2356.

Zhang, B., D. S. Small, K. B. Lasater, M. McHugh, J. H. Silber, and P. R. Rosenbaum (2023) <doi:10.1080/01621459.2021.1981337> Matching one sample according to two criteria in observational studies. Journal of the American Statistical Association, 118, 1140-1151.
(This is the basic reference for the makematch() function.  It generalizes the concepts
of fine balance, near-fine balance and refined balance that were developed in other
references.)

Zubizarreta, J. R., Reinke, C. E., Kelz, R. R., Silber, J. H. and Rosenbaum, P. R. (2011) <doi:10.1198/tas.2011.11072> Matching for several sparse nominal variables in a case control study of readmission following surgery. The American Statistician, 65(4), 229-238.
(This paper combines near-exact matching and fine balance for the same nominal covariate.  It is implemented in makematch() by placing the same covariate on the left and the right, as with smokenow in the example below.)
}
\author{
Paul R. Rosenbaum
}

\examples{
# See also the examples for binge in the documentation for the dataset binge.
\donttest{
data(binge)
# matchNever creates the B-N match for the binge data.
# matchPast creates the B-P match for the binge data.
# Each match has its own propensity score, and these
# mean different things in the B-N match and the B-P
# match.  The propensity score is denoted p.
# The Mahalanobis distance is the rank-based method
# described in Rosenbaum (2020, section 9.3).
# A directional caliper from Yu and Rosenbaum (2019)
# favors controls with high propensity scores, p.
#
# The two matches share most features, including:
# 1. A heavy left penalty for mismatching for female and bpRX.
# 2. Left penalties for mismatching for ageC, vigor, smokenow.
# 3. A left Mahalanobis distance for all covariates.
# 4. A heavy right penalty for mismatching smokenow.
# 5. Right penalties for education and smokeQuit.
# 6. A right Mahalanobis distance for just female, age and p.
# 7. An asymmetric, directional caliper on p.
# Because the left distance determines pairing, it emphasizes
# covariates that are out-of-balance and thought to be related
# to blood pressure.  Because the right distance affects balance
# but not pairing, it worries about education and smoking in the
# distant past, as well as the propensity score which again is
# focused on covariate balance.  The asymmetric caliper is
# tolerant of mismatches in the desired direction, so it too
# is placed on the right.  Current smoking, smokenow, is placed
# on both the left and the right: even if we cannot always pair
# for it, perhaps we can balance it.
# In common practice, before examining outcomes, one compares
# several matched designs, fixing imperfections by adjusting
# penalties and other considerations.
#
# The B-P match is more difficult, because the P group is
# smaller than the N group.  The B-P match uses the
# controlcosts feature while the B-N match does not.
# In the B-P match, there are too few young controls
# and too few controls with high propensity scores.
# Therefore, controlcosts penalize the use of controls
# with both low propensity scores (p<.4) and higher
# ages (age>42), thereby minimizing the use of these
# controls, even though the use of some of these
# controls is unavoidable in a pair match.

matchNever<-function(z,ncontrols=1){
  dt<-binge[!is.na(z),]
  z<-z[!is.na(z)]
  dt<-dt[order(1-z,dt$SEQN),]
  z<-z[order(1-z,dt$SEQN)]
  rownames(dt)<-dt$SEQN
  names(z)<-dt$SEQN
  left<-startcost(z)
  right<-startcost(z)
  attach(dt)
  propmod<-glm(z~age+female+education+smokenow+smokeQuit+bpRX+
           bmi+vigor+waisthip,family=binomial)
  p<-propmod$fitted.values
  left<-addinteger(left,z,as.integer(ageC),penalty=100)
  left<-addNearExact(left,z,female,penalty = 10000)
  left<-addNearExact(left,z,bpRX,penalty = 10000)
  left<-addNearExact(left,z,vigor,penalty=10)
  left<-addinteger(left,z,smokenow,penalty=10)
  left<-addMahal(left,z,cbind(age,bpRX,female,education,smokenow,
        smokeQuit,bmi,vigor,waisthip))
  right<-addMahal(right,z,cbind(female,age,p))
  right<-addinteger(right,z,education,penalty=10)
  right<-addinteger(right,z,smokenow,penalty=1000)
  right<-addinteger(right,z,smokeQuit,penalty=10)
  right<-addcaliper(right,z,p,caliper=c(-1,.03),penalty=10)
  detach(dt)
  dt<-cbind(dt,z,p)
  m<-makematch(dt,left,right,ncontrols=ncontrols)
  m$mset<-as.integer(m$mset)
  treated<-m$SEQN[m$z==1]
  treated<-as.vector(t(matrix(treated,length(treated),ncontrols+1)))
  m<-cbind(m,treated)
  list(m=m,dt=dt)
}



matchPast<-function(z,ncontrols=1){
  dt<-binge[!is.na(z),]
  z<-z[!is.na(z)]
  dt<-dt[order(1-z,dt$SEQN),]
  z<-z[order(1-z,dt$SEQN)]
  rownames(dt)<-dt$SEQN
  names(z)<-dt$SEQN
  left<-startcost(z)
  right<-startcost(z)
  attach(dt)
  propmod<-glm(z~age+female+education+smokenow+smokeQuit+bpRX+
                 bmi+vigor+waisthip,family=binomial)
  p<-propmod$fitted.values
  left<-addinteger(left,z,as.integer(ageC),penalty=100)
  left<-addNearExact(left,z,female,penalty = 10000)
  left<-addNearExact(left,z,bpRX,penalty = 10000)
  left<-addNearExact(left,z,vigor,penalty=10)
  left<-addinteger(left,z,smokenow,penalty=10)
  left<-addMahal(left,z,cbind(age,bpRX,female,education,smokenow,
        smokeQuit,bmi,vigor,waisthip))
  right<-addMahal(right,z,cbind(female,age,p))
  right<-addinteger(right,z,education,penalty=10)
  right<-addinteger(right,z,smokenow,penalty=1000)
  right<-addinteger(right,z,smokeQuit,penalty=10)
  right<-addcaliper(right,z,p,caliper=c(-1,.03),penalty=10)
  controlcosts<-((p[z==0]<.4)&(age[z==0]>42))*1000
  detach(dt)
  dt<-cbind(dt,z,p)
  m<-makematch(dt,left,right,ncontrols=ncontrols,controlcosts=controlcosts)
  m$mset<-as.integer(m$mset)
  treated<-m$SEQN[m$z==1]
  treated<-as.vector(t(matrix(treated,length(treated),ncontrols+1)))
  m<-cbind(m,treated)
  list(m=m,dt=dt)
}

z<-rep(NA,dim(binge)[1])
z[binge$AlcGroup=="B"]<-1
z[binge$AlcGroup=="P"]<-0
mPastComplete<-matchPast(z,ncontrols=1)
mPast<-mPastComplete$m
mPastComplete<-mPastComplete$dt
rm(z)

z<-rep(NA,dim(binge)[1])
z[binge$AlcGroup=="B"]<-1
z[binge$AlcGroup=="N"]<-0
mNeverComplete<-matchNever(z,ncontrols=1)
mNever<-mNeverComplete$m
mNeverComplete<-mNeverComplete$dt
rm(z)

bingeM<-rbind(mNever,mPast[mPast$z==0,])
bingeM<-bingeM[order(bingeM$treated,bingeM$AlcGroup,bingeM$SEQN),]
w<-which(colnames(bingeM)=="p")[1]
bingeM<-bingeM[,-w]
rm(binge,matchNever,matchPast,w)

old.par <- par(no.readonly = TRUE)

par(mfrow=c(1,2))
 boxplot(mNever$p[mNever$z==1],mNever$p[mNever$z==0],
 mNeverComplete$p[!is.element(mNeverComplete$SEQN,mNever$SEQN)],
 names=c("B","mN","uN"),ylab="Propensity Score",main="Never",
 ylim=c(0,.8))

 boxplot(mPast$p[mPast$z==1],mPast$p[mPast$z==0],
 mPastComplete$p[!is.element(mPastComplete$SEQN,mPast$SEQN)],
 names=c("B","mP","uP"),ylab="Propensity Score",main="Past",
 ylim=c(0,.8))

 par(mfrow=c(1,2))
 boxplot(mNever$age[mNever$z==1],mNever$age[mNever$z==0],
 mNeverComplete$age[!is.element(mNeverComplete$SEQN,mNever$SEQN)],
 names=c("B","mN","uN"),ylab="Age",main="Never",
 ylim=c(20,80))

 boxplot(mPast$age[mPast$z==1],mPast$age[mPast$z==0],
 mPastComplete$age[!is.element(mPastComplete$SEQN,mPast$SEQN)],
 names=c("B","mP","uP"),ylab="Age",main="Past",
 ylim=c(20,80))

 par(mfrow=c(1,2))
 boxplot(mNever$education[mNever$z==1],mNever$education[mNever$z==0],
 mNeverComplete$education[!is.element(mNeverComplete$SEQN,mNever$SEQN)],
 names=c("B","mN","uN"),ylab="Education",main="Never",
 ylim=c(1,5))

 boxplot(mPast$education[mPast$z==1],mPast$education[mPast$z==0],
 mPastComplete$education[!is.element(mPastComplete$SEQN,mPast$SEQN)],
 names=c("B","mP","uP"),ylab="Education",main="Past",
 ylim=c(1,5))

 par(mfrow=c(1,2))
 boxplot(mNever$bmi[mNever$z==1],mNever$bmi[mNever$z==0],
 mNeverComplete$bmi[!is.element(mNeverComplete$SEQN,mNever$SEQN)],
 names=c("B","mN","uN"),ylab="BMI",main="Never",
 ylim=c(14,70))

 boxplot(mPast$bmi[mPast$z==1],mPast$bmi[mPast$z==0],
 mPastComplete$bmi[!is.element(mPastComplete$SEQN,mPast$SEQN)],
 names=c("B","mP","uP"),ylab="BMI",main="Past",
 ylim=c(14,70))

 par(mfrow=c(1,2))
 boxplot(mNever$waisthip[mNever$z==1],mNever$waisthip[mNever$z==0],
 mNeverComplete$waisthip[!is.element(mNeverComplete$SEQN,mNever$SEQN)],
 names=c("B","mN","uN"),ylab="Waist/Hip",main="Never",
 ylim=c(.65,1.25))

 boxplot(mPast$waisthip[mPast$z==1],mPast$waisthip[mPast$z==0],
 mPastComplete$waisthip[!is.element(mPastComplete$SEQN,mPast$SEQN)],
 names=c("B","mP","uP"),ylab="Waist/Hip",main="Past",
 ylim=c(.65,1.25))

 par(old.par)
 }
}

