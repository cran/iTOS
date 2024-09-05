\name{bingeM}
\alias{bingeM}
\docType{data}
\title{
Binge Drinking and High Blood Pressure -- Matched With Two Control Groups
}
\description{
The bingeM data set is the matched data set built from the unmatched binge data using the makematch() function.  The documentation for the makematch() function builds bingeM from binge.
}
\usage{data("bingeM")}
\format{
  A data frame with 618 observations on the following 20 variables.
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
    \item{\code{z}}{Treatment/control indicator, z[i]=1 if i is in AlcGroup category B and z[i]=0 otherwise.}
    \item{\code{mset}}{Indicator of the matched set, 1, 2, ..., 206.}
    \item{\code{treated}}{The SEQN for the treated individual in this matched set.}
  }
}
\details{
bingeM is the matched data set built from the unmatched binge data using the makematch() function.  The documentation for the makematch() function builds bingeM from binge.

bpCombined is the sum of two standardized measures, one for systolic blood pressure and one for diastolic blood pressure.  In the larger NHANES data set of individuals at least 20 years of age who are not pregnant, the median and the mad (=median absolute deviation from the median) were determined separately for systolic and diastolic blood pressure. The calculation used the median and mad functions in the 'stats' package, so the mad was by default scaled to resemble the standard deviation for a Normal distribution. bpCombined is the sum of two quantities: systolic blood pressure minus its median divided by its mad plus diastolic blood pressure minus its median divided by its mad.
}
\source{
The data are from the US National Health and Nutrition Examination Survey, NHANES 2017-March 2020 Pre-pandemic.  The 2017-2020 data were affected by COVID-19 and are not a survey.  The complete data are available from the CDC web page.  With minor differences, the data were used as an example in Rosenbaum (2023).
}
\references{
Rosenbaum, P. R. (2023) <doi:10.1111/biom.13921> A second evidence factor for a second control group. Biometrics, 79(4), 3968-3980.

US National Health and Nutrition Examination Survey, 2017-2020.  Atlanta: US Centers for Disease Control.
}
\examples{
data(bingeM)
table(bingeM$AlcGroup)
tapply(bingeM$age,bingeM$AlcGroup,median)
tapply(bingeM$bmi,bingeM$AlcGroup,median)
tapply(bingeM$education,bingeM$AlcGroup,mean)
tapply(bingeM$female,bingeM$AlcGroup,mean)
tapply(bingeM$smokenow,bingeM$AlcGroup,mean)
tapply(bingeM$vigor,bingeM$AlcGroup,mean)
tapply(bingeM$smokeQuit,bingeM$AlcGroup,mean)
boxplot(bingeM$bpCombined~bingeM$AlcGroup,ylab="Combined BP",
        xlab="Alcohol Group",las=1)
y<-t(matrix(bingeM$bpCombined,3,206))
weightedRank::wgtRank(y,gamma=2)
amplify(2,3)
# In iTOS book, comparison of Tables 13.2 and 13.3
weightedRank::ef2C(y,gamma=2,upsilon=2,m1=c(6,6),range=TRUE,scores=1:3)
weightedRank::ef2C(y,gamma=2,upsilon=2.3,m1=c(6,8),range=FALSE,scores=c(1,2,5))
}
\keyword{datasets}
