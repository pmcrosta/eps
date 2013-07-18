## Code for Early Dropouts CCRC Analytics
## Peter M Crosta (pmcrosta@gmail.com)

stop("Don't run this all at once.")
library(ggplot2)
library(descr)
library(pastecs)
options(scipen=100)
options(digits=7)

## Depends on data cleaned in EnrollmentPatterns.r
setwd("/projects/CBD/crosta/intensity")
load("termpat.Rdata")

## Simple descriptives of sample 

#oneanddone <- studpatcredit$attach.termpat=="100000000000000000"
firstfour <- unlist(lapply(strsplit(substr(studpatcredit$attach.termpat, 1, 4),""), function(x) sum(as.numeric(x))))
compgrp <- firstfour>1 & firstfour<=4
  
round(t(stat.desc(studpatcredit[oneanddone,demog])),4)
round(t(stat.desc(studpatcredit[compgrp,demog])),4)

sesdex <- with(studpatcredit, rowMeans(cbind(AvgHouseholdIncome,PctHouseWithBachelorsAbove,PctEmployedManagementProf,PctHealthcareCoverage), na.rm=T))
round(t(stat.desc(sesdex[oneanddone])),4)
round(t(stat.desc(sesdex[compgrp])),4)


envirostats <- lapply(enviro, function(x) freq(get(x, studpatcredit), plot=F))
names(envirostats) <- enviro
lapply(envirostats, function(x) {
  quints <- x[1:5, c(1,3)]
  Total <- sum(x[1:5, "Frequency"])
  rbind(quints, c(Total, 100))
})

lapply(deved, function(x) {
  basetab <- table(get(x, studpatcredit[oneanddone,]))
  pct <- prop.table(basetab)
  Total <- sum(basetab)
  rbind(cbind(basetab, pct), c(Total, 100))
})

lapply(deved, function(x) {
  basetab <- table(get(x, studpatcredit[compgrp,]))
  pct <- prop.table(basetab)
  Total <- sum(basetab)
  rbind(cbind(basetab, pct), c(Total, 100))
})

## first term in college
dropout <- courses$StudentIdEncrypted %in% studpatcredit$StudentIdEncrypted[oneanddone]
compid <- courses$StudentIdEncrypted %in% studpatcredit$StudentIdEncrypted[compgrp]

N.max <- 158

topcourses.drop <- sort(table(courses$CourseNumber2[dropout]))
topcourses.drop <- topcourses.drop[topcourses.drop>=N.max]

topcoursename.drop <- sort(table(courses$CourseName[dropout]))
topcoursename.drop <- topcoursename.drop[topcoursename.drop>=N.max]

sort(table(courses$CourseName[dropout]))
sort(table(courses$CourseNumber2[dropout]))
sort(table(courses$grade.fac[dropout]))

addmargins(table(courses$CourseName[dropout], courses$passed[dropout])[names(topcoursename.drop),])
prop.table(table(courses$CourseName[dropout], courses$passed[dropout])[names(topcoursename.drop),],1)

addmargins(table(courses$CourseName[compid&courses$newterm==1], courses$passed[compid&courses$newterm==1])[names(topcoursename.drop),])
prop.table(table(courses$CourseName[compid&courses$newterm==1], courses$passed[compid&courses$newterm==1])[names(topcoursename.drop),],1)

## course delivery method
cbind(prop.table(table(courses$delivery[dropout])))
cbind(prop.table(table(courses$delivery[compid&courses$newterm==1])))

prop.table(table(courses$delivery[dropout & courses$CourseNumber2%in%names(topcourses.drop)], courses$passed[dropout& courses$CourseNumber2%in%names(topcourses.drop)]),1)
prop.table(table(courses$delivery[compid], courses$passed[compid]),1)
prop.table(table(courses$delivery[compid & courses$CourseNumber2%in%names(topcourses.drop)], courses$passed[compid& courses$CourseNumber2%in%names(topcourses.drop)]),1)

table(studpatcredit$OutcomeAsOfYear5[oneanddone])
