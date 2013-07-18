#######################
# INPUT DATASETS: JBL raw data
# OUTPUT DATASETS: TBD
# PREVIOUS PROGRAMS: TBD
# FOLLOWING PROGRAMS: EnrollmentIntensity.r
# $Id: EnrollmentPatterns.r 66 2012-06-13 20:27:36Z crosta $
###############################################
stop("Don't run this all at once.")
library(foreign)
library(parallel)
library(ggplot2)
library(descr)
library(pastecs)
library(mlogit)
library(lme4)
options(scipen=100)
options(digits=7)

lunique <- function(x) length(unique(x))
length2 <- function (x, na.rm=FALSE) {
  # New version of length which can handle NA's: if na.rm==T, don't count them
  if (na.rm) sum(!is.na(x))
  else       length(x)
}
cum.na <- function(x) {
  ## cumulative sum function that counts NA as 0 in sum
  y <- x
  x[which(is.na(x))] <- 0 
  x <- cumsum(x)
  x[which(is.na(y))] <- NA
  return(x)
} 

setwd("/projects/cbd/")

FT <- 12
icodes <- c(102, 103, 104, 105, 106)
inames <- c("Guilford", "CP", "Davidson", "Martin", "Wake")
longnames <- c("Guilford Technical Community College", "Central Piedmont Community College",
             "Davidson County Community College", "Martin Community College", "Wake Technical Community College")
yearsst <- c(2005, 2006)
termvec <- c(1, 3, 6)
yearvec <- 2005:2011
yearterms <- paste(sapply(yearvec, rep, length(termvec)), rep(termvec, length(termvec)), sep=".")

## read in data sets
courses <- read.dta("CourseEnrollments.dta")
courses <- subset(courses, subset=CohortId %in% yearsst & cntInstitution %in% icodes)
courses$idinst <- paste(courses$StudentIdEncrypted, courses$cntInstitution, sep=".")
courses$yearterms <- paste(courses$AcademicYearId, courses$TermId, sep='.')

students <- read.dta("StudentStatic_Derived_modCONC.dta")
students <- subset(students, subset=CohortId %in% yearsst & cntInstitution %in% icodes)
students$idinst <- paste(students$StudentIdEncrypted, students$cntInstitution, sep=".")
students$concfield <- factor(students$ConcentratorFieldOfStudy, levels=c(0:24), 
                      labels=c("Missing",
 "Arts/Hum/Eng", "Math/Sci", "Soc/Behav", "Agriculture", "Auto and Aero", "Bus/Market",
 "Secretarial", "Comm/Design", "Comp Info Sci", "Cosmetology", "Culinary", 
 "Eng/Arch", "Eng/Sci", "Education", "Allied Health", "Nursing",
 "Construction", "Manufacturing", "Mechanics", "Transportation", "Protective",
 "Other CTE", "Undeclared", "Other Liberal Arts"))

progress <- read.dta("StudentProgress.dta")
progress <- subset(progress, subset=CohortId %in% yearsst & cntInstitution %in% icodes)
progress$yearterms <- paste(progress$AcademicYearId, progress$TermId, sep='.')
progress$idinst <- paste(progress$StudentIdEncrypted, progress$cntInstitution, sep='.')
prog_award <- subset(progress, Complete1!=0 & Complete1!=8)
prog_award <- prog_award[order(prog_award$yearterms),]

allcourse <- read.dta("Courses.dta")
allcourse <- subset(allcourse, subset=cntInstitution %in% icodes)
courses$CourseName <- allcourse$CourseName[match(courses$CourseNumber, allcourse$CourseNumber)]

## lots of nsc work here
nsc <- read.dta("NSC.dta")
nsc <- subset(nsc, subset=cntInstitution %in% icodes)
nsc <- subset(nsc, subset=StudentIdEncrypted %in% students$StudentIdEncrypted)
nsc$StartDate <- as.POSIXct(nsc$EnrollmentBegin/1000, origin="1960-01-01")
nsc$EndDate <- as.POSIXct(nsc$EnrollmentEnd/1000, origin="1960-01-01")

# flag and remove nsc records from before Jan 2006 for the 2005 cohort
# and before Jan 2007 for the 2006 cohort. 
# We originally assumed FTIC was correct, but it really only is for fall entrants
## pull out records concerning graduation. 
nsc_4year <- subset(nsc, subset=CollegeType==4)
nsc_grads <- nsc_4year[is.na(nsc_4year$StartDate),]

## remove graduations
nsc_4year <- subset(nsc_4year, subset=!is.na(nsc_4year$StartDate))
nsc_4year$idinst <- paste(nsc_4year$StudentIdEncrypted, nsc_4year$cntInstitution, sep=".")

nsc_4year$CohortId <- students$CohortId[match(nsc_4year$idinst, students$idinst)]
nsc_4year$TermId <- students$TermId[match(nsc_4year$idinst, students$idinst)]
nsc_4year$pre4yr <- FALSE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2005-12-31 23:59:59", tz="EST") & nsc_4year$CohortId==2005 & nsc_4year$TermId ==1] <- TRUE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2006-04-30 23:59:59", tz="EST") & nsc_4year$CohortId==2005 & nsc_4year$TermId ==3] <- TRUE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2006-07-31 23:59:59", tz="EST") & nsc_4year$CohortId==2005 & nsc_4year$TermId ==6] <- TRUE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2006-12-31 23:59:59", tz="EST") & nsc_4year$CohortId==2006 & nsc_4year$TermId ==1] <- TRUE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2007-04-30 23:59:59", tz="EST") & nsc_4year$CohortId==2006 & nsc_4year$TermId ==3] <- TRUE
nsc_4year$pre4yr[nsc_4year$StartDate < as.POSIXct("2007-07-31 23:59:59", tz="EST") & nsc_4year$CohortId==2006 & nsc_4year$TermId ==6] <- TRUE

## redefine FTIC
nonftic <- unique(nsc_4year$idinst[nsc_4year$pre4yr])
students$nonftic <- students$idinst %in% nonftic
students$ftic2 <- students$ftic==0 & students$nonftic==FALSE

## only keep nsc data for students who transferred (drop those who were used to flag non ftic)
nsc_4year <- subset(nsc_4year, subset=!pre4yr)

##  find earliest enrollment in 4-year and the number of terms enrolled in the 4 year
earliest4 <- as.POSIXct(with(nsc_4year, tapply(StartDate, idinst, min, na.rm=T)), origin="1970-01-01")
nenroll4 <- table(nsc_4year$idinst)

## consider student as transferred if stuck around for at least 2 semesters (has at last 2 NSC 4-year records)
tranok <- names(nenroll4[which(nenroll4>=2)])
tran_dates <- earliest4[tranok]
tran_year <- format(tran_dates, "%Y")
tran_month <- format(tran_dates, "%m")

## align NSC transfer dates with CBD dates
cbdtran_year <- tran_year
cbdtran_year[as.numeric(tran_month) %in% c(1:7)] <- as.numeric(tran_year[as.numeric(tran_month) %in% c(1:7)])-1
cbdtran_term <- rep(NA, length(tran_year))
cbdtran_term[as.numeric(tran_month) %in% c(1:4)] <- 3
cbdtran_term[as.numeric(tran_month) %in% c(5:7)] <- 6
cbdtran_term[as.numeric(tran_month) %in% c(8:12)] <- 1

## so this is the first term observed in the 4-year institution. In theory, the student is not enrolled
## at the two year anymore. Therefore the "event" term should be one the last term enrolled before
## this transfer term.
nsc_tranterm <- paste(cbdtran_year, cbdtran_term, sep='.')
names(nsc_tranterm) <- names(tran_dates)

## graduation using progress file
students$gradterm <- paste(students$FirstCompletionSchoolYear, students$FirstCompletionTerm, sep='.')
students$gradterm[students$gradterm=="0.0"] <- NA
students$gradterm_prog <- prog_award$yearterms[match(students$idinst, prog_award$idinst)]
students$gradtype <- prog_award$Complete1[match(students$idinst, prog_award$idinst)]


students$concterm <- paste(students$ConcentratorSchoolYear, students$ConcentratorTerm, sep='.')
students$concterm[students$concterm=="0.0"] <- NA
students$tranterm <- nsc_tranterm[match(students$idinst, names(nsc_tranterm))]

## Now restrict to ftic (ftic2, which is newftic)
students <- subset(students, subset=ftic2)

## assigns the left-shifted term to observations
## this left shifts every courses so that t=1 is student's first enrollment
firstterm <- with(courses[courses$NumCreditsAttempt>0,], tapply(yearterms, idinst, FUN=function(x) min(as.double(x))))
offset <- match(as.character(firstterm), yearterms)-1
names(offset) <- names(firstterm)

courses$offset <- offset[match(courses$idinst, names(offset))]
students$offset <- offset[match(students$idinst, names(offset))]

courses$termorder <- match(courses$yearterms,yearterms)
students$gradorder <- match(students$gradterm,yearterms)
students$gradorder_prog <- match(students$gradterm_prog,yearterms)
students$concorder <- match(students$concterm,yearterms)
students$tranorder <- match(students$tranterm, yearterms)

courses$newterm <- courses$termorder-courses$offset
students$newgrad <- students$gradorder-students$offset
students$newgrad_prog <- students$gradorder_prog-students$offset
students$newconc <- students$concorder-students$offset
students$newtran <- students$tranorder-students$offset
students$newtran[!(students$newtran %in% 1:18)] <- NA

## need students last enrolled term. newtran_left marks transfer term as the last enrolled term
## evidence of swirling here when looking at newtran and newtran_left
maxterm <- with(courses, tapply(newterm, idinst, max, na.rm=T))
students$maxterm <- maxterm[match(students$idinst, rownames(maxterm))]
students$newtran_left <- students$newtran
students$newtran_left[students$newtran!=students$maxterm&!is.na(students$newtran)] <- students$maxterm[students$newtran!=students$maxterm&!is.na(students$newtran)]

## Fix NC Course names since they have sections appended in raw data.
lencc = length(courses$CourseNumber)
first3 = substr(courses$CourseNumber, 1, 3)
abeflag <- let4 <- let7 <- thenorm <- rep(NA, length(lencc))
abeflag[first3 %in% c("ABE", "GED", "ABL", "HSD", "CED", "TST", "HRD")] <- 1
let4[substr(courses$CourseNumber, 4, 4)=="C"] <- 1
let7[substr(courses$CourseNumber, 7, 7)=="A"] <- 1
thenorm[is.na(let7) & is.na(abeflag) & is.na(let4)] <- substr(courses$CourseNumber[is.na(let7) & is.na(abeflag) & is.na(let4)], 1, 6)
thenorm[!is.na(let7) & is.na(abeflag)] <- substr(courses$CourseNumber[!is.na(let7) & is.na(abeflag)], 1, 7)
thenorm[!is.na(abeflag)] <- substr(courses$CourseNumber[!is.na(abeflag)], 1, 7)
thenorm[!is.na(let4)] <- substr(courses$CourseNumber[!is.na(let4)], 1, 8)
thenorm[unlist(regexec("^[A-Z][A-Z][A-Z][34567]", thenorm))!=-1] <- substr(courses$CourseNumber[unlist(regexec("^[A-Z][A-Z][A-Z][34567]", thenorm))!=-1], 1, 7)
courses$CourseNumber2 <- thenorm
rm(lencc, first3, abeflag, let7, let4, thenorm)

## factor for course grade and delivery type
courses$grade.fac <- factor(courses$Grade, labels=c("Other", "Audit", "Withdraw", "Incomplete", "Missing", "Fail", "D", "C", "B", "A", "PASS"))
courses$delivery <- factor(courses$Method, labels=c("Online", "Face-to-face", "Hybrid"))

## some progress measures
attach.long <- subset(courses, select=c("idinst", "NumCreditsAttempt", "NumCreditsComplete", "newterm", "TermId"))
attach.sum <- with(attach.long[attach.long$NumCreditsAttempt>0,], tapply(NumCreditsAttempt, INDEX=list(idinst, newterm), FUN=sum, na.rm=T))
attach.compl <- with(attach.long[attach.long$NumCreditsAttempt>0,], tapply(NumCreditsComplete, INDEX=list(idinst, newterm), FUN=sum, na.rm=T))
creditratio <- attach.compl/attach.sum
attach.cumcompl <- t(apply(attach.compl, MARGIN=1, FUN=cum.na))  
ncourseattempt <- with(courses, tapply(CourseNumber2, INDEX=list(idinst, newterm), FUN=length))
courses$passed <- courses$grade.fac %in% c("Other", "D", "C", "B", "A", "PASS")
ncoursepass <- with(courses, tapply(passed, INDEX=list(idinst, newterm), FUN=sum ))
ncoursepass[is.na(ncoursepass)] <- 0
courseratio <- ncoursepass/ncourseattempt

## create character vector of enrollment - Enrolled/not enrolled
attachment.df <- attach.sum
attachment.df[attach.sum>=0] <- "1"
attachment.df[is.na(attach.sum)] <- "0"
attach.termpat <- apply(attachment.df, MARGIN=1, paste, collapse="")
       
#i want >=6 credits in summer to be considered FT
attach.longSum <- attach.long
attach.longSum$NumCreditsAttempt[attach.longSum$TermId==6] <-2*attach.longSum$NumCreditsAttempt[attach.longSum$TermId==6]
attach.sum6 <- with(attach.longSum[attach.longSum$NumCreditsAttempt>0,], tapply(NumCreditsAttempt, INDEX=list(idinst, newterm), FUN=sum, na.rm=T))

## create character vector of enrollment - FT/PT/not enrolled, FT as defined above
credattempt.df <- attach.sum
credattempt.df[is.na(attach.sum)] <- "."
credattempt.df[attach.sum<FT] <- "0"
credattempt.df[attach.sum>=FT] <- "1"
termpat <- apply(credattempt.df, MARGIN=1, paste, collapse="")

credattempt.df6 <- attach.sum6
credattempt.df6[is.na(attach.sum6)] <- "."
credattempt.df6[attach.sum6<FT] <- "0"
credattempt.df6[attach.sum6>=FT] <- "1"
termpat6 <- apply(credattempt.df6, MARGIN=1, paste, collapse="")

## create character vector of enrollment - FT/PT/not enrolled, FT as defined as >=9
credattempt.df9 <- attach.sum
credattempt.df9[is.na(attach.sum)] <- "."
credattempt.df9[attach.sum<9] <- "0"
credattempt.df9[attach.sum>=9] <- "1"
termpat9 <- apply(credattempt.df9, MARGIN=1, paste, collapse="")

## merge pattern data (replcomp, attach.termpat) onto FTIC student data
pats <- cbind(termpat, attach.termpat, termpat9, termpat6)
studpat <- merge(students, pats, by.x="idinst", by.y="row.names", all.x=T)

## insert an X into attendance character vector in the term OF first completion
studpat$termpat.x <- as.character(studpat$termpat)
studpat$termpat.x[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat.x, 1, newgrad_prog-1), "X", sep=''))
therest <- sapply(studpat$termpat.x[!is.na(studpat$newgrad_prog)], function(x) paste(rep(".", times=18-nchar(x)), collapse=''))
studpat$termpat.x[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat.x[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

studpat$termpat9.x <- as.character(studpat$termpat9)
studpat$termpat9.x[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat9.x, 1, newgrad_prog-1), "X", sep=''))
therest <- sapply(studpat$termpat9.x[!is.na(studpat$newgrad_prog)], function(x) paste(rep(".", times=18-nchar(x)), collapse=''))
studpat$termpat9.x[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat9.x[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

studpat$termpat6.x <- as.character(studpat$termpat6)
studpat$termpat6.x[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat6.x, 1, newgrad_prog-1), "X", sep=''))
therest <- sapply(studpat$termpat6.x[!is.na(studpat$newgrad_prog)], function(x) paste(rep(".", times=18-nchar(x)), collapse=''))
studpat$termpat6.x[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat6.x[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

studpat$attach.x <- as.character(studpat$attach.termpat)
studpat$attach.x[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(attach.x, 1, newgrad_prog-1), "X", sep=''))
therest <- sapply(studpat$attach.x[!is.na(studpat$newgrad_prog)], function(x) paste(rep(".", times=18-nchar(x)), collapse=''))
studpat$attach.x[!is.na(studpat$newgrad_prog)]  <- paste(studpat$attach.x[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

## insert an X into attendance character vector in the term AFTER first completion and all subsequent terms
studpat$termpat.comp <- as.character(studpat$termpat)
studpat$termpat.comp[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat.comp, 1, newgrad_prog), "X", sep=''))
therest <- sapply(studpat$termpat.comp[!is.na(studpat$newgrad_prog)], function(x) {
  ntimes <- 18-nchar(x)
  if (ntimes<0) return("") else return(paste(rep("X", times=18-nchar(x)), collapse=''))
})
studpat$termpat.comp[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat.comp[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

studpat$termpat9.comp <- as.character(studpat$termpat9)
studpat$termpat9.comp[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat9.comp, 1, newgrad_prog), "X", sep=''))
therest <- sapply(studpat$termpat9.comp[!is.na(studpat$newgrad_prog)], function(x) {
  ntimes <- 18-nchar(x)
  if (ntimes<0) return("") else return(paste(rep("X", times=18-nchar(x)), collapse=''))
})
studpat$termpat9.comp[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat9.comp[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)

studpat$termpat6.comp <- as.character(studpat$termpat6)
studpat$termpat6.comp[!is.na(studpat$newgrad_prog)] <- with(studpat[!is.na(studpat$newgrad_prog),], paste(substring(termpat6.comp, 1, newgrad_prog), "X", sep=''))
therest <- sapply(studpat$termpat6.comp[!is.na(studpat$newgrad_prog)], function(x) {
  ntimes <- 18-nchar(x)
  if (ntimes<0) return("") else return(paste(rep("X", times=18-nchar(x)), collapse=''))
})
studpat$termpat6.comp[!is.na(studpat$newgrad_prog)]  <- paste(studpat$termpat6.comp[!is.na(studpat$newgrad_prog)], therest, sep='')
rm(therest)


## students in studpat who have enrollment patterns are those who have taken at least one course for
## credit. if you have no attempted credits, you never make it in.

## generate some  ways to describe the patterns - one number summaries
studpat$startft <- grepl( "^\\.*1", as.character(studpat$termpat6))
studpat$numft <- sapply(as.character(studpat$termpat6), function(x) sum(attr(gregexpr("1", x)[[1]],which="match.length")))
studpat$numft[studpat$numft==-1] <- 0
studpat$numpt <- sapply(as.character(studpat$termpat6), function(x) sum(attr(gregexpr("0", x)[[1]],which="match.length")))
studpat$ftonly <- FALSE
studpat$ftonly[studpat$numpt==-1] <- TRUE
studpat$numpt[studpat$numpt==-1] <- 0
studpat$numterms <- studpat$numft+studpat$numpt
studpat$pctft <- studpat$numft/studpat$numterms
studpat$earncred <- grepl( "X", as.character(studpat$termpat6.x))

## create subset of credit students
startlevel <- c("DevEd", "College-Ready", "No_Placement_Info")
studpatcredit <- subset(studpat, subset=StartingProgram %in% startlevel) #results in 14688
studpatcredit <- studpatcredit[-which(is.na(studpatcredit$termpat)),] #259 lost here

## subset for displaying patterns
sset <- rownames(credattempt.df) %in% studpatcredit$idinst

##### Make sure all variables are around for the creation of our person period dataset.
## compute age in years around first registration (everyone born on 15th of month/year)
studpatcredit$bdate <- strptime(paste(paste(studpatcredit$BirthYear,ifelse(studpatcredit$BirthMonth%in%c(1:9), "0", ""), studpatcredit$BirthMonth, sep=""), "15", sep=""),"%Y%m%d")
studpatcredit$regdate <- as.POSIXct(studpatcredit$RegistrationDate/1000, origin="1960-01-01")
studpatcredit$agefirstreg <- as.numeric((as.POSIXct(studpatcredit$RegistrationDate/1000, origin="1960-01-01") - as.POSIXct(studpatcredit$bdate))/365.24)

## turn a bunch of categorical variables into dichotomous indicators or continuous vars
studpatcredit$female[studpatcredit$Gender==2] <- 1
studpatcredit$female[studpatcredit$Gender==1] <- 0

studpatcredit$hsdip[studpatcredit$HighschoolCredentialEarned==1] <- 1
studpatcredit$hsdip[studpatcredit$HighschoolCredentialEarned %in% 2:4] <- 0

studpatcredit$ged[studpatcredit$HighschoolCredentialEarned==2] <- 1
studpatcredit$ged[studpatcredit$HighschoolCredentialEarned %in% c(1,3:4)] <- 0

studpatcredit$nohsd[studpatcredit$HighschoolCredentialEarned==3] <- 1
studpatcredit$nohsd[studpatcredit$HighschoolCredentialEarned %in% c(1:2,4)] <- 0

studpatcredit$agelt19[studpatcredit$AgeOverview==1] <- 1
studpatcredit$agelt19[studpatcredit$AgeOverview %in% c(2:4)] <- 0

studpatcredit$age2026[studpatcredit$AgeOverview==2] <- 1
studpatcredit$age2026[studpatcredit$AgeOverview %in% c(1, 3:4)] <- 0

studpatcredit$agegt27[studpatcredit$AgeOverview==3] <- 1
studpatcredit$agegt27[studpatcredit$AgeOverview %in% c(1:2, 4)] <- 0

studpatcredit$white[studpatcredit$Ethnicity==1] <- 1
studpatcredit$white[studpatcredit$Ethnicity %in% c(2:8)] <- 0

studpatcredit$black[studpatcredit$Ethnicity==2] <- 1
studpatcredit$black[studpatcredit$Ethnicity %in% c(1,3:8)] <- 0

studpatcredit$amerind[studpatcredit$Ethnicity==3] <- 1
studpatcredit$amerind[studpatcredit$Ethnicity %in% c(1:2,4:8)] <- 0

studpatcredit$asian[studpatcredit$Ethnicity==4] <- 1
studpatcredit$asian[studpatcredit$Ethnicity %in% c(1:3,5:8)] <- 0

studpatcredit$hispanic[studpatcredit$Ethnicity==6] <- 1
studpatcredit$hispanic[studpatcredit$Ethnicity %in% c(1:5,7:8)] <- 0

studpatcredit$mixed[studpatcredit$Ethnicity==7] <- 1
studpatcredit$mixed[studpatcredit$Ethnicity %in% c(1:6,8)] <- 0

studpatcredit$nonres[studpatcredit$Ethnicity==8] <- 1
studpatcredit$nonres[studpatcredit$Ethnicity %in% c(1:7)] <- 0

studpatcredit$AvgHouseholdIncome[studpatcredit$AvgHouseholdIncome==0] <- NA
studpatcredit$PctHouseWithBachelorsAbove[studpatcredit$PctHouseWithBachelorsAbove==0] <- NA
studpatcredit$PctEmployedManagementProf[studpatcredit$PctEmployedManagementProf==0] <- NA
studpatcredit$PctNonEnglishSpokenAtHome[studpatcredit$PctNonEnglishSpokenAtHome==0] <- NA
studpatcredit$PctHealthcareCoverage[studpatcredit$PctHealthcareCoverage==0] <- NA

studpatcredit$RecvFinancialAidFirstTerm[studpatcredit$RecvFinancialAidFirstTerm==2] <- 0
studpatcredit$RecvPellFirstTerm[studpatcredit$RecvPellFirstTerm==2] <- 0

studpatcredit$DevPlacementLvlOverall[studpatcredit$DevPlacementLvlOverall=="Missing"] <-NA
studpatcredit$DevPlacementLvlMath[studpatcredit$DevPlacementLvlMath=="Math_Missing"] <-NA
studpatcredit$DevPlacementLvlEngl[studpatcredit$DevPlacementLvlEngl=="Engl_Missing"] <-NA
studpatcredit$DevPlacementLvlRead[studpatcredit$DevPlacementLvlRead=="Read_Missing"] <-NA

levels(studpatcredit$DevPlacementLvlOverall) <- c(NA, levels(studpatcredit$DevPlacementLvlOverall)[-1])
levels(studpatcredit$DevPlacementLvlMath) <- c(NA, levels(studpatcredit$DevPlacementLvlMath)[-1])
levels(studpatcredit$DevPlacementLvlEngl) <- c(NA, levels(studpatcredit$DevPlacementLvlEngl)[-1])
levels(studpatcredit$DevPlacementLvlRead) <- c(NA, levels(studpatcredit$DevPlacementLvlRead)[-1])

## basic demog/finaid
demog <- c("female", "agefirstreg", "agelt19", "age2026", "agegt27", "white", "black", 
           "amerind", "asian", "hispanic", "mixed", "nonres", "hsdip", "ged", "nohsd",
           "RecvFinancialAidFirstTerm", "RecvPellFirstTerm")

## enviro quintiles
enviro <- c("AvgHouseholdIncome", "PctHouseWithBachelorsAbove", "PctEmployedManagementProf",  
            "PctNonEnglishSpokenAtHome", "PctHealthcareCoverage")

## Dev Ed
deved <- c("DevPlacementLvlOverall", "DevPlacementLvlMath",  "DevPlacementLvlEngl", "DevPlacementLvlRead")

## 5 year outcomes, programs measures 
prog_num <- c("pctft", "numterms", "earncred")
prog_fact <- c("concyear", "OutcomeAsOfYear5", "ConcentratorFieldOfStudy", "FirstMajor",
               "LastMajor")

### descriptive analytics - early dropouts
oneanddone <- studpatcredit$attach.termpat=="100000000000000000"

## these functions help build the matrices that are plotted
df.sorter <- function(xx, sset) as.data.frame(xx[sset,])[do.call(order,as.data.frame(xx[sset,])),]
x.coords <- function(plotback, gradconc) {
  attch_awrd <- data.frame(studpatcredit[,gradconc][match(rownames(plotback), studpatcredit$idinst)])
  rownames(attch_awrd) <- rownames(plotback)
  attch_awrd$x <- (attch_awrd[,1]-1)/17
  attch_awrd$y_prime <- 1/dim(attch_awrd)[1]
  attch_awrd$y <- cumsum(attch_awrd$y_prime)
  return(attch_awrd[,c("x", "y")])
}

c.coords <- function(plotback=attch, subject=mathdev) {
  tmpdf <- courses[courses$CourseNumber2 %in% subject,c("grade.fac", "idinst", "newterm")]
  tmpdf2 <- tmpdf[match(rownames(plotback), tmpdf$idinst),]
  rownames(tmpdf2) <- rownames(plotback)
  tmpdf2$x <- (tmpdf2$newterm-1)/17
  tmpdf2$y_prime <- 1/dim(tmpdf2)[1]
  tmpdf2$y <- cumsum(tmpdf2$y_prime)
  return(tmpdf2[,c("x", "y", "grade.fac", "newterm")])
}

save.image("termpat.Rdata")
######################################################################################

############### GRAPHICS of ENROLLMENT PATTERNS ######################################
## How many patterns - attachment?
lunique(studpatcredit$attach.termpat)
cbind(head(sort(table(studpatcredit$attach.termpat), decreasing=T)))
cbind(sort(table(studpatcredit$attach.termpat)[table(studpatcredit$attach.termpat)>0], decreasing=T))


################  Several ways to visually look at rate of progress ###############

## matrix image of attachment
## sset is defined above to be anyone with a row in credattempt.df
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$DevPlacementLvlMath=="Math_One_Below" & !is.na(studpatcredit$DevPlacementLvlMath)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$OutcomeAsOfYear5=="Assoc" & !is.na(studpatcredit$OutcomeAsOfYear5)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$concfield =="Mechanics" & !is.na(studpatcredit$concfield)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$concfield =="Other Liberal Arts" & !is.na(studpatcredit$concfield)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$newgrad_prog >1 & !is.na(studpatcredit$newgrad_prog)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Bus_Market" & !is.na(studpatcredit$FirstMajor)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Allied_Health" & !is.na(studpatcredit$FirstMajor)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Undeclared" & !is.na(studpatcredit$FirstMajor)])
sset2 <- sset & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$attach.termpat!="100000000000000000"])
sset3 <- sset2 & rownames(credattempt.df) %in% unique(studpatcredit$idinst[is.na(studpatcredit$concterm)])
sset3 <- sset2 & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Undeclared" & !is.na(studpatcredit$FirstMajor)])
sset3 <- sset2 & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Allied_Health" & !is.na(studpatcredit$FirstMajor)])
sset3 <- sset2 & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$FirstMajor =="Liberal_Arts" & !is.na(studpatcredit$FirstMajor)])
sset3 <- sset2 & rownames(credattempt.df) %in% unique(studpatcredit$idinst[studpatcredit$concyear==0])

attch <- df.sorter(attachment.df, sset)
## first completion term
attch_awrd <- x.coords(plotback=attch, gradconc="newgrad_prog")
## first concentration term
attch_conc <- x.coords(plotback=attch, gradconc="newconc")
## transfer term
attch_tran <- x.coords(plotback=attch, gradconc="newtran")

## display images - attachment
image(t(apply(attch, 2, as.numeric)), col=c("white", "orange4"), xlab="Term", ylab="Enrolled-Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
with(attch_conc, points(jitter(x), y, pch=16, cex=.5, col=alpha("green", .3)))
with(attch_awrd, points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .5)))
with(attch_tran, points(jitter(x), y, pch=16, cex=.5, col=alpha("red", .5)))

contour(with(na.omit(attch_awrd), kde2d(x,y)), nlevels=30, add=T)
contour(with(na.omit(attch_conc), kde2d(x,y)), nlevels=30, add=T, col="blue")

## showing passing rates for dev math courses
mathdev <- c("MAT050", "MAT060", "MAT070", "MAT080", "MAT090")
showmathdev <- c.coords(plotback=attch, subject=mathdev)
mdev <- showmathdev[showmathdev$newterm<=18,]
mdev$color <- "grey"
mdev$color[mdev$grade.fac %in% c("A", "B", "C", "PASS")] <- "lightblue"
with(mdev, points(jitter(x), y, pch=17, cex=1, col=color))

with(attch_conc[studpatcredit$idinst[studpatcredit$concfield=="Education"],], points(jitter(x), y, pch=16, cex=.5, col=alpha("red", .5)))
with(attch_conc[studpatcredit$idinst[studpatcredit$concfield=="Nursing"],], points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .5)))
with(attch_conc[studpatcredit$idinst[studpatcredit$concfield=="Allied Health"],], points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .5)))


################################################################################

## matrix image of intensity
intens <- df.sorter(credattempt.df, sset) 
## first completion term
intens_awrd <-  x.coords(plotback=intens, gradconc="newgrad_prog")
## first concentration term
intens_conc <-  x.coords(plotback=intens, gradconc="newconc")

## display images - intensity
image(t(apply(intens, 2, as.numeric)), col=c("red", "blue"), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
with(intens_conc, points(jitter(x), y, pch=16, cex=.5, col=alpha("yellow", .5)))
with(intens_awrd, points(jitter(x), y, pch=16, cex=.5, col=alpha("black", .5)))

contour(with(na.omit(intens_awrd), kde2d(x,y)), nlevels=35, add=T)
################################################################################

## credit completion ratio and cumulative creds? creditratio, attach.cumcompl, courseratio
credrat <- df.sorter(creditratio, sset) 
## first completion term
credrat_awrd <-  x.coords(plotback=credrat, gradconc="newgrad_prog")
## first concentration term
credrat_conc <-  x.coords(plotback=credrat, gradconc="newconc")

image(t(apply(credrat, 2, as.numeric)), col=rev(heat.colors(18)), xlab="Term", ylab="Credit Completion Ratio", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
with(credrat_conc, points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .5)))
with(credrat_awrd, points(jitter(x), y, pch=16, cex=.5, col=alpha("black", .5)))

contour(with(na.omit(credrat_awrd), kde2d(x,y)), nlevels=30, add=T)
contour(with(na.omit(credrat_conc), kde2d(x,y)), nlevels=30, add=T)
################################################################################

## course completion ratio and cumulative creds? creditratio, attach.cumcompl, courseratio
courserat <- df.sorter(courseratio, sset) 
## first completion term
courserat_awrd <-  x.coords(plotback=courserat, gradconc="newgrad_prog")
## first concentration term
courserat_conc <-  x.coords(plotback=courserat, gradconc="newconc")

image(t(apply(courserat, 2, as.numeric)), col=rev(heat.colors(18)), xlab="Term", ylab="Course Completion Ratio", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
with(courserat_conc, points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .5)))
with(courserat_awrd, points(jitter(x), y, pch=16, cex=.5, col=alpha("black", .5)))

################################################################################

## cumulative creds? creditratio, attach.cumcompl, courseratio
cumcomp <- df.sorter(attach.cumcompl, sset) 
## first completion term
cumcomp_awrd <-  x.coords(plotback=cumcomp, gradconc="newgrad_prog")
## first concentration term
cumcomp_conc <-  x.coords(plotback=cumcomp, gradconc="newconc")

image(t(apply(cumcomp, 2, as.numeric)), col=rainbow(20), xlab="Term", ylab="Credit Accumulation", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
with(cumcomp_conc, points(jitter(x), y, pch=16, cex=.5, col=alpha("blue", .4)))
with(cumcomp_awrd, points(jitter(x), y, pch=16, cex=.5, col=alpha("black", .4)))

contour(with(na.omit(cumcomp_awrd), kde2d(x,y)), nlevels=25, add=T)
contour(with(na.omit(cumcomp_conc), kde2d(x,y)), nlevels=40, add=T)




################ t-tests ##################
## we create some dichotomous comparison groups and cmopare student
## characteristics in thes groups. 
## t-tests for ft vs pt start


do.compare <- function(compgroup,...) {
  cat("var", "quintile", "compgroupT", "compgroupF", "differenceT-F", "p-value\n", sep=",")
  
  for (ii in 1:length(demog)) {
    tres <- t.test(get(demog[ii], studpatcredit)[compgroup], get(demog[ii], studpatcredit)[!compgroup])
    Signif <- symnum(tres$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
    cat(demog[ii], " ", tres$estimate, -diff(tres$estimate), tres$p.value, Signif, sep=",")
    cat("\n")
  }
  
  for (ii in 1:length(enviro)) {    
    for (jj in 1:5) {
      if (jj==1) cat(enviro[ii], "\n", sep=",")
      tres <- t.test(get(enviro[ii], studpatcredit)[compgroup]==jj, get(enviro[ii], studpatcredit)[!compgroup]==jj)
      Signif <- symnum(tres$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
      cat(" ", jj, tres$estimate, -diff(tres$estimate), tres$p.value, Signif, sep=",")
      cat("\n")
    }
  }
  
  for (ii in 1:length(deved)) {    
    levopt <- levels(get(deved[ii], studpatcredit))
    for (jj in 1:length(levopt)) {
      if (jj==1) cat(deved[ii], "\n", sep=",")
      tres <- t.test(get(deved[ii], studpatcredit)[compgroup]==levopt[jj], get(deved[ii], studpatcredit)[!compgroup]==levopt[jj])
      Signif <- symnum(tres$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
      cat(" ", levopt[jj], tres$estimate, -diff(tres$estimate), tres$p.value, Signif, sep=",")
      cat("\n")
    }
  }
    
  for (ii in 1:length(prog_num)) {    
    tres <- t.test(get(prog_num[ii], studpatcredit)[compgroup], get(prog_num[ii], studpatcredit)[!compgroup])
    Signif <- symnum(tres$p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
    cat(prog_num[ii], " ", tres$estimate, -diff(tres$estimate), tres$p.value, Signif, sep=",")
    cat("\n")
  }
}
cat("---\nSignif. codes: ", attr(Signif, "legend"), "\n")

startftpt <- studpatcredit$startft
do.compare(startftpt)
sum(table(startftpt))

fallspring <- rep(NA, length=dim(studpatcredit)[1])
fallspring[grepl( "^11", as.character(studpatcredit$attach.termpat))] <- TRUE
fallspring[grepl( "^10", as.character(studpatcredit$attach.termpat))] <- FALSE
do.compare(fallspring)
sum(table(fallspring))

ftfallftspring <- rep(NA, length=dim(studpatcredit)[1])
ftfallftspring[grepl( "^11", as.character(studpatcredit$termpat))] <- TRUE
ftfallftspring[grepl( "^00", as.character(studpatcredit$termpat))] <- FALSE
do.compare(ftfallftspring)
sum(table(ftfallftspring))

fallspringfall <- rep(NA, length=dim(studpatcredit)[1])
fallspringfall[grepl( "^11.1", as.character(studpatcredit$attach.termpat))] <- TRUE
fallspringfall[grepl( "^11.0", as.character(studpatcredit$attach.termpat))] <- FALSE
do.compare(fallspringfall)
sum(table(fallspringfall))

ftfallftspring2 <- rep(NA, length=dim(studpatcredit)[1])
ftfallftspring2[grepl( "^11", as.character(studpatcredit$termpat))] <- TRUE
ftfallftspring2[grepl( "^10", as.character(studpatcredit$termpat))] <- FALSE
do.compare(ftfallftspring2)
sum(table(ftfallftspring2))


####### Looking at transcripts - why students persist or not?
courses$yearterm <- paste(courses$AcademicYearId, courses$TermId, sep=".")

compcourses <- function(compgroup, ncourse, term,...) {
  retlist <- list()
  names(fallspring) <- studpatcredit$StudentIdEncrypted
  commoncourses <- names(tail(sort(table(courses[courses$StudentIdEncrypted %in% as.vector(na.omit(names(compgroup)[!compgroup])) & courses$yearterm==term, "CourseNumber2"])), ncourse))
  
  coursegrademat_stop <- sapply(commoncourses, function(x) {
    table(courses[courses$StudentIdEncrypted %in% as.vector(na.omit(names(compgroup)[!compgroup])) & courses$yearterm==term & courses$CourseNumber2==x, "grade.fac"])
  })
  N_stop <- colSums(coursegrademat_stop)
  retlist[[1]] <- cbind(N_stop, t(prop.table(coursegrademat_stop, margin=2)[c("Withdraw", "Fail"),]))
  
  coursegrademat_per <- sapply(commoncourses, function(x) {
    table(courses[courses$StudentIdEncrypted %in% as.vector(na.omit(names(compgroup)[compgroup])) & courses$yearterm==term & courses$CourseNumber2==x, "grade.fac"])
  })
  N_per <- colSums(coursegrademat_per)
  retlist[[2]] <- cbind(N_per, t(prop.table(coursegrademat_per, margin=2)[c("Withdraw", "Fail"),]))
  return(retlist)
}


ncourse <- 25
compgroup <- fallspring
compcourses(compgroup, ncourse, "2005.1")

ncourse <- 25
compgroup <- fallspringfall
compcourses(compgroup, ncourse, "2005.3")

ncourse <- 25
compgroup <- ftfallftspring2
compcourses(compgroup, ncourse, "2005.1")
