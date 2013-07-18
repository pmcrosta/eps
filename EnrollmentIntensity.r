## Code for enrollment patterns paper and analytic
## Peter M Crosta (pmcrosta@gmail.com)

stop("Don't run this all at once.")
library(ggplot2)
library(descr)
library(scales)
options(descr.plot = FALSE)
library(stringr)
options(scipen=100)
options(digits=7)

pdensity <- function(x) plot(density(x, na.rm=T))
cut.at.n.tile <- function(X , n = 4) { 
  cut(X, breaks = quantile(X , probs = (0:n)/n , na.rm = TRUE), include.lowest = TRUE)
}

#### CCRC ppt template colors ####
ccrc_green <- rgb(163,178,34, maxColorValue=255)
ccrc_cadet <- rgb(0,140,153, maxColorValue=255)
ccrc_turq <- rgb(66,190,202, maxColorValue=255)
ccrc_blue <- rgb(0,101,164, maxColorValue=255)
ccrc_oran <- rgb(223,139,7, maxColorValue=255)
ccrc_red <- rgb(196,18,48, maxColorValue=255)
ccrc_purp <- rgb(135,33,117, maxColorValue=255)
ccrc_dodg <- rgb(59,115,185, maxColorValue=255)

setwd('/projects/CBD/crosta/intensity/')
load("termpat.Rdata")


#### Basic descriptive stats (Table A1) ####
## Panel A
t(stat.desc(studpatcredit[,demog]))
## Panel B
envirostats <- lapply(enviro, function(x) freq(get(x, studpatcredit), plot=F))
names(envirostats) <- enviro
lapply(envirostats, function(x) {
  quints <- x[1:5, c(1,3)]
  Total <- sum(x[1:5, "Frequency"])
  rbind(quints, c(Total, 100))
})
## Panel C
lapply(deved, function(x) {
  basetab <- table(get(x, studpatcredit))
  pct <- prop.table(basetab)
  Total <- sum(basetab)
  rbind(cbind(basetab, pct), c(Total, 100))
})

## how many summer, fall, spring starters?
freq(studpatcredit$CohortTermId,plot=F)

## How many credits and courses are taken in fall, spring, and summer
prop.table(table(courses$TermId[courses$idinst %in% studpatcredit$idinst])) ##Courses 8.2% summer
prop.table(with(courses[courses$idinst %in% studpatcredit$idinst, ], tapply(NumCreditsAttempt, TermId, FUN=sum, na.rm=T))) ##Credits 8.0% summer
with(courses[courses$idinst %in% studpatcredit$idinst & courses$TermId==6, ], lunique(idinst))/14429 #37% of students took a summer course

## How many patterns - intensity? (Table 1) ####
lunique(studpatcredit$termpat6)
cbind(head(sort(table(studpatcredit$termpat6), decreasing=T), 10))
cbind(sample(sort(table(studpatcredit$termpat6)[table(studpatcredit$termpat6)>0], decreasing=T), 10))
cbind(head(sort(table(studpatcredit$termpat6.x), decreasing=T), 10))

## any key differences by starting term
cbind(head(sort(table(studpatcredit$termpat6[studpatcredit$CohortTermId==1]), decreasing=T), 10))
cbind(head(sort(table(studpatcredit$termpat6[studpatcredit$CohortTermId==3]), decreasing=T), 10))
cbind(head(sort(table(studpatcredit$termpat6[studpatcredit$CohortTermId==6]), decreasing=T), 10))

## outcomes for one and dones
table(studpatcredit$OutcomeAsOfYear5[oneanddone])

#### talking points ####
studpatcredit$evertran <- !is.na(studpatcredit$newtran)
## switching. count number of 10, 01, 11, 00
studpatcredit$termpatnodot <- gsub(pattern="\\.",replacement="", x=studpatcredit$termpat6)
studpatcredit$num10 <- str_count(studpatcredit$termpatnodot, "10")
studpatcredit$num01 <- str_count(studpatcredit$termpatnodot, "01")
studpatcredit$num11 <- str_count(studpatcredit$termpatnodot, perl("1(?=1)"))
studpatcredit$num00 <- str_count(studpatcredit$termpatnodot, perl("0(?=0)"))
studpatcredit$num10_cons <- str_count(studpatcredit$termpat6, "10")
studpatcredit$num01_cons <- str_count(studpatcredit$termpat6, "01")
studpatcredit$num11_cons <- str_count(studpatcredit$termpat6, perl("1(?=1)"))
studpatcredit$num00_cons <- str_count(studpatcredit$termpat6, perl("0(?=0)"))
##total number of "transitions" not including nonenrollment
studpatcredit$tottran <- with(studpatcredit, num10+num01+num11+num00)
studpatcredit$tottran_dif <- with(studpatcredit, num10+num01)
## number of times a nonenrollment period is followed by an enrollment period
studpatcredit$numnonenr <- str_count(studpatcredit$termpat6, perl("0\\.")) + str_count(studpatcredit$termpat6, perl("1\\."))
studpatcredit$nonenr <- str_count(studpatcredit$termpat6, "\\.")
studpatcredit$firstbreak <- str_locate(studpatcredit$termpat6, "\\.")[,1]
studpatcredit$firstbreak[is.na(studpatcredit$firstbreak)] <- 18
studpatcredit$firstbreak2 <- str_locate(studpatcredit$termpat6, "\\.\\.")[,1]
studpatcredit$firstbreak2[is.na(studpatcredit$firstbreak2)] <- 18
studpatcredit$firstbreak3 <- str_locate(studpatcredit$termpat6, "\\.\\.\\.")[,1]
studpatcredit$firstbreak3[is.na(studpatcredit$firstbreak3)] <- 18
studpatcredit$traditional <- str_count(studpatcredit$termpat6, "^11\\.11\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.")

studpatcredit$fig1trad <- str_detect(studpatcredit$termpat6, 
"^110110\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.|^11011\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.|^11\\.11\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.\\.")
  

addmargins(table(studpatcredit$newtran[oneanddone]==2, studpatcredit$CohortTermId[oneanddone]))
table(intens$`6`)

## intensity for some more traditional pathways
credattfss <- credattempt.df6[sset,]
intens <- df.sorter(credattfss, studpatcredit$fig1trad==TRUE) 
par(mfcol=c(1,1), mar=c(4,4,1,1))
image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
abline(v=seq(1,34,2)/34, col="grey")
abline(h=cumsum(table(as.character(studpatcredit$termpat6)[studpatcredit$fig1trad])/sum(studpatcredit$fig1trad)), col="black")

## matrix image of intensity ####
intens <- df.sorter(credattempt.df6, sset) 
## first completion term
intens_awrd <-  x.coords(plotback=intens, gradconc="newgrad_prog")
## first concentration term
intens_conc <-  x.coords(plotback=intens, gradconc="newconc")
## transfer term
intens_tran <- x.coords(plotback=intens, gradconc="newtran")
## transfer term - left/last term justified
intens_tran_left <- x.coords(plotback=intens, gradconc="newtran_left")

## display images - intensity,
par(mfcol=c(1,1), mar=c(4,4,1,1))
image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
abline(v=seq(1,34,2)/34, col="grey")

## Intensity figure with completions
intens_awrd$type <- studpatcredit$gradtype[match(rownames(intens_awrd), studpatcredit$idinst)]
coltype <- c(alpha(ccrc_red, .75), alpha(ccrc_red, .75), alpha("black", .75), alpha("black", .75), alpha(ccrc_cadet, .75))
names(coltype) <- names(table(intens_awrd$type))
intens_awrd$coltype <- coltype[match(intens_awrd$type, names(coltype))]
intens_awrd$type[intens_awrd$type==1] <- 15
intens_awrd$type[intens_awrd$type==2] <- 16
intens_awrd$type[intens_awrd$type==4] <- 3
intens_awrd$type[intens_awrd$type==5] <- 4
intens_awrd$type[intens_awrd$type==6] <- 17
with(intens_awrd, points(jitter(x), y, pch=intens_awrd$type, cex=.75, col=intens_awrd$coltype))
legend("bottom", legend=c("SC","LC","AA","AS","AAS"), col=coltype,pch=c(15, 16, 3, 4, 17), horiz=T, bty="n", cex=1, x.intersp=.3, xjust=0)

## Intensity image with transfers
image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
abline(v=seq(1,34,2)/34, col="grey")
with(intens_tran, points(jitter(x), y, pch=16, cex=.75, col=alpha(ccrc_purp, .5)))

## First, vanilla, starting part-time or full-time. Table 3
with(studpatcredit, CrossTable(startft, earncred, prop.c=F, prop.t=F, prop.chisq=F))
with(studpatcredit, CrossTable(startft, evertran, prop.c=F, prop.t=F, prop.chisq=F))

## Next, relationship to intensity as number of ft and pt terms and percentage
par(mfcol=c(2,1))
# distcred <- with(studpatcredit, tapply(earncred,numft, mean))
# plot(as.numeric(names(distcred)), distcred, type="l", col=ccrc_oran, xlab="Number of PT/FT terms", ylab="Proportion earning credential")
# distcred <- with(studpatcredit, tapply(earncred,numpt, mean))
# lines(as.numeric(names(distcred)), distcred, type="l", col=ccrc_blue)
# hist(0, breaks=seq(from=0, to=1, by=.1), plot=F)$mids

#figure 4
distcred <- with(studpatcredit, tapply(earncred,cut(pctft,breaks=seq(from=0, to=1, by=.1),include.lowest=T), mean)) 
barplot(distcred, col=ccrc_red, xlab="Percent FT terms", ylab="Proportion earning credential", horiz=F)
distcred <- with(studpatcredit, tapply(evertran,cut(pctft,breaks=seq(from=0, to=1, by=.1),include.lowest=T), mean)) 
barplot(distcred, col=ccrc_purp, xlab="Percent FT terms", ylab="Proportion transferring", horiz=F)


## kmeans to cluster these enrollment patterns by attributes related to switching ####
## just using kmeans here for simplicity
clusdat <- with(studpatcredit, cbind(numterms, pctft, num10/numterms, num11/numterms, num00/numterms, num01/numterms, numnonenr/numterms, num10_cons/numterms, num11_cons/numterms, num00_cons/numterms, num01_cons/numterms, tottran_dif, firstbreak, firstbreak2, firstbreak3))
wss <- rep(NA,10)
for (ii in 2:10) {
  assign(paste("km", ii, sep=''), kmeans(scale(clusdat), centers=ii))
  wss[ii] <- eval(parse(text=paste("km", ii, "$tot.withinss", sep='')))
}
par(mfcol=c(1,1))
plot(wss, type="b")
## 6 clusters is actually pretty good. elbow is actually at 4

km6.names <- c("Full-time Persisters", "Early Leavers", "Early Persistent Switchers", "Mostly Part-timers", "Early Attachers", "Later Attachers") 


## describing k-means clusters ####
table(km6$cluster)
km6$centers
apply(clusdat, MARGIN=2, FUN=function(x) tapply(x, km6$cluster, mean, na.rm=T))
sort(table(as.character(studpatcredit$termpat6)[km6$cluster==1]))

## matrix image of intensity clusters ####
credclus.df <- credattempt.df6[sset,]
intens <- df.sorter(credclus.df, km6$cluster==5) 
par(mfcol=c(1,1), mar=c(4,4,1,1))
image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
abline(v=seq(1,34,2)/34, col="grey")

## demographics etc of clusters ####
t(stat.desc(studpatcredit[km6$cluster==6,demog]))[,c("mean", "std.dev")]

sesdex <- with(studpatcredit, rowMeans(cbind(AvgHouseholdIncome,PctHouseWithBachelorsAbove,PctEmployedManagementProf,PctHealthcareCoverage), na.rm=T))
sapply(1:6, FUN=function(x) t(stat.desc(sesdex[km6$cluster==x]))[,c("mean", "std.dev")])

sapply(1:6, FUN=function(y)
sapply(deved, function(x) {
  basetab <- table(get(x, studpatcredit[km6$cluster==y,]))
  pct <- prop.table(basetab)[1]
}))

sapply(1:6, function(x) prop.table(table(studpatcredit$CohortTermId[km6$cluster==x])))

table(studpatcredit$num10[!km6$cluster==3])

## postsecondary outcome figures ####
# Figure 5 grad
distcred <- with(studpatcredit, tapply(earncred,km6$cluster, mean))
names(distcred) <- km6.names
bp <- barplot(distcred, xlab="Cluster", ylab="Proportion earning credential", horiz=F, col=ccrc_red, cex.names=.75 ,ylim=c(0, max(distcred)*1.2))
text(bp, distcred, paste(round(distcred*100, 0),"%",sep=''),cex=.8,pos=3)
# Figure 6 transfer
distcred <- with(studpatcredit, tapply(evertran,km6$cluster, mean))
names(distcred) <- km6.names
bp <- barplot(distcred, xlab="Cluster", ylab="Proportion transferring", horiz=F, col=ccrc_purp, cex.names=.75, ylim=c(0, max(distcred)*1.1))
text(bp, distcred, paste(round(distcred*100, 0),"%",sep=''),cex=.8,pos=3)
# Figure 7 Grad or transfer
distcred <- with(studpatcredit, tapply(evertran | earncred ,km6$cluster, mean))
names(distcred) <- km6.names
bp <- barplot(distcred, xlab="Cluster", ylab="Proportion who earn a credential or transfer", horiz=F, col=ccrc_oran, cex.names=.75, ylim=c(0, max(distcred)*1.1))
text(bp, distcred, paste(round(distcred*100, 0),"%",sep=''),cex=.8,pos=3)

save.image("intensity.Rdata")


## Intensity figures for students with summer attendance and
## figures for those without summer attendance.
sset_summer <- sset & rownames(credattempt.df) %in% unique(attach.longSum$idinst[attach.longSum$TermId==6])
sset_nosummer <- sset & !(rownames(credattempt.df) %in% unique(attach.longSum$idinst[attach.longSum$TermId==6]))

## matrix image of intensity ####
intens <- df.sorter(credattempt.df6, sset_nosummer) 
## display images - intensity,
par(mfcol=c(1,1), mar=c(4,4,1,1))
image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
axis(side=1, at=seq(0,1,1/17), labels=c(1:18), tick=F, hadj=0, padj=-1)
abline(v=seq(1,34,2)/34, col="grey")
