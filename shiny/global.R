## Load in smaller dataset constructed for web application use
## This is in global file so that browser reload does not take long reloading data.
load("./data/ep_shiny.Rdata")

## Create individual variables for race
demoset$white <- ifelse(demoset$race.fac=="White", 1, 0)
demoset$black <- ifelse(demoset$race.fac=="Black", 1, 0)
demoset$amerind <- ifelse(demoset$race.fac=="AmerIndian", 1, 0)
demoset$hispanic <- ifelse(demoset$race.fac=="Hispanic", 1, 0)
demoset$asian <- ifelse(demoset$race.fac=="AsianPI", 1, 0)
demoset$otherrace <- ifelse(demoset$race.fac=="OtherUnkMult", 1, 0)

## Create academic outcome variables
demoset$getaa <- demoset$studentid %in% credset$studentid[credset$credential %in% c("AA")]
demoset$getas <- demoset$studentid %in% credset$studentid[credset$credential %in% c("AS")]
demoset$getaas <- demoset$studentid %in% credset$studentid[credset$credential %in% c("AAS")]
demoset$getafa <- demoset$studentid %in% credset$studentid[credset$credential %in% c("AFA")]
demoset$getage <- demoset$studentid %in% credset$studentid[credset$credential %in% c("AGE")]
demoset$getcert <- demoset$studentid %in% credset$studentid[credset$credential %in% c("C")]
demoset$getdipl <- demoset$studentid %in% credset$studentid[credset$credential %in% c("D")]

## Mark students as in the first program code in which they enrolled
aaprog <- c("A10100", "A1010A","A1010B","A1010C","A1010D","A1010E","A1010F","A1010G","A1010H","A1010I","A1010J","A1010K",
            "A1010L","A1010M","A1010N","A1010O","A1010P","A1010Q","A1010R","A1010S", "A1010T", "A1010U", 
            "A1010V", "A1010W", "A1010X", "A1010Y", "A1010Z") 
asprog <- c("A10400", "A1040A","A1040B","A1040C","A1040D", "A1040E","A1040F")
afaprog <- c("A10200", "A1020A", "A1020C", "A1020D")
aasprog <- sort(unique(trans1$currcode))[34:209]
demoset$aaprog <- demoset$studentid %in% unique(trans1$studentid[trans1$currcode %in% aaprog])
demoset$asprog <- demoset$studentid %in% unique(trans1$studentid[trans1$currcode %in% asprog])
demoset$afaprog <- demoset$studentid %in% unique(trans1$studentid[trans1$currcode %in% afaprog])
demoset$aasprog <- demoset$studentid %in% unique(trans1$studentid[trans1$currcode %in% aasprog])

## To get name of institutions, use IPEDS data and link with unitid-localid crosswalk.
## This code is more elaborate than necessary
postsec <- read.csv("./data/postsec_loc.csv")
nccode <- read.csv("./data/NCCCS_unitid.csv")
nccode <- nccode[,1:4]

colleges <- postsec[,c("ICLEVEL", "STABBR", "INSTNM", "UNITID")]
colleges <- colleges[colleges$ICLEVEL!=-3,]
colleges$ICLEVEL <- factor(colleges$ICLEVEL)
nccolleges <- subset(colleges, subset=STABBR=="NC")
ncccscolleges <- subset(nccolleges, subset=UNITID %in% nccode$UNITID) 
ncccscolleges <- merge(ncccscolleges, nccode, by="UNITID")
ncccscolleges$shortnm <- substr(ncccscolleges$collegename,1,3)
nc1 <- ncccscolleges[,c("collegecode", "shortnm")]

collvec <- nc1$collegecode
names(collvec) <- paste(nc1$collegecode,nc1$shortnm,sep='-')
