options(scipen=3)
lunique <- function(x) length(unique(x))
pdensity <- function(x) plot(density(x, na.rm=T))
r2 <- function(x) round(x,2)
r3 <- function(x) round(x,3)

library(stringr)
library(scales)

tran.coords <- function(plotback,...) {
  attch_awrd <- data.frame(rownames(plotback))
  attch_awrd$aterm <- earliest4_rt[match(attch_awrd$rownames.plotback.,names(earliest4_rt))]
  rownames(attch_awrd) <- attch_awrd$rownames.plotback.
  attch_awrd$x <- termvec[as.character(attch_awrd$aterm)]
  attch_awrd$x <- (attch_awrd$x-1)/(ncol(plotback)-1)
  attch_awrd$y_prime <- 1/dim(attch_awrd)[1]
  attch_awrd$y <- cumsum(attch_awrd$y_prime)
  return(attch_awrd[,c("x", "y")])
}

award.coords <- function(plotback, atype=c("AA", "AS"),...) {
  attch_awrd <- data.frame(rownames(plotback))
  attch_awrd$aterm <- with(credset[credset$credential %in% atype,], awardterm[match(attch_awrd$rownames.plotback.,studentid)])
  rownames(attch_awrd) <- attch_awrd$rownames.plotback.
  attch_awrd$x <- termvec[as.character(attch_awrd$aterm)]
  attch_awrd$x <- (attch_awrd$x-1)/(ncol(plotback)-1)
  attch_awrd$y_prime <- 1/dim(attch_awrd)[1]
  attch_awrd$y <- cumsum(attch_awrd$y_prime)
  return(attch_awrd[,c("x", "y")])
}

ba.coords <- function(plotback,...) {
  attch_awrd <- data.frame(rownames(plotback))
  attch_awrd$aterm <- with(nsc_gradrecords, reportingterm[match(attch_awrd$rownames.plotback.,studentid2)])
  rownames(attch_awrd) <- attch_awrd$rownames.plotback.
  attch_awrd$x <- termvec[as.character(attch_awrd$aterm)]
  attch_awrd$x <- (attch_awrd$x-1)/(ncol(plotback)-1)
  attch_awrd$y_prime <- 1/dim(attch_awrd)[1]
  attch_awrd$y <- cumsum(attch_awrd$y_prime)
  return(attch_awrd[,c("x", "y")])
}

#sset <- TRUE

shinyServer(function(input, output) {
  
  output$graphsoutput <- reactivePlot(function() {
    ## this will need to change based on inputs.
    if (input$male & !input$female) {
      studsetsex <- unique(demoset$studentid[demoset$gender=="M"])
    }
    else if (input$female & !input$male) {
      studsetsex <- unique(demoset$studentid[demoset$gender=="F"])
    } else {
      studsetsex <- unique(demoset$studentid)
    }
    
    studsetrace <- NULL
    if (input$white) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$white==1]))
    }
    if (input$black) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$black==1]))
    }
    if (input$amerind) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$amerind==1]))
    }
    if (input$hispanic) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$hispanic==1]))
    }
    if (input$asian) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$asian==1]))
    }
    if (input$otherrace) {
      studsetrace <- c(studsetrace,unique(demoset$studentid[demoset$otherrace==1]))
    }
    if (is.null(studsetrace)) studsetrace <- unique(demoset$studentid)
    
    if (input$firstcollege!="All") {
      studsetcoll <- unique(demoset$studentid[demoset$collegecode %in% input$firstcollege])
    } else {
      studsetcoll <- unique(demoset$studentid)
    }
        
    if (input$collxferprog=="collxferprog") {
      studsetcollxferprog <- unique(demoset$studentid[demoset$aaprog | demoset$asprog | demoset$afaprog])
    } else if (input$collxferprog=="aas") {
      studsetcollxferprog <- unique(demoset$studentid[demoset$aasprog])
    } else {
      studsetcollxferprog <- unique(demoset$studentid)
    }
    
    if (input$collxfer=="collxfer") {
      studsetcollxfer <- unique(demoset$studentid[demoset$studentid %in% names(earliest4_rt)])
    } else if (input$collxfer=="nocollxfer") {
      studsetcollxfer <- unique(demoset$studentid[!(demoset$studentid %in% names(earliest4_rt))])
    } else {
      studsetcollxfer <- unique(demoset$studentid)
    }
    
    if (input$deved=="anydev") {
      studsetdev <- unique(demoset$studentid[demoset$tookanydev])
    } else if (input$deved=="nodev") {
      studsetdev <- unique(demoset$studentid[!demoset$tookanydev])
    } else {
      studsetdev <- unique(demoset$studentid)
    }
    
    
    ## identify subset
    studset <- intersect(studsetdev, intersect(studsetcollxferprog, intersect(studsetcollxfer, intersect(intersect(studsetsex, studsetrace), studsetcoll))))
    #if (length(studset)==0) studset <- unique(demoset$studentid)
    sset <- rownames(attachment.df) %in% studset
    
    output$enequal <- reactiveText(function() {
      return(paste0("Number of students displayed: ", sum(sset, na.rm=T)))
    })
    
    if (input$ftpt==FALSE) {
      attch <- df.sorter(attachment.df, sset)
      par(mfcol=c(1,1), mar=c(4,4,1,1))
      image(t(apply(attch, 2, as.numeric)), col=c("white", ccrc_oran), xlab="Term", ylab="Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
      axis(side=1, at=seq(0,1,1/(ncol(attch)-1)), labels=c(1:ncol(attch)), tick=F, hadj=0, padj=-1)
      pb <<- attch
    } else if (input$ftpt==TRUE) {
      intens <- df.sorter(credattempt.df6, sset)
      par(mfcol=c(1,1), mar=c(4,4,1,1))
      image(t(apply(intens, 2, as.numeric)), col=c(ccrc_blue, ccrc_oran), xlab="Term", ylab="PT/FT/Not Enrolled", xaxt="n", yaxt="n",mgp=c(1,1,0.5))
      axis(side=1, at=seq(0,1,1/(ncol(intens)-1)), labels=c(1:ncol(intens)), tick=F, hadj=0, padj=-1)
      pb <<- intens
    }
      
    earliest4_rt <- earliest4_rt
    if (input$showtran) {
      intens_tran <- tran.coords(plotback=pb, earliest4_rt)
      with(intens_tran, points(jitter(x), y, pch=15, cex=.75, col=alpha(ccrc_purp, .75)))
      output$xequal <- reactiveText(function() {
        return(paste0("Number of transfers displayed: ", sum(!is.na(intens_tran$x))))
      })
    } else {
      output$xequal <- reactiveText(function() {
        return("Number of transfers displayed: 0")
      })
    }
    
    if (input$showcred!="None") {
      intens_aaas <- award.coords(plotback=pb,atype=input$showcred, credset)
      with(intens_aaas, points(jitter(x), y, pch=16, cex=.85, col=alpha("black", .75)))
      output$oequal <- reactiveText(function() {
        return(paste0("Number of awards displayed: ", sum(!is.na(intens_aaas$x))))
      })
    } else {
      output$oequal <- reactiveText(function() {
        return("Number of awards displayed: 0")
      })
    }
    
    if (input$showba) {
      intens_ba <- ba.coords(plotback=pb, nsc_gradrecords)
      with(intens_ba , points(jitter(x), y, pch=17, cex=.75, col=alpha(ccrc_green, .75)))
      output$fourequal <- reactiveText(function() {
        return(paste0("Number of BAs displayed: ", sum(!is.na(intens_ba$x))))
      })
    } else {
      output$fourequal <- reactiveText(function() {
        return("Number of BAs displayed: 0")
      })
    }
  })
   
})
