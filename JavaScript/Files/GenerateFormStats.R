library(readxl)
library(ggplot2)
library(reshape2)
library(tcltk)
library(extrafont)
font_import()


#setwd("/home/cvkramer/Welch Class")
setwd(tk_choose.dir(caption="Select Directory for Plots"))

# Import dataset with Item and Item Characteristics
#x <- read_excel("/home/cvkramer/Welch Class/Copy of 6257_Unit7_Weekly Assignment 10_Item Statistics.xlsx")
x <- read_excel(tk_choose.files(caption="Item Pool Statistics File"))


# Group Item Selection
#####
#####  INPUT GROUP ITEM SELECTIONS HERE
#####
##Group 2
g2f1 <- c(41,18,49,31,36,4 ,7 ,14,25,27)
g2f2 <- c(34,5 ,43,10,44,2 ,9 ,11,12,48)

### ALL BELOW IS AUTOMATED

# Gather the rows that contain item statistics for chosen items
g21 <- which(x$"Item Number" %in% g2f1)
g22 <- which(x$"Item Number" %in% g2f2)

# Create datasets of item statistics for chosen items
i21 <- x[g21,]
i22 <- x[g22,]

# define ranges of ICC and IFF values.
icc <- c(9:49)
iff <- c(50:90)

# Function that returns classic statistics
clsc_est <- function(dat,rnd=2){
   xbar <- round(sum(dat$PVALUE),rnd)
   dat.si <- dat$PVALUE*(1-dat$PVALUE)
   dat.sx <- round(sum(dat.si*dat$Biserial),rnd)
   dat.k <- length(dat$"Item Number")
   dat.ca <- round(dat.k/(dat.k-1)*(1-sum(dat.si^2)/dat.sx),rnd)
   return(list(estMean = xbar, estSD = dat.sx, estAlpha = dat.ca))
}

create.iif <- function(dat,title_in,flag=0,inclCS=0) {
   dat.iif <- dat[,50:90]
   dat.itm <- dat[,1]
   dat.iift <- melt(dat.iif)
   dat.iift$Item <- dat.itm
   dat.iift$variable <- rep(seq(-3,3,6/40),each=10)
   dat.iift$Item <- as.factor(dat.iift$Item)
   cs <- clsc_est(dat)
   g <- ggplot(dat.iift, aes(variable, value, group=Item)) +
      geom_line(aes(color=Item))+
      scale_x_continuous(breaks = seq(-3, 3, 1),limits=c(-3,3)) +
      xlab("Theta") + 
      ylab("Information") +
      ylim(0,3.25) +
      ggtitle(title_in)
   
   if(flag==1){
      g <- g + guides(colour=FALSE)
   }
   
   if(inclCS==1){
      Text1 <- paste(title_in,"\n Est Mean=",cs$estMean," Est SD=",cs$estSD," Est Rel=",cs$estAlpha,sep='')
      g <- g + ggtitle(Text1)
   }
   
   return(g)
}

create.icc <- function(dat,title_in,flag=0) {
   dat.iif <- dat[,9:49]
   dat.itm <- dat[,1]
   dat.iift <- melt(dat.iif)
   dat.iift$Item <- dat.itm
   dat.iift$variable <- rep(seq(-3,3,6/40),each=10)
   dat.iift$Item <- as.factor(dat.iift$Item)
   g <- ggplot(dat.iift, aes(variable, value, group=Item)) +
      geom_line(aes(color=Item))+
      scale_x_continuous(breaks = seq(-3, 3, 1),limits=c(-3,3)) +
      xlab("Theta") + 
      ylab("Probability") +
      ggtitle(title_in)
   if(flag==1){
      g <- g + guides(colour=FALSE)
   }
   return(g)
}

create.tif <- function(dat,title_in) {
   
   dat.iif <- apply(dat[,iff],2,sum)
   dat.iift <- melt(dat.iif)
   dat.iift$variable <- seq(-3,3,6/40)

   g <- ggplot(dat.iift, aes(variable, value)) +
      geom_line()+
      scale_x_continuous(breaks = seq(-3, 3, 1),limits=c(-3,3)) +
      xlab("Theta") + 
      ylab("Information") +
      scale_y_continuous(breaks = seq(0, 10, 1),limits=c(0,10)) +
      ggtitle(title_in)
   
   return(g)
}

create.tcc <- function(dat,title_in) {
   
   dat.iif <- apply(dat[,icc],2,sum)
   dat.iift <- melt(dat.iif)
   dat.iift$variable <- seq(-3,3,6/40)

   g <- ggplot(dat.iift, aes(variable, value)) +
      geom_line()+
      scale_x_continuous(breaks = seq(-3, 3, 1),limits=c(-3,3)) +
      xlab("Theta") + 
      ylab("Score") +
      ylim(0,10) +
      ggtitle(title_in)
   
   return(g)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Classic Results
c21<-clsc_est(i21)
c22<-clsc_est(i22)

a <- as.data.frame.matrix(
   rbind(
      c(1,c21$estMean,c21$estSD,c21$estAlpha),
      c(2,c22$estMean,c22$estSD,c22$estAlpha)
   )
)  
names(a) <- c("Form","Est Mean", "Est SD", "Est Rel")

png("ClassicStats.png")
plot(1, type="n", axes=F, xlab="", ylab="",col="white")
text(0.6,1, paste(capture.output(a), collapse='\n'), pos=4, family="monospace")
dev.off()

pdf("IIF.pdf")
# IIF
multiplot(
   create.iif(i21,"IIF : Form 1"),
   create.iif(i22,"IIF : Form 2")
)
dev.off()

pdf("ICC.pdf")
# ICC
multiplot(
   create.icc(i21,"ICC : Form 1"),
   create.icc(i22,"ICC : Form 2")
)
dev.off()

pdf("TIF.pdf")
multiplot(
   create.tif(i21,"TIF : Form 1"),
   create.tif(i22,"TIF : Form 2")
)
dev.off()

pdf("TCC.pdf")
multiplot(
   create.tcc(i21,"TCC : Form 1"),
   create.tcc(i22,"TCC : Form 2")
)
dev.off()

pdf("TCCICC.pdf")
multiplot(
   create.tcc(i21,"TCC : Form 1"), 
   create.tcc(i22,"TCC : Form 2"),
   create.icc(i21,"ICC : Form 1",1),
   create.icc(i22,"ICC : Form 2",1),
   cols=2
)
dev.off()

pdf("TIFIIF.pdf")
multiplot(
   create.tif(i21,"TIF : Form 1"), 
   create.tif(i22,"TIF : Form 2"),
   create.iif(i21,"IIF : Form 1",1),
   create.iif(i22,"IIF : Form 2",1),
   cols=2
)
dev.off()