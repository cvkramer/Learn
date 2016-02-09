library(readxl)
library(ggplot2)
library(reshape2)
library(tcltk)

#setwd("/home/cvkramer/Welch Class")
setwd(tk_choose.dir())

# Import dataset with Item and Item Characteristics
x <- read_excel("/home/cvkramer/Documents/Welch Class/Copy of 6257_Unit7_Weekly Assignment 10_Item Statistics.xlsx")
#x <- read_excel(tk_choose.files())


# Group Item Selection
#####
#####  INPUT GROUP ITEM SELECTIONS HERE
#####
##Group 2
g2f1 <- c(41,18,49,31,36,4 ,7 ,14,25,27)
g2f2 <- c(34,5 ,43,10,44,2 ,9 ,11,12,48)
##Group 3
g3f1 <- c(35,47,32,23,19,13,3 ,5 ,31,36)
g3f2 <- c(42,9 ,11,6 ,38,49,33,28,26,41)
##Group 4
g4f1 <- c(34,22,49,39,8 ,17,29,40,23,4 )
g4f2 <- c(41,42,37,28,36,30,27,35,25,2 )




### ALL BELOW IS AUTOMATED

# Gather the rows that contain item statistics for chosen items
g21 <- which(x$"Item Number" %in% g2f1)
g22 <- which(x$"Item Number" %in% g2f2)
g31 <- which(x$"Item Number" %in% g3f1)
g32 <- which(x$"Item Number" %in% g3f2)
g41 <- which(x$"Item Number" %in% g4f1)
g42 <- which(x$"Item Number" %in% g4f2)

# Create datasets of item statistics for chosen items
i21 <- x[g21,]
i22 <- x[g22,]
i31 <- x[g31,]
i32 <- x[g32,]
i41 <- x[g41,]
i42 <- x[g42,]

# define ranges of ICC and IFF values.
icc <- c(9:49)
iff <- c(50:90)

# Function that returns classic statistics
clsc_est <- function(dat,rnd=2){
   xbar <- round(sum(dat$PVALUE),rnd)
   dat.si <- dat$PVALUE*(1-dat$PVALUE)
   dat.sx <- round(sum(dat.si*dat$Biserial),rnd)
   dat.k <- length(dat$"Item Number")
   dat.ca <- round(dat.k/(dat.k-1)*(1-sum(dat.si^2)/dat.sx^2),rnd)
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
create.tif(i21,'')

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
c31<-clsc_est(i31)
c32<-clsc_est(i32)
c41<-clsc_est(i41)
c42<-clsc_est(i42)


a <- as.data.frame.matrix(
   rbind(
      c(2,1,c21$estMean,c21$estSD,c21$estAlpha),
      c(2,2,c22$estMean,c22$estSD,c22$estAlpha),
      c(3,1,c31$estMean,c31$estSD,c31$estAlpha),
      c(3,2,c32$estMean,c32$estSD,c32$estAlpha),
      c(4,1,c41$estMean,c41$estSD,c41$estAlpha),
      c(4,2,c42$estMean,c42$estSD,c42$estAlpha)
   )
)  
names(a) <- c("Group","Form","Est Mean", "Est SD", "Est Rel")

pdf("ClassicStats.pdf")
plot(1, type="n", axes=F, xlab="", ylab="",col="white")
text(1,1,
"
Group  Form  Est Mean  Est SD  Est Rel
2      1       5.87        1.20        0.63
2      2       5.83        1.24        0.64
3      1       5.21        1.09        0.59
3      2       4.67        0.98        0.59
4      1       4.42        1.12        0.57
4      2       4.67        1.23        0.63"
)
dev.off()

pdf("gIIF.pdf")
# IIF
multiplot(
   create.iif(i21,"IIF : Group 2 Form 1"),
   create.iif(i22,"IIF : Group 2 Form 2")
)
dev.off()
multiplot(
   create.iif(i31,"IIF : Group 3 - Form 1"),
   create.iif(i32,"IIF : Group 3 - Form 2")
)
multiplot(
   create.iif(i41,"IIF : Group 4 - Form 1"),
   create.iif(i42,"IIF : Group 4 - Form 2")
)
dev.off()

pdf("ICC.pdf")
# ICC
multiplot(
   create.icc(i21,"ICC : Group 2 | Form 1"),
   create.icc(i22,"ICC : Group 2 | Form 2")
)
multiplot(
   create.icc(i31,"ICC : Group 3 | Form 1"),
   create.icc(i32,"ICC : Group 3 | Form 2")
)
multiplot(
   create.icc(i41,"ICC : Group 4 | Form 1"),
   create.icc(i42,"ICC : Group 4 | Form 2")
)
dev.off()

pdf("TIF.pdf")
multiplot(
   create.tif(i21,"TIF : Group 2 | Form 1"),
   create.tif(i22,"TIF : Group 2 | Form 2")
)

multiplot(
   create.tif(i31,"TIF : Group 3 | Form 1"),
   create.tif(i32,"TIF : Group 3 | Form 2")
)
multiplot(
   create.tif(i41,"TIF : Group 4 | Form 1"),
   create.tif(i42,"TIF : Group 4 | Form 2")
)
dev.off()

pdf("gTCC.pdf")
multiplot(
   create.tcc(i21,"TCC : Group 2 | Form 1"),
   create.tcc(i22,"TCC : Group 2 | Form 2")
)

multiplot(
   create.tcc(i31,"TCC : Group 3 | Form 1"),
   create.tcc(i32,"TCC : Group 3 | Form 2")
)
multiplot(
   create.tcc(i41,"TCC : Group 4 | Form 1"),
   create.tcc(i42,"TCC : Group 4 | Form 2")
)
dev.off()

pdf("TCCICC.pdf")
multiplot(
   create.tcc(i21,"TCC : Group 2 | Form 1"), 
   create.tcc(i22,"TCC : Group 2 | Form 2"),
   create.icc(i21,"ICC : Group 2 | Form 1",1),
   create.icc(i22,"ICC : Group 2 | Form 2",1),
   cols=2
)
multiplot(
   create.tcc(i31,"TCC : Group 3 | Form 1"),
   create.tcc(i32,"TCC : Group 3 | Form 2"),
   create.icc(i31,"ICC : Group 3 | Form 1",1),
   create.icc(i32,"ICC : Group 3 | Form 2",1),
   cols=2
)
multiplot(
   create.tcc(i41,"TCC : Group 4 | Form 1"),
   create.tcc(i42,"TCC : Group 4 | Form 2"),
   create.icc(i41,"ICC : Group 4 | Form 1",1),
   create.icc(i42,"ICC : Group 4 | Form 2",1),
   cols=2
)
dev.off()

pdf("TIFIIF.pdf")
multiplot(
   create.tif(i21,"TIF : Group 2 | Form 1"), 
   create.tif(i22,"TIF : Group 2 | Form 2"),
   create.iif(i21,"IIF : Group 2 | Form 1",1),
   create.iif(i22,"IIF : Group 2 | Form 2",1),
   cols=2
)
multiplot(
   create.tif(i31,"TIF : Group 3 | Form 1"),
   create.tif(i32,"TIF : Group 3 | Form 2"),
   create.iif(i31,"IIF : Group 3 | Form 1",1),
   create.iif(i32,"IIF : Group 3 | Form 2",1),
   cols=2
)
multiplot(
   create.tif(i41,"TIF : Group 4 | Form 1"),
   create.tif(i42,"TIF : Group 4 | Form 2"),
   create.iif(i41,"IIF : Group 4 | Form 1",1),
   create.iif(i42,"IIF : Group 4 | Form 2",1),
   cols=2
)
dev.off()