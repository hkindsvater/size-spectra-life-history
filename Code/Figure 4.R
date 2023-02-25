 require(fields)

Tmax=18
time=1:(Tmax*12)
 

#define functions to make summary plots of results to check they are sensible
plot_length <- function(data, filenames) {
  
  
  matplot(t(data[,-1]), type="l", main=substr(filenames, 23, 31), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
 
     
  return((data[1, -1]))
    
}


plot_repro <- function(repro_data, length_data, repro_filenames) {
  #matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="red", lwd=1.75, lty=1,   
         # ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 5e+08), xlim=c(0.5, Tmax*12))
   
  
  return(as.numeric(repro_data[1, -1]) )
   
}
 
# 
fit_age <- 1:215 #note we are focused only on growth from 0.5 year to 18 years of age..... 
# 
# 
#### now define functions to calculate metrics
lifetimeR <-  function(reprodata, survdata, filenames) {
   
  data1 <- (as.data.frame(t(rbind(fit_age, reprodata[1, 1:215]))))
  
  data2 <- survdata$x[-216]
   maxage <- which(survdata$x < 0.05)
 # plot(data1[,2]*data2, type="l",main=substr(filenames, 23, 31), xaxt="n", lwd=3,ylim=c(0, 5e+07), xlim=c(0.5, Tmax*12), ylab="Reproductive value", xlab="Age (years)")
  #axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  return(as.vector(data1[,2]*data2))
  
} 
realized_M <-  function(survdata, filenames) {
  annual_index<- seq(12, 215, 12)/12
  data1  <- log(survdata$x[annual_index])
  reg1 <- lm(data1~annual_index)
  plot(annual_index, data1, ylab = "Ln(proportion surviving)", xlab = "Age (years)")
  abline(reg1)
   return(coef(reg1)[[2]])
  
} 
 
maxage<-  function(survdata, filenames) {
  
  
  lifespan <- min(which(survdata$x < 0.03))
  
  return(lifespan)

}
  
  
#point to the files you want to compare
    setwd("~/size-spectra-life-history/Model_output/fig4")
 data_files <- list.files(pattern = "\\.csv$")

# 
# ## read in files
# 
repro_filenames <- data_files[((length(data_files)/4)+1):(2*(length(data_files)/4))]
state_filenames <- data_files[(2*(length(data_files)/4)+1):(3*(length(data_files)/4))]
length_filenames <- data_files[1:(length(data_files)/4)]
surv_filenames <- data_files[(3*(length(data_files)/4)+1):(4*(length(data_files)/4))]

length_filenames <- unique(length_filenames)
repro_filenames <- unique(repro_filenames)
state_filenames <- unique(state_filenames)
surv_filenames <- unique(surv_filenames)

length_data <- lapply(length_filenames, read.csv)
repro_data <- lapply(repro_filenames, read.csv)
state_data <- lapply(state_filenames, read.csv)
surv_data <- lapply(surv_filenames, read.csv)
 
L<-mapply(plot_length, length_data, length_filenames)    
R<-mapply(plot_repro, repro_data, length_data, repro_filenames)
RV<-mapply(lifetimeR, repro_data, surv_data, length_filenames)
 
lifespan <- mapply(maxage, surv_data, length_filenames) 
 

truncate_age <- function(kappamat, lifespan) { #where kappamat is L, R, or RV
  
  for (i in 1:5) {
  
  kappamat[lifespan[i]:(Tmax*11+12), i] <- NA
  }
  return(kappamat)
  
}

truncL <- truncate_age(L, lifespan)
truncR <- truncate_age(R, lifespan)
truncRV <- truncate_age(RV, lifespan)
 
kappa <- c(0.25, 0.5, 1, 2, 5)
  index <- c(1, 2, 3, 4, 5)
  
 
dev.new(height = 5.5,width = 5,units ="in")
 

  pal = gray.colors(length(index), start=0.9, end=0.3)
 split.screen( rbind(c(0, .8,0,1), c(.8,1,0,1)))
 
 # now divide up the figure region 
 split.screen(c(2,1), screen=1)-> ind
 
 zr<- range(kappa[index])
 
 # first image
 screen( ind[1])
 par(mar = c(4,5, 0.5, 0.5))  
 
 matplot(truncL[1:150, index] , las = 1, type = "l", lty=1, lwd=3, col= pal,   xaxt="n", xlab="", ylab = "Length (cm)")
 axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))
 
screen( ind[2])
 par(mar = c(4, 5, 0.5, 0.5))  
 Jdensity <-  4.2e+6 

 plot(1:150, truncR[1:150, index[length(index)]]/Jdensity , las = 1, type = "n", xaxt="n", xlab="", ylab = "Reproductive\noutput (kg)")
 axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))
 
 for (i in 1:length(index)){
   
   lines(lowess(1:(lifespan[index[i]]-1), truncR[1:(lifespan[index[i]]-1), index[i]]/Jdensity, f = .45), lwd=3, col=pal[i])
   
 }
 
  
 # move to skinny region on right and draw the legend strip 

 screen( 2)
 image.plot( zlim=zr,legend.only=TRUE, smallplot=c(.1,.2, .3,.7),  breaks = (c(0.25, 0.5, 1, 2, 5,6)-.25), lab.breaks=names(c(0,0.25, 0.5, 1, 2, 5)),
             col=pal)
 
 close.screen( all=TRUE)

 

 
 
 #to make SUPPLEMENTAL FIGURE 4###
 
 
dev.new(height = 5.5,width = 5,units ="in")
 
  
  pal = gray.colors(length(index), start=.9, end=0.3)
 split.screen( rbind(c(0, .8,0,1), c(.8,1,0,1)))
 
 # now divide up the figure region 
 split.screen(c(2,1), screen=1)-> ind
 
 # first image
 screen( ind[1])
 par(mar = c(4,5, 0.5, 0.5))  
 
 matplot(truncL[1:150, index] , las = 1, type = "l", lty=1, lwd=3, col= pal,   xaxt="n", xlab="", ylab = "Length (cm)")
 axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))
 
  

 #second image
 
 screen( ind[2])
 par(mar = c(4, 5, 0.5, 0.5))  
 
  Jdensity <-  4.2e+6 
 matplot(truncR[1:150, index]/Jdensity ,  las = 1,type = "p", pch=19, col= pal, cex=.3,   xaxt="n", xlab="Age (years)", ylab = "Reproductive\noutput (kg)")
 axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))
  
 
 # move to skinny region on right and draw the legend strip 
 screen( 2)
 image.plot( zlim=zr,legend.only=TRUE, smallplot=c(.1,.2, .3,.7),  breaks = (c(0.25, 0.5, 1, 2, 5,6)-.25), lab.breaks=names(c(0,0.25, 0.5, 1, 2, 5)),
             col=pal)
 
 close.screen( all=TRUE)
 
  