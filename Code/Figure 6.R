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


    setwd("~/size-spectra-life-history/Model_output/fig6")
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
 lifespan[1]<- 18*12  #bluefin tuna lives past the maximum time (18 years) so we need to replace infinite lifespan
 #R[, 2] <- R[,2]/ 4.2e+6 #need to convert this file from J to KG
 quartz()
 par(mfrow =c(1, 3), mar = c(3, 4, 1, 2), oma = c(1,2,0,2),   family = "sans", las=1)
 # note that in the text figure, reproductive data axes are manually adjusted, and the bluefin tuna reproductive output is plotted without the loess (type = "l").  
  for (i in 3:1) {
  
  L[lifespan[i]:(Tmax*12), i] <- NA
  R[lifespan[i]:(Tmax*12), i] <- NA
  
   
  # maxL <- max(max(as.numeric(L[, i]), na.rm=T), 230)
  plot( 1:(Tmax*12), as.numeric(L[, i]), type="l", col=1, lwd=3, ylim=c(0, 375), las=1, ylab="Length (cm)",   xlim=c(0.5, Tmax*12), xlab= "", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
    
  
  }
 
 quartz()
 
 #plot first two panels using lowess
 par(mfrow =c(1, 3), mar = c(3, 4, 1, 2), oma = c(1,2,0,2),   family = "sans", las=1)
 # note that in the text figure, reproductive data axes are manually adjusted, and the bluefin tuna reproductive output is plotted without the loess (type = "l").  
 for (i in 3:2) {
   
   L[lifespan[i]:(Tmax*12), i] <- NA
   R[lifespan[i]:(Tmax*12), i] <- NA
   
   maxR <- c(NA, 4, 1)
    plot( 1:(Tmax*12), as.numeric(R[, i]), type="n",  lwd=3, lty=1,  las=1,   ylab="Reproductive output (kg)", ylim=c(0, maxR[i]), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)), xlab = "Age (years)")
   lines(lowess(1:(lifespan[i]-1), R[1:lifespan[i]-1, i],  f = .45), lwd=3, col=1)
     
   }
   
 #plot third panel without lowess
 plot( 1:(Tmax*12), as.numeric(R[, 1]), type="l",  lwd=3, lty=1,  las=1,   ylab="Reproductive output (kg)", ylim=c(0, 50), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
 axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)), xlab= "Age (years)")
 #lines(lowess(1:(lifespan[i]-1), R[1:lifespan[i]-1, i],  f = .45), lwd=3, col=1)
 
