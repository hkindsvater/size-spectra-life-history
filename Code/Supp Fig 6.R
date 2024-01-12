 

Tmax=18
time=1:(Tmax*12)
# quartz()
# windowframe=c(4,1)
#  
# par(mfrow=windowframe)

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
lifetimeM <-  function(survdata, filenames) {
    
  data2 <- survdata$x[-216]
   maxage <- which(survdata$x < 0.06)
 # plot(data1[,2]*data2, type="l",main=substr(filenames, 23, 31), xaxt="n", lwd=3,ylim=c(0, 5e+07), xlim=c(0.5, Tmax*12), ylab="Reproductive value", xlab="Age (years)")
  #axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  return(data2)
  
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
  
  
  lifespan <- min(which(survdata$x < 0.06))
  
  return(lifespan)

}
  
  
#point to the files you want to compare

#steps to define the working directory are outlined in the main text figure code files
    setwd("~/size-spectra-life-history/Model_output/suppfig6/")
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



quartz()
windowframe=c(4,1)

par(mfrow=windowframe)
L<-mapply(plot_length, length_data, length_filenames)    
R<-mapply(plot_repro, repro_data, length_data, repro_filenames)
# RV<-mapply(lifetimeR, repro_data, surv_data, length_filenames)
# RV <- rbind(RV, rep(0, 12))
lifespan <- mapply(maxage, surv_data, length_filenames) 
lifespan[2]<-215
lifespan[3]<-91
lifespan[4]<-76
lifespan[5]<-40
lifespan[6]<-33
M <- mapply(lifetimeM, surv_data, length_filenames)

truncate_age <- function(kappamat, lifespan) { #where kappamat is L, R, or RV
  
  for (i in 1:6) {
  
  kappamat[lifespan[i]:(Tmax*12-1), i] <- NA
  }
  return(kappamat)
  
}

truncL <- truncate_age(L, lifespan)
truncR <- truncate_age(R, lifespan)/4.2e+6 
truncM <- truncate_age(M, lifespan) 
 
 
 
 kappa<-c(0.5, 2)
 index <- 1:6
 linetypes <-c( 3, 1, 3, 1, 3, 1)
  
dev.new(height = 3.5,width = 4,units ="in")
 
 pal = c("#FF9E79","#FFCE30",  "#C23B22", "#FB6D4C",    "#8A0000","#8A0000")
 
 matplot(truncL[, 1:6], col=pal, type="l", lty=c( 3, 1, 3, 1, 3, 1), xaxt="n", xlab = "", las=1, lwd=4,   ylab= "Length (cm)")
         axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))   
           legend(x=132, y=150, bty="n", col=pal,  lty=c( 3, 1, 3, 1, 3 ), lwd=4, cex=0.5, legend=c("H = 4, K = 0.5", "H = 4, K = 2",  "H = 8, K = 0.5", "H = 8, K = 2","H = 12, K = 0.5 and 2"))
          
 plot(1:215, truncR[1:215, 2], type = "n", xaxt="n", las=1, xlab="", ylab = "Reproductive output (kg)")

 for (i in 1:length(index)){
   
   lines(lowess(1:(lifespan[index[i]]-1), truncR[1:(lifespan[index[i]]-1), index[i]], f = .5), lwd=3, lty=linetypes[i], col=pal[i])
   
 }
   axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))   

        
         
           
 matplot(truncM[, 1:6], col=pal, type="l", lty=c( 3, 1, 3, 1, 3, 1), xaxt="n", xlab = "Age (years)", las=1, lwd=4,  ylab= "Percent of cohort surviving")
    axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))      
    
    
    

    
    