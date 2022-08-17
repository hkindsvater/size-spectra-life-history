 
 
Tmax=18
time=1:(Tmax*12)
 
##Define functions to read in, plot results to check

plot_length <- function(data, filenames) {
  
  # 
  # matplot(t(data[,-1]), type="l", main=substr(filenames, 23, 31), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  # axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  # 
 
     
  return((data[1, -1]))
    
}


plot_repro <- function(repro_data, length_data, repro_filenames) {
  #matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="red", lwd=1.75, lty=1,   
         # ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 5e+08), xlim=c(0.5, Tmax*12))
   
  
  return(as.numeric(repro_data[1, -1]) )
   
}
 
# 
fit_age <- 1:215 #note we are focused only on growth from 0.5 year to 18 years of age.
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
  
  
 lifespan <- min(which(survdata$x < 0.05))
 lifespan <- ifelse(lifespan == Inf, 216, lifespan)
  print(survdata$x)
  return(lifespan)

}
  
  
##Now read in data files 
#the following is to specifically produce panels C and F in Figure 7. The directory can be modified to produce the remaining panels. 

  setwd("~/Group3/")
  
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


###summarize data
L<-mapply(plot_length, length_data, length_filenames)    
R<-mapply(plot_repro, repro_data, length_data, repro_filenames)

lifespan <- mapply(maxage, surv_data, length_filenames) 



truncate_age <- function(kappamat, lifespan) { #where kappamat is L, R, or RV
  
  for (i in 1:(ncol(L))) {
    
    kappamat[lifespan[i]:(Tmax*12), i] <- NA
  }
  return(kappamat)
  
}

##make plots

quartz()
windowframe=c(4,1)

par(mfrow=windowframe)

Jdensity <-  4.2e+6 #J/kg #from Chapman et al. 2011
truncL <- truncate_age(L, lifespan)
truncR <- log(truncate_age(R, lifespan))
 truncR[, 1] <- truncR[, 1]/Jdensity
 
  
 matplot(L[1:214, 1], type="l", las = 1, xaxt="n", xlab="", ylim=c(0, 330), col=1, lty=1, lwd=3, ylab="Length (cm)")
  axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))
 #text(x=c(  150 ), y = c(305), labels = c(  expression(paste(kappa," = 10.0")) ) )
 #note - average temperature is 290.4K
 longlife <- as.matrix(R[, 1])
 pal = gray.colors(4, start=0.8, end=0.3)
 plot(R[1:214, 1], las = 1, type = "n", xaxt="n", xlab="Age (years)",  ylab = "Reproductive output (kg)")
 axis(1, at = seq(0, (17)*12, by=12), labels = (seq(1, 18, by=1)))

  for (i in 1:length(1)){

   lines(lowess(1:(lifespan[i]-1), R[1:(lifespan[i]-1), 1], f = 1/50 ), lwd=3, col=1)

 }


