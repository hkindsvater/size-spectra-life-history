# install.packages("FSA")
# install.packages("nlstools")

 # install.packages("ggplot2")
library(ggplot2)

Tmax=18
time=1:(Tmax*12)
quartz()
windowframe=c(4,1)
 
par(mfrow=windowframe)


##Define functions
plot_length <- function(data, filenames) {
  
  
  matplot(t(data[,-1]), type="l", main=substr(filenames, 36, 40), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  maxsize <- (min(which(as.numeric(data[1, (2:215)]) == max(as.numeric(data[1, (2:215)]), na.rm =TRUE)))) + 1 
  #print(data[1, maxsize])
   #age_m <- min(which(as.numeric(data[1, -1]) >= 0.5*as.numeric(data[1, maxsize])))/12 
  
    
   legend("topleft", legend=paste0("Lmax is ", data[1, maxsize], " cm"), bty="n")
   return(data[1, maxsize])
   
   
}


plot_repro <- function(repro_data,  repro_filenames) {
  matplot(t(repro_data[,-1]), type="l", main= substr(repro_filenames, 8, 23), col="red", lwd=1.75, lty=1,   
          ylab="Reproduction (J)",   xlab= "Age (years)", xaxt="n",  ylim=c(0, 5e+08), xlim=c(0.5, Tmax*12))
   #print(repro_data[1, -1])
  maxrepro <- max(as.numeric(repro_data[1, -1]), na.rm=TRUE) + 1 
 
  
   return(maxrepro)
}
 
# 
fit_age <- 1:215 #note we are focused only on growth from 0.5 year to 18 years of age..... 
# 
# 
#### now define functions to calculate reproduction
lifetimeR <-  function(reprodata, survdata, filenames) {
   
  data1 <- (as.data.frame(t(rbind(fit_age, reprodata[1, 1:215]))))
  
  data2 <- survdata$x[-216]
   
  plot(data1[,2]*data2, type="l",main=substr(filenames, 23, 31), xaxt="n", lwd=3,ylim=c(0, 5e+07), xlim=c(0.5, Tmax*12), ylab="Reproductive value", xlab="Age (years)")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  TotalR <- sum(data1[,2]*data2)
  return(c(substr(filenames, 23, 31), TotalR) )
} 
cumsurv <-  function(reprodata, survdata, filenames) {
  
  data1 <- (as.data.frame(t(rbind(fit_age, reprodata[1, 1:215]))))
  
  data2 <- survdata$x[-216]
  
  plot(data2, type="l",main=substr(filenames, 23, 31), xaxt="n", lwd=3,ylim=c(0, 5e+07), xlim=c(0.5, Tmax*12), ylab="Probability of survival to age", xlab="Age (years)")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
   
} 



##define wrapper function to calculate summary data for each temperature
calc_metrics <- function(data_files)
{
# ## read in files

repro_filenames <-
  data_files[((length(data_files) / 4) + 1):(2 * (length(data_files) / 4))]
state_filenames <-
  data_files[(2 * (length(data_files) / 4) + 1):(3 * (length(data_files) /
                                                        4))]
length_filenames <- data_files[1:(length(data_files) / 4)]
surv_filenames <-
  data_files[(3 * (length(data_files) / 4) + 1):(4 * (length(data_files) /
                                                        4))]

length_filenames <- unique(length_filenames)
repro_filenames <- unique(repro_filenames)
state_filenames <- unique(state_filenames)
surv_filenames <- unique(surv_filenames)

length_data <- lapply(length_filenames, read.csv)
repro_data <- lapply(repro_filenames, read.csv)
state_data <- lapply(state_filenames, read.csv)
surv_data <- lapply(surv_filenames, read.csv)
  
  
  
  food_tab1 <-
  substr(length_filenames, 37, 40) 
    kappa <- c(0.25, 0.5, 1, 2, 5)

###define the environmental context for these results
Temp <- substr(length_filenames, 13, 15) 
TempC <- as.numeric(Temp)-273.15
maxlength <- mapply(plot_length, length_data, length_filenames) 
maxR <- mapply(plot_repro, repro_data,   repro_filenames) 

repro_results <-
  mapply(lifetimeR, repro_data, surv_data, length_filenames)
lifetime_repro <- as.numeric(repro_results[2, ]) 

####create the dataframe summarizing the results of all metrics
tabdata <- cbind(TempC,  rep(kappa, 4), maxlength, maxR, lifetime_repro)
colnames(tabdata) <-
  c("Temp",    "kappa", "Max_length", "Max_R", "lifetime_R")

 return(tabdata)
}

 
#point to the files you want to compare

    setwd("/Users/hkindsvater/Documents/size-spectra-life-history/Model_output/fig5")
      data_files <- list.files(pattern = "\\.csv$")
 tabdata  <- calc_metrics(data_files)
 
 alldata <- as.data.frame(tabdata)

alldata$Max_size <- as.numeric(alldata$Max_length)
alldata$kappa <- as.numeric(alldata$kappa)
alldata$lifetime_R <- as.numeric(alldata$lifetime_R)
alldata$Max_R <- as.numeric(alldata$Max_R)
 
 dev.new(height = 4, width = 4, unit = "in")
 ggplot(data = alldata,  aes(x = kappa, y = Max_size, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 19, size = 4) +       
   
   scale_color_manual(values = alpha(c("#018571", "#80CDC1", "#DFC27D", "#A6611A" ), 0.85), name = "Temp (C)" ) + 
    
   
   ylim(c(0, 400)) +
   ylab("Maximum length (cm)")   +
   scale_x_continuous(expression(kappa), 1:10, 1:10) +
   theme_bw()
  

 Jdensity <-  4.2e+6 
 ggplot(data = alldata,  aes(x = kappa, y = lifetime_R/Jdensity, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 19, size = 4)  +
   scale_color_manual(values = alpha(c("#018571", "#80CDC1", "#DFC27D", "#A6611A" ), 0.85), name = "Temp (C)") + 
   scale_x_continuous(expression(kappa), 1:10, 1:10) +
   
   ylab("Expected Lifetime Reproduction (kg)")   +
   theme_bw()
 
 alldata$lifetime_R <- round(alldata$lifetime_R/Jdensity,2)
 alldata2 <- alldata[, -c(3,4)]
 write.csv(alldata2, file="suppTable1.csv")
  
 
 
 
 
 
 
 
 
 
 
 
 
 