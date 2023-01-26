 

 # install.packages("ggplot2")
library(ggplot2)

Tmax=18
time=1:(Tmax*12)
quartz()
windowframe=c(4,1)
 
par(mfrow=windowframe)


##Define functions
plot_length <- function(data, filenames) {
  
  
  matplot(t(data[,-1]), type="l", main=substr(filenames, 23, 31), col="darkgray", lwd=1.75, lty=1,  ylab="Length (cm)", ylim=c(0, 400), xlim=c(0.5, Tmax*12), xlab= "Age (years)", xaxt="n")
  axis(1, at = seq(0, (Tmax)*12, by=12), labels = (seq(1, Tmax+1, by=1)))
  
  maxsize <- (min(which(as.numeric(data[1, (2:215)]) == max(as.numeric(data[1, (2:215)]), na.rm = TRUE)))) + 1 
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
food_tab2  <- round(as.numeric(gsub("r", 0, food_tab1)) * 12, 1) 

###define the environmental context for these results
Temp <- substr(length_filenames, 13, 15)
TempC <- as.numeric(Temp)-273.15
maxlength <- mapply(plot_length, length_data, length_filenames)
maxR <- mapply(plot_repro, repro_data,   repro_filenames)

repro_results <-
  mapply(lifetimeR, repro_data, surv_data, length_filenames)
lifetime_repro <- as.numeric(repro_results[2, ]) 

####create the dataframe summarizing the results of all metrics
tabdata <- cbind(TempC,  food_tab2, maxlength, maxR, lifetime_repro)
colnames(tabdata) <-
  c("Temp",    "kappa", "Max_length", "Max_R", "lifetime_R")

 return(tabdata)
}

 
#point to the files you want to compare
setwd("/Users/hkindsvater/Documents/size-spectra-life-history/Model_output/Results_ss/SeasKOnly")
data_filesKseas <- list.files(pattern = "\\.csv$")
tabdata1 <- calc_metrics(data_filesKseas)
 
setwd("/Users/hkindsvater/Documents/size-spectra-life-history/Model_output/Results_ss/SeasTempOnly")
data_filesTempSeas <- list.files(pattern = "\\.csv$")
tabdata2 <- calc_metrics(data_filesTempSeas)
 
 
 
 seasTdata <- tabdata2
seasTdata <- as.data.frame(seasTdata)

seasTdata$Max_size <- as.numeric(seasTdata$Max_length)
seasTdata$kappa <- as.numeric(seasTdata$kappa)
seasTdata$lifetime_R <- as.numeric(seasTdata$lifetime_R)
seasTdata$Max_R <- as.numeric(seasTdata$Max_R)
 
 #quartz()
 ggplot(data = seasTdata,  aes(x = kappa, y = Max_size, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 18, size = 4) +       
   
   scale_color_manual(values = alpha(c("#018571",  "#DFC27D"  ), 0.85), name = "Temp (C)" ) + 
    
   
   ylim(c(0, 300)) +
   ylab("Maximum length (cm)")   +
   scale_x_continuous(expression(kappa), 1:10, 1:10) +
   theme_bw()
  
 
 ggplot(data = seasTdata,  aes(x = kappa, y = lifetime_R, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 18, size = 4) +       
   
   scale_color_manual(values = alpha(c("#018571",  "#DFC27D"  ), 0.85), name = "Temp (C)" ) + 
   scale_x_continuous(expression(kappa), 1:10, 1:10) +
   
   ylab("Expected Lifetime Reproduction (kg)")   +
   theme_bw()
 
  
 
 
 avgK <- (9*3*tabdata1[, 2]+3*tabdata1[, 2])/12
 seasKdata <- tabdata1
 seasKdata <- as.data.frame(seasKdata)
 
 
 seasKdata$Max_size <- as.numeric(seasKdata$Max_length)
 seasKdata$kappa <- as.numeric(seasKdata$kappa)
 seasKdata$lifetime_R <- as.numeric(seasKdata$lifetime_R)
 seasKdata$Max_R <- as.numeric(seasKdata$Max_R)
 
 #quartz()
 ggplot(data = seasKdata,  aes(x = kappa, y = Max_size, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 17, size = 4) +       
   
   scale_color_manual(values = alpha(c("#018571",  "#DFC27D"  ), 0.85), name = "Temp (C)" ) + 
   
   
   ylim(c(0, 300)) +
   ylab("Maximum length (cm)")   +
   scale_x_continuous(expression(avgK), 1:10, 1:10) +
   theme_bw()
 
 
 ggplot(data = seasKdata,  aes(x = kappa, y = lifetime_R, group = as.factor(Temp))) +
   geom_point(aes(color = as.factor(Temp)), shape = 17, size = 4) +       
   
   scale_color_manual(values = alpha(c("#018571",  "#DFC27D"  ), 0.85), name = "Temp (C)" ) + 
   scale_x_continuous(expression(avgK), 1:10, 1:10) +
   
   ylab("Expected Lifetime Reproduction (kg)")   +
   theme_bw()
 
 
 
 
 
 
 
 
 