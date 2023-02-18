 

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



##define wrapper function to calculate summary data for each theta
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
  substr(length_filenames, 37, 41)
 
###define the environmental context for these results
Temp <- substr(length_filenames, 13, 15)
TempC <- as.numeric(Temp)-273.15
thet <-substr(length_filenames, 28, 31)
theta <- gsub("K", 0, thet)
  
maxlength <- mapply(plot_length, length_data, length_filenames)
maxR <- mapply(plot_repro, repro_data,   repro_filenames)

repro_results <-
  mapply(lifetimeR, repro_data, surv_data, length_filenames)
lifetime_repro <- as.numeric(repro_results[2, ]) 

####create the dataframe summarizing the results of all metrics
tabdata <- cbind(TempC,  theta, food_tab1, maxlength, maxR, lifetime_repro)
colnames(tabdata) <-
  c("Temp", "theta",    "kappa", "Max_length", "Max_R", "lifetime_R")
print(length_filenames)
 return(tabdata)
}
 
setwd("~/size-spectra-life-history/Model_output/suppfig9/295/")
data_file295 <- list.files(pattern = "\\.csv$")
tabdata2 <- calc_metrics(data_file295)
 

data295 <- as.data.frame(tabdata2)

data295$Max_size <- round(as.numeric(data295$Max_length), 1)
data295$kappa <- c(0.25, 0.5, 1, 2, 4, 0.25, 0.5,1, 2, 4)
data295$lifetime_R <- as.numeric(data295$lifetime_R)
data295$Max_R <- as.numeric(data295$Max_R)
 
 dev.new(height = 4, width = 4, unit = "in")
ggplot(data = data295,  aes(x = kappa, y = Max_size, group = as.factor(theta))) +
  geom_point(aes(color = as.factor(theta)), shape = 18, size = 4) +       
  
  scale_color_manual(values = alpha(c("#45b6fe",   "#296d98" ), 0.85), name = "theta" ) + 
  
  
  ylim(c(0, 400)) +
  ylab("Maximum length (cm)")   +
  scale_x_continuous(expression(kappa), 1:10, 1:10) +
  theme_bw()

Jdensity <-  4.2e+6 
ggplot(data = data295,  aes(x = kappa, y = lifetime_R/Jdensity, group = as.factor(theta))) +
  geom_point(aes(color = as.factor(theta)), shape = 18, size = 4) +       
  
  scale_color_manual(values = alpha(c("#45b6fe",   "#296d98" ), 0.85), name = "theta" ) + 
  scale_x_continuous(expression(kappa), 1:10, 1:10) +
  
  ylab("Expected Lifetime Reproduction (kg)")   +
  theme_bw()




 
 
 
 
 
 
 
 
 