seasons = "NO"

#Environmental Parameters
timebin=12
f_h <- 8/timebin
Temp <- 290
reprolimit=0.2
Kappa = 8/timebin
Tmax = 18*timebin  #monthly time steps, maximum lifespan is 18 years

#Parameters for temperature dependent costs
k=1.3e-23
E = 1.04e-19
theta=0.66
coef1  =5e+16 ##normalization constant puts tuna SMR near the costs in Kitchell et al. (1978) Bioenergetic spectra of skipjack and yellowfin tunas, pp 359 IN Sharp G.D. and Dizon A.E. eds. The Physiological Ecology of Tunas, Academic press.  

#physiological parameters
a <- 1e-5 #from Lombardo et al 2019
Jdensity <-  4.2e+6 #J/kg #from Chapman et al. 2011
b=1.8
d = 2.4

#STATE VARIABLES
phi <- 1 # only 1 environment
Lmax <- 400  #maximum size of 4 meters
Lmin <- 1 
Yindexmax <- 100 #maximum index of stores in state loop  

storemax= 0.6 #proportion of structural mass that individuals can devote to energy storage
storemin = 0.1

###################################################################################################################################################################################################
###Lookup Tables - look up costs and food functions so they are not calculated every time

###Sizespectra allow us to descripbe prey preference, encounter, consumption to predict prey availability and mass-specific mortality:
#total mass maximum in kg
##Prey availability   
phi_a <- 3 #from table 2.2 in Andersen book
lam <- 1.95
K_c=10 #from Table 2.2, this changes with season (and is scaled by Kappa)
##mass dependent mortality
phi_p <- 0.07 #from table 2.2 in Andersen book
f_0 <- 0.6 #somewhere between 0 and 1, but predators rarely caught with totally full stomach
hprime <- 17.2

#coefficient on the consumption rate from table 2.2
met_mort <- -0.25 #the argument in Andersen book is that mass-specific rates such as mortality Jdensitys with the metabolic esp of 3/4 (Brown et al. 2004). 

#steepness of penalty to survival near EcritL threshold state value
q = 5e-5 


####ADD SEASONALITY IN RESOURCES AND FOOD TO SOME MONTHS
if(seasons == "NO") {	
  kmult <-   rep(1, timebin)   
  raiseT <-      rep(0, timebin)  	
} else {
  #hardcoded for timebin = 12
  kmult <-    c(rep(3, 9), rep(1, 3))  #currently short breeding season with low resources
  raiseT <-   c(rep(0,9), rep(9, 3))#currently raise T from 15 to 24
  
} #end if 

Mass <- a*(Lmin:Lmax)^3 

mu<- phi_p*f_h*Mass^met_mort #note we are excluding "background" mortality that is independent of size.... 

Income = matrix(nrow = timebin, ncol = length(Mass))
MTcosts = matrix(nrow = timebin, ncol = length(Mass))

for (kap in 1:timebin) {
  
  Income[kap, ] <- kmult[kap]*Kappa*3*Mass^(0.05) #this describes the scaling with size and ecostystem richness
  MTcosts[kap, ] <-coef1*(Mass)^theta*(exp(-E/(k*(Temp+raiseT[kap])))) 
  
}

 

###Plot vs mass (Fig 2A - 2C)
kappas <- c(  0.5, 2, 4)
kap_inc <- matrix(nrow = length(kappas), ncol = length(Mass))

for (kp in 1:length(kappas)) {
  
 kap_inc[kp, ] <- kappas[kp]*3*Mass^(0.05)
}

 quartz() 
matplot( Mass, t(kap_inc), type = "l", las = 1,lty=1, lwd=2,   xlab="Mass (kg)", ylab = "Income (kg/month)", col = c("light green", "green", "dark green"))
        
 
hseq = c(4, 8, 12) 
mu_h = matrix(nrow = 3, ncol = length(Mass))
for (h in 1:length(hseq) ) {
  
  mu_h[h, ] <- phi_p*(hseq[h])*Mass^met_mort 
  
}
   

  matplot( Mass, t(exp(-mu_h)), type = "l", las = 1, lty=1, lwd=2, ylim = c(0,1), xlab="Mass (kg)", ylab = "Survival", col = c("light blue", "blue", "dark blue"))
  #ylab="Metabolic rate in J/season", col=c(4, 3, "orange", 2, "dark red")))


  temp_seq = c(285, 290, 295, 300)
  temp_mat <- matrix(nrow = 4, ncol = length(Mass))
  
  for (temp in 1:length(temp_seq)) {
  	
   
   temp_mat[temp, ] <- coef1*(Mass)^theta*(exp(-E/(k*(temp_seq[temp])))) 

    
  }
  
  

  matplot(Mass, t((temp_mat)), type = "l",  lty=1, lwd=2,  xlab="Mass (kg)", ylab="Metabolic costs in J/month", col=c("orange", "red", "dark red", "#301934") )  
  
          