 
#!/usr/bin/env Rscript --vanilla
#install.packages("fields")
# library(fields)
 
 
filepath <- "~/size-spectra-life-history/"
 
seasons = "NO"

#Environmental Parameters
 timebin=12
 f_h <- 8/timebin
 Temp <- 300
 reprolimit=0.2
 Kappa = 2/timebin
 Tmax = 18*timebin  #monthly time steps, maximum lifespan is 18 years
 
#Parameters for temperature dependent costs
k=1.3e-23
E = 1.04e-19
theta=0.66
 coef1  =5e+16 ##normalization constant puts tuna SMR near the costs in Kitchell et al. (1978) Bioenergetic spectra of skipjack and yellowfin tunas, pp 359 IN Sharp G.D. and Dizon A.E. eds. The Physiological Ecology of Tunas, Academic press.  

#physiological parameters
a <- 1e-5 #from Lombardo et al. 2019
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

# ##############################################################################################################################################################################################


#DYNAMIC MODEL: life history in a single environment (phi = 1)

#dynamic behaviors
u <-  seq(0, 1, by = 0.1) #fraction stored (rest allocated to structure)

r<-  seq(0, 1, by = 0.1) #fraction allocated to reproduction (rest is saved)


#set up arrays to store fitness values  
MaxF=array(dim=c(Yindexmax, Lmax, phi, Tmax),data=-1) #this will store the max fitness for every combination of state and times
MaxF[,,,Tmax]=0 #Fill fitness of Tmax column with 0
 

Vmat=array(dim=c(Yindexmax, Lmax, phi, Tmax-1, length(u), length(r) ))
#Vmat is the fitness value of all possible actions
Vmat[,,,,,]=rep(-1, length(Vmat))
Cmat<- array(dim=c(Yindexmax, Lmax, phi, Tmax-1, length(u), length(r) ))
Fmat<- array(dim=c(Yindexmax, Lmax, phi, Tmax-1, length(u), length(r) ))
#store optimal behaviors
optU=array(dim=c(Yindexmax, Lmax, phi, Tmax-1))
optR=array(dim=c(Yindexmax, Lmax, phi, Tmax-1))


#the dynamic loop:
i <- Tmax-1	
for (i in (Tmax-1):1) { #where i is time (age) in months
Y <- 1
 for (Y in 1:Yindexmax) { #for all values of Energy Stores in loop (unJdensityd)
  
  #setTxtProgressBar(pb, Y) #progress bar tracks which value of Y we are on
  L <- Lmin
   for (L in Lmin:Lmax) { #for all possible values of Length
    p <- 1
   for (p in 1:phi) { #for every temp environment
     			spawning <- "NO"
    		month = ((i-1) %% 12) + 1 #for the seasonality convert time in months to specific season-month (1-12)
			 if (raiseT[month] > 0 | seasons == "NO") spawning <- "YES"
			          ############################
          #calculate the critical stored energy needed for this length to be viable 
            Wstructure<-a*L^3 #  find structural mass in kg
            Estores <-   Jdensity*a*Lmax^3*(Y^3)*storemax/(Yindexmax^3)
            EcritL <- Wstructure*storemin*Jdensity
            EstoresmaxL <-Wstructure*storemax*Jdensity #following Chapman et al
            
            if (Estores > EstoresmaxL)  Estores <- EstoresmaxL #stored energy (Estores) capped at the maximum allowed (60% of the structural body mass in J) 
	       
	        #two loops over allocation strategies:
	        g <- 1
	        for (g in 1:length(u)) { #fractional placeholder (placeholder variable so we can loop over non-integers)
	            growth = u[g] #Convert integer loop index to fractional value for allocation     
	            	      	       
	      
	        for(h in 1:length(r)) { #where r is fractional allocation to reproductive effort	
	            
	             if (spawning == "YES") {
 
	           reprod = r[h]  #Convert integer loop index to fractional value for allocation
	               } else 	reprod = 0 
	              	           
	           #check if this combination of allocation is viable, and this number of stores is adequate for this length: 
		          if (growth + reprod > 1 )   Vmat[Y,L, p, i, g, h]	<- 0 else 
		            { #given the above conditions are met calculate all states:
		               
		                Wstores<-Estores/Jdensity  #stored mass in kG
		                Wtotal <- Wstores+Wstructure  #body mass in KG
		                Estructure <- Wstructure*Jdensity #body structure in Joules
		                Rlimit <- Estructure*reprolimit #rlimit in Joules
		              
		                #future state calculations		              
		                EstoresP <- Estores*(1-reprod-growth) +Income[month, L]*Jdensity - MTcosts[month, L] #combines mass-dependent food intake and mass-dependent metabolic costs
		               
		                EstructureP <- Estructure + growth*Estores
		                #WstructureP <- Wstructure + growth*Estores/Jdensity 
		                LengthP <- (L^3 + growth*(Estores /(a*Jdensity)) )^(1/3)
		                EstoresmaxLP <- EstructureP*storemax 
		                EstoresP <- min(EstoresP, EstoresmaxLP) # this statement caps stores max storage allowed for that size
		                EstoresP <- max(EstoresP, 0)	 #if future stored energy is negative, it is cut off at zero.   
		              
		              
		              ####Interpolation of future fitness between non-integer values of stored energy
		                YP <- ((EstoresP*Yindexmax^3)/((Jdensity*a*Lmax^3)*storemax))^(1/3) #convert future stores to a future state index on the Y scale
		                YP_floor <- floor(YP)
		                 dx <- YP - YP_floor
		               
		                
		              if (ceiling(LengthP) > Lmax) Lindex <- Lmax else
		                Lindex <- ceiling(LengthP)
		             
		              if(YP_floor >= Yindexmax )
		              {
		                  FutureFitness <- MaxF[Yindexmax,Lindex,p,i+1] } else {
		              	    	  FutureFitness <- dx*MaxF[YP_floor+1, Lindex, p, i+1]
		              	    	  
		              	  if(YP_floor > 0) FutureFitness <- FutureFitness + (1-dx)*MaxF[YP_floor,Lindex,p,i+1] 
		              } #end else if
		               
		                 if (reprod*Estores > Rlimit) {
		                    RealizedR <- Rlimit } else {
		                        RealizedR <- reprod*Estores
		                    } 
		                Ecrit_survival <- 1 /(1 + exp(- q*(Estores - EcritL))) 
		                Rlimit_smooth <-  (reprod * Estores)  / (1 + ((reprod*Estores) / Rlimit)) 
		                
		                Vmat[Y,L, p, i, g, h]	<-  (Rlimit_smooth  + exp(-mu[L])*FutureFitness )*Ecrit_survival
		                #Cmat[Y,L, p, i, g, h] <- Rlimit_smooth*Ecrit_survival
		                #Fmat[Y,L, p, i, g, h] <- exp(-mu[L])*FutureFitness*Ecrit_survival
		                     
            			 
	            		  } #end else if growth + reprod < 1 and EcritL < Estores	  		
	         		 } #end h loop
	      			 } #end g loop
	        
        #find and store the highest fitness from growth and reproduction combinations given the other values for length, stored energy, and age
        MaxF[Y,L,p,i] <-  max(Vmat[Y,L, p, i, , ])
        
        if(MaxF[Y,L,p,i] == 0)
        {
          optU[Y, L,p,i]=-1
          optR[Y, L,p,i]=0
        } else {
        #find out the differences in fitness of all strategies from the maximum, 
        #use the mean if a tie, record the difference for later error introduction:
        
        nStrats = length(u) * length(r)
        cursor = 0
        mult_u <- rep(NA, nStrats)
        mult_r <- rep(NA, nStrats)
        
        for (g in 1:length(u)) {
          for(h in 1:length(r)) {
             
            if (MaxF[Y, L,p,i] == Vmat[Y,L, p, i, g, h]) {
              cursor = cursor + 1
              mult_u[cursor] <- u[g]	   #record all behaviors that have the same fitness as the max.         
              mult_r[cursor] <- r[h]
              
              
            }  #end if
            
          } # end 2nd h loop  
        } #end 2nd g loop
        
        eqGrowth = mult_u[1:cursor]
        eqRepro = mult_r[1:cursor]
        smallestRepro = min(eqRepro)
        largestGrowth = max(eqGrowth[eqRepro == smallestRepro])
        
        #take mean of all behaviors with same fitness as max
        
        optU[Y, L,p,i]=largestGrowth
        optR[Y, L,p,i]=smallestRepro
        }
        
        
        
    
    }	#end p loop
  } #end L loop
  
} #end Y loop

} #end i loop
 
 
###Plotting code to make plots like that in main text Figure 3
 dev.new(height = 8, width = 8, unit="in")
# par(mfrow=c(3,2))
# gcol=     c("orange", two.colors(n=10, start="white", middle="gray", end="black"))
#      
# image.plot(1:Lmax, 1:Yindexmax, t(optU[,, 1,1]),     ylab="State", xlab="Length", main="growth")       
# 
# image.plot(1:Lmax, 1:Yindexmax, t(optR[,, 1,1]),     ylab="State", xlab="Length", main="reproduction")       
# 
# image.plot(1:Lmax, 1:Yindexmax, t(optU[,, 1,(Tmax-20)]),      ylab="State", xlab="Length", main="growth Tmax-20")       
# 
# image.plot(1:Lmax, 1:Yindexmax, t(optR[,, 1,(Tmax-20)]),    ylab="State", xlab="Length", main="reproduction Tmax-20")       
# 
# image.plot(1:Lmax, 1:Yindexmax, t(optU[,, 1,(Tmax-1)]),     ylab="State", xlab="Length", main="growth")       
# 
# image.plot(1:Lmax, 1:Yindexmax, t(optR[,, 1,(Tmax-1)]),     ylab="State", xlab="Length", main="reproduction")        
# 
# 
# 
image.plot( 1:(Tmax-1), 1:Yindexmax, t(optU[,100, 1,]),     ylab="State", xlab="Age (months)", main="growth for L = 100 cm")       

image.plot(  1:(Tmax-1), 1:Yindexmax, t(optR[,100, 1,]),     ylab="State", xlab="Age (months)", main="reproduction for L = 100 cm")        



image.plot( 1:(Tmax-1), 1:Yindexmax, t(optU[,200, 1,]),     ylab="State", xlab="Age (months)", main="growth for L = 200 cm")       

image.plot(  1:(Tmax-1), 1:Yindexmax, t(optR[,200, 1,]),     ylab="State", xlab="Age (months)", main="reproduction for L = 200 cm")        





##Forward simulation
 
set.seed(2001)
 
nindiv=2  
 
Ngroups=1
group=1

initialsize <- rep(5,  2) #as.integer(rnorm(nindiv, mean=50, sd=2.5))
alive=matrix(ncol = Tmax, nrow= Ngroups, data=0)

idist=matrix(data=NA, nrow=nindiv, ncol=Tmax) #keeps track of energetic state over time
sizedist=matrix(data=NA, nrow=nindiv, ncol=Tmax)
g_allo= array(dim=c(nindiv, Tmax), data = 0 )
repro= array(dim=c(nindiv, Tmax), data = 0 ) 
income=array(dim=c(nindiv, Tmax), data = 0 )
#these will give storage fraction and reproduction for each individual, given its two states at each time
 
z<-  Jdensity*a*Lmax^3/(Yindexmax^3) #fixed initial state
 
idist[,1]=ceiling(z) #this rounds every z up to the nearest integer for the first time step  
 sizedist[,1]<- initialsize   
#stores number of survivors at each time

reproduction=matrix(0, nindiv, Tmax) #stores how much they reproduce at each time. 

#draw random numbers for every individual's survival chance at every time (compare randroaw to exp(-mu))  
randraw=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
randraw2=matrix(runif(nindiv*(Tmax), max=1, min=0), nrow=nindiv, ncol=Tmax)
normdraw=matrix(rnorm(nindiv*(Tmax), mean=1, sd=0.005), nrow=nindiv, ncol=Tmax) #add stochasticity in food intake

survival=rep(0, Tmax)
survival[1]<-1

	for (i in 1:(Tmax-1)) { 
	   month = ((i-1) %% 12) + 1 
	   state  <- idist[,i] #in Joules
	   size <- round(sizedist[,i])  #in cm
	   EcritL <-  Jdensity*a*size^3*storemin   #find the critical value of state, in joules, neede dfor a fish this size
	   index <- which(state > EcritL) #which individuals are still alive (didn't starve)
	  
	   
	    #now calculate Wtotal, Costs, and Net energy intake
	   Wstructure<- a*size[index]^3 #structural mass in kilograms 
	   Estructure <- Wstructure*Jdensity #structural energy in joules
	     Replim <- reprolimit*Estructure 
	   EstoresmaxL <-Estructure*storemax #find the maximum jules of energy that can be stored for fish of this length  
	   
	   state[index] <- ifelse(state[index] > EstoresmaxL, EstoresmaxL, state[index]) #stored energy capped at a certain body size
	   
	   Wstores<-state[index]/Jdensity
	   
	   Wtotal <-  Wstores+Wstructure   #body mass
	   
	       if(length(index) > 1) {
	    
		    ##find interpolated behaviors
	         #first convert state to the Y index value it corresponds to and find the lower integer (floor)
		    Estores = ((state[index]*Yindexmax^3)/((Jdensity*a*Lmax^3)*storemax))^(1/3)
	         Ilo <- floor(Estores)
		    
		    
		    #make sure it's not the maximum value that Y index can be
		    Ilo <- ifelse(Ilo >= Yindexmax, Yindexmax-1, Ilo)   
		    
		    # then assuming it's not at the maximum, find the remainder and store it
		    dx <- ifelse(Ilo >= Yindexmax, 1, Estores - Ilo) 
		    
		    ##LOOK UP THE OPTIMAL BEHAVIORS FROM THE BACKWARD SOLUTION
		    #We use the interpotion of state to find the allocation strategies  regardless of whether they survive  (since they allocate first) 
		    
		    #create and index of low-state individuals where the Yindex value is between 0 and 1)
		    condind <- Ilo == 0
		    
		    ##   if state is between 0 and 1 (Ilo = 0)the strategy is only dx*(state=1)
		    g_allo[index[condind==TRUE], i] <- round((dx[condind==TRUE])*diag(optU[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1)  
		    repro[index[condind==TRUE], i] <- round((dx[condind==TRUE])*diag(optR[Ilo[condind==TRUE]+1, size[index[condind==TRUE]], p, i]),1) 
		    
		    ##if state is greater than 1 then we add (1-dx) by the lower state
		    g_allo[index[condind==FALSE], i] <- round((1-dx[condind==FALSE])*diag(optU[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) + 
		                                                (dx[condind==FALSE])*diag(optU[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1)  
		    repro[index[condind==FALSE], i] <-round((1-dx[condind==FALSE])*diag(optR[Ilo[condind==FALSE], size[index[condind==FALSE]], p, i]) +  
		                                              (dx[condind==FALSE])*diag(optR[Ilo[condind==FALSE]+1, size[index[condind==FALSE]], p, i]),1	)	  					  
	    
		
		    
		    # REPRODUCE AND GROW BEFORE SURVIAL IS DETERMINED    
		    
		    reproduction[index, i]<-  (repro[index, i] * state[index])  / (1 + ( repro[index, i]*state[index]  /  Replim ) )
		    
		    nextsize <-   (size[index]^3 +  g_allo[index,i]*(state[index] /(a*Jdensity)) )^(1/3)
		    
		     
		    #########################
		    survival[i+1] <- survival[i]*exp(-mu[size[index[1]]]) #this stores the cumulative probability an individual survives to age i+1
		     
		    #future state calculation:
		    idist[index,i+1] <- (1-repro[index, i]-g_allo[index,i])*state[index] + Income[month, size[index]]*Jdensity - MTcosts[month, size[index]] 
		    
		    sizedist[index, i+1] <- nextsize
		    
		       
	 	    } #end if
		}     #end time (i) loop

###write output files
#Convert state from joules to kilos if needed

idist[, -Tmax]<-ifelse(idist[, -Tmax]>0,  idist[, -Tmax], NA)
write.csv(idist, file=paste0(filepath,"03State", "Temp", Temp, "f_h", round(f_h, 2), "phi_p", phi_p, "Kappa", round(Kappa,2), "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))
write.csv(sizedist, file=paste0(filepath,"01Length","Temp", Temp, "f_h", round(f_h, 2),  "phi_p", phi_p, "Kappa", round(Kappa,2),  "reprolimit", reprolimit, "Tmax", Tmax, ".csv"))
write.csv(reproduction, file=paste0(filepath,"02Repro", "Temp", Temp, "f_h", round(f_h, 2),  "phi_p", phi_p, "Kappa", round(Kappa,2),  "reprolimit", reprolimit, "Tmax", Tmax, ".csv")) 
write.csv(survival, file=paste0(filepath,"04Surv", "Temp", Temp, "f_h", round(f_h, 2), "phi_p", phi_p,  "Kappa", round(Kappa,2),   "reprolimit", reprolimit, "Tmax", Tmax, ".csv")) 

