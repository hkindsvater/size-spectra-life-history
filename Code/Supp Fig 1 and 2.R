Lmax <- 400  #maximum size of 4 meters
Lmin <- 1 
Yindexmax <-  #maximum index of stores in state loop  

Jdensity <-  4.2e+6 #
a <- 5e-5
q = 5e-5 
storemax= 0.6 #proportion of structural mass that individuals can devote to energy storage
storemin = 0.1
reprolimit<-0.2

L<-c(250)
 
      #calculate the critical stored energy needed for this length to be viable 
      Wstructure<-a*(L)^3 #  find structural mass in kg
      Estores <-   Jdensity*a*Lmax^3*((seq(1, Yindexmax, by=1))^3)*storemax/(Yindexmax^3)
      EcritL <- Wstructure*storemin*Jdensity
      Estructure <- Wstructure*Jdensity #body structure in Joules
      Rlimit <- Estructure*reprolimit #rlimit in Joules
      
      ProportionMaxR <- 1*Estores/storemax
      Ecrit_survival <- matrix(nrow=2, ncol = length(Estores))
    #   for(i in 1:2) {
    #   Ecrit_survival[i,] <- 1 /(1 + exp(-q*(Estores - EcritL[i]))) 
    #   }
    # quartz()  
    #   matplot(ProportionMaxR, t(Ecrit_survival), type="l", lwd=2, lty=1, xlim=c(3.4e+07, 3.6e+07), xlab = "60% of Lipid Stores (J)", ylab="Probability of survival")
    #   legend("bottomright", legend = c("Length = 100 cm", "Length = 250 cm"), lwd=2, col=c(1:2), bty="n" )
    # 
     # quartz()
      par(mfrow=c(2, 2))
       f=c(0.5, 1, 2)
         Rlimit_smooth <- matrix(nrow=3, ncol = length(Estores))
      
         #This is the fitness for a hypothetical allocation strategy of 1 (devote maximum stores to reproduction)
         for(i in 1:3) {
        Rlimit_smooth[i,] <-  (1 * Estores)  / (1 + f[i]*((1*Estores) / Rlimit)) 
        
           }
        
 
         matplot(Estores, t(Rlimit_smooth), type="l", lwd=2, lty=1, col=c(2, 1, 4),  xlab = "Lipid Stores (kg)",   ylab="Fitness from allocation in this time increment")
         legend("bottomright", legend = c("f = 0.5", "f = 1", "f = 2"), cex=0.75, lwd=2, col=c(2, 1, 4), bty="n" )
         #  