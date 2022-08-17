
alpha <- 0.005
 
w <- 1:2500
p1 = 0.1 
p2 = 0.3 

a1 =  0.01
a0 = 500

focal_w <- 550
Kappa=2000000 

ss <- (a1*w[1]^a1*w^(-a1-1)  ) * Kappa

 pdf("~/Dropbox/Manuscripts in Progress/Life history of tunas/fig1.pdf")
plot(ss, type= "l", lwd = 2, xlab = "Body mass (kg)", ylab = "Relative abundance or biomass", las = 1, ylim=c(0, a0))
lines(x=c(focal_w, focal_w), y = c(0, ss[focal_w]), lwd = 3)
polygon(x=c(focal_w*p1, (focal_w*p1):(focal_w*p2), focal_w*p2), y= c(0, ss[(focal_w*p1):(focal_w*p2)], 0), col="light gray")

polygon(x=c(focal_w/p2, (focal_w/p2):(focal_w/p1), focal_w/p1), y= c(0, ss[(focal_w/p2):(focal_w/p1)], 0), col="dark gray")


text(x=c(focal_w*(p1+0.7*p1),focal_w*(p2+0.7*p1), (focal_w/p2)+0.7*p1, (focal_w/p1)+0.7*p1), y=c(ss[focal_w*p1]+0.1*ss[focal_w*p1], ss[focal_w*p2]+0.2*ss[focal_w*p2], ss[focal_w/p2]+15, ss[focal_w/p1]+15), labels = c("P1*w", "P2*w", "w/P2", "w/P1"), cex=1)


dev.off()
plot(ss, type= "l", lwd = 2, xlab = "Body mass (g)", ylab = "Relative abundance or biomass", las = 1, ylim=c(0, a0))
lines(x=c(focal_w, focal_w), y = c(0, ss[focal_w]), lwd = 3)
polygon(x=c(focal_w*p1, (focal_w*p1):(focal_w*p2), focal_w*p2), y= c(0, ss[(focal_w*p1):(focal_w*p2)], 0), col="light gray")

polygon(x=c(focal_w/p2, (focal_w/p2):(focal_w/p1), focal_w/p1), y= c(0, ss[(focal_w/p2):(focal_w/p1)], 0), col="dark gray")


#text(x=c(focal_w*(p1+0.7*p1),focal_w*(p2+0.7*p1), (focal_w/p2)+0.7*p1, (focal_w/p1)+0.7*p1), y=c(ss[focal_w*p1]+0.1*ss[focal_w*p1], ss[focal_w*p2]+0.2*ss[focal_w*p2], ss[focal_w/p2]+15, ss[focal_w/p1]+15), labels = c("P1*w", "P2*w", "w/P2", "w/P1"), cex=1)

