# Patterns of Democracy in the USA
# April 4, 2018 

# Julian Bernauer 

# main model 

library(foreign)
library(R2jags)
library(rjags)

setwd("F:/Bern post/Patterns USA Paper")

load("USpatterns.Rdata")

N <- 50

attach(USmeansfull)

us.data <- list(N=N,par=par,dom=dom,g=g,jud=jud,dir=dir,pal=pal)


us.model <- "model{

for(i in 1:N){

g[i] ~ dnorm(mu.g[i],tau.g)
mu.g[i] <- alpha.g + gamma.g2*cx2[i] 

par[i] ~ dnorm(mu.par[i],tau.par)
mu.par[i] <- alpha.par + gamma.par2*cx2[i] 

dom[i] ~ dnorm(mu.dom[i],tau.dom)
mu.dom[i] <- alpha.dom + gamma.dom2*cx1[i] 

pal[i] ~ dnorm(mu.pal[i],tau.pal)
mu.pal[i] <- alpha.pal + gamma.pal2*cx1[i]

jud[i] ~ dnorm(mu.jud[i],tau.jud)
mu.jud[i] <- alpha.jud + gamma.jud1*cx1[i] 

dir[i] ~ dnorm(mu.dir[i],tau.dir)
mu.dir[i] <- alpha.dir + gamma.dir1*cx1[i] 

}

tau.g <- pow(sigma.g, -2)
sigma.g ~ dunif(0, 50)
tau.par <- pow(sigma.par, -2)
sigma.par ~ dunif(0, 50)
tau.dom <- pow(sigma.dom, -2)
sigma.dom ~ dunif(0, 50)
tau.pal <- pow(sigma.pal, -2)
sigma.pal ~ dunif(0, 50)
tau.jud <- pow(sigma.jud, -2)
sigma.jud ~ dunif(0, 50)
tau.dir <- pow(sigma.dir, -2)
sigma.dir ~ dunif(0, 50)

alpha.g ~ dnorm(0, .001)
alpha.par ~ dnorm(0, .001)
alpha.dom ~ dnorm(0, .001)
alpha.pal ~ dnorm(0, .001)
alpha.jud ~ dnorm(0, .001)
alpha.dir ~ dnorm(0, .001)

gamma.g2 ~ dnorm(0, .001) 
gamma.par2 ~ dnorm(0, .001) I(0,)
gamma.dom2 ~ dnorm(0, .001) 
gamma.pal2 ~ dnorm(0, .001) 
gamma.rig1 ~ dnorm(0, .001) 
gamma.jud1 ~ dnorm(0, .001) 
gamma.dir1 ~ dnorm(0, .001) I(0,)

for(k in 1:36){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)  
}

for(k in 38:50){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)  
}

# state restrictions for identification (Oregon)
cx1[37] ~ dnorm(0,1) I(0,)
cx2[37] ~ dnorm(0,1) I(0,)

}"

#JAGS language
write(us.model, file="us.model.jags")

us.parameters <- c("sigma.g", "sigma.par", "sigma.dom", "sigma.pal", 
                   "sigma.jud", "sigma.dir", "alpha.g", "alpha.par", "alpha.dom", 
                   "alpha.jud", "alpha.dir", "gamma.g2", "gamma.par2","gamma.dom2", "gamma.pal2", 
                   "gamma.jud1", "gamma.dir1",  
                   "cx1", "cx2")

jags.us <- jags.model(file="us.model.jags", data = us.data, n.chains = 3, n.adapt = 100)
sampleshelp <- coda.samples(jags.us, us.parameters, n.iter=100, thin=1)
samplesburn <- coda.samples(jags.us, us.parameters, n.iter=9800, thin=98)
samples <- coda.samples(jags.us, us.parameters, n.iter=10000, thin=100)

# plot(sampleshelp, ask=TRUE)
# plot(samples, ask=TRUE)


# parameters  
kette <- as.matrix(samples)

gamma.jud1 <- kette[,"gamma.jud1"]
mgj1 <- mean(gamma.jud1)
sgj1 <- sd(gamma.jud1)
gamma.dir1 <- kette[,"gamma.dir1"]
mgd1 <- mean(gamma.dir1)
sgd1 <- sd(gamma.dir1)
gamma.g2 <- kette[,"gamma.g2"]
mgg2 <- mean(gamma.g2)
sgg2 <- sd(gamma.g2)
gamma.par2 <- kette[,"gamma.par2"]
mgp2 <- mean(gamma.par2)
sgp2 <- sd(gamma.par2)
gamma.dom2 <- kette[,"gamma.dom2"]
mgdo2 <- mean(gamma.dom2)
sgdo2 <- sd(gamma.dom2)
gamma.pal2 <- kette[,"gamma.pal2"]
mgpa2 <- mean(gamma.pal2)
sgpa2 <- sd(gamma.pal2)

mcx1c_raw <- c(mgj1,mgd1,mgg2,mgp2,mgdo2,mgpa2)
scx1c_raw <- c(sgj1,sgd1,sgg2,sgp2,sgdo2,sgpa2)

mcx1c <- c(((mgj1/sd(jud, na.rm=TRUE))*3.29),((mgd1/sd(dir, na.rm=TRUE))*3.29),((mgdo2/sd(dom, na.rm=TRUE))*3.29),((mgpa2/sd(pal, na.rm=TRUE))*3.29),((mgg2/sd(g, na.rm=TRUE))*3.29),((mgp2/sd(par, na.rm=TRUE))*3.29))

scx1c <- c(((sgj1/sd(jud, na.rm=TRUE))*3.29),((sgd1/sd(dir, na.rm=TRUE))*3.29),((sgdo2/sd(dom, na.rm=TRUE))*3.29),((sgpa2/sd(pal, na.rm=TRUE))*3.29),((sgg2/sd(g, na.rm=TRUE))*3.29),((sgp2/sd(par, na.rm=TRUE))*3.29))

clabels <- c("Judicial review", "Direct democracy", "Executive dominance","Power of legislature","Electoral disproport.", "Number of parties")

var.names <- c(clabels)

m.v <- mcx1c
sd.v <- scx1c

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-4,4, by = 2), label = seq(-4,4, by = 2), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.95)*sd.v, y.axis -.1, m.v-qnorm(.95)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.95)*sd.v, y.axis -.1, m.v+qnorm(.95)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .7 
segments(left.side,6,left.side,3) 
segments(left.side,6,left.side+.1,6) 
segments(left.side,3,left.side+.1,3)
text(.5, 4.5, "Executives-Veto", srt = 90, cex=.8)
segments(left.side,2,left.side,1) 
segments(left.side,2,left.side+.1,2) 
segments(left.side,1,left.side+.1,1)
text(.5, 1.5, "Parties-Elections", srt = 90, cex=.8)


###
#Plot Veto-Dimension
cx11 <- kette[,"cx1[1]"]
cx12 <- kette[,"cx1[2]"]
cx13 <- kette[,"cx1[3]"]
cx14 <- kette[,"cx1[4]"]
cx15 <- kette[,"cx1[5]"]
cx16 <- kette[,"cx1[6]"]
cx17 <- kette[,"cx1[7]"]
cx18 <- kette[,"cx1[8]"]
cx19 <- kette[,"cx1[9]"]
cx110 <- kette[,"cx1[10]"]
cx111 <- kette[,"cx1[11]"]
cx112 <- kette[,"cx1[12]"]
cx113 <- kette[,"cx1[13]"]
cx114 <- kette[,"cx1[14]"]
cx115 <- kette[,"cx1[15]"]
cx116 <- kette[,"cx1[16]"]
cx117 <- kette[,"cx1[17]"]
cx118 <- kette[,"cx1[18]"]
cx119 <- kette[,"cx1[19]"]
cx120 <- kette[,"cx1[20]"]
cx121 <- kette[,"cx1[21]"]
cx122 <- kette[,"cx1[22]"]
cx123 <- kette[,"cx1[23]"]
cx124 <- kette[,"cx1[24]"]
cx125 <- kette[,"cx1[25]"]
cx126 <- kette[,"cx1[26]"]
cx127 <- kette[,"cx1[27]"]
cx128 <- kette[,"cx1[28]"]
cx129 <- kette[,"cx1[29]"]
cx130 <- kette[,"cx1[30]"]
cx131 <- kette[,"cx1[31]"]
cx132 <- kette[,"cx1[32]"]
cx133 <- kette[,"cx1[33]"]
cx134 <- kette[,"cx1[34]"]
cx135 <- kette[,"cx1[35]"]
cx136 <- kette[,"cx1[36]"]
cx137 <- kette[,"cx1[37]"]
cx138 <- kette[,"cx1[38]"]
cx139 <- kette[,"cx1[39]"]
cx140 <- kette[,"cx1[40]"]
cx141 <- kette[,"cx1[41]"]
cx142 <- kette[,"cx1[42]"]
cx143 <- kette[,"cx1[43]"]
cx144 <- kette[,"cx1[44]"]
cx145 <- kette[,"cx1[45]"]
cx146 <- kette[,"cx1[46]"]
cx147 <- kette[,"cx1[47]"]
cx148 <- kette[,"cx1[48]"]
cx149 <- kette[,"cx1[49]"]
cx150 <- kette[,"cx1[50]"]

mcx11 <- mean(cx11)
mcx12 <- mean(cx12)
mcx13 <- mean(cx13)
mcx14 <- mean(cx14)
mcx15 <- mean(cx15)
mcx16 <- mean(cx16)
mcx17 <- mean(cx17)
mcx18 <- mean(cx18)
mcx19 <- mean(cx19)
mcx110 <- mean(cx110)
mcx111 <- mean(cx111)
mcx112 <- mean(cx112)
mcx113 <- mean(cx113)
mcx114 <- mean(cx114)
mcx115 <- mean(cx115)
mcx116 <- mean(cx116)
mcx117 <- mean(cx117)
mcx118 <- mean(cx118)
mcx119 <- mean(cx119)
mcx120 <- mean(cx120)
mcx121 <- mean(cx121)
mcx122 <- mean(cx122)
mcx123 <- mean(cx123)
mcx124 <- mean(cx124)
mcx125 <- mean(cx125)
mcx126 <- mean(cx126)
mcx127 <- mean(cx127)
mcx128 <- mean(cx128)
mcx129 <- mean(cx129)
mcx130 <- mean(cx130)
mcx131 <- mean(cx131)
mcx132 <- mean(cx132)
mcx133 <- mean(cx133)
mcx134 <- mean(cx134)
mcx135 <- mean(cx135)
mcx136 <- mean(cx136)
mcx137 <- mean(cx137)
mcx138 <- mean(cx138)
mcx139 <- mean(cx139)
mcx140 <- mean(cx140)
mcx141 <- mean(cx141)
mcx142 <- mean(cx142)
mcx143 <- mean(cx143)
mcx144 <- mean(cx144)
mcx145 <- mean(cx145)
mcx146 <- mean(cx146)
mcx147 <- mean(cx147)
mcx148 <- mean(cx148)
mcx149 <- mean(cx149)
mcx150 <- mean(cx150)

scx11 <- sd(cx11)
scx12 <- sd(cx12)
scx13 <- sd(cx13)
scx14 <- sd(cx14)
scx15 <- sd(cx15)
scx16 <- sd(cx16)
scx17 <- sd(cx17)
scx18 <- sd(cx18)
scx19 <- sd(cx19)
scx110 <- sd(cx110)
scx111 <- sd(cx111)
scx112 <- sd(cx112)
scx113 <- sd(cx113)
scx114 <- sd(cx114)
scx115 <- sd(cx115)
scx116 <- sd(cx116)
scx117 <- sd(cx117)
scx118 <- sd(cx118)
scx119 <- sd(cx119)
scx120 <- sd(cx120)
scx121 <- sd(cx121)
scx122 <- sd(cx122)
scx123 <- sd(cx123)
scx124 <- sd(cx124)
scx125 <- sd(cx125)
scx126 <- sd(cx126)
scx127 <- sd(cx127)
scx128 <- sd(cx128)
scx129 <- sd(cx129)
scx130 <- sd(cx130)
scx131 <- sd(cx131)
scx132 <- sd(cx132)
scx133 <- sd(cx133)
scx134 <- sd(cx134)
scx135 <- sd(cx135)
scx136 <- sd(cx136)
scx137 <- sd(cx137)
scx138 <- sd(cx138)
scx139 <- sd(cx139)
scx140 <- sd(cx140)
scx141 <- sd(cx141)
scx142 <- sd(cx142)
scx143 <- sd(cx143)
scx144 <- sd(cx144)
scx145 <- sd(cx145)
scx146 <- sd(cx146)
scx147 <- sd(cx147)
scx148 <- sd(cx148)
scx149 <- sd(cx149)
scx150 <- sd(cx150)

mcx1 <- c(mcx11, mcx12, mcx13, mcx14, mcx15, mcx16, mcx17, mcx18, mcx19, mcx110, mcx111, mcx112, mcx113, mcx114, mcx115, mcx116, mcx117, mcx118, mcx119, mcx120, mcx121, mcx122, mcx123, mcx124, mcx125, mcx126, mcx127, mcx128, mcx129, mcx130, mcx131, mcx132, mcx133, mcx134, mcx135, mcx136, mcx137, mcx138, mcx139, mcx140, mcx141, mcx142, mcx143, mcx144, mcx145, mcx146, mcx147, mcx148, mcx149, mcx150)

scx1 <- c(scx11, scx12, scx13, scx14, scx15, scx16, scx17, scx18, scx19, scx110, scx111, scx112, scx113, scx114, scx115, scx116, scx117, scx118, scx119, scx120, scx121, scx122, scx123, scx124, scx125, scx126, scx127, scx128, scx129, scx130, scx131, scx132, scx133, scx134, scx135, scx136, scx137, scx138, scx139, scx140, scx141, scx142, scx143, scx144, scx145, scx146, scx147, scx148, scx149, scx150)

clabels <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

var.names <- c(clabels)

m.v <- mcx1
sd.v <- scx1

#sort
pic <- data.frame(var.names,m.v,sd.v)
pic.sort <- pic[order(m.v) , ]
pic.sort

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-4,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-4,4, by = 1), label = seq(-4,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.7)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$m.v-qnorm(.975)*pic.sort$sd.v, y.axis, pic.sort$m.v+qnorm(.975)*pic.sort$sd.v, y.axis, lwd =  1.5)
#abline(v=0, lty = 2)
segments(pic.sort$m.v-qnorm(.9)*pic.sort$sd.v, y.axis -.1, pic.sort$m.v-qnorm(.9)*pic.sort$sd.v, y.axis +.1, lwd = 1.5) 
segments(pic.sort$m.v+qnorm(.9)*pic.sort$sd.v, y.axis -.1, pic.sort$m.v+qnorm(.9)*pic.sort$sd.v, y.axis +.1, lwd = 1.5)


par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .9 
segments(left.side,50,left.side,1) 
segments(left.side,50,left.side+.1,50) 
segments(left.side,1,left.side+.1,1)
text(.5, 25, "Executives-Veto Power Diffusion in the USA (Posterior Means)", srt = 90, cex=.8)


##
#Plot exepar
cx21 <- kette[,"cx2[1]"]
cx22 <- kette[,"cx2[2]"]
cx23 <- kette[,"cx2[3]"]
cx24 <- kette[,"cx2[4]"]
cx25 <- kette[,"cx2[5]"]
cx26 <- kette[,"cx2[6]"]
cx27 <- kette[,"cx2[7]"]
cx28 <- kette[,"cx2[8]"]
cx29 <- kette[,"cx2[9]"]
cx210 <- kette[,"cx2[10]"]
cx211 <- kette[,"cx2[11]"]
cx212 <- kette[,"cx2[12]"]
cx213 <- kette[,"cx2[13]"]
cx214 <- kette[,"cx2[14]"]
cx215 <- kette[,"cx2[15]"]
cx216 <- kette[,"cx2[16]"]
cx217 <- kette[,"cx2[17]"]
cx218 <- kette[,"cx2[18]"]
cx219 <- kette[,"cx2[19]"]
cx220 <- kette[,"cx2[20]"]
cx221 <- kette[,"cx2[21]"]
cx222 <- kette[,"cx2[22]"]
cx223 <- kette[,"cx2[23]"]
cx224 <- kette[,"cx2[24]"]
cx225 <- kette[,"cx2[25]"]
cx226 <- kette[,"cx2[26]"]
cx227 <- kette[,"cx2[27]"]
cx228 <- kette[,"cx2[28]"]
cx229 <- kette[,"cx2[29]"]
cx230 <- kette[,"cx2[30]"]
cx231 <- kette[,"cx2[31]"]
cx232 <- kette[,"cx2[32]"]
cx233 <- kette[,"cx2[33]"]
cx234 <- kette[,"cx2[34]"]
cx235 <- kette[,"cx2[35]"]
cx236 <- kette[,"cx2[36]"]
cx237 <- kette[,"cx2[37]"]
cx238 <- kette[,"cx2[38]"]
cx239 <- kette[,"cx2[39]"]
cx240 <- kette[,"cx2[40]"]
cx241 <- kette[,"cx2[41]"]
cx242 <- kette[,"cx2[42]"]
cx243 <- kette[,"cx2[43]"]
cx244 <- kette[,"cx2[44]"]
cx245 <- kette[,"cx2[45]"]
cx246 <- kette[,"cx2[46]"]
cx247 <- kette[,"cx2[47]"]
cx248 <- kette[,"cx2[48]"]
cx249 <- kette[,"cx2[49]"]
cx250 <- kette[,"cx2[50]"]

mcx21 <- mean(cx21)
mcx22 <- mean(cx22)
mcx23 <- mean(cx23)
mcx24 <- mean(cx24)
mcx25 <- mean(cx25)
mcx26 <- mean(cx26)
mcx27 <- mean(cx27)
mcx28 <- mean(cx28)
mcx29 <- mean(cx29)
mcx210 <- mean(cx210)
mcx211 <- mean(cx211)
mcx212 <- mean(cx212)
mcx213 <- mean(cx213)
mcx214 <- mean(cx214)
mcx215 <- mean(cx215)
mcx216 <- mean(cx216)
mcx217 <- mean(cx217)
mcx218 <- mean(cx218)
mcx219 <- mean(cx219)
mcx220 <- mean(cx220)
mcx221 <- mean(cx221)
mcx222 <- mean(cx222)
mcx223 <- mean(cx223)
mcx224 <- mean(cx224)
mcx225 <- mean(cx225)
mcx226 <- mean(cx226)
mcx227 <- mean(cx227)
mcx228 <- mean(cx228)
mcx229 <- mean(cx229)
mcx230 <- mean(cx230)
mcx231 <- mean(cx231)
mcx232 <- mean(cx232)
mcx233 <- mean(cx233)
mcx234 <- mean(cx234)
mcx235 <- mean(cx235)
mcx236 <- mean(cx236)
mcx237 <- mean(cx237)
mcx238 <- mean(cx238)
mcx239 <- mean(cx239)
mcx240 <- mean(cx240)
mcx241 <- mean(cx241)
mcx242 <- mean(cx242)
mcx243 <- mean(cx243)
mcx244 <- mean(cx244)
mcx245 <- mean(cx245)
mcx246 <- mean(cx246)
mcx247 <- mean(cx247)
mcx248 <- mean(cx248)
mcx249 <- mean(cx249)
mcx250 <- mean(cx250)

scx21 <- sd(cx21)
scx22 <- sd(cx22)
scx23 <- sd(cx23)
scx24 <- sd(cx24)
scx25 <- sd(cx25)
scx26 <- sd(cx26)
scx27 <- sd(cx27)
scx28 <- sd(cx28)
scx29 <- sd(cx29)
scx210 <- sd(cx210)
scx211 <- sd(cx211)
scx212 <- sd(cx212)
scx213 <- sd(cx213)
scx214 <- sd(cx214)
scx215 <- sd(cx215)
scx216 <- sd(cx216)
scx217 <- sd(cx217)
scx218 <- sd(cx218)
scx219 <- sd(cx219)
scx220 <- sd(cx220)
scx221 <- sd(cx221)
scx222 <- sd(cx222)
scx223 <- sd(cx223)
scx224 <- sd(cx224)
scx225 <- sd(cx225)
scx226 <- sd(cx226)
scx227 <- sd(cx227)
scx228 <- sd(cx228)
scx229 <- sd(cx229)
scx230 <- sd(cx230)
scx231 <- sd(cx231)
scx232 <- sd(cx232)
scx233 <- sd(cx233)
scx234 <- sd(cx234)
scx235 <- sd(cx235)
scx236 <- sd(cx236)
scx237 <- sd(cx237)
scx238 <- sd(cx238)
scx239 <- sd(cx239)
scx240 <- sd(cx240)
scx241 <- sd(cx241)
scx242 <- sd(cx242)
scx243 <- sd(cx243)
scx244 <- sd(cx244)
scx245 <- sd(cx245)
scx246 <- sd(cx246)
scx247 <- sd(cx247)
scx248 <- sd(cx248)
scx249 <- sd(cx249)
scx250 <- sd(cx250)

mcx2 <- c(mcx21, mcx22, mcx23, mcx24, mcx25, mcx26, mcx27, mcx28, mcx29, mcx210, mcx211, mcx212, mcx213, mcx214, mcx215, mcx216, mcx217, mcx218, mcx219, mcx220, mcx221, mcx222, mcx223, mcx224, mcx225, mcx226, mcx227, mcx228, mcx229, mcx230, mcx231, mcx232, mcx233, mcx234, mcx235, mcx236, mcx237, mcx238, mcx239, mcx240, mcx241, mcx242, mcx243, mcx244, mcx245, mcx246, mcx247, mcx248, mcx249, mcx250)

scx2 <- c(scx21, scx22, scx23, scx24, scx25, scx26, scx27, scx28, scx29, scx210, scx211, scx212, scx213, scx214, scx215, scx216, scx217, scx218, scx219, scx220, scx221, scx222, scx223, scx224, scx225, scx226, scx227, scx228, scx229, scx230, scx231, scx232, scx233, scx234, scx235, scx236, scx237, scx238, scx239, scx240, scx241, scx242, scx243, scx244, scx245, scx246, scx247, scx248, scx249, scx250)

clabels <- c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming")

var.names <- c(clabels)

m.v <- mcx2
sd.v <- scx2

#sort
pic <- data.frame(var.names,m.v,sd.v)
pic.sort <- pic[order(m.v) , ]
pic.sort

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-6,4), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-6,4, by = 1), label = seq(-6,4, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.7)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(pic.sort$m.v-qnorm(.975)*pic.sort$sd.v, y.axis, pic.sort$m.v+qnorm(.975)*pic.sort$sd.v, y.axis, lwd =  1.5)
#abline(v=0, lty = 2)
segments(pic.sort$m.v-qnorm(.9)*pic.sort$sd.v, y.axis -.1, pic.sort$m.v-qnorm(.9)*pic.sort$sd.v, y.axis +.1, lwd = 1.5) 
segments(pic.sort$m.v+qnorm(.9)*pic.sort$sd.v, y.axis -.1, pic.sort$m.v+qnorm(.9)*pic.sort$sd.v, y.axis +.1, lwd = 1.5)


par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .9 
segments(left.side,50,left.side,1) 
segments(left.side,50,left.side+.1,50) 
segments(left.side,1,left.side+.1,1)
text(.5, 25, "Parties-Elections Power Diffusion in the USA (Posterior Means)", srt = 90, cex=.8)


# map 
plot(mcx2, mcx1, xlab="Parties-Elections", ylab="Executives-Veto", type="n", xlim=c(-2.5,1.5), ylim=c(-2,2))
text(jitter(mcx2, amount=.1), jitter(mcx1, amount=.1), short, cex=.8)
abline(v=0, lty = 2)
abline(h=0, lty = 2)

