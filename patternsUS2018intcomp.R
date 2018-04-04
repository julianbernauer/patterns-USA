# Patterns of Democracy in the USA
# April 4, 2018 

# Julian Bernauer 

# international comparison

library(foreign)
library(R2jags)
library(rjags)

setwd("F:/Bern post/Patterns USA Paper")
# setwd("[...]")

load("USintdata.Rdata")


# model 

int.model <- "model{

for(i in 1:N){

par[i] ~ dnorm(mu.par[i],tau.par)
mu.par[i] <- alpha.par + gamma.par2*cx2[i] 

elec[i] ~ dnorm(mu.elec[i],tau.elec)
mu.elec[i] <- alpha.elec + gamma.elec2*cx2[i] 

dir[i] ~ dnorm(mu.dir[i],tau.dir)
mu.dir[i] <- alpha.dir + gamma.dir3*cx1[i] 


}

tau.par <- pow(sigma.par, -2)
sigma.par ~ dunif(0, 50)
tau.elec <- pow(sigma.elec, -2)
sigma.elec ~ dunif(0, 50)
tau.dir <- pow(sigma.dir, -2)
sigma.dir ~ dunif(0, 50)

alpha.par ~ dnorm(0, .001)
alpha.elec ~ dnorm(0, .001)
alpha.dir ~ dnorm(0, .001)

gamma.par2 ~ dnorm(0, .001) I(0,)
gamma.elec2 ~ dnorm(0, .001) 
gamma.dir3 ~ dnorm(0, .001) I(0,)


for(k in 1:60){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)  
}

for(k in 62:111){
cx1[k] ~ dnorm(0,1)
cx2[k] ~ dnorm(0,1)  
}

# switzerland restricted to positive values 
cx1[61] ~ dnorm(0,1) I(0,)
cx2[61] ~ dnorm(0,1) I(0,)

}"

#JAGS language
write(int.model, file="int.model.jags")

int.parameters <- c("sigma.elec","sigma.par","sigma.dir","alpha.elec", "alpha.par",
                    "alpha.dir","gamma.elec2","gamma.par2","gamma.dir3","cx1","cx2")

jags.int <- jags.model(file="int.model.jags", data = int.data, n.chains = 3, n.adapt = 100)
sampleshelp <- coda.samples(jags.int, int.parameters, n.iter=100, thin=1)
samplesburn <- coda.samples(jags.int, int.parameters, n.iter=4800, thin=18)
samples <- coda.samples(jags.int, int.parameters, n.iter=5000, thin=20)

#plot(sampleshelp, ask=TRUE)
#plot(samples, ask=TRUE)


kette <- as.matrix(samples)

gamma.par2 <- kette[,"gamma.par2"]
mgp2 <- mean(gamma.par2)
sgp2 <- sd(gamma.par2)
gamma.elec2 <- kette[,"gamma.elec2"]
mge2 <- mean(gamma.elec2)
sge2 <- sd(gamma.elec2)
gamma.dir3 <- kette[,"gamma.dir3"]
mgd3<- mean(gamma.dir3)
sgd3 <- sd(gamma.dir3)

m_raw <- c(mgd3,mgp2,mge2)
s_raw <- c(sgd3,sgp2,sge2)

m_c <- c(((mgd3/sd(dir, na.rm=TRUE))*3.29),((mgp2/sd(par, na.rm=TRUE))*3.29),((mge2/sd(elec, na.rm=TRUE))*3.29))

s_c <- c(((sgd3/sd(dir, na.rm=TRUE))*3.29),((sgp2/sd(par, na.rm=TRUE))*3.29),((sge2/sd(elec, na.rm=TRUE))*3.29))

clabels <- c("Direct democracy","Number of parties", "Disproportionality")

var.names <- c(clabels)

m.v <- m_c
sd.v <- s_c

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(-5,5), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(-5,5, by = 2.5), label = seq(-5,5, by = 2.5), cex.axis=.9)
axis(2, at = y.axis, label = var.names, las = 1, tick = T, font=1, cex.axis=.8)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
segments(m.v-qnorm(.975)*sd.v, y.axis, m.v+qnorm(.975)*sd.v, y.axis, lwd =  1.5)
abline(v=0, lty = 2)
segments(m.v-qnorm(.95)*sd.v, y.axis -.1, m.v-qnorm(.95)*sd.v, y.axis +.1, lwd = 1.5) 
segments(m.v+qnorm(.95)*sd.v, y.axis -.1, m.v+qnorm(.95)*sd.v, y.axis +.1, lwd = 1.5)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .7 
segments(left.side,3,left.side,3) 
segments(left.side,3,left.side+.1,3) 
segments(left.side,3,left.side+.1,3)
text(.5, 3, "Dir.", srt = 90, cex=.8)
segments(left.side,2,left.side,1) 
segments(left.side,2,left.side+.1,2) 
segments(left.side,1,left.side+.1,1)
text(.5, 1.5, "Parties-Elections", srt = 90, cex=.8)


###
#Map
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
cx151 <- kette[,"cx1[51]"]
cx152 <- kette[,"cx1[52]"]
cx153 <- kette[,"cx1[53]"]
cx154 <- kette[,"cx1[54]"]
cx155 <- kette[,"cx1[55]"]
cx156 <- kette[,"cx1[56]"]
cx157 <- kette[,"cx1[57]"]
cx158 <- kette[,"cx1[58]"]
cx159 <- kette[,"cx1[59]"]
cx160 <- kette[,"cx1[60]"]
cx161 <- kette[,"cx1[61]"]
cx162 <- kette[,"cx1[62]"]
cx163 <- kette[,"cx1[63]"]
cx164 <- kette[,"cx1[64]"]
cx165 <- kette[,"cx1[65]"]
cx166 <- kette[,"cx1[66]"]
cx167 <- kette[,"cx1[67]"]
cx168 <- kette[,"cx1[68]"]
cx169 <- kette[,"cx1[69]"]
cx170 <- kette[,"cx1[70]"]
cx171 <- kette[,"cx1[71]"]
cx172 <- kette[,"cx1[72]"]
cx173 <- kette[,"cx1[73]"]
cx174 <- kette[,"cx1[74]"]
cx175 <- kette[,"cx1[75]"]
cx176 <- kette[,"cx1[76]"]
cx177 <- kette[,"cx1[77]"]
cx178 <- kette[,"cx1[78]"]
cx179 <- kette[,"cx1[79]"]
cx180 <- kette[,"cx1[80]"]
cx181 <- kette[,"cx1[81]"]
cx182 <- kette[,"cx1[82]"]
cx183 <- kette[,"cx1[83]"]
cx184 <- kette[,"cx1[84]"]
cx185 <- kette[,"cx1[85]"]
cx186 <- kette[,"cx1[86]"]
cx187 <- kette[,"cx1[87]"]
cx188 <- kette[,"cx1[88]"]
cx189 <- kette[,"cx1[89]"]
cx190 <- kette[,"cx1[90]"]
cx191 <- kette[,"cx1[91]"]
cx192 <- kette[,"cx1[92]"]
cx193 <- kette[,"cx1[93]"]
cx194 <- kette[,"cx1[94]"]
cx195 <- kette[,"cx1[95]"]
cx196 <- kette[,"cx1[96]"]
cx197 <- kette[,"cx1[97]"]
cx198 <- kette[,"cx1[98]"]
cx199 <- kette[,"cx1[99]"]
cx1100 <- kette[,"cx1[100]"]
cx1101 <- kette[,"cx1[101]"]
cx1102 <- kette[,"cx1[102]"]
cx1103 <- kette[,"cx1[103]"]
cx1104 <- kette[,"cx1[104]"]
cx1105 <- kette[,"cx1[105]"]
cx1106 <- kette[,"cx1[106]"]
cx1107 <- kette[,"cx1[107]"]
cx1108 <- kette[,"cx1[108]"]
cx1109 <- kette[,"cx1[109]"]
cx1110 <- kette[,"cx1[110]"]
cx1111 <- kette[,"cx1[111]"]

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
mcx151 <- mean(cx151)
mcx152 <- mean(cx152)
mcx153 <- mean(cx153)
mcx154 <- mean(cx154)
mcx155 <- mean(cx155)
mcx156 <- mean(cx156)
mcx157 <- mean(cx157)
mcx158 <- mean(cx158)
mcx159 <- mean(cx159)
mcx160 <- mean(cx160)
mcx161 <- mean(cx161)
mcx162 <- mean(cx162)
mcx163 <- mean(cx163)
mcx164 <- mean(cx164)
mcx165 <- mean(cx165)
mcx166 <- mean(cx166)
mcx167 <- mean(cx167)
mcx168 <- mean(cx168)
mcx169 <- mean(cx169)
mcx170 <- mean(cx170)
mcx171 <- mean(cx171)
mcx172 <- mean(cx172)
mcx173 <- mean(cx173)
mcx174 <- mean(cx174)
mcx175 <- mean(cx175)
mcx176 <- mean(cx176)
mcx177 <- mean(cx177)
mcx178 <- mean(cx178)
mcx179 <- mean(cx179)
mcx180 <- mean(cx180)
mcx181 <- mean(cx181)
mcx182 <- mean(cx182)
mcx183 <- mean(cx183)
mcx184 <- mean(cx184)
mcx185 <- mean(cx185)
mcx186 <- mean(cx186)
mcx187 <- mean(cx187)
mcx188 <- mean(cx188)
mcx189 <- mean(cx189)
mcx190 <- mean(cx190)
mcx191 <- mean(cx191)
mcx192 <- mean(cx192)
mcx193 <- mean(cx193)
mcx194 <- mean(cx194)
mcx195 <- mean(cx195)
mcx196 <- mean(cx196)
mcx197 <- mean(cx197)
mcx198 <- mean(cx198)
mcx199 <- mean(cx199)
mcx1100 <- mean(cx1100)
mcx1101 <- mean(cx1101)
mcx1102 <- mean(cx1102)
mcx1103 <- mean(cx1103)
mcx1104 <- mean(cx1104)
mcx1105 <- mean(cx1105)
mcx1106 <- mean(cx1106)
mcx1107 <- mean(cx1107)
mcx1108 <- mean(cx1108)
mcx1109 <- mean(cx1109)
mcx1110 <- mean(cx1110)
mcx1111 <- mean(cx1111)

mcx1 <- c(mcx11, mcx12, mcx13, mcx14, mcx15, mcx16, mcx17, mcx18, mcx19, mcx110, mcx111, mcx112, mcx113, mcx114, mcx115, mcx116, mcx117, mcx118, mcx119, mcx120, mcx121, mcx122, mcx123, mcx124, mcx125, mcx126, mcx127, mcx128, mcx129, mcx130, mcx131, mcx132, mcx133, mcx134, mcx135, mcx136, mcx137, mcx138, mcx139, mcx140, mcx141, mcx142, mcx143, mcx144, mcx145, mcx146, mcx147, mcx148, mcx149, mcx150, mcx151, mcx152, mcx153, mcx154, mcx155, mcx156, mcx157, mcx158, mcx159, mcx160, mcx161, mcx162, mcx163, mcx164, mcx165, mcx166, mcx167, mcx168, mcx169, mcx170, mcx171, mcx172, mcx173, mcx174, mcx175, mcx176, mcx177, mcx178, mcx179, mcx180, mcx181, mcx182, mcx183, mcx184, mcx185, mcx186, mcx187, mcx188, mcx189, mcx190, mcx191, mcx192, mcx193, mcx194, mcx195, mcx196, mcx197, mcx198, mcx199, mcx1100, mcx1101, mcx1102, mcx1103, mcx1104, mcx1105, mcx1106, mcx1107, mcx1108, mcx1109, mcx1110, mcx1111)


#exepar
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
cx251 <- kette[,"cx2[51]"]
cx252 <- kette[,"cx2[52]"]
cx253 <- kette[,"cx2[53]"]
cx254 <- kette[,"cx2[54]"]
cx255 <- kette[,"cx2[55]"]
cx256 <- kette[,"cx2[56]"]
cx257 <- kette[,"cx2[57]"]
cx258 <- kette[,"cx2[58]"]
cx259 <- kette[,"cx2[59]"]
cx260 <- kette[,"cx2[60]"]
cx261 <- kette[,"cx2[61]"]
cx262 <- kette[,"cx2[62]"]
cx263 <- kette[,"cx2[63]"]
cx264 <- kette[,"cx2[64]"]
cx265 <- kette[,"cx2[65]"]
cx266 <- kette[,"cx2[66]"]
cx267 <- kette[,"cx2[67]"]
cx268 <- kette[,"cx2[68]"]
cx269 <- kette[,"cx2[69]"]
cx270 <- kette[,"cx2[70]"]
cx271 <- kette[,"cx2[71]"]
cx272 <- kette[,"cx2[72]"]
cx273 <- kette[,"cx2[73]"]
cx274 <- kette[,"cx2[74]"]
cx275 <- kette[,"cx2[75]"]
cx276 <- kette[,"cx2[76]"]
cx277 <- kette[,"cx2[77]"]
cx278 <- kette[,"cx2[78]"]
cx279 <- kette[,"cx2[79]"]
cx280 <- kette[,"cx2[80]"]
cx281 <- kette[,"cx2[81]"]
cx282 <- kette[,"cx2[82]"]
cx283 <- kette[,"cx2[83]"]
cx284 <- kette[,"cx2[84]"]
cx285 <- kette[,"cx2[85]"]
cx286 <- kette[,"cx2[86]"]
cx287 <- kette[,"cx2[87]"]
cx288 <- kette[,"cx2[88]"]
cx289 <- kette[,"cx2[89]"]
cx290 <- kette[,"cx2[90]"]
cx291 <- kette[,"cx2[91]"]
cx292 <- kette[,"cx2[92]"]
cx293 <- kette[,"cx2[93]"]
cx294 <- kette[,"cx2[94]"]
cx295 <- kette[,"cx2[95]"]
cx296 <- kette[,"cx2[96]"]
cx297 <- kette[,"cx2[97]"]
cx298 <- kette[,"cx2[98]"]
cx299 <- kette[,"cx2[99]"]
cx2100 <- kette[,"cx2[100]"]
cx2101 <- kette[,"cx2[101]"]
cx2102 <- kette[,"cx2[102]"]
cx2103 <- kette[,"cx2[103]"]
cx2104 <- kette[,"cx2[104]"]
cx2105 <- kette[,"cx2[105]"]
cx2106 <- kette[,"cx2[106]"]
cx2107 <- kette[,"cx2[107]"]
cx2108 <- kette[,"cx2[108]"]
cx2109 <- kette[,"cx2[109]"]
cx2110 <- kette[,"cx2[110]"]
cx2111 <- kette[,"cx2[111]"]

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
mcx251 <- mean(cx251)
mcx252 <- mean(cx252)
mcx253 <- mean(cx253)
mcx254 <- mean(cx254)
mcx255 <- mean(cx255)
mcx256 <- mean(cx256)
mcx257 <- mean(cx257)
mcx258 <- mean(cx258)
mcx259 <- mean(cx259)
mcx260 <- mean(cx260)
mcx261 <- mean(cx261)
mcx262 <- mean(cx262)
mcx263 <- mean(cx263)
mcx264 <- mean(cx264)
mcx265 <- mean(cx265)
mcx266 <- mean(cx266)
mcx267 <- mean(cx267)
mcx268 <- mean(cx268)
mcx269 <- mean(cx269)
mcx270 <- mean(cx270)
mcx271 <- mean(cx271)
mcx272 <- mean(cx272)
mcx273 <- mean(cx273)
mcx274 <- mean(cx274)
mcx275 <- mean(cx275)
mcx276 <- mean(cx276)
mcx277 <- mean(cx277)
mcx278 <- mean(cx278)
mcx279 <- mean(cx279)
mcx280 <- mean(cx280)
mcx281 <- mean(cx281)
mcx282 <- mean(cx282)
mcx283 <- mean(cx283)
mcx284 <- mean(cx284)
mcx285 <- mean(cx285)
mcx286 <- mean(cx286)
mcx287 <- mean(cx287)
mcx288 <- mean(cx288)
mcx289 <- mean(cx289)
mcx290 <- mean(cx290)
mcx291 <- mean(cx291)
mcx292 <- mean(cx292)
mcx293 <- mean(cx293)
mcx294 <- mean(cx294)
mcx295 <- mean(cx295)
mcx296 <- mean(cx296)
mcx297 <- mean(cx297)
mcx298 <- mean(cx298)
mcx299 <- mean(cx299)
mcx2100 <- mean(cx2100)
mcx2101 <- mean(cx2101)
mcx2102 <- mean(cx2102)
mcx2103 <- mean(cx2103)
mcx2104 <- mean(cx2104)
mcx2105 <- mean(cx2105)
mcx2106 <- mean(cx2106)
mcx2107 <- mean(cx2107)
mcx2108 <- mean(cx2108)
mcx2109 <- mean(cx2109)
mcx2110 <- mean(cx2110)
mcx2111 <- mean(cx2111)


mcx2 <- c(mcx21, mcx22, mcx23, mcx24, mcx25, mcx26, mcx27, mcx28, mcx29, mcx210, mcx211, mcx212, mcx213, mcx214, mcx215, mcx216, mcx217, mcx218, mcx219, mcx220, mcx221, mcx222, mcx223, mcx224, mcx225, mcx226, mcx227, mcx228, mcx229, mcx230, mcx231, mcx232, mcx233, mcx234, mcx235, mcx236, mcx237, mcx238, mcx239, mcx240, mcx241, mcx242, mcx243, mcx244, mcx245, mcx246, mcx247, mcx248, mcx249, mcx250, mcx251, mcx252, mcx253, mcx254, mcx255, mcx256, mcx257, mcx258, mcx259, mcx260, mcx261, mcx262, mcx263, mcx264, mcx265, mcx266, mcx267, mcx268, mcx269, mcx270, mcx271, mcx272, mcx273, mcx274, mcx275, mcx276, mcx277, mcx278, mcx279, mcx280, mcx281, mcx282, mcx283, mcx284, mcx285, mcx286, mcx287, mcx288, mcx289, mcx290, mcx291, mcx292, mcx293, mcx294, mcx295, mcx296, mcx297, mcx298, mcx299, mcx2100, mcx2101, mcx2102, mcx2103, mcx2104, mcx2105, mcx2106, mcx2107, mcx2108, mcx2109, mcx2110, mcx2111)

short <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY","ARG","AUS","AUT","BEL","BEN","BGR,","BHS","BRB","BWA","CAN","CHE","CHL","CPV","CRI","CZE","DEU","DNK","DOM","ESP","EST","FIN","FRA","GBR","GHA","GRC","HRV","HUN","IND","IRL","ISL","ISR","ITA","JAM","JPN","KOR","LTU","LUX","LVA","MEX","MLI","MLT","MNG","MUS","NAM","NLD","NOR","NZL","PAN","POL","PRT","ROU","SLV","SUR","SVK","SVN","SWE","TTO","TWN","URY","USA","ZAF")


#scatter
scd <- data.frame(mcx1,mcx2,short)

scd$short <- as.character(scd$short)

scd$selec <- 0
scd$selec[nchar(scd$short)<3] <- 1
scd$selec[scd$short=="AUS"] <- 1
scd$selec[scd$short=="CAN"] <- 1
scd$selec[scd$short=="GBR"] <- 1
scd$selec[scd$short=="IND"] <- 1
scd$selec[scd$short=="ISR"] <- 1
scd$selec[scd$short=="ITA"] <- 1
scd$selec[scd$short=="JPN"] <- 1
scd$selec[scd$short=="MEX"] <- 1
scd$selec[scd$short=="SWE"] <- 1
scd$selec[scd$short=="URY"] <- 1
scd$selec[scd$short=="USA"] <- 1
scd$selec[scd$short=="ZAF"] <- 1

scdr <- scd[scd$selec==1,]

# jittered 
plot(scdr$mcx2,scdr$mcx1, xlab="Parties-Elections", ylab="Direct democracy", type="n")
text(jitter(scdr$mcx2, amount=.1),jitter(scdr$mcx1, amount=.1),scdr$shorty,cex=.5) 

