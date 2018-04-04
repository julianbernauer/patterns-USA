# Patterns of Democracy in the USA
# April 4, 2018 

# Julian Bernauer 

# descriptives and principal components analysis 

# setwd("F:/Bern post/Patterns USA Paper")
# setwd("") 

load("USpatterns.Rdata")

# plot index direct democracy 

clabels <- c("Alabama (AL)","Alaska (AK)","Arizona (AZ)","Arkansas (AR)","California (CA)","Colorado (CO)","Connecticut (CT)",
             "Delaware (DE)","Florida (FL)","Georgia (GA)","Hawaii (HI)","Idaho (ID)","Illinois (IL)","Indiana (IN)","Iowa (IA)",
             "Kansas (KS)","Kentucky (KY)","Louisiana (LA)","Maine (ME)","Maryland (MD)","Massachusetts (MA)","Michigan (MI)",
             "Minnesota (MN)","Mississippi (MS)","Missouri (MO)","Montana (MT)","Nebraska (NE)","Nevada (NV)","New Hampshire (NH)",
             "New Jersey (NJ)","New Mexico (NM)","New York (NY)","North Carolina (NC)","North Dakota (ND)","Ohio (OH)","Oklahoma (OK)",
             "Oregon (OR)","Pennsylvania (PA)","Rhode Island (RI)","South Carolina (SC)","South Dakota (SD)","Tennessee (TN)","Texas (TX)",
             "Utah (UT)","Vermont (VT)","Virginia (VA)","Washington (WA)","West Virginia (WV)","Wisconsin (WI)","Wyoming (WY)")

var.names <- c(clabels)

m.v <- dir

#sort
pic <- data.frame(var.names,m.v)
pic.sort <- pic[order(m.v) , ]
pic.sort

y.axis <- length(var.names):1 
layout(matrix(c(2,1),1,2),  
       widths = c(1.5, 5)) 

par(mar=c(2,6,.5,1), lheight = .8) 
plot(pic.sort$m.v, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, xlim = c(0,7), cex=1, ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,at = seq(0,7, by = 1), label = seq(0,7, by = 1), cex.axis=.9)
axis(2, at = y.axis, label = pic.sort$var.names, las = 1, tick = T, font=1, cex.axis=.7)
abline(h = y.axis, lty = 2, lwd = .5, col = "grey")
abline(v=0, lty = 2)

par(mar=c(2,0,.5,0)) 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F,  xlab = "", ylab = "")

left.side <- .9 
segments(left.side,50,left.side,1) 
segments(left.side,50,left.side+.1,50) 
segments(left.side,1,left.side+.1,1)
text(.5, 25, "Subnational Direct Democratic Power Diffusion in the USA", srt = 90, cex=.8)


# comparison direct democracy jittered 
plot(dir_comp$dir,dir_comp$bowl, xlab="Our index of direct democracy", ylab="Index Bowler and Donovan", type="n")
text(jitter(dir_comp$dir, amount=.12),jitter(dir_comp$bowl, amount=.12),dir_comp$shorty,cex=.8)

cor.test(dir_comp$dir,dir_comp$bowl,"complete.obs",alternative="two.sided",method="pearson")


##
#PCA - 6 indicators 

pcaN <- USmeans[complete.cases(USmeans),]
length(pcaN$par)

USpca <- prcomp(~ ., USmeans, scale.=TRUE, na.action=na.omit)
USpca$sdev
USpca$rotation
USpca$x

varimax4 <- varimax(USpca$rotation[,1:4])
varimax4

varimax3 <- varimax(USpca$rotation[,1:3])
varimax3

varimax2 <- varimax(USpca$rotation[,1:2])
varimax2

ls(varimax2)
varimax2$rotmat
varimax2$loadings


##
# nine indicators 

USpcaf <- prcomp(~ ., USmeansfull, scale.=TRUE, na.action=na.omit)
USpcaf$sdev
varimax4 <- varimax(USpcaf$rotation[,1:4])
varimax4
varimax3 <- varimax(USpcaf$rotation[,1:3])
varimax3
varimax2 <- varimax(USpcaf$rotation[,1:2])
varimax2

