# Patterns of Democracy in the USA
# April 4, 2018 

# Julian Bernauer 

# descriptives and principal components analysis 

# setwd("F:/Bern post/Patterns USA Paper")
# setwd("") 

load("USpatterns.Rdata")


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

