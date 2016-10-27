library(corrplot)
library(devtools)
library(ggbiplot)

options("device"="RStudioGD")
graphics.off()

D1 <- read.table("Assistments-confidence.csv", sep = ",", header = TRUE)

D1 <- dplyr::select(D1, 2:8)

COR <- cor(D1)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

D2 <- dplyr::select(D1, 1:3, 5:7)

pca <- prcomp(D2, scale = TRUE)

pca$sdev
pca$sdev^2
summary(pca)
plot(pca, type = "lines")
D3 <- as.data.frame(pca$x)
D4 <- cbind(D3, as.data.frame(D1$mean_correct))

COR2 <- cor(D4)
corrplot(COR2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")


pca$rotation

loadings <- abs(pca$rotation)

# stats has to contain row summary statistics; 
#margin=2 refers to the columns
#stats then has to contain column summary statistics. 

sweep(loadings, 2, colSums(loadings), "/") 

biplot(pca)
quartz()
fit <- princomp(D3,cor=TRUE)
biplot(fit)




