library(corrplot)
library(devtools)
library(ggbiplot)


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
fit <- princomp(D3,cor=TRUE)
biplot(fit)

######## failure, attempts ###############
D5 <- abs(D3)
tmp <- rowSums(D5, 1:6)

D6 <- data.frame(D1$mean_correct, tmp)

COR3 <- cor(D6)
cor.test(D1$mean_correct, tmp)
#p-value = 1.617e-10 significant correlated

D7 <- dplyr::select(D4, 1:4, 7)
COR4 <- cor(D7)
corrplot(COR4, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")
D8 <- dplyr::select(D7, 1:4)
D9 <- abs(D8)
tmp2 <- rowSums(D9, 1:4)
cor.test(D1$mean_correct, tmp2)
#p-value = 1.891e-07 significant correlated

##### Maybe got this right? ###########

D10 <- t(D2)
pca2 <- prcomp(D10, scale = TRUE)
pca2$sdev
pca2$sdev^2
summary(pca2)
plot(pca2, type = "lines")
D11 <- as.data.frame(pca2$x)
D12 <- cbind(D11, as.data.frame(D1$mean_correct)) 
# I dont understant how this works, but it works....

COR5 <- cor(D12)
corrplot(COR5, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

pca2$rotation

loadings2 <- abs(pca2$rotation)
sweep(loadings2, 2, colSums(loadings2), "/") 

biplot(pca2)
fit <- princomp(D11,cor=TRUE)
biplot(fit)
########################partB ########################

P1 <- read.table("humor_data.csv", sep = ",", header = TRUE)

P2 <- dplyr::select(P1, 1:32)

COR6 <- cor(P2)
corrplot(COR6, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

pca3 <- prcomp(P2, scale = TRUE)
pca3$sdev
pca3$sdev^2
summary(pca3)
plot(pca3, type = "lines")
#6 PCs explain the majority of variences

pca3$rotation
loadings3 <- abs(pca3$rotation)
sweep(loadings3, 2, colSums(loadings3), "/") 
biplot(pca3)

fit <- princomp(P2,cor=TRUE)
biplot(fit)

P3 <- as.data.frame(pca3$x)
P4 <- dplyr::select(P3, 1:6)

P5 <- cbind(P4, as.data.frame(P1$affiliative))
COR7 <- cor(P5)
corrplot(COR7, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

P6 <- cbind(P4, as.data.frame(P1$selfenhancing))
COR8 <- cor(P6)
corrplot(COR8, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

P7 <- cbind(P4, as.data.frame(P1$agressive))
COR9 <- cor(P7)
corrplot(COR9, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

P8 <- cbind(P4, as.data.frame(P1$selfdefeating))
COR10 <- cor(P8)
corrplot(COR10, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")