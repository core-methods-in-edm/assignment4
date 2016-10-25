library(corrplot)

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
