D1 <- read.csv("Assistment-confidence.csv")
getwd()
read.csv(Assistments-confidence.csv)
?find
find("Assistment-confidence.csv")
D1 <- read.csv("Assistments-confidence.csv")
### Removing the id column in my new data set. ##
D2 <- D1
D2$id <- NULL


#You can install the corrplot package to plot some pretty correlation matrices (sometimes called correlograms)


library(corrplot)


#Generate pairwise correlations
COR <- cor(D2)


corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")


#Study your correlogram image and save it, you will need it later


## Creating a new dataframe removing Mean_Correct


D3 <- D2


D3$mean_correct <- NULL
#The, scale and center your data for easier interpretation
D3 <- scale(D3, center = TRUE)


pca <- prcomp(D3, scale = TRUE)


pca$sdev


#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue


pca$sdev^2


#A summary of our pca will give us the proportion of variance accounted for by each component


summary(pca)


#We can lot this to get an idea of which components we should keep and which we should drop


plot(pca, type = "lines")
### Think about which components you would drop and make a decision ###


#Now, create a data frame of the transformed data from your pca.


D4 <- as.data.frame(pca$x)


#Attach the variable "mean_correct" from your original data frame to D3.


D5 <- cbind(D4, as.data.frame(D1$mean_correct))


#Now re-run your scatterplots and correlations between the transformed data and mean_correct. If you had dropped some components would you have lost important infomation about mean_correct?


COR2 <- cor(D5)


corrplot(COR2, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

