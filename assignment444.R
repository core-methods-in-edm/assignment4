---
  title: "Principle Component Aanalysis"
output: html_document
---
  #Data
  The data you will be using comes from teh Assistments online intelligent tutoring system (https://www.assistments.org/). It describes students working through online math problems. Each student has the following data associated with them:
  
  - id
- prior_prob_count: How many problems a student has answered in the system prior to this session
- prior_percent_correct: The percentage of problems a student has answered correctly prior to this session
- problems_attempted: The number of problems the student has attempted in the current session
- mean_correct: The average number of correct answers a student made on their first attempt at problems in the current session
- mean_hint: The average number of hints a student asked for in the current session
- mean_attempt: The average number of attempts a student took to answer a problem in the current session
- mean_confidence: The average confidence each student has in their ability to answer the problems in the current session

#Start by uploading the data
```{r}

K1<-"Users/Ben/Documents/Assignment6.Rproj/Assistments-confidence.csv"

K1 <- read.table("Assistments-confidence.csv", sep = ",", header =  TRUE)



#We won't need to id variable, so remove that.

K1$id <- NULL
```



#Create a correlation matrix of the relationships between the variables, including correlation coefficients for each pair of variables/features.

```{r}
#You can install the corrplot package to plot some pretty correlation matrices (sometimes called correlograms)

library(corrplot)

#Generate pairwise correlations
COR <- cor(K1)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

#Study your correlogram image and save it, you will need it later
```

#Create a new data frame with the mean_correct variables removed

```{r}
K1$mean_correct <- NULL

library




K2 <-  as.data.frame(K1$prior_prob_count,K1$prior_percent_correct,K1$problems_attempted,K1$mean_hint,K1$mean_attempt,K1$mean_confidence)
#The, scale and center your data for easier interpretation

K2 <- scale(K1, center = TRUE)
```

#Now run the PCA on the new data frame

```{r}
pca <- prcomp(K2, scale = TRUE)

```

#Although the algorithm does not generate the eigenvalues directly for us, we can print a list of the standard deviation of the variance accounted for by each component.

```{r}
pca$sdev

#To convert this into variance accounted for we can square it, these numbers are proportional to the eigenvalue

pca$sdev^2

#A summary of our pca will give us the proportion of variance accounted for by each component

summary(pca)

#We can lot this to get an idea of which components we should keep and which we should drop

plot(pca, type = "lines")
```

#Think about which components you would drop and make a decision

```{r}
#Now, create a data frame of the transformed data from your pca.


K3 <- as.data.frame(pca$x)

#Attach the variable "mean_correct" from your original data frame to D3.



K4<- cbind(K3, as.data.frame(K1$mean_correct))

#Now re-run your scatterplots and correlations between the transformed data and mean_correct. If you had dropped some components would you have lost important infomation about mean_correct?

COR2 <- cor(K4)



K4
```
#Now print out the eigenvectors (often called loadings) for the components you generated:

```{r}
pca$rotation

#Examine the eigenvectors, notice that they are a little difficult to interpret. It is much easier to make sense of them if we make them proportional within each component

loadings <- abs(pca$rotation) #abs() will make all eigenvectors positive

sweep(loadings, 2, colSums(loadings), "/") #sweep() computes each row as a proportion of the column. (There must be a way to do this with dplyr()?)


#Now examine your components and try to come up with substantive descriptions of what some might represent?

##It appears that PC4 substantially reducing the dimensions of the problem count since .347 represents the highest amount of variance out of the 7 components. 
## same  prior percent correct for PC5
## meane_confidence drops the most out of the variables with the greatest variance at an epigen value of .46... at PC3
##Mean confidence must be the the easiest of all the variables to reduce. It's intial complexity is uncessary. I guess that the confidence students had must have been over complicated Should the variable be eliminated due to its lack of predictive value? 



#You can generate a biplot to help you, though these can be a bit confusing. They plot the transformed data by the first two components. Therefore, the axes represent the direction of maximum variance. Then mapped onto this point cloud are the original directions of the variables, depicted as red arrows. It is supposed to provide a visualization of which variables "go together". Variables that possibly represent the same underlying construct point in the same direction.  

biplot(pca)

#Calculate values for each student that represent these your composite variables and then create a new correlogram showing their relationship to mean_correct.








```



#Also in this repository is a data set and codebook from Rod Martin, Patricia Puhlik-Doris, Gwen Larsen