library(corrplot)
library(devtools)
library(ggbiplot)
library(MASS)
library(outliers)


D1 <- read.table("Assistments-confidence.csv", sep = ",", header = TRUE)

D1 <- dplyr::select(D1, 2:8)

COR <- cor(D1)

corrplot(COR, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

D2 <- dplyr::select(D1, 1:3, 5:7)

pca <- prcomp(D2, retx=TRUE, center=TRUE, scale = TRUE)

#the standard deviations of the principal components
pca$sdev

#extract the variances of the components
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
#use the loadings matrix, PCA$rotation, to extract a matrix with the number of components 
#(columns) you want to use. Rather than use predict(), just multiply your test data matrix 
#by this loading matrix to get the transformed data returned by predict.
loadings <- abs(pca$rotation)
loadings2 <- sweep(loadings, 2, colSums(loadings), "/") 
# stats has to contain row summary statistics; 
#margin=2 refers to the columns
#stats then has to contain column summary statistics. 
loadings3 <- t(loadings2)
biplot(pca)

persistence <- 0.124231133802369*D2$prior_prob_count+0.0803595580597633*D2$prior_percent_correct+0.217447373356448*D2$problems_attempted+0.302237796251017*D2$mean_hint+0.258634577853408*D2$mean_attempt+0.0170895606769939*D2$mean_confidence
prior_knowledge <- 0.250811862373885*D2$prior_prob_count + 0.446776211920339*D2$prior_percent_correct+0.173444694422021*D2$problems_attempted+0.0684338702175256*D2$mean_hint+0.046588440440991*D2$mean_attempt+0.0139449206252386*D2$mean_confidence
confidence <- 0.221017004387105*D2$prior_prob_count+0.0510899830782748*D2$prior_percent_correct+0.200602878158414*D2$problems_attempted+0.0441521668541959*D2$mean_hint+0.0252787785501249*D2$mean_attempt+0.457859188971885*D2$mean_confidence
prior_experience <- 0.315162570310492*D2$prior_prob_count + 0.120626986454428*D2$prior_percent_correct + 0.144756640556877*D2$problems_attempted + 0.0512924612431948*D2$mean_hint + 0.142039865428019*D2$mean_attempt + 0.226121476006989*D2$mean_confidence
attemptation <- 0.00366446772476213*D2$prior_prob_count+0.15331501423353*D2$prior_percent_correct+0.303884750456673*D2$problems_attempted+0.0524837639560854*D2$mean_hint+0.357699023479297*D2$mean_attempt+0.128952980149652*D2$mean_confidence
hints_gathering <- 0.140116512821902*D2$prior_prob_count + 0.177701543338857*D2$prior_percent_correct + 0.157489829816897*D2$problems_attempted + 0.356088362436716*D2$mean_hint + 0.161654781654518*D2$mean_attempt+0.00694896993111117*D2$mean_confidence

##looking for a simplification of this process#####

D5 <- cbind(persistence,prior_knowledge,confidence,prior_experience,attemptation,hints_gathering, as.data.frame(D1$mean_correct))

COR3 <- cor(D5)
corrplot(COR3, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

########################partB ########################

P1 <- read.table("humor_data.csv", sep = ",", header = TRUE)
P2 = dplyr::filter(P1, Q1!= -1, Q2!= -1,Q3!= -1,Q4!= -1,Q5!= -1,Q6!= -1,Q7!= -1,Q8!= -1,Q9!= -1,Q10!= -1,Q11!= -1,Q12!= -1,Q13!= -1,Q14!= -1,Q15!= -1,Q16!= -1,Q17!= -1,Q18!= -1,Q19!= -1,Q20!= -1,Q21!= -1,Q22!= -1,Q23!= -1,Q24!= -1,Q25!= -1,Q26!= -1,Q27!= -1,Q28!= -1,Q29!= -1,Q30!= -1, Q31!= -1,Q32!= -1)
P2 = dplyr::filter(P2, age < 110)
P2 = dplyr::filter(P2, gender != 0)
P2 = dplyr::filter(P2, selfenhancing != 1)
#this person filing all the questions with 1, not a valid response.
P2 = dplyr::filter(P2, agressive != 5)
#this person filing all the questions with 5 4 5 4 5 4 pattern, not a valid response.

rm.outlier(P2, fill = FALSE, median = FALSE, opposite = FALSE)

P3 <- dplyr::select(P2, 1:32)
COR6 <- cor(P3)
corrplot(COR6, order="AOE", method="circle", tl.pos="lt", type="upper",        
         tl.col="black", tl.cex=0.6, tl.srt=45, 
         addCoef.col="black", addCoefasPercent = TRUE,
         sig.level=0.50, insig = "blank")

pca3 <- prcomp(P3, scale = TRUE)
pca3$sdev
pca3$sdev^2
summary(pca3)
plot(pca3, type = "lines")
#10 PCs explain the majority of variences

pca3$rotation
biplot(pca3)

P4 <- as.data.frame(pca3$x)
P5 <- dplyr::select(P4, 1:10)

loadings4 <- abs(pca3$rotation)
loadings5 <- sweep(loadings4, 2, colSums(loadings3), "/") 
loadings6 <- t(loadings5)
#top 3 questions in each PC, reading from loading5
#PC1 represents spontaneous humor: Q17 tell jokes  Q5 naturally humorous person Q14 humor keeps me from getting depressed
#PC2 represents put myself down for fun: Q20 go overboard-put myself down Q8 get carried away-put myself down Q4 let people laugh at me
#PC3 represents not offended: Q31 not laugh to offend Q15 not use humor as a way of criticizing Q7 no others hurt by my humor
#PC4 represents positive thinking: Q18 cheer myself up Q10 make myself feel better Q17 do not like to tell jokes(unmatched)
#PC5 represents self-entertainment:  Q6 amused by the absurdities of life Q30 laugh by myself Q22 sad->lose humor(unmatched) 
#PC6 represents openness: Q28 cover problem up by joking Q22 sad->lose humor (unmatched) Q30 laugh by myself 
#PC7 represents shyness: Q23 not laugh at others Q29 can not think of witty things to say  Q25 do not joke around with my friends
#PC8 represents interaction with others: Q24 other people make fun of me Q27 use humor or teasing to put them down Q30 laugh by myself(unmatched) 
#PC9 represents self-control: Q22 sad->lose humor Q9 not make others laugh by telling funny stories about myself Q19 cannot stop saying it
#PC10 represents consider others' feeling: 28 cover problem up by joking 7 not hurt others by my humor Q23 not laugh at others

