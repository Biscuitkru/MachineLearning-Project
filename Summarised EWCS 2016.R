ewcs <- read.csv("EWCS_2016.csv")
ewcs[,][ewcs[, ,] == -999] <- NA
kk=complete.cases(ewcs)
ewcs <- ewcs[kk,]
## More guys than girls
## Average age = 43 
## Average Cheerful Rating = 2.4/6
## Average Calm Rating = 2.6/6
## Average Active Rating = 2.4/6
## Average Fresh Rating = 2.7/6
## Average interest Rating = 2.4/6

## Average energy rating = 2.1/5
## Average Enthusiasm rating = 2.2/5
## Average Time rating = 2.1/5
## Average Good rating = 1.5/5

## Average ratings across all questions are below half
pairs(~ . , panel=panel.smooth, data = ewcs, main = "Scatterplot Matrix")
## Can see that gender has a positive relation with Q87A - 87E, Negative relation with Q90C and 90F
## Most questions tend to have a positive relationship with each other
## Eg. The higher they rate the initial first few questions the more likely the rest of the questions they rated will have a high rating
## Age does show some positive correlation with Q87
## Q90 has correlation with each other just like Q87 has correlation with each other

names(ewcs) <- c("gender","age","cheerful","calm","active","fresh","interest","energy","enthusiasm","time","good")

cor(ewcs, ewcs$good)
# Good is weakly positively correlated with the other Q90s, weakly +ve correlated with age, weakly -ve corr with gen
cor(ewcs$good, ewcs$interest)
# Interest is moderately positively correlated with the other Q87s, weakly +ve correlated with gender & age

## Principal component analysis
ewcs.pc <- prcomp(ewcs, scale.=T)
summary(ewcs.pc)
## First two PC Captures 52% of Variance, if i use 3 PC it captures 62.1% of variance
ewcs.pc$rotation
## Can see that gender has the most effect on PC3

plot(ewcs.pc$x[,1],ewcs.pc$x[,2], xlab="PC1 (40%)", ylab = "PC2 (12.8%)", main = "Plot Using PC1 & 2")

## Can see clustering on then left side of the graph, but most points are scattered
## across the plot
library("factoextra")
fviz_pca_biplot(ewcs.pc, geom.ind = "point", pointshape = 20, 
                pointsize = 2, 
                addEllipses = F,
                label = "var",
                col.var = "red",
                repel = TRUE) +
  ggtitle("PCA plot for ecws") +
  theme(plot.title = element_text(hjust = 0.5))
## Those who rate themselves highly at being good at the job would find that time
## passes at a much faster rate for them, close relation between good and time. 
## Another close relation between those who feel cheerful and in good spirits tend to also be
## fairly active and vigorous 

# However since we only used the first two PCs to plot which only has a cumulative variance proportion of 52%, it might
# not be a very accurate representation of EWCS's components

ewcs.scaled <- scale(ewcs)

## Hierarchical clustering
library(ISLR)
set.seed(2021)
# Use scaled function
hcsecws = hclust(dist(t(ewcs.scaled)), method = "complete")
plot(hcsecws, main = "HC for EWCS")
# 4 main separate clusters with gender and age being independent clusters

set.seed(2021)
ewcs.km <- kmeans(ewcs.scaled, centers = 2)

## Kmeans for Q87 and Q90 with gender
ewcs.kmR <- data.frame(ewcs$gender, ewcs$cheerful, ewcs$calm, ewcs$active, ewcs$fresh, ewcs$interest,ewcs$energy, ewcs$enthusiasm, ewcs$time, ewcs$good, ewcs.km$cluster)
cluster1 <- subset(ewcs.kmR, ewcs.km$cluster==1)
cluster2 <- subset(ewcs.kmR, ewcs.km$cluster==2)

cluster1$ewcs.good <- factor(cluster1$ewcs.good)
cluster2$ewcs.good <- factor(cluster2$ewcs.good)
cluster1$ewcs.interest <- factor(cluster1$ewcs.interest)
cluster2$ewcs.interest <- factor(cluster2$ewcs.interest)

summary(cluster1$ewcs.good)
summary(cluster2$ewcs.good)
## Q90: Cluster 1 has a lower average rating as compared to cluster 2, 1 vs 2

summary(cluster1$ewcs.interest)
summary(cluster2$ewcs.interest)
## Q87: Cluster 3 has a lower average rating as compared to cluster 4, 2 vs 3
cluster1$ewcs.gender <- factor(cluster1$ewcs.gender)
cluster2$ewcs.gender <- factor(cluster2$ewcs.gender)
round(prop.table(table(cluster1$ewcs.gender)),2)
round(prop.table(table(cluster2$ewcs.gender)),2)
## Gender proportions are at M 53% on cluster1, 48% cluster 2

M1 <- as.matrix(table(cluster1$ewcs.gender))
p.null1 <- as.vector(prop.table(table(cluster2$ewcs.gender)))
chisq.test(M1, p = p.null1)
## Is cluster 1 statistically the same as cluster 2 in terms of gender?
## k-means clustering concludes that gender is significant differentiating cluster 1 from 2

## Gender is significant for affecting the ratings for Q87a and Q87c, both closely correlated where
## Males tend to have a lower rating as compared to females, 2 vs 3
## Gender is also significant for affecting the ratings for Q90c and Q90f, both closely correlated
## Males tend to have a lower rating as compared to females, 1 vs 2
## Can see the trend in which Males generally have a much lower rating as compared to females

## how does age affect the ratings?
summary(ewcs$age)
# Median age is 43, segregate age into two groups at age 43
# Younger vs Older, 
ewcs$age <- ifelse((ewcs$age>=15 & ewcs$age<=43) , 'Younger',ewcs$age)
ewcs$age <- ifelse((ewcs$age>43 & ewcs$age<=87) , 'Older',ewcs$age)

ewcs$age <- as.factor(ewcs$age)
# Questions 
ewcs.kmA <- data.frame(ewcs$age, ewcs$cheerful, ewcs$calm, ewcs$active, ewcs$fresh, ewcs$interest,  ewcs$energy, ewcs$enthusiasm, ewcs$time, ewcs$good, ewcs.km$cluster)
cluster1 <- subset(ewcs.kmA, ewcs.km$cluster==1)
cluster2 <- subset(ewcs.kmA, ewcs.km$cluster==2)

cluster1$ewcs.interest <- factor(cluster1$ewcs.interest)
cluster2$ewcs.interest <- factor(cluster2$ewcs.interest)
cluster1$ewcs.good <- factor(cluster1$ewcs.good)
cluster2$ewcs.good <- factor(cluster2$ewcs.good)

summary(cluster1$ewcs.interest)
summary(cluster2$ewcs.interest)
summary(cluster1$ewcs.good)
summary(cluster2$ewcs.good)

round(prop.table(table(cluster1$ewcs.age)),2)
round(prop.table(table(cluster2$ewcs.age)),2)
## 45% are Older in C1 and 56% are older in C2
## Cluster 1 has lower average score as compared to cluster 2, 2 vs 3 for Q87s
# Test significance
M <- as.matrix(table(cluster1$ewcs.age))
p.null <- as.vector(prop.table(table(cluster2$ewcs.age)))
chisq.test(M, p = p.null)
# Can see since p-value < 0.05, Age is a significant differentiator for Q87s

