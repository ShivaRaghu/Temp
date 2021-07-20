# Perform Clustering on Basketball Game Data
```
library(Stat2Data)
library(PerformanceAnalytics)
library(cluster)
library(dplyr)
library(knitr)
data(Hoops)
View(Hoops)
```
 Trim data to Grinnell statisitcs data columns#
```
grin_data <- Hoops[, c(5,6,7,9,10,13,14,16,17,18,22)]

```
 Remaining variables which includes opponent's stats
```
rest_var <- Hoops[, -c(1,5,6,7,9,10,13,14,16,17,18,22)]
head(grin_data)
```

 a). Examine strong correlations in remaining variables ###
chart.Correlation(grin_data)
 There is a strong correlation b/w 5.GrAtt(no. of field goal attempts)
 and 6.Gr3Att (no. of 3-point field goal attempts) and between 9(GrRB: Total Rebounds)
 and 10 (GrOR: no. of Grinnell Offensive rebounds)
 For continuing the evaluation, I have decided to exclude var 6 and 10
grin_data1 <- Hoops[, c(5,7,9,13,14,16,17,18,22)]
 Examine correlations
chart.Correlation(grin_data1)
 Now I do not see very strong correlation between any two var

 b)Fit HC algorithm on the trimmed data
 step 1 Cal Euclidean and Manhattan Distance between games for selected variables ( 9 dimensions)
```
d_euclid <- dist(grin_data1, method = "euclidean")
d_manhat <- dist(grin_data1, method = 'manhattan')

# Step 2 Agglomerative clustering/ Euclidean distance / Single linkage
plot(hclust(d_euclid, method = "single"),
     main="Agglomerative\nEuclidean\nSingle Linkage",
     xlab = "Clustering games by Att,FT,RB,Point, Ass,TO,Blocks, Steal, Pt Diff ")
```
 Looks pretty horrible with all 140 games.
 Make a subset of first 15 games and then re-calculate distance 
 and then HC

 Randomly sample 15 games from dataset for evaluating HC
```
random_15rows <- grin_data1[sample(nrow(grin_data1),15),]
```
 step 1 Cal Euclidean and Manhattan Distance between games for selected variables ( 9 dimensions)
```
d_euclidean <- dist(random_15rows, method = "euclidean")
d_manhattan <- dist(random_15rows, method = 'manhattan')

```
 Plot HC for Agglomerative Nesting Algorithm on 15 games
par(mfrow=c(2,2))
 Step 2 Agglomerative clustering/ Euclidean distance / Single linkage
```
plot(hclust(d_euclidean, method = "single"),hang = -1,
     main="Agglomerative\nEuclidean\nSingle Linkage",
     xlab = "Clustering 15 random games\nby Att,FT,RB,Point, Ass,TO,Blocks, Steal, Pt Diff ")
rect.hclust(hclust(d_euclidean, method = 'single'),
            k=5,border=2:5) #add rectangle

plot(hclust(d_manhattan, method = 'single'),hang = -1,
     main = "Agglomerative\nManhattan\nSingle Linkage",
     xlab = "Clustering 15 random games\nby Att,FT,RB,Point, Ass,TO,Blocks, Steal, Pt Diff ")
rect.hclust(hclust(d_manhattan, method = 'single'),
            k=5,border=2:5) #add rectangle

plot(hclust(d_euclidean, method = 'complete'),hang = -1,
     main = "Agglomerative\nEuclidean\nComplete Linkage",
     xlab = "Clustering 15 random games\nby Att,FT,RB,Point, Ass,TO,Blocks, Steal, Pt Diff ")
rect.hclust(hclust(d_euclidean, method = 'complete'),
            k=5,border=2:5) #add rectangle

plot(hclust(d_manhattan, method = 'complete'), hang = -1,
     main = "Agglomerative\nManhattan\ncomplete Linkage",
     xlab = "Clustering 15 random games\nby Att,FT,RB,Point, Ass,TO,Blocks, Steal, Pt Diff ")
rect.hclust(hclust(d_manhattan, method = 'complete'),
            k=5,border=2:5) #add rectangle

# Observation
group_euc_single <- cutree(hclust(d_euclidean, method = "single"),k=5)
group_man_single <- cutree(hclust(d_manhattan, method = "single"),k=5)
group_euc_comple <- cutree(hclust(d_euclidean, method = "complete"),k=5)
group_man_comple <- cutree(hclust(d_manhattan, method = "complete"),k=5)


table(group_euc_single)
table(group_man_single)
table(group_euc_comple)
table(group_man_comple)

# Added a cluster column to 15-rows data
a <- random_15rows %>% 
  mutate(cluster_a = group_euc_single) %>% group_by(cluster_a) %>% head
a <- as.data.frame(a)
a$cluster_a <- as.factor(a$cluster_a)
str(a)
```
 Boxplot of Total Rebounds by Clusters
```
boxplot(GrRB ~ cluster_a, data = a, col = "lightgray")

# Plot HC with DIANA method on 15 games subset
par(mfrow=c(2,2))
pltree(diana(d_euclidean, diss = TRUE, metric = 'euclidean'),hang = -1,
       main = "Divisive\nEuclidean distance")
rect.hclust(diana(d_euclidean, diss = TRUE, metric = 'euclidean'),
            k=5,border=2:5) #add rectangle

pltree(diana(d_manhattan, diss=TRUE, metric = 'manhattan'), hang = -1,
       main = "Divisive\nManhattan distance")
rect.hclust(diana(d_manhattan, diss = TRUE, metric = 'manhattan'),
            k=5,border=2:5) #add rectangle

# Now consider looking into all 147 games with selected Grinnell's stats and perform HC 
grin_data1 <- Hoops[, c(5,7,9,13,14,16,17,18,22)]

```

