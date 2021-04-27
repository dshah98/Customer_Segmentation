# Customer Segmentation is one the most important applications of unsupervised learning. Using clustering techniques 
# companies can identify the several segments of customers allowing them to target the potential user base.

# Customer Segmentation is the process of division of customer base into several groups of individuals that share a 
# similarity in different ways that are relevant to marketing such as gender, age, interests, and miscellaneous 
# spending habits.

# Companies that deploy customer segmentation are under the notion that every customer has different requirements 
# and require a specific marketing effort to address them appropriately. Companies aim to gain a deeper approach of 
# the customer they are targeting. Therefore, their aim has to be specific and should be tailored to address the 
# requirements of each and every individual customer. Furthermore, through the data collected, companies can gain a 
# deeper understanding of customer preferences as well as the requirements for discovering valuable segments that 
# would reap them maximum profit. This way, they can strategize their marketing techniques more efficiently and 
# minimize the possibility of risk to their investment.

# The technique of customer segmentation is dependent on several key differentiators that divide customers into 
# groups to be targeted. Data related to demographics, geography, economic status as well as behavioral patterns 
# play a crucial role in determining the company direction towards addressing the various segments.


# IMPORTING DATASET

customer_data = read.csv("Mall_Customers.csv")

# Information of dataset
str(customer_data)
summary(customer_data)

# CUSTOMER GENDER VISUALIZATION

gen = table(customer_data$Gender)

# Barplot
barplot(gen, 
        main = "Gender Comparision",
        ylab = "Count",
        xlab = "Gender",
        col = c("black","palevioletred1"))

# Piechart
pct = round(gen / sum(gen) * 100)
lbs = paste(c("Female", "Male"), " ", pct, "%", sep = " ")
# install.packages('plotrix')
library(plotrix)
pie3D(gen,
      labels = lbs,
      main = "Ratio of Female and Male")

# VISUALIZATION OF AGE DISTRIBUTION

# Histogram
hist(customer_data$Age,
     col = "blue",
     main = "Histogram to Show Count of Age Class",
     xlab = "Age Class",
     ylab = "Frequency",
     labels = TRUE)

# Boxplot
boxplot(customer_data$Age,
        main = "Boxplot for Descriptive Analysis of Age",
        ylab = "Frequency",
        xlab = "Age")

# From the above two visualizations, we conclude that the maximum customer ages are between 30 and 35. 
# The minimum age of customers is 18, whereas, the maximum age is 70.

# ANALYSIS OF THE ANNUAL INCOME OF THE CUSTOMER

# Histogram
hist(customer_data$Annual.Income..k..,
     col = "palevioletred1",
     main = "Annual Income",
     xlab = "Annual Income Class",
     ylab = "Frequency",
     labels = TRUE)

# Density Plot
plot(density(customer_data$Annual.Income..k..),
     col = "yellow",
     main = "Density Plot for Annual Income",
     xlab = "Annual Income Class",
     ylab = "Density")
polygon(density(customer_data$Annual.Income..k..),
        col = "#ccff66")

# The minimum annual income of the customers is 15 and the maximum income is 137. People earning an average income 
# of 70 have the highest frequency count in our histogram distribution. The average salary of all the customers is 
# 60.56. In the Kernel Density Plot that we displayed above, we observe that the annual income has a normal 
# distribution.

# ANALYZING SPENDING SCORE OF THE CUSTOMER

hist(customer_data$Spending.Score..1.100.,
     main = "Spending Score",
     xlab = "Spending Score Class",
     ylab ="Frequency",
     col = "#6600cc",
     labels = TRUE)

# The minimum spending score is 1, maximum is 99 and the average is 50.20. We can see Descriptive Analysis of 
# Spending Score is that Min is 1, Max is 99 and avg. is 50.20. From the histogram, we conclude that customers 
# between class 40 and 50 have the highest spending score among all the classes.

# DETERMINING OPTIMAL CLUSTERS

# install.packages('purrr')
library(purrr)
set.seed(123)

# Function to calculate Total Intra-Cluster Sum of Square 
iss <- function(k) {
  kmeans(customer_data[, 3:5], k, iter.max = 100, nstart = 100, algorithm = "Lloyd" )$tot.withinss
}

k.values <- 1:10
iss_values <- map_dbl(k.values, iss)
plot(k.values, 
     iss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total Intra-Clusters Sum of Squares")

# We conclude that 4 is the appropriate number of clusters since it seems to be appearing at the bend in the 
# elbow plot.

# AVERAGE SILHOUETTE METHOD

# With the help of the average silhouette method, we can measure the quality of our clustering operation. With this,
# we can determine how well within the cluster is the data object. If we obtain a high average silhouette width, it 
# means that we have good clustering. The average silhouette method calculates the mean of silhouette observations 
# for different k values. With the optimal number of k clusters, one can maximize the average silhouette over 
# significant values for k clusters.

# install.packages('cluster')
library(cluster) 
# install.packages('gridExtra')
library(gridExtra)
# install.packages('grid')
library(grid)

centers = 2
k2 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k2$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 3
k3 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k3$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 4
k4 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k4$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 5
k5 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k5$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 6
k6 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k6$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 7
k7 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k7$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 8
k8 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k8$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 9
k9 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k9$cluster, dist(customer_data[,3:5], "euclidean")))

centers = 10
k10 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
plot(silhouette(k10$cluster, dist(customer_data[,3:5], "euclidean")))

# install.packages('NbClust')
library(NbClust)
# install.packages('factoextra')
library(factoextra)

fviz_nbclust(customer_data[,3:5], kmeans, method = "silhouette")
# fviz_nbclust() function to determine and visualize the optimal number of clusters

# GAP STATIC METHOD

# We can use this method to any of the clustering method like K-means, hierarchical clustering etc. Using the gap 
# statistic, one can compare the total intracluster variation for different values of k along with their expected 
# values under the null reference distribution of data. With the help of Monte Carlo simulations, one can produce 
# the sample dataset. For each variable in the dataset, we can calculate the range between min(xi) and max (xj) 
# through which we can produce values uniformly from interval lower bound to upper bound.

set.seed(125)
stat_gap <- clusGap(customer_data[,3:5], FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
fviz_gap_stat(stat_gap)

centers = 6
k6 <- kmeans(customer_data[,3:5], centers, iter.max=100, nstart=50, algorithm="Lloyd")
k6

# cluster – This is a vector of several integers that denote the cluster which has an allocation of each point.
# totss – This represents the total sum of squares.
# centers – Matrix comprising of several cluster centers
# withinss – This is a vector representing the intra-cluster sum of squares having one component per cluster.
# tot.withinss – This denotes the total intra-cluster sum of squares.
# betweenss – This is the sum of between-cluster squares.
# size – The total number of points that each cluster holds.

# VISUALIZING CLUSTER RESULT USING FIRST PRINCIPLE COMPONENT

#principal component analysis
pcclust = prcomp(customer_data[,3:5], scale=FALSE) 
summary(pcclust)

# Selecting particular columns
pcclust$rotation[,1:2]

# Plotting
set.seed(1)
ggplot(customer_data, aes(x =Annual.Income..k.., y = Spending.Score..1.100.)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

# Cluster 6 and 4 – These clusters represent the customer_data with the medium income salary as well as the medium 
# annual spend of salary.
# Cluster 1 – This cluster represents the customer_data having a high annual income as well as a high annual spend.
# Cluster 3 – This cluster denotes the customer_data with low annual income as well as low yearly spend of income.
# Cluster 2 – This cluster denotes a high annual income and low yearly spend.
# Cluster 5 – This cluster represents a low annual income but its high yearly expenditure.

# Plotting
ggplot(customer_data, aes(x =Spending.Score..1.100., y =Age)) + 
  geom_point(stat = "identity", aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name=" ",
                       breaks=c("1", "2", "3", "4", "5","6"),
                       labels=c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5","Cluster 6")) +
  ggtitle("Segments of Mall Customers", subtitle = "Using K-means Clustering")

# Plotting for PCs
kCols=function(vec){cols=rainbow (length (unique (vec)))
return (cols[as.numeric(as.factor(vec))])}

digCluster<-k6$cluster; dignm<-as.character(digCluster); # K-means clusters

plot(pcclust$x[,1:2], col =kCols(digCluster),pch =19,xlab ="K-means",ylab="classes")
legend("topleft", unique(dignm), fill = unique(kCols(digCluster)), ncol = 2)

# Cluster 4 and 1 – These two clusters consist of customers with medium PCA1 and medium PCA2 score.
# Cluster 6 – This cluster represents customers having a high PCA2 and a low PCA1.
# Cluster 5 – In this cluster, there are customers with a medium PCA1 and a low PCA2 score.
# Cluster 3 – This cluster comprises of customers with a high PCA1 income and a high PCA2.
# Cluster 2 – This comprises of customers with a high PCA2 and a medium annual spend of income.
