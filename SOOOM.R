# Set working directory
setwd("C:/Users/ACER S7/Desktop/Assessment Data")

#install.packages(c("kohonen", "ggplot2", "rgdal", "rgeos", "gridExtra", "grid"))
#install.packages("maptools")
library(kohonen)
library(ggplot2)
library(rgdal)
library(rgeos)
library(gridExtra)
library(grid)
library(maptools)

#read in the boundary data for the edinburgh area, already matched up by row with the census data
edinburgh_map <- readOGR(dsn="SG_SIMD_2016_EDINBURGH.shp", layer="SG_SIMD_2016_EDINBURGH")

#convert the object into latitude and longitude for easier use with ggmap
edinburgh_map <- spTransform(edinburgh_map, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#plot the spatial polygons data frame 
plot(edinburgh_map)

#convert to data frame
edin_data <- as.data.frame(edinburgh_map)

#change the row index from 1
rownames(edin_data) <- 1:nrow(edin_data)

#rename
names(edin_data) <- c("Datazone", "Area Name","Total Population","Working Age Population","SIMD Domain Rank","Quantile","Decile SIMD Rank","Vigintile SIMD Rank","Percentile SIMD Ranking","Income Rate","Income Count","Income Domain Rank","Employment Rate","Employment Count","Employment Domain Ranking","Comparative Illness Factor","Hospital-Alcohol","Hospital-Drugs","Standard Mortality Rate","Depression","Low Birth Weight","Emergancy stays in hospital","Health Domain Ranking","Education Attendance","Education Attainment", "Education- No Qualifications","Education-In full time employment or ed.","Education-Entering higher education", "Education Domain Rank","Access- Drive to petrol", "Access- Drive to gp","Access- Drive to post office","Access- Drive to primary school","Access- Drive to retail","Access-Drive to secondary school","Access-Public transport to GP","Access-Public transport to post office","Access-Public transport to retail","Geographical Access Domain Ranking","Crime Count","Crime Rate","Crime Domain Rank", "Housing- Overcrowded count","Housing-No central heating count","Housing - Overcrowding rate","Housing- No central heating rate","Housing Domain Ranking","Shape Length","Shape Area","Intermediary Name" )

#convert spatial polygon to dataframe including columns of spatial information
edinburgh_fort <- fortify(edinburgh_map, region= "DataZone")
#merge the new dataframe with the edinburgh simd data using their shared column
edinburgh_fort <- merge(x=edinburgh_fort, y=edin_data, by.x="id", by.y="Datazone")

##HEALTH DATA
health <- edin_data[, c(20,21)]
## standardise (between 0 and 1)
health_st<-apply(health, MARGIN = 2, FUN=function(X) (X-min(X))/(max(X) - min(X))) #Margin=2 rescales column wise
## sum standardised values
health_sum<-rowSums(health_st[,c(1,2)])
health_sum

##EDUCATION DATA
education <- edin_data[, c(27,28)]
education_st <- apply(education, MARGIN = 2, FUN=function(X) (X-min(X))/(max(X) - min(X)))
education_sum <- rowSums(education_st)
education_sum

##GEOGRAPHIC ACCESS DATA
access <- edin_data[, c(30:38)]
access_st<-apply(access, MARGIN = 2, FUN=function(X) (X-min(X))/(max(X) - min(X))) 
access_sum <- rowSums(access_st[,c(1:9)]) #sum the colums
access_sum

##HOUSING DATA
housing <- edin_data[, c(45,46)]
housing_st <-apply(housing, MARGIN = 2, FUN=function(X) (X-min(X))/(max(X) - min(X)))
housing_sum <- rowSums(housing_st[,c(1,2)])

#INCOME
income <- edin_data[,10]

#EMPLOYMENT
employment <- edin_data[,13]

##Bringing all the variables into a data frame
summed_vars<-as.data.frame(matrix(c(health_sum,education_sum,access_sum,housing_sum,income,employment),nrow=length(access_sum)))
names(summed_vars) <- c("Health", "Education", "Access", "Housing","Income", "Employment")


#SOM TRAINING
#standardise the data creating z-scores and convert to a matrix
data_train_matrix <- as.matrix(scale(summed_vars))
#keep the column names of data_train as names in new matrix 
names(data_train_matrix) <- names(summed_vars)

#literature
#define the size, shape and topology of the som grid
som_grid <- somgrid(xdim = 15, ydim=8, topo="hexagonal",neighbourhood.fct="gaussian")


# Train the SOM model, alpha is learning rate, rlen is number of iterations
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.1,0.01), 
                 keep.data = TRUE )

#reset margins
par(mar=c(5,5,5,5))

# Plot of the training progress - how the node distances have stabilised over time.
# mean distance to closes codebook vector during training
plot(som_model, type = "changes")

## load custom palette, created by Shane Lynn
source('coolBlueHotRed.R')

#create both plots next to each other
par(mfrow = c(1,1)) 

#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed, shape="straight", border="transparent")

#map quality
plot(som_model, type = "quality", main="Distances within Nodes (Quality)", palette.name=coolBlueHotRed, shape="straight", border="darkgrey")

#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", shape="straight", palette.name=grey.colors, border="darkgrey")

dev.off() #mfrow off

#code spread, plot codebook vectors
plot(som_model, type = "codes", shape="straight", bgcol="lightgrey", palette.name=rainbow, border="darkgrey")

# Plot the heatmap for a variable at scaled / normalised values
##plot all the component planes into the same image
par(mfrow = c(3,2)) # 3 x 2 grid
for (i in 1:6) { # loop through all of them and plot
  plot(som_model, type = "property", property = getCodes(som_model)[,i],
       main=colnames(getCodes(som_model))[i], palette.name=coolBlueHotRed, shape="straight", border="transparent")
  add.cluster.boundaries(som_model, som_cluster)
  }
dev.off()

#reset margins
par(mar=c(5,5,5,5))

# show the WCSS metric for kmeans for different clustering sizes.
# Can be used as a "rough" indicator of the ideal number of clusters
mydata <- getCodes(som_model) #extract codebook vectors
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
#calculate sums of squares for 2-15 clusters
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
#plot wcss
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")


# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(dist(getCodes(som_model))), 6)

# Colour palette definition
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b')

# plot codes with cluster colours as background
plot(som_model, type="codes", bgcol = pretty_palette[som_cluster], main = "Clusters", shape="straight", palette.name=rainbow, border="transparent")
add.cluster.boundaries(som_model, som_cluster)

#MAKE GEOGRAPHIC MAP
#create dataframe of the small area id and of the cluster unit
cluster_details <- data.frame(id=edin_data$Datazone, cluster=som_cluster[som_model$unit.classif])

#we can just merge our cluster details onto the fortified spatial polygon dataframe we created earlier
mappoints <- merge(edinburgh_fort, cluster_details, by.x="id", by.y="id")

# Finally map the areas and colour by cluster
ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) + 
  geom_polygon(colour="transparent")  + 
  coord_equal() + 
  scale_fill_manual(values = pretty_palette) 

#assign each plot as an object
p1 <- ggplot(data=mappoints, aes(x=long, y=lat, group=group, fill=factor(cluster))) +
  geom_polygon(colour="transparent")+
  theme(legend.position="bottom")+
  coord_equal()+
  scale_fill_manual(values = pretty_palette) 

p2 <- ggplot(data=edinburgh_fort, aes(x=long, y=lat, fill=Quantile, group=group)) +
  geom_polygon(colour="transparent")+
  theme(legend.position="bottom")+
  coord_equal()+
  theme()
#use the GridExtra package to plot our two plots side by side
grid.arrange(p1, p2, ncol=2, 
             top=textGrob("Data Comparison Plot", vjust=2, gp=gpar(fontsize=15,font=3)))

ggplot(data=edinburgh_fort, aes(x=long, y=lat, fill=Quantile, group=group)) +
  geom_polygon(colour="transparent")+
  theme(legend.position="right")+
  coord_equal()+
  theme()

# combine map with cluster details
ed_map <- merge(edinburgh_map, cluster_details, by.x="DataZone", by.y="id")

# save as an esri shapefile
writeOGR(obj=ed_map, dsn="edinburgh_map_clustered", layer="edinburgh_map", driver="ESRI Shapefile")
