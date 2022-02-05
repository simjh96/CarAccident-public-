library("kohonen")
roads = read.csv("C:/Users/simjh96/OneDrive/문서/Car_Accident/Feature/data/0_8856.csv")

cols = c("floating_pop_count50",
         "parking_count25",
         "car_count1000", 
         "child_count1000",
         "elem_kinder_count400",
         "numberSchoolZone_count400",
         "chaos1_nearby_count25",
         "width_nearby_count25",
         "cross_road_nearby_count25",
         "num_cram_school_count400",
         "shortest_cross_count0",
         "shortest_traffic_signal_count0")
roads_som <- roads[cols]

set.seed(1)
som.roads_som = som(scale(roads_som), grid = somgrid(3, 3, "hexagonal"),keep.data = TRUE)
som.roads_som

plot(som.roads_som, main = "roads data Kohonen SOM", palette.name = rainbow)
par(mfrow = c(1,1))

#https://clarkdatalabs.github.io/soms/SOM_NBA
plot(som.roads_som, type = "changes", main = "roads data: SOM")
plot(som.roads_som, type = "counts", main = "roads data: SOM")

#cbind(roads_som,som.roads_som$unit.classif)

#summary(som.roads_som)

#som.roads_som$




#https://stackoverflow.com/questions/58376107/kohonen-som-plot-is-displaying-observations-within-cluster-plot-how-to-remove
install.packages("magrittr")

library(magrittr)
# A dataset for testing the code
data(yeast)
#X <- matrix(rnorm(100000), nrow=1000)
som_model <- som(scale(roads_som), somgrid(30, 30, "hexagonal"))
kmeansDat.t <- som_model$codes[[1]] %>% as.matrix
pretty_palette <- rainbow(10)   
som_cluster <- cutree(hclust(dist(kmeansDat.t)), 10) %>% as.matrix
# Plot Kohonen's map
plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], 
     main = "Clusters", pchs="") 
add.cluster.boundaries(som_model, som_cluster)
