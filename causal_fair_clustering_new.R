
# Fair clustering using Bayesian networks

library(graphClust)
library(ggplot2)
library(ggraph)
library(igraph)
library(ggpubr)

# setwd("/Users/frbayer/Documents/phd_main/projects/fairClust")

# define useful function
# plot_clusters <- function(german_clusters){
#   # plot DAGs of each cluster
#   n_dags <- length(german_clusters$DAGs)
#   par(mfrow = c(2, 1+as.integer(n_dags/2)))
#   for (ii in 1:n_dags){
#     SubGroupSeparation:::plot_dag(as.matrix(german_clusters$DAGs[ii][[1]]))
#   }
#   par(mfrow = c(1,1))
# }

## Plot the learned networks
plot_clusters(readRDS("results/adult_clusters.rds"))
plot_clusters(readRDS("results/compas_clusters.rds"))
plot_clusters(readRDS("results/german_clusters.rds"))

# read data
data_adult_binarized <- read.csv("data/processed_data/adult_binerized.csv")
data_compas_binerized <- read.csv("data/processed_data/compas_binerized.csv")
data_german_binerized <- read.csv("data/processed_data/german_binerized.csv")

# display variables
head(data_adult_binarized)
head(data_compas_binerized)
head(data_german_binerized)

# display dimensions
dim(data_adult_binarized)
dim(data_compas_binerized)
dim(data_german_binerized)

## CLUSTERING ##

# learn clusters
adult_clusters <- graphClust::get_clusters(data_adult_binarized)
# plot clusters
plot_clusters(adult_clusters)

# learn clusters
compas_clusters <- graphClust::get_clusters(data_compas_binerized, n_bg = 1)
# plot clusters
plot_clusters(compas_clusters)

# learn clusters
german_clusters <- graphClust::get_clusters(data_german_binerized, bdepar = list(chi=1, edgepf=8))
# plot clusters
plot_clusters(german_clusters)

# save results
saveRDS(adult_clusters, "results/adult_clusters.rds")
saveRDS(compas_clusters, "results/compas_clusters.rds")
saveRDS(german_clusters, "results/german_clusters.rds")
