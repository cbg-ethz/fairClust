# loading the package
library(fairadapt)
library(faircause)
library(clustMixType)
library(FairMclus)
library(dplyr)
library(ggplot2)
library(tictoc)

rm(list=ls())

set.seed(1)

# load data
compas_dat <- get(data("compas", package = "fairadapt"))
head(compas_dat)


# select training data
n_samp <- 1000
compas_trn <- head(compas_dat, n = n_samp)
data_unfair <- compas_trn[,c(3,1,2,4:9)]

# Set number of clusters
num_clusters <- 3

# define standard fairness model graph
X <- "race" # protected attribute
Z <- c("age", "sex") # confounders
W <- c("juv_fel_count", "juv_misd_count", "juv_other_count", "priors_count", "c_charge_degree", 
       "two_year_recid") # mediators
Y <- "cluster" # outcome

compas_trn$cluster <- 0

cols <- c(Z, W, X, Y)

gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                  dimnames = rep(list(cols), 2))

gov_cfd <- gov_adj

gov_adj[Z, c(Y)] <- 1
gov_adj[W, Y] <- 1
gov_adj[X, c(W, Y)] <- 1
# gov_adj[X, Z] <- 1
gov_cfd[X, Z] <- 1
gov_cfd[Z, X] <- 1
gov_grph <- graphModel(gov_adj, gov_cfd)


# adapt data
compas_ada <- fairadapt(cluster ~ ., train.data = compas_trn,
                     adj.mat = gov_adj, prot.attr = X)

# adapt data for spurious effect
gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                  dimnames = rep(list(cols), 2))

gov_adj[Z, c(W, Y)] <- 1
gov_adj[W, Y] <- 1
gov_adj[X, c(Z, W, Y)] <- 1
# gov_cfd[X, Z] <- 1
# gov_cfd[Z, X] <- 1
# gov_grph <- graphModel(gov_adj, gov_cfd)


# adapt data
compas_ada_se <- fairadapt(cluster ~ ., train.data = compas_trn,
                        adj.mat = gov_adj, prot.attr = X)

data_fair <- compas_ada$adapt.train
data_fair$race <- data_unfair$race

data_fair_se <- compas_ada_se$adapt.train
data_fair_se$race <- data_unfair$race

data_fair$cluster <- NULL
data_fair_se$cluster <- NULL

## Clustering ## 

# Perform vanialla clustering
result <- kproto(x = data_unfair, k = num_clusters)
data_unfair$cluster_vanilla <- result$cluster

# Perform fairness through awareness clustering
result <- kproto(x = data_unfair[,-c(1,10)], k = num_clusters)
data_unfair$cluster_unaware <- result$cluster

# Perform causally fair clustering
result <- kproto(x = data_fair[-3], k = num_clusters)
data_fair$cluster_fair <- result$cluster

# Perform causally fair clustering including spurious effect
result <- kproto(x = data_fair_se[-3], k = num_clusters)
data_fair_se$cluster_fair <- result$cluster

# balanced clustering

# Convert all character variables to factor and convert all factor variables to integer
df <- as.data.frame(data_unfair[,c(1:9)])
df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
df[] <- lapply(df, function(x) if(is.factor(x)) as.integer(x) else x)
# Convert all integer variables to factor and convert all factor variables to integer
df[] <- lapply(df, function(x) if(is.integer(x)) as.factor(x) else x)
df[] <- lapply(df, function(x) if(is.factor(x)) as.integer(x) else x)

df_test <- df
df_temp <- cbind(1:nrow(df_test), df_test)
colnames(df_temp) <- paste0("V", 1:ncol(df_temp)-1)
transformed_data <- as.matrix(df_temp)

tic()
result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=4, kclus=num_clusters, numpos=c(2,7))
# result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=0, kclus=num_clusters, numpos=c(2:14))
# result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=0, kclus=num_clusters, numpos=c(12))
# result <- FairMclus::FairMclus(f=transformed_data, typedata="c", protected="V1", ncores=0, kclus=num_clusters, numpos=c(0))
toc()
data_unfair$cluster_balanced <- result$cluster

# calculate fairness measures

# causally fair clustering
tvd_fair <- fairness_cookbook(data = data_fair, X = X, W = W, Z = Z, Y = "cluster_fair", 
                              x0 = "Non-White", x1 = "White", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Fair Clusters)")

# causally fair clustering with spurious effect
tvd_fair_se <- fairness_cookbook(data = data_fair_se, X = X, W = W, Z = Z, Y = "cluster_fair", 
                                 x0 = "Non-White", x1 = "White", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair_se, decompose = "xspec", dataset = "Census 2018 (Fair Clusters with SE)")

# vanialla clustering
tvd_fair2 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_vanilla", 
                               x0 = "Non-White", x1 = "White", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair2, decompose = "xspec", dataset = "Census 2018 (naive vanilla clustering)")

# fairness through unawareness
tvd_fair3 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_unaware", 
                               x0 = "Non-White", x1 = "White", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair3, decompose = "xspec", dataset = "Census 2018 (FtU)")

# fairness through balanced clusters
tvd_fair4 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_balanced", 
                               x0 = "Non-White", x1 = "White", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair4, decompose = "xspec", dataset = "Census 2018 (Balanced Clusters)")

# Define the measures of interest
measures <- c("tv", "ctfde", "ctfie", "ctfse")

# Create a function to filter the measures and assign the method
filter_and_assign <- function(data, method) {
  data <- data[data$measure %in% measures, ]
  data$Method <- method
  return(data)
}

# Apply the function to each dataframe
plot_vals_fair5 <- filter_and_assign(tvd_fair_se$measures, "Causally Fair (NDE+NIE+SE)")
plot_vals_fair4 <- filter_and_assign(tvd_fair4$measures, "Balanced")
plot_vals_fair3 <- filter_and_assign(tvd_fair3$measures, "FtU")
plot_vals_fair2 <- filter_and_assign(tvd_fair2$measures, "Unadjusted")
plot_vals_fair <- filter_and_assign(tvd_fair$measures, "Causally Fair (NDE+NIE)")

# Combine all dataframes
plot_vals <- rbind(plot_vals_fair5, plot_vals_fair4, plot_vals_fair3, plot_vals_fair2, plot_vals_fair)

# Define color scheme
my_colors <- c("#D73027","#ABD9E9", "#4575B4")
my_colors <- c("#D73027", "#117777","#708090","#ABD9E9", "#4575B4")


# Preprocess the data for plotting
data_new <- plot_vals
data_new$measure <- factor(data_new$measure, measures)
data_new$Method <- factor(data_new$Method, c("Causally Fair (NDE+NIE+SE)", "Causally Fair (NDE+NIE)", "Balanced", "FtU", "Unadjusted"))


# Create the plot
p1 <- data_new %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ 
  theme_minimal() +
  xlab("Fairness Measure")+
  ylab("Value"); p1

# saveRDS(data_new, "results/compas.rds")
