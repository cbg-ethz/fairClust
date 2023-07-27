# loading the package
library(fairadapt)
library(faircause)
library(clustMixType)
library(FairMclus)
library(dplyr)
library(ggplot2)
library(tictoc)
# library(BalKmeans)

# dplyr, irr, rlist, tidyr, parallel, magrittr, cluster, base, data.table, foreach, doParallel

rm(list=ls())

set.seed(10)

# load data
gov_dat <- get(data("gov_census", package = "fairadapt"))
head(gov_dat)

# transform salary data to log scale
gov_dat$salary <- log(gov_dat$salary)

# select training data
n_samp <- 1000
gov_trn <- head(gov_dat, n = n_samp)
data_unfair <- gov_trn

# Set number of clusters
num_clusters <- 3

# # define standard fairness model graph
# X <- "sex" # protected attribute
# Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
#        "economic_region") # confounders
# W <- c("marital", "family_size", "children", "education_level", "english_level", 
#        "hours_worked", "weeks_worked", "occupation", "industry", "salary") # mediators
# Y <- "cluster" # outcome
# 
# cols <- c(Z, W, X)
# 
# gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
#                   dimnames = rep(list(cols), 2))
# 
# gov_adj[Z, W] <- 1
# gov_adj[X, W] <- 1
# gov_cfd[X, Z] <- 1
# gov_cfd[Z, X] <- 1
# gov_grph <- graphModel(gov_adj, gov_cfd)

# define standard fairness model graph
X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "salary" # outcome

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
gov_ada <- fairadapt(salary ~ ., train.data = gov_trn,
                     adj.mat = gov_adj, prot.attr = X)

# adapt data for spurious effect
gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                  dimnames = rep(list(cols), 2))

gov_adj[Z, Y] <- 1
gov_adj[W, Y] <- 1
gov_adj[X, c(Z, W, Y)] <- 1
# gov_cfd[X, Z] <- 1
# gov_cfd[Z, X] <- 1
# gov_grph <- graphModel(gov_adj, gov_cfd)


# adapt data
gov_ada_se <- fairadapt(salary ~ ., train.data = gov_trn,
                     adj.mat = gov_adj, prot.attr = X)

data_fair <- gov_ada$adapt.train
data_fair$sex <- data_unfair$sex

data_fair_se <- gov_ada_se$adapt.train
data_fair_se$sex <- data_unfair$sex


## Clustering ## 

# Perform vanialla clustering
result <- kproto(x = data_unfair, k = num_clusters)
data_unfair$cluster_vanilla <- result$cluster

# Perform fairness through awareness clustering
result <- kproto(x = data_unfair[,-1], k = num_clusters)
data_unfair$cluster_unaware <- result$cluster

# Perform causally fair clustering
result <- kproto(x = data_fair[-2], k = num_clusters)
data_unfair$cluster_fair <- result$cluster

# Perform causally fair clustering including spurious effect
result <- kproto(x = data_fair_se[-2], k = num_clusters)
data_unfair$cluster_fair_se <- result$cluster

# balanced clustering

# Convert all character variables to factor and convert all factor variables to integer
df <- as.data.frame(data_unfair[,c(1:17)])
df[] <- lapply(df, function(x) if(is.character(x)) as.factor(x) else x)
df[] <- lapply(df, function(x) if(is.factor(x)) as.integer(x) else x)
# Convert all integer variables to factor and convert all factor variables to integer
df[] <- lapply(df, function(x) if(is.integer(x)) as.factor(x) else x)
df[] <- lapply(df, function(x) if(is.factor(x)) as.integer(x) else x)

df_test <- df
df_temp <- cbind(1:nrow(df_test), df_test)
colnames(df_temp) <- paste0("V", 1:ncol(df_temp)-1)
transformed_data <- as.matrix(df_temp)

temp_results <- balance_cluster(transformed_data[,-c(1:2)], transformed_data[,2], num_clusters)
data_unfair$cluster_balanced <- temp_results$cluster

# tic()
# result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=4, kclus=num_clusters, numpos=c(2,8,9,10,11,12,13,14))
# # result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=0, kclus=num_clusters, numpos=c(2:14))
# # result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=0, kclus=num_clusters, numpos=c(12))
# # result <- FairMclus::FairMclus(f=transformed_data, typedata="c", protected="V1", ncores=0, kclus=num_clusters, numpos=c(0))
# toc()
# data_unfair$cluster_balanced <- result$cluster

# calculate fairness measures

# include salary in W
W <- c("salary", "marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators

# causally fair clustering
tvd_fair <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_fair", 
                              x0 = "female", x1 = "male", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Fair Clusters)")

# causally fair clustering with spurious effect
tvd_fair_se <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_fair_se", 
                                 x0 = "female", x1 = "male", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair_se, decompose = "xspec", dataset = "Census 2018 (Fair Clusters with SE)")

# vanialla clustering
tvd_fair2 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_vanilla", 
                               x0 = "female", x1 = "male", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair2, decompose = "xspec", dataset = "Census 2018 (naive vanilla clustering)")

# fairness through unawareness
tvd_fair3 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_unaware", 
                               x0 = "female", x1 = "male", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair3, decompose = "xspec", dataset = "Census 2018 (Neglecting Sex)")

# fairness through balanced clusters
tvd_fair4 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_balanced", 
                               x0 = "female", x1 = "male", nboot1=5)
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
plot_vals_fair5 <- filter_and_assign(tvd_fair_se$measures, "Causally Fair (IE+SE)")
plot_vals_fair4 <- filter_and_assign(tvd_fair4$measures, "Balanced")
plot_vals_fair3 <- filter_and_assign(tvd_fair3$measures, "FtU")
plot_vals_fair2 <- filter_and_assign(tvd_fair2$measures, "Vanilla")
plot_vals_fair <- filter_and_assign(tvd_fair$measures, "Causally Fair (IE)")

# Combine all dataframes
plot_vals <- rbind(plot_vals_fair5, plot_vals_fair4, plot_vals_fair3, plot_vals_fair2, plot_vals_fair)

# Define color scheme
my_colors <- c("#D73027","#ABD9E9", "#4575B4")
my_colors <- c("#D73027", "#FDAE61","#ABD9E9","#708090", "#4575B4")
my_colors <- c("#D73027", "#117777","#708090","#ABD9E9", "#4575B4")


# Preprocess the data for plotting
data_new <- plot_vals
data_new$measure <- factor(data_new$measure, measures)
data_new$Method <- factor(data_new$Method, c("Causally Fair (IE+SE)", "Causally Fair (IE)", "Balanced", "FtU", "Vanilla"))

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

# saveRDS(data_new, "results/census.rds")

# library("extrafont")
# loadfonts()
# pdf("~/Desktop/gov_census_plot.pdf", height = 3.7, width = 6,
#     family = "Arial", paper = "special", onefile = FALSE)
# # family = "Times New Roman", paper = "special", onefile = FALSE)
# op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
# p1
# par(op)
# dev.off()
