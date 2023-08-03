# loading the package
library(fairadapt)
library(faircause)
library(clustMixType)
library(FairMclus)
library(ggplot2)
rm(list=ls())


set.seed(321)

# load data
gov_dat <- get(data("gov_census", package = "fairadapt"))
head(gov_dat)

# transform salary data to log scale
gov_dat$salary <- log(gov_dat$salary)

# Set number of clusters
num_clusters <- 3

# select training data
n_samp <- 1000
gov_trn <- head(gov_dat, n = n_samp)

gov_trn$race[gov_trn$race=="white"] <- "white"
gov_trn$race[!gov_trn$race=="white"] <- "other"
gov_trn$race <- as.factor(gov_trn$race)
gov_trn$race <- droplevels(gov_trn$race)

data_unfair$race <- gov_trn$race

# define standard fairness model graph
X <- "race" # protected attribute
Z <- c("age", "sex", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("salary", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "marital" # outcome

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
gov_ada <- fairadapt(marital ~ ., train.data = gov_trn,
                     adj.mat = gov_adj, prot.attr = X)

## Test ##
gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                  dimnames = rep(list(cols), 2))

gov_adj[Z, Y] <- 1
gov_adj[W, Y] <- 1
gov_adj[X, c(Z, W, Y)] <- 1
# gov_cfd[X, Z] <- 1
# gov_cfd[Z, X] <- 1
# gov_grph <- graphModel(gov_adj, gov_cfd)


# adapt data
gov_ada_se <- fairadapt(marital ~ ., train.data = gov_trn,
                        adj.mat = gov_adj, prot.attr = X)


data_unfair <- head(gov_dat, n_samp)
data_fair_se <- gov_ada_se$adapt.train
data_fair_se$race <- gov_trn$race


data_unfair <- head(gov_dat, n_samp)
data_fair <- gov_ada$adapt.train
data_fair$race <- gov_trn$race

## Clustering ## 

data_fair$marital <- NULL
data_fair_se$marital <- NULL
data_unfair$marital <- NULL

# Perform vanialla clustering
result <- kproto(x = data_unfair, k = num_clusters)
data_unfair$cluster_vanilla <- result$cluster

# Perform fairness through awareness clustering
result <- kproto(x = data_unfair[,-1], k = num_clusters)
data_unfair$cluster_unaware <- result$cluster

# Perform causally fair clustering
result <- kproto(x = data_fair[-3], k = num_clusters)
data_fair$cluster_fair <- result$cluster

# Perform causally fair clustering including spurious effect
result <- kproto(x = data_fair_se[-3], k = num_clusters)
data_fair_se$cluster_fair <- result$cluster

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

tic()
result <- FairMclus::FairMclus(f=transformed_data, typedata="m", protected="V1", ncores=4, kclus=num_clusters, numpos=c(2,8,9,10,11,12,13,14))
toc()
data_fair$cluster_balanced <- result$cluster

# calculate fairness measures

# include salary in W
W <- c("salary", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators

# causally fair clustering
tvd_fair <- fairness_cookbook(data = data_fair, X = X, W = W, Z = Z, Y = "cluster_fair", 
                              x0 = "other", x1 = "white", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Fair Clusters)")

# causally fair clustering with spurious effect
tvd_fair_se <- fairness_cookbook(data = data_fair_se, X = X, W = W, Z = Z, Y = "cluster_fair", 
                                 x0 = "other", x1 = "white", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair_se, decompose = "xspec", dataset = "Census 2018 (Fair Clusters with SE)")

# vanialla clustering
tvd_fair2 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_vanilla", 
                               x0 = "other", x1 = "white", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair2, decompose = "xspec", dataset = "Census 2018 (naive vanilla clustering)")

# fairness through unawareness
tvd_fair3 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = "cluster_unaware", 
                               x0 = "other", x1 = "white", nboot1=5)
# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair3, decompose = "xspec", dataset = "Census 2018 (Neglecting Sex)")

# fairness through balanced clusters
tvd_fair4 <- fairness_cookbook(data = data_fair[,c(1:16,18)], X = X, W = W, Z = Z, Y = "cluster_balanced", 
                               x0 = "other", x1 = "white", nboot1=5)
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
plot_vals_fair5 <- filter_and_assign(tvd_fair_se$measures, "Causally Fair (SE)")
plot_vals_fair4 <- filter_and_assign(tvd_fair4$measures, "Balanced")
plot_vals_fair3 <- filter_and_assign(tvd_fair3$measures, "FtU")
plot_vals_fair2 <- filter_and_assign(tvd_fair2$measures, "Vanilla")
plot_vals_fair <- filter_and_assign(tvd_fair$measures, "Causally Fair")

# Combine all dataframes
plot_vals <- rbind(plot_vals_fair5, plot_vals_fair4, plot_vals_fair3, plot_vals_fair2, plot_vals_fair)

# Define color scheme
my_colors <- c("#D73027","#ABD9E9", "#4575B4")
my_colors <- c("#D73027", "#FDAE61","#ABD9E9","#708090", "#4575B4")


# Preprocess the data for plotting
data_new <- plot_vals
data_new$measure <- factor(data_new$measure, measures)
data_new$Method <- factor(data_new$Method, c("Causally Fair", "Causally Fair (SE)", "Balanced", "FtU", "Vanilla"))

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

