# loading the package
library(fairadapt)
library(faircause)
library(ggplot2)

vars <- c("sex", "age", "native_country", "marital_status", "education_num",
          "workclass", "hours_per_week", "occupation", "income")

# initialising the adjacency matrix
adj.mat <- c(
  0, 0, 0, 1, 1, 1, 1, 1, 1, # sex
  0, 0, 0, 1, 1, 1, 1, 1, 1, # age
  0, 0, 0, 1, 1, 1, 1, 1, 1, # native_country
  0, 0, 0, 0, 1, 1, 1, 1, 1, # marital_status
  0, 0, 0, 0, 0, 1, 1, 1, 1, # education_num
  0, 0, 0, 0, 0, 0, 0, 0, 1, # workclass
  0, 0, 0, 0, 0, 0, 0, 0, 1, # hours_per_week
  0, 0, 0, 0, 0, 0, 0, 0, 1, # occupation
  0, 0, 0, 0, 0, 0, 0, 0, 0  # income
)

adj.mat <- matrix(adj.mat, nrow = length(vars), ncol = length(vars),
                  dimnames = list(vars, vars), byrow = TRUE)

# reading in the UCI Adult data
adult <- readRDS(
  system.file("extdata", "uci_adult.rds", package = "fairadapt")
)

n <- nrow(adult) / 2

mod <- fairadapt(income ~ ., train.data = head(adult, n = n),
                 test.data = tail(adult, n = n), prot.attr = "sex",
                 adj.mat = adj.mat, res.vars = "hours_per_week")

adapt.train <- mod[["adapt.train"]]
adapt.test  <- mod[["adapt.test"]]

# ------------------------

gov_dat <- data("gov_census", package = "fairadapt")
gov_dat <- get(gov_dat)

head(gov_dat)


dem <- c("age", "race", "hispanic_origin", "citizenship",
                 "nativity", "economic_region")
fam <- c("marital", "family_size", "children")
edu <- c("education_level", "english_level")
occ <- c("hours_worked", "weeks_worked", "occupation",
                   "industry")
prt <- "sex"
res <- "salary"

cols <- c(dem, fam, edu, occ, prt, res)

gov_adj <- matrix(0, nrow = length(cols), ncol = length(cols),
                                     dimnames = rep(list(cols), 2))


gov_cfd <- gov_adj

gov_adj[dem, c(fam, edu, occ, res)] <- 1
gov_adj[fam, c(edu, occ, res)] <- 1
gov_adj[edu, c(occ, res)] <- 1
gov_adj[occ, res]<-1
gov_adj[prt, c(fam, edu, occ, res)] <- 1
gov_cfd[prt, dem] <- 1
gov_cfd[dem, prt] <- 1
gov_grph <- graphModel(gov_adj, gov_cfd)


gov_dat$salary <- log(gov_dat$salary)

n_samp <- 3000
n_pred <- 5
gov_trn <- head(gov_dat, n = n_samp)
gov_prd <- tail(gov_dat, n = n_pred)



gov_ada <- fairadapt(salary ~ ., train.data = gov_trn,
          adj.mat = gov_adj, prot.attr = prt)

autoplot(gov_ada, when = "after") +
  theme_minimal() +
  ggtitle("Adapted salary density by gender")

autoplot(gov_ada, when = "before") +
  theme_minimal() +
  # theme_bw() +
  ggtitle("Adapted salary density by gender")

data_unfair <- head(gov_dat, 600)
data_fair <- gov_ada$adapt.train
data_fair$sex <- data_unfair$sex


TV <- mean(data_unfair$salary[data_unfair$sex == "male"]) -
  mean(data_unfair$salary[data_unfair$sex == "female"])

TV

TV <- mean(data_fair$salary[data_unfair$sex == "male"]) -
  mean(data_fair$salary[data_unfair$sex == "female"])

TV

X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "salary" # outcome


# decompose the total variation measure
set.seed(2022)
tvd <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
                         x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd, decompose = "xspec", dataset = "Census 2018")

set.seed(2022)
tvd_fair <- fairness_cookbook(data = data_fair, X = X, W = W, Z = Z, Y = Y, 
                         x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018")


# df <- read.csv("yourdata.csv") # Uncomment this if you are loading a CSV file

library(clustMixType)

# Set number of clusters
num_clusters <- 5

# Perform k-prototypes clustering
result <- kproto(x = data_unfair, k = num_clusters)

# Print cluster assignment for each data point
print(result$cluster)

# You can also add these clusters to your original dataframe
data_unfair$cluster_v1 <- result$cluster

# Perform k-prototypes clustering
result <- kproto(x = data_unfair[,-1], k = num_clusters)

# You can also add these clusters to your original dataframe
data_unfair$cluster_v2 <- result$cluster

# Perform k-prototypes clustering
result <- kproto(x = data_fair[-2], k = num_clusters)

# You can also add these clusters to your original dataframe
data_fair$cluster_v3 <- result$cluster



X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("salary", "marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "cluster_v3" # outcome


tvd_fair <- fairness_cookbook(data = data_fair, X = X, W = W, Z = Z, Y = Y, 
                              x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Fair Clusters)")


X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("salary", "marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "cluster_v1" # outcome

tvd_fair2 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
                              x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair2, decompose = "xspec", dataset = "Census 2018 (naive vanilla clustering)")


X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("salary", "marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "cluster_v2" # outcome

tvd_fair3 <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
                              x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair3, decompose = "xspec", dataset = "Census 2018 (Neglecting Sex)")


plot_vals <- tvd_fair3$measures
plot_vals <- rbind(plot_vals[plot_vals$measure=="tv",],plot_vals[plot_vals$measure=="ctfde",], 
      plot_vals[plot_vals$measure=="ctfie",], plot_vals[plot_vals$measure=="ctfse",])
plot_vals$Method <- "FtU"

plot_vals_temp <- plot_vals

plot_vals <- tvd_fair2$measures
plot_vals <- rbind(plot_vals[plot_vals$measure=="tv",],plot_vals[plot_vals$measure=="ctfde",], 
                   plot_vals[plot_vals$measure=="ctfie",], plot_vals[plot_vals$measure=="ctfse",])
plot_vals$Method <- "Vanilla"

plot_vals_temp2 <- plot_vals

plot_vals <- tvd_fair$measures
plot_vals <- rbind(plot_vals[plot_vals$measure=="tv",],plot_vals[plot_vals$measure=="ctfde",], 
                   plot_vals[plot_vals$measure=="ctfie",], plot_vals[plot_vals$measure=="ctfse",])
plot_vals$Method <- "Causally Fair"

plot_vals <- rbind(plot_vals, plot_vals_temp, plot_vals_temp2)

library(hrbrthemes)
library(viridis)

my_colors <- c("#D73027","#91BFDB", "#4575B4")
my_colors <- c("#D73027","#ABD9E9", "#4575B4")

data_new <- plot_vals                             # Duplicate data
data_new$measure <- factor(data_new$measure,     # Reorder factor levels
                         c("tv", "ctfde", "ctfie", "ctfse"))


p1 <- data_new %>%
  ggplot( aes(x=measure, y=value, fill=Method, colour=Method)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.3) +
  geom_hline(aes(yintercept = 0), color = "black") +
  # scale_fill_viridis(discrete = TRUE, alpha=0.6, colors) +
  scale_colour_manual(values=my_colors) +
  scale_fill_manual(values=my_colors) +
  geom_point(pch = 21, position = position_jitterdodge(0.15),cex=0.4, alpha = 0.3)+ ## THIS IS FOR JITTER
  # geom_jitter(size=0.4, alpha=0.3) +
  theme_minimal() +
  theme(
    # legend.position="none",
    # plot.title = element_text(size=11)
  ) +
  # ggtitle("A boxplot with jitter") +
  xlab("Fairness Measure")+
  ylab("Value"); p1

library("extrafont")
loadfonts()
pdf("~/Desktop/gov_census_fair_clusters.pdf", height = 3.7, width = 6,
    family = "Arial", paper = "special", onefile = FALSE)
# family = "Times New Roman", paper = "special", onefile = FALSE)
op <- par(mar = c(5, 4, 0.05, 0.05) + 0.1)
p1
par(op)
dev.off()


write.csv(data_unfair,"/Users/frbayer/Documents/phd_main/projects/fairClust/data_test/data_unfair.csv")



fairlet_data <- read.csv("/Users/frbayer/Documents/phd_main/projects/fairClust/data_test/fairlet_data.csv")

X <- "gender" # protected attribute
Z <- c("age", "fnlwgt") # confounders
W <- c("capital_gain", "hours_per_week", "education_num") # mediators
Y <- "clusters" # outcome

tvd_fair <- fairness_cookbook(data = fairlet_data, X = X, W = W, Z = Z, Y = Y, 
                              x0 = 0, x1 = 1)

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Neglecting Sex)")


