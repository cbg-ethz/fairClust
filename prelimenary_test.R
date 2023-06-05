# loading the package
library(fairadapt)
library(ggplot2)
library(faircause)

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

n_samp <- 30000
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
  ggtitle("Unadapted salary density by gender")

data_unfair <- head(gov_dat, 30000)
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

# set.seed(2022)
# tvd <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
#                          x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd, decompose = "xspec", dataset = "Census 2018")

# set.seed(2022)
# tvd_fair <- fairness_cookbook(data = data_fair, X = X, W = W, Z = Z, Y = Y, 
#                          x0 = "female", x1 = "male")

# # visualize the x-specific measures of direct, indirect, and spurious effect
# autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018")


data_unfair


# df <- read.csv("yourdata.csv") # Uncomment this if you are loading a CSV file

# Set number of clusters
num_clusters <- 5

# Perform k-prototypes clustering
result <- kproto(x = data_unfair, k = num_clusters)

# Print cluster assignment for each data point
print(result$cluster)

# You can also add these clusters to your original dataframe
data_unfair$cluster_v1 <- result$cluster

# Perform k-prototypes clustering
result <- kproto(x = data_unfair[-1], k = num_clusters)

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

tvd_fair <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
                              x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (naive vanilla clustering)")


X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("salary", "marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "cluster_v2" # outcome

tvd_fair <- fairness_cookbook(data = data_unfair, X = X, W = W, Z = Z, Y = Y, 
                              x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd_fair, decompose = "xspec", dataset = "Census 2018 (Neglecting Sex)")


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


