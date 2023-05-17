# devtools::install_github("dplecko/CFA")

library(faircause)

census <- head(faircause::gov_census, n = 40000)
# census <- faircause::gov_census

# kmeans(census,4)

TV <- mean(census$salary[census$sex == "male"]) -
  mean(census$salary[census$sex == "female"])

TV

X <- "sex" # protected attribute
Z <- c("age", "race", "hispanic_origin", "citizenship", "nativity", 
       "economic_region") # confounders
W <- c("marital", "family_size", "children", "education_level", "english_level", 
       "hours_worked", "weeks_worked", "occupation", "industry") # mediators
Y <- "salary" # outcome


# decompose the total variation measure
set.seed(2022)
tvd <- fairness_cookbook(data = census, X = X, W = W, Z = Z, Y = Y, 
                         x0 = "female", x1 = "male")

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd, decompose = "xspec", dataset = "Census 2018")

setwd("/Users/frbayer/Documents/phd_main/projects/fairClust")

# read data
data_adult_binarized <- read.csv("data/processed_data/adult_binerized.csv")

cluster_res <- kmeans(data_adult_binarized[,-c(2,4,12,13)],2)

cluster <- cluster_res$cluster-1

combined_data <- cbind(data_adult_binarized,cluster)

TV <- mean(combined_data$cluster[combined_data$sex == 1]) -
  mean(combined_data$cluster[combined_data$sex == 0])

TV

# read data
data_adult_binarized <- read.csv("data/processed_data/adult_binerized.csv")

cluster_res <- kmeans(data_adult_binarized[,-c(2,4,12,13)],2)

cluster <- cluster_res$cluster-1

combined_data <- cbind(data_adult_binarized,cluster)

TV <- mean(combined_data$cluster[combined_data$sex == 1]) -
  mean(combined_data$cluster[combined_data$sex == 0])

TV


X <- "sex" # protected attribute
Z <- c("race", "native.country") # confounders
W <- c("marital.status", "workclass", "education", 
       "hours.per.week", "occupation", "relationship") # mediators
Y <- "target" # outcome


# decompose the total variation measure
set.seed(2022)
tvd <- fairness_cookbook(data = combined_data[,], X = X, W = W, Z = Z, Y = Y, 
                         x0 = 1, x1 = 0)

# visualize the x-specific measures of direct, indirect, and spurious effect
autoplot(tvd, decompose = "xspec", dataset = "Census 2018")


