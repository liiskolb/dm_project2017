# recommneder system
library(recommenderlab)
library(ggplot2)
library(dplyr)
library(shiny)

# setwd("/dm_project2017/scripts/client/")


# Data
load(file="/dm_project2017/data/sales_flt1000top_dmy.RData") 
products <- read.csv2("products.csv")
#load("/Users/ahtosalumets/Downloads/sales_flt1000top_dmy.RData")

####### General information about package recommendeerlab #######
# Recommenderlab data packages:
data_package <- data(package = "recommenderlab")
data_package$results[, "Item"]

# Information about models
recommender_models <- recommenderRegistry$get_entries(dataType =
                                                        "realRatingMatrix")
names(recommender_models)
# "ALS_realRatingMatrix"          "ALS_implicit_realRatingMatrix" "IBCF_realRatingMatrix"        
# "POPULAR_realRatingMatrix"      "RANDOM_realRatingMatrix"       "RERECOMMEND_realRatingMatrix" 
# "SVD_realRatingMatrix"          "SVDF_realRatingMatrix"         "UBCF_realRatingMatrix"   

# Short description
lapply(recommender_models, "[[", "description")

# We will use:
# IBCF_realRatingMatrix - "Recommender based on item-based collaborative filtering."
# UBCF_realRatingMatrix - "Recommender based on user-based collaborative filtering."

# Default parameters
recommender_models$IBCF_realRatingMatrix$parameters # k = 30, method = Cosine, normalize = center, na_as_zero = F
recommender_models$UBCF_realRatingMatrix$parameters # method = cosine, nn = 25, normalize = center

# Ours:
# IBCF - k = 10, method = Cosine & pearson, normalize = Z-score
# UBCF - method = cosine & pearson, nn = 10, normalize = Z-score

################################################################

################### realRatingMatrix data ######################

# Data to realRatingMatrix
head(sales_flt1000top_dmy[, 1:10])
sales_mat <- as.matrix(sales_flt1000top_dmy)
head(sales_mat[, 1:10])
rownames(sales_mat) <- sales_mat[,1]
sales_mat <- sales_mat[, -1]
sales_mat[sales_mat == 0] <- NA
head(sales_mat[, 1:10])
sales_mat <- as(sales_mat, "realRatingMatrix")
sales_mat_bin <- binarize(sales_mat, minRating = 1)

# Characteristics of this data type

# How much is this datatype smaller than acutal the data?
object.size(as(sales_mat, "matrix")) / object.size(sales_mat)
# 8.7X
object.size(as(sales_mat_bin, "matrix")) / object.size(sales_mat_bin)
# 12.6X

class(sales_mat)
# [1] "realRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"
class(sales_mat_bin)
# [1] "binaryRatingMatrix"
# attr(,"package")
# [1] "recommenderlab"

dim(sales_mat)
dim(sales_mat_bin)
# [1] 10000  5767

slotNames(sales_mat)
slotNames(sales_mat_bin)
# [1] "data"      "normalize"

class(sales_mat@data)
# [1] "dgCMatrix"
# attr(,"package")
# [1] "Matrix"

class(sales_mat_bin@data)
# [1] "itemMatrix"
# attr(,"package")
# [1] "arules"

dim(sales_mat_bin@data)
dim(sales_mat@data)
# [1] 10000  5767

# Comparsion of cosine and pearson
similarity_cos <- similarity(sales_mat[1:4, ], method =
                               "cosine", which = "users")
as.matrix(similarity_cos)

similarity_corr <- similarity(sales_mat[1:4, ], method =
                                "pearson", which = "users")
as.matrix(similarity_corr)


# Distribution of values in the data
vector_counts <- as.vector(sales_mat@data)
# unique(vector_counts)

# Because 0 means that the item is not bought then we are interested in everything that is >0.
vector_counts <- vector_counts[vector_counts != 0]
vector_counts <- as.numeric(vector_counts)
vector_counts_table <- table(cut(vector_counts, b = 12))
vector_counts_table <- as.data.frame(vector_counts_table)
head(vector_counts_table)
ggplot(vector_counts_table, aes(x = Var1, y = Freq)) + geom_bar(stat = "Identity") +
  theme_bw() +
  labs(title = "Distribution of the counts", x = "Interval", y = "Count") +
  scale_y_log10()

# Popularity of goods
counts_per_product <- colSums(sales_mat)

table_product_counts <- data.frame(
  products = names(counts_per_product),
  counts = counts_per_product
)

# The most popular product
as.character(products[products$TOOTEID == 227826, 2])
# "Farmi piim 2.5% 1L kilepakk     

table_product_counts <- table_product_counts[order(table_product_counts$counts, decreasing = TRUE), ]

products_top <- products %>%
  filter(TOOTEID %in% table_product_counts$products) %>%
  select(TOOTENIMI, TOOTEID)

products_top <- products_top[match(table_product_counts$products, products_top$TOOTEID), ]
table_product_counts <- cbind(table_product_counts, products_top)
rownames(table_product_counts) <- NULL
#gsub(" {2,}", "", as.character(products_top$TOOTENIMI[1]), perl = T)

table_product_counts$TOOTENIMI <- gsub(" {2,}", "", as.character(products_top$TOOTENIMI), perl = T)

# Plot of the top 10 products
ggplot(table_product_counts[1:10, ], aes(x = reorder(TOOTENIMI, -counts), y = counts)) +
  geom_bar(stat="identity") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  labs(x = "Product name", y = "Count", title = "Number of counts of the top 10 products")

image(sales_mat, main = "Heatmap of the sales matrix")

# How does normalizd data look like?
sales_mat_norm <- normalize(sales_mat, method = "Z-score")
sales_mat_bin <- binarize(sales_mat, minRating = 1)

sales_mat_norm@data[1:10, 1:10]
max(sales_mat_norm@data) #72.87605
min(sales_mat_norm@data) #-0.2904996
mean(sales_mat_norm@data) # 8.557085e-17
quantile(sales_mat_norm@data)
# <sparse>[ <logic> ] : .M.sub.i.logical() maybe inefficient
# 0%        25%        50%        75%       100% 
# -0.2904996 -0.1597620 -0.1343477 -0.1065809 72.8760540 
quantile(sales_mat@data)
sales_mat_bin@data[1:10, 1:10]

# Model names used:
# "UBCF_bin_corr" 
# "UBCF_corr"
# "IBCF_bin_corr"
# "IBCF_corr"
# "UBCF_bin_cos"
# "UBCF_cos"
# "IBCF_bin_cos"
# "IBCF_cos"
# corr - pearson, cos - cosine
set.seed(1)
which_train <- sample(x = c(TRUE, FALSE), size = nrow(sales_mat), replace = TRUE, prob = c(0.8, 0.2))
head(which_train)

sales_mat_train <- sales_mat[which_train, ]
sales_mat_test <- sales_mat[!which_train, ]
sales_mat_bin_train <- sales_mat_bin[which_train, ]
sales_mat_bin_test <- sales_mat_bin[!which_train, ]

# Models
# UBCF
UBCF_cos <- Recommender(data = sales_mat_train[1:nrow(sales_mat_train)], method = "UBCF", parameter = list(nn = 10, normalize = "Z-score", method = "Cosine"))
UBCF_corr <- Recommender(data = sales_mat_train[1:nrow(sales_mat_train)], method = "UBCF", parameter = list(nn = 10, normalize = "Z-score", method = "pearson"))
UBCF_bin_cos <- Recommender(data = sales_mat_bin_train[1:nrow(sales_mat_bin_train)], method = "UBCF", parameter = list(nn = 10, method = "Cosine"))
UBCF_bin_corr <- Recommender(data = sales_mat_bin_train[1:nrow(sales_mat_bin_train)], method = "UBCF", parameter = list(nn = 10, method = "pearson"))

# IBCF
IBCF_cos <- Recommender(data = sales_mat_train[1:nrow(sales_mat_train)], method = "IBCF", parameter = list(k = 10, normalize = "Z-score", method = "Cosine"))
IBCF_corr <- Recommender(data = sales_mat_train[1:nrow(sales_mat_train)], method = "IBCF", parameter = list(k = 10, normalize = "Z-score", method = "pearson"))
IBCF_bin_cos <- Recommender(data = sales_mat_bin_train[1:nrow(sales_mat_bin_train)], method = "IBCF", parameter = list(k = 10, method = "Cosine"))
IBCF_bin_corr <- Recommender(data = sales_mat_bin_train[1:nrow(sales_mat_bin_train)], method = "IBCF", parameter = list(k = 10, method = "pearson"))

save(UBCF_cos, file="UBCF_cos.RData")
save(UBCF_corr, file="UBCF_corr.RData")
save(UBCF_bin_cos, file="UBCF_bin_cos.RData")
save(UBCF_bin_corr, file="UBCF_bin_corr.RData")
save(IBCF_cos, file="IBCF_cos.RData")
save(IBCF_corr, file="IBCF_corr.RData")
save(IBCF_bin_cos, file="IBCF_bin_cos.RData")
save(IBCF_bin_corr, file="IBCF_bin_corr.RData")
save(sales_mat_bin_train, file="sales_mat_bin_train.RData")
save(sales_mat_train, file="sales_mat_train.RData")
save(sales_mat_bin, file="sales_mat_bin.RData")
save(sales_mat, file="sales_mat.RData")

load("UBCF_cos.RData")
load("UBCF_corr.RData")
load("UBCF_bin_cos.RData")
load("UBCF_bin_corr.RData")
load("IBCF_cos.RData")
load("IBCF_corr.RData")
load("IBCF_bin_cos.RData")
load("IBCF_bin_corr.RData")

# model info
model_details_UBCF <- getModel(UBCF_cos)
model_details_UBCF$description
# [1] "UBCF-Real data: contains full or sample of data set"

model_details_IBCF <- getModel(IBCF_cos)
model_details_IBCF$description
# [1] "IBCF: Reduced similarity matrix"

model_details_UBCF$data
# 8013 x 5767 rating matrix of class ‘realRatingMatrix’ with 46332078 ratings.
# Normalized using z-score on rows.

model_details_IBCF$data
# NULL
# IBCF recommends items on the basis of the similarity matrix. It's an eager-learning
# model, that is, once it's built, it doesn't need to access the initial data. For each item,
# the model stores the k-most similar, so the amount of information is small once the
# model is built. This is an advantage in the presence of lots of data.

# Models on the test data

n_recommended <- 6
# recc_predicted <- predict(object = recc_model, newdata = recc_data_test, n = n_recommended)
recc_predicted
## Recommendations as 'topNList' with n = 6 for 449 users.
class(recc_predicted)
## [1] "topNList"
## attr(,"package")
## [1] "recommenderlab"
slotNames(recc_predicted)
## [1] "items" "itemLabels" "n"

# items: This is the list with the indices of the recommended items for each user
# itemLabels: This is the name of the items
# n: This is the number of recommendations

# For instance, these are the recommendations for the first user: 
recc_predicted@items[[1]]
recc_user_1 <- recc_predicted@items[[1]]

# Product names
products_user_1 <- recc_predicted@itemLabels[recc_user_1]
products_user_1

# Function fo retrieving suggestions
predict_product <- function(model, customer, n_recommended = 3) {
  if(grepl("bin", as.character(deparse(substitute(model))))) {
    customer_key <- grep(paste("^", customer, "$", sep = ""), rownames(sales_mat_bin@data))
    cust_products <- sales_mat_bin[customer_key,]
    sp_products <- as(cust_products, "binaryRatingMatrix")
  } else {
    customer_key <- grep(paste("^", customer, "$", sep = ""), rownames(sales_mat@data))
    cust_products <- sales_mat[customer_key,]
    sp_products <- as(cust_products, "realRatingMatrix")
  }
  recom <- predict(model, sp_products, type="topNList", n = n_recommended)
  return(as(recom,"list"))
}

#costumer <- 921
predict_product(IBCF_corr, costumer)


#### evaluation
# cv 
n_fold <- 4
items_to_keep <- 10
rating_threshold <- 1
eval_sets <- evaluationScheme(data = sales_mat, method = "cross-validation",
                              k = n_fold, given = items_to_keep, goodRating = rating_threshold)
rating_threshold <- 0.5
eval_sets_bin <- evaluationScheme(data = sales_mat_bin, method = "cross-validation",
                                  k = n_fold, given = items_to_keep, goodRating = rating_threshold)
# How many is in each set?
size_sets <- sapply(eval_sets@runsTrain, length)
size_sets
size_sets_bin <- sapply(eval_sets_bin@runsTrain, length)
size_sets_bin

models_to_evaluate <- list(
  IBCF_cos = list(name = "IBCF", param = list(method = "cosine", k = 10, normalize = "Z-score")),
  IBCF_cor = list(name = "IBCF", param = list(method = "pearson", k = 10, normalize = "Z-score")),
  UBCF_cos = list(name = "UBCF", param = list(method = "cosine", nn = 10, normalize = "Z-score")),
  UBCF_corr = list(name = "UBCF", param = list(method = "pearson", nn = 10, normalize = "Z-score")),
  random = list(name = "RANDOM", param=NULL)
)

models_to_evaluate_bin <- list(
  IBCF_cos_bin = list(name = "IBCF", param = list(method = "cosine", k = 10)),
  IBCF_cor_bin = list(name = "IBCF", param = list(method = "pearson", k = 10)),
  UBCF_cos_bin = list(name = "UBCF", param = list(method = "cosine", nn = 10)),
  UBCF_corr_bin = list(name = "UBCF", param = list(method = "pearson", nn = 10)),
  random_bin = list(name = "RANDOM", param=NULL)
)

n_recommendations <- c(1, 5, seq(10, 100, 10))
list_results <- evaluate(x = eval_sets, method = models_to_evaluate, n = n_recommendations)
list_results_bin <- evaluate(x = eval_sets_bin, method = models_to_evaluate_bin, n = n_recommendations)

getwd()
setwd("/dm_project2017/data/")
save(list_results, file="list_results.RData")
save(list_results_bin, file="list_results_bin.RData")

avg_matrices <- lapply(list_results, avg)
avg_matrices_bin <- lapply(list_results_bin, avg)
save(avg_matrices, file="avg_matrices.RData")
save(avg_matrices_bin, file="avg_matrices_bin.RData")

avg_matrices_bin$IBCF_cos

# confusion matrix 
plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")

plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall")

plot(list_results_bin, annotate = 1, legend = "topleft") title("ROC curve (bin)")

plot(list_results_bin, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall (bin)")

