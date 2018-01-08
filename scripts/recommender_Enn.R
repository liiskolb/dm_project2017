library(data.table)
library(ggplot2)
library(dplyr)
library(tidyr)
library(recommenderlab)


getwd()
setwd('/dm_project2017/data/')
data = read.table("/dm_project2017/data/Sales.tsv", sep="\t")
head(data)
colnames(data)[5] <- "user"
dim( subset(data, is.na(user)))
data<- subset(data, !is.na(user))

#Ostja kohta oste
user.oste <- data %>% group_by(user, TSEKKID ) %>%
  summarise(oste=n()) %>% group_by(user ) %>%
  summarise(oste=n())

head(user.oste)
colnames(sales)[5] <- "user"
head(sales)
ostja.un <- unique(sales$user)
toode.un <- unique(sales$TOOTEID)


ggplot(user.oste, aes(x=oste)) +
  geom_histogram(fill="#7ba367", color='white', bins = 20) + theme_bw() +   scale_x_log10()

#Liiga sageli ostjaid
ostja.sage<-as.vector(subset(user.oste, oste > 365)$user)
sales.1 <- subset(sales, !(sales$user %in%  ostja.sage ))

#Liiga harva ostjaid
ostja.harv <- as.vector(subset(user.oste, oste < 100)$user)
sales.1 <- subset(sales.1, !(sales.1$user %in%  ostja.harv ))

head(sales.1)

#Ostja kohta oste kohandatud
user.oste <- sales.1 %>% group_by(user, TSEKKID ) %>%
  summarise(oste=n()) %>% group_by(user ) %>%
  summarise(oste=n())

ggplot(user.oste, aes(x=oste)) +
  geom_histogram(fill="#7ba367", color='white', bins = 20) + theme_bw() #+   scale_x_log10()



products = read.csv2("/dm_project2017/data/products.csv")
products$PRNIMI = unlist(lapply(products$PRNIMI, function(x) str_trim(x)))
head(products)
toode.un <- unique(sales.1$TOOTEID)
products <- subset(products, products$TOOTEID %in% toode.un) 
table(products$PRNIMI)

toode.un <- unique(products$TOOTEID)

#Tundmatute toodete eemaldamine müükide tabelist
sales.1 <- subset(sales.1,sales.1$TOOTEID %in%  toode.un)
head(sales.1)

#Ostja toode kogus

ostja.toode.kogus <- sales.1 %>% group_by(user, TOOTEID) %>%
  summarise(kogus=sum(KOGUS))

head(ostja.toode.kogus)

length(unique(ostja.toode.kogus$user))
length(unique(ostja.toode.kogus$TOOTEID))

sales_wide <- spread(ostja.toode.kogus,TOOTEID, kogus)

head(sales_wide)

vector_users <- as.vector(sales_wide$user)

length(vector_users)

sales.matrix <- as.matrix(sales_wide[,-1])

rownames(sales.matrix) <- vector_users
head(sales.matrix[,1:5])
sales.matrix <- as(sales.matrix, "realRatingMatrix")
sales.matrix
sales.matrix.norm <- normalize(sales.matrix, method= "z-score", row=TRUE)
max(sales.matrix@data)
max(sales.matrix.norm@data)

dim(sales.matrix$data)

# visualize the top matrix
min_items <- quantile(rowCounts(sales.matrix), 0.999)
min_users <- quantile(colCounts(sales.matrix), 0.999)

# visualize the matrix
image(sales.matrix[rowCounts(sales.matrix) > min_items,
                     colCounts(sales.matrix) > min_users], main = "Heatmap of the top
users and items")

# visualize the normalized matrix
image(sales.matrix.norm[rowCounts(sales.matrix) > min_items,
                   colCounts(sales.matrix) > min_users], main = "Heatmap of the top
      users and items")

average_amounts_per_user <- rowMeans(sales.matrix)
qplot(average_amounts_per_user) + stat_bin(binwidth = 0.1) +
  ggtitle("Distribution of the average amounts per user")

#Sarnasuste 
similarity.users<- similarity(sales.matrix[1:20,], method = "cosine", which="users")

#as.matrix(similarity.buyers)
image(as.matrix(similarity.users), main = "User similarity")




#Jagamine treening ja testandmeteks
which_train <- sample(x = c(TRUE, FALSE), size = nrow(sales.matrix), replace = TRUE, prob = c(0.8, 0.2))
head(which_train)

recc_data_train <- sales.matrix[which_train, ]
recc_data_test <- sales.matrix[!which_train, ]

#Mudeli tegemine
recc_model <- Recommender(data = recc_data_train, method = "UBCF")
recc_model



#Mudeli kirjeldus
model_details <- getModel(recc_model)
model_details$description

pred <- predict(recc_model, recc_data_test[1:5,], n = 5)
getList(pred)
pred@ratings

getList(bestN(pred, 5))[[1]]

recc_model.2 <- Recommender(data = recc_data_train, method = "UBCF", parameter = list(method = "Euclidean", nn = 10, normalize = "Z-score")                        )
recc_model.2
pred2 <- predict(recc_model.2, recc_data_test[1:5,], n = 5)
getList(pred2)
getList(bestN(pred2, 5))[[1]]



recc_predicted <- predict(recc_model, recc_data_test, n = 10)

recc_matrix <- sapply(recc_predicted@items, function(x){
  colnames(ratings_movies)[x]
})
dim(recc_matrix)
