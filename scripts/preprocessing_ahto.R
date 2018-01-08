# tail -n +2 Sales10.txt > Sales10t.txt
# echo 'TSEKKID,KP,TOOTEID,KAMPID,KLIENDIKAARDIKOOD,KOGUS' | cat - Sales10t.txt > temp && mv temp Sales10t.txt

# getwd()
# setwd("/dm_project2017/scripts")

library(data.table)
library(dplyr)
library(stringr)
library(recommenderlab)
library(ggplot2)

# Data
sales <- read.table("/dm_project2017/data/Sales.tsv", sep = "\t")

# Preprocessing
# Exclude clients without client card
# Exclude tobacco products
# Exclude products that are not in products table
# Exclude products that are bought less than 1000 times a year
# Exclude too frequent cosutmers (>365 visits) because they are not regular clients
# Exclude too infrequent clients (<12)
# Keep only food and primary prodcuts
# Keep only 10k most frequent buyers

products <- read.csv2("/dm_project2017/data/products.csv")
head(products)

sales <- read.table("/dm_project2017/data/Sales.tsv", sep = "\t")
#load(file="/dm_project2017/data/sales_filtered.Rdata")
head(sales)
dim(sales)
# 51959048 X 6

#  Exclude clients without client card
initial_nrow <- nrow(sales)
sum(is.na(sales$KLIENDIKAARDIKOOD))
# 9614188
sales_flt <- sales[!is.na(sales$KLIENDIKAARDIKOOD),]
sum(is.na(sales_flt$KLIENDIKAARDIKOOD))
# 0
initial_nrow - nrow(sales_flt)
# 9614188
nrow(sales_flt)
# 42344860

# Exclude tobacco products (TRNIMI == Tubakatooted)
head(products)
levels(products$PRNIMI)
levels(products$TRNIMI)

smoke <- levels(products$TRNIMI)[169]
smoke
products_smoke <- products %>%
  filter(TRNIMI == smoke)
smokeID <- products_smoke$TOOTEID
smokeID[1:10]
products_smoke$TOOTEID[1:10]

# vector of tobacco product codes
initial <- nrow(sales_flt)
sales_flt <- sales_flt %>%
  filter(!(TOOTEID %in% smokeID))
initial - nrow(sales_flt)
# 559133

# Exclude too frequnt (>365) infrequent (<12) clients
clients <- sales_flt %>%
  group_by(KLIENDIKAARDIKOOD, TSEKKID) %>%
  summarise(count = n()) %>%
  arrange(count) %>%
  select(KLIENDIKAARDIKOOD, TSEKKID) %>%
  group_by(KLIENDIKAARDIKOOD) %>%
  summarise(tsekk_count = n()) %>%
  arrange(tsekk_count)

tail(clients)

# Interesting data
# KLIENDIKAARDIKOOD tsekk_count
# 1             27359         930
# 2             77381         977
# 3            234478        1056
# 4             18520        1071
# 5              3034        1382
# 6              8138       24964

clients_interesting <- clients %>%
  filter(tsekk_count >= 12 & tsekk_count <= 365)
nrow(clients_interesting)
# 76134
# tail(clients_interesting)

sales_flt <- sales_flt %>%
  filter(KLIENDIKAARDIKOOD %in% clients_interesting$KLIENDIKAARDIKOOD)

nrow(sales_flt)
# 37916008

# Exclude products that do not belong to food or primary goods list
initial_nrow <- nrow(sales_flt)
interesting_products <- levels(products$PRNIMI)[c(3, 6, 10)]
interesting_products <- products %>%
  filter(PRNIMI %in% interesting_products)

nrow(interesting_products)
# 61765

sales_flt <- sales_flt %>%
  filter(TOOTEID %in% interesting_products$TOOTEID)

initial_nrow - nrow(sales_flt)
# 3785439
nrow(sales_flt)
# 34130569

# Exclude products that have been bought only once
products_count <- sales_flt %>%
  group_by(TOOTEID) %>%
  summarise(count = n(), count_kogus = sum(KOGUS)) %>%
  arrange(count)

tail(products_count)

# plot, x-axis is log10
ggplot(products_count, aes(count)) + geom_density(fill = "lightblue") + theme_bw() +
  labs(title = "Product Count Distribution", x = "Count", y = "Density") +
  scale_x_log10() 

nrow(products_count)
# 29453
nrow(products_count[products_count$count >= 1, ])
# > 1: 29255
# >= 10: 24721
# >= 100: 15497
# >= 1000: 5767

# Data that includes products that are bought more than 1000 times a year
sales_flt1000 <- sales_flt %>%
  filter(TOOTEID %in% products_count[products_count$count >= 1000, ]$TOOTEID)

# Data that includes products that are bought more than 100 times a year
sales_flt100 <- sales_flt %>%
  filter(TOOTEID %in% products_count[products_count$count >= 100, ]$TOOTEID)


# 10 000 most frequent buyers
sales_flt1000_top_user <- sales_flt1000 %>%
  group_by(KLIENDIKAARDIKOOD) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

head(sales_flt1000_top_user)
top10k_users <- sales_flt1000_top_user$KLIENDIKAARDIKOOD[1:10000]
top10k_users[1:10]

sales_flt1000top <- sales_flt1000 %>%
  filter(KLIENDIKAARDIKOOD %in% top10k_users)
nrow(sales_flt1000top)
# 12098852

# Wide data formatt (one hot encoding or dummy variables)
# sales_flt1000
head(sales_flt1000top)
sales_flt1000top_kk <- sales_flt1000top %>% select(KLIENDIKAARDIKOOD, TOOTEID)
class(sales_flt1000top_kk)
sales_flt1000top_kk <- data.table(sales_flt1000top_kk)
class(sales_flt1000top_kk)
head(sales_flt1000top_kk)
nrow(sales_flt1000top_kk)
# 12098852

# sales_flt100 (won't be used)
# sales_flt100_kk <- sales_flt100 %>% select(KLIENDIKAARDIKOOD, TOOTEID)
# sales_flt100_kk <- data.table(sales_flt100_kk)
# class(sales_flt100_kk)
# head(sales_flt100_kk)
# 
# start <- Sys.time()
# sales_flt1000top_dmy <- data.table::dcast(sales_flt1000top_kk, KLIENDIKAARDIKOOD ~ TOOTEID, fun = length)
# Sys.time() - start
# 
# start <- Sys.time()
# sales_flt100_dmy <- data.table::dcast(sales_flt100_kk, KLIENDIKAARDIKOOD ~ TOOTEID, fun = length)
# Sys.time() - start

# Won't use quantities either"
# sales_flt1000_kk_kogus <- sales_flt1000 %>% select(KLIENDIKAARDIKOOD, TOOTEID, KOGUS)
# sales_flt1000_kk_kogus <- data.table(sales_flt1000_kk_kogus)
# class(sales_flt1000_kk_kogus)
# head(sales_flt1000_kk_kogus)
# 
# sales_flt100_kk_kogus <- sales_flt100 %>% select(KLIENDIKAARDIKOOD, TOOTEID, KOGUS)
# sales_flt100_kk_kogus <- data.table(sales_flt100_kk_kogus)
# class(sales_flt100_kk_kogus)
# head(sales_flt100_kk_kogus)
# 
# start <- Sys.time()
# sales_flt1000_dmy_kogus <- data.table::dcast(sales_flt1000_kk_kogus, KLIENDIKAARDIKOOD ~ TOOTEID, fun = sum)
# Sys.time() - start
# 
# start <- Sys.time()
# sales_flt100_dmy_kogus <- data.table::dcast(sales_flt100_kk_kogus, KLIENDIKAARDIKOOD ~ TOOTEID, fun = sum)
# Sys.time() - start
# 
# dim(sales_flt1000_dmy)
# dim(sales_flt100_dmy)
# 
# rm(sales_flt100_kk, sales_flt100_kk_kogus, sales_flt1000_kk, sales_flt1000_kk_kogus)

# Savings
# setwd("/home/ahto/")
# getwd()
# setwd("/dm_project2017/data/")
# getwd()
save(sales_flt, file="sales_flt.RData")
save(sales_flt1000, file="sales_flt1000.RData")
save(sales_flt1000_dmy, file="sales_flt1000_dmy.RData")
save(sales_flt1000_dmy_kogus, file="sales_flt1000_dmy_kogus.RData")
save(sales_flt100, file="sales_flt100.RData")
save(sales_flt100_dmy, file="sales_flt100_dmy.RData")
save(sales_flt100_dmy_kogus, file="sales_flt100_dmy_kogus.RData")

### Newer version 10k X 5.7K
getwd()
setwd("/dm_project2017/data")
ls("/dm_project2017/data")
sales_flt1000top_dmy[1:10, 1:10]
dim(sales_flt1000top_dmy)
save(sales_flt1000top_dmy, file = "sales_flt1000top_dmy.RData")
save(sales_flt1000top, file = "sales_flt1000top.RData")
save(sales_flt, file="sales_flt.RData")
