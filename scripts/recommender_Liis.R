library(recommenderlab)
library(Matrix)
library(reshape2)
library(tidyr)
library(data.table)
library(stringr)

data = read.table("/dm_project2017/data/Sales.tsv", sep="\t")
products = read.csv2("/dm_project2017/data/products.csv")
products$PRNIMI = unlist(lapply(products$PRNIMI, function(x) str_trim(x)))

# Filter out users without client number
sales = data[!is.na(data$KLIENDIKAARDIKOOD),] # removed 9614188 rows
dim(sales) # 42344860 X 6

# Filter out products without info in products
sales = sales[sales$TOOTEID %in% products$TOOTEID,] # removed 2229169 rows

# Remove items only bought once

sales = sales[with(sales, ave(TOOTEID,TOOTEID,FUN=length))>1, ] # removed 2560 rows

# Not for sale products
notforsale = products[products$PRNIMI=="Not for sale","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% notforsale),] # removed 1185955 rows

# Seasonal products
seasonal = products[products$PRNIMI=="Hooaeg","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% seasonal),] # removed 83611 rows

# Teenused (loto)
teenus = products[products$PRNIMI=="Teenus","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% teenus),] # removed 118 rows

# Ehituskaup
ehitus = products[products$PRNIMI=="Ehituskaup","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% ehitus),] # removed 13791 rows

# Garderoob  
garderoob = products[products$PRNIMI=="Garderoob","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% garderoob),] # removed 189586 rows

# Autokaubad
auto = products[products$PRNIMI=="Autokaup","TOOTEID"]
sales = sales[!(sales$TOOTEID %in% auto),] # removed 17555 rows

# Save filtered data
save(sales, file="/dm_project2017/data/sales_filtered.Rdata")
#load(file="/dm_project2017/data/sales_filtered.Rdata") # sales data


liis = sales[sales$KLIENDIKAARDIKOOD==5036,]
# Create 1-hot matrix of users and products purchased

mat = data.table::dcast(sales, KLIENDIKAARDIKOOD ~ TOOTEID, length)

dummy_function <- function(data, formula, colname) {
  require(caret)
  require(data.table)
  arguments <- as.list(match.call())
  data <-  as.data.frame(apply(data, 2, as.factor))
  dmy <- dummyVars(formula, data = data)
  data2 <- data.frame(predict(dmy, newdata = data))
  data <- cbind(data[,1], data2)
  colnames(data)[1] <- colname
  colnames(data)[2:ncol(data)] <- gsub("(\\D+)", "X",
                                       colnames(data)[2:ncol(data)])
  data <- data.table(data)
  by_arg = eval(arguments$colname, data)
  data <- data[, lapply(.SD, sum), by=by_arg]
  return(data)
}