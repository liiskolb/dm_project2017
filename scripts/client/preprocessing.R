# Data preprocessing for app
library(stringr)

load("/dm_project2017/data/sales_flt1000top.RData")

products = read.csv2("/dm_project2017/data/products.csv")
products$TRNIMI = unlist(lapply(products$TRNIMI, function(x) str_trim(x)))
products$PRNIMI = unlist(lapply(products$PRNIMI, function(x) str_trim(x)))
products$ARNIMI = unlist(lapply(products$ARNIMI, function(x) str_trim(x)))
products$SEGNIMI = unlist(lapply(products$SEGNIMI, function(x) str_trim(x)))
products$TOOTENIMI = unlist(lapply(products$TOOTENIMI, function(x) str_trim(x)))
save(products, file="/dm_project2017/data/products.RData")

load("/dm_project2017/data/products.RData")
sales_flt1000top$KP = as.Date(as.character(sales_flt1000top$KP), "%Y%m%d")
sales_flt1000top%>% mutate(month = months(KP), year = format(KP, "%Y")) -> sales_flt1000_months
sales_flt1000_months$month <- factor(sales_flt1000_months$month, levels=month.name)


sales_flt1000_months2 = merge(sales_flt1000_months, products, by="TOOTEID")
sales_flt1000_months2$day = weekdays(as.Date(sales_flt1000_months2$KP))

# Data loaded to application
save(sales_flt1000_months2, file="/dm_project2017/data/sales_flt1000_months2.RData")

# Purchase image
load("/dm_project2017/data/sales_flt.RData") # without 1000 filtering

products_count <- sales_flt %>%
  group_by(TOOTEID) %>%
  summarise(count = n(), count_kogus = sum(KOGUS)) %>%
  arrange(count)

# graafik, x telg on log10
ggplot(products_count, aes(count)) + geom_density(fill = "lightblue") + theme_bw() +
  labs(title = "Product Count Distribution", x = "Count", y = "Density") + scale_x_log10()

ggplot(products_count, aes(count,..density..)) + geom_histogram(fill = "blue", breaks=seq(1, 70000,1000)) + theme_bw() +
  labs(title = "Product Count Distribution", x = "Count", y="Density") 


ggplot(products_count, aes(count)) + geom_histogram(fill = "blue", breaks=seq(1, 70000,1000)) + theme_bw() +
  labs(title = "Product Count Distribution", x = "Count", y="Count") 


sales_flt1000_months2 %>% group_by(TOOTENIMI) %>% summarise(n=n()) %>% arrange(-n) %>% top_n(10) -> t

ggplot(t, aes(x=reorder(TOOTENIMI,n), y=n)) + geom_bar(stat="identity", fill=colors()[371]) + theme_bw() + coord_flip() + xlab("") + ylab("Number of purchases") + scale_y_continuous(labels=function(n){format(n, scientific = FALSE)}) + 
  theme(text = element_text(size=18))
ggsave("/dm_project2017/data/popularproducts.png", width = 10)


## ROC curves

load("/dm_project2017/data/list_results.RData")
load("/dm_project2017/data/list_results_bin.RData")

#png(filename="/dm_project2017/data/ROC_UBCF.png")
#plot(list_results, annotate = 1, legend = "topleft")
#title("ROC curve")
#dev.off()
#plot(list_results, "prec/rec", annotate = 1, legend = "bottomright")
#title("Precision-recall")

png(filename="/dm_project2017/scripts/client/www/ROC.png")
plot(list_results, annotate = 1, legend = "topleft")
title("ROC curve")
dev.off()

png(filename="/dm_project2017/scripts/client/www/ROC_bin.png")
plot(list_results_bin, annotate = 1, legend = "topleft") 
title("ROC curve (binary)")
dev.off()

png(filename="/dm_project2017/scripts/client/www/prec_bin.png")
plot(list_results_bin, "prec/rec", annotate = 1, legend = "bottomright")
title("Precision-recall (binary)")
dev.off()

png(filename="/dm_project2017/scripts/client/www/prec.png")
plot(list_results, "prec/rec", annotate = 1, legend = "topright")
title("Precision-recall")
dev.off()