# Helper functions

clients_per_day = function(sales, sel_month){
  if(sel_month=="all"){
    # plot by month
    sales %>% group_by(month) %>% summarise(clients = n_distinct(KLIENDIKAARDIKOOD)) -> all
    all$month <- factor(all$month, levels=month.name)
    p <- ggplot(all) + geom_bar(aes(x=month, y=clients), stat="identity", fill="darkblue") +  ylab("Total number of clients") + xlab("Month") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust=.5)) + scale_fill_discrete(guide=FALSE)
  }
  else{
    sales %>% filter(month==sel_month) %>%
      group_by(KP, day) %>% summarise(clients = n_distinct(KLIENDIKAARDIKOOD)) -> m
    m$day <- factor(m$day, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    p <- ggplot(m, aes(x=KP, y=clients, fill=day)) + geom_bar(stat="identity") +  ylab("Total number of clients") + xlab("Date") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = .5)) + scale_x_date(date_breaks = "1 day") + guides(fill=guide_legend(title="Weekday"))
  }
  return(p)
}

purchases_per_day = function(sales, sel_month, arname){
  if(sel_month=="all"){
    # plot by month
    
    if(arname=="All"){
      sales %>% group_by(month, PRNIMI) %>% summarise(n=n()) %>% data.frame -> all
      all$month <- factor(all$month, levels=month.name)
      all$PRNIMI <- factor(all$PRNIMI)
      all$PRNIMI <- factor(all$PRNIMI, levels=levels(all$PRNIMI)[c(1,3,2)])
      p <- ggplot(all) + geom_bar(aes(x=month, y=n, fill=PRNIMI), stat="identity") + ylab("Number of products purchased") + xlab("Month") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.position="top", legend.title=element_text(size=14)) + guides(fill=guide_legend(title="", title.position="top"))
    }
    else{
      sales %>% filter(ARNIMI==arname) %>% group_by(month, ARNIMI) %>% summarise(n=n()) %>% data.frame -> suball
      suball$month <- factor(suball$month, levels=month.name)
      p <- ggplot(suball) + geom_bar(aes(x=month, y=n, fill=ARNIMI), stat="identity") + ylab("Number of products purchased") + xlab("Month") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.position="top", legend.title=element_text(size=14)) + guides(fill=guide_legend(title="", title.position="top"))
    }
    
  }
  else{
    if(arname=="All"){
      sales %>% filter(month==sel_month) %>% group_by(KP, PRNIMI) %>% summarise(n=n()) %>% data.frame -> all
      all$PRNIMI <- factor(all$PRNIMI)
      all$PRNIMI <- factor(all$PRNIMI, levels=levels(all$PRNIMI)[c(1,3,2)])
      p <- ggplot(all) + geom_bar(aes(x=KP, y=n, fill=PRNIMI), stat="identity") +  ylab("Number of products purchased") + xlab("Date") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.position="top", legend.title=element_text(size=14)) + guides(fill=guide_legend(title="", title.position="top")) + scale_x_date(date_breaks = "1 day")
    }
    else{
      sales %>% filter(month==sel_month & ARNIMI==arname) %>% group_by(KP, ARNIMI) %>% summarise(n=n()) %>% data.frame -> suball
      p <- ggplot(suball) + geom_bar(aes(x=KP, y=n, fill=ARNIMI), stat="identity") + ylab("Number of products purchased") + xlab("Date") + theme_bw() + theme(axis.text.x = element_text(angle = 90, vjust = .5), legend.title=element_text(size=14)) + guides(fill=guide_legend(title="", title.position="top")) + scale_x_date(date_breaks = "1 day")
    }
    
  }
  return(p)
}

client_profile2 <- function(sales, client_id){
  sales %>% filter(KLIENDIKAARDIKOOD==client_id) %>% group_by(ARNIMI) %>% summarise(n=n()) %>% arrange(-n) %>% top_n(20) -> client_data
  p <- ggplot(client_data, aes(x=reorder(ARNIMI, n), y=n)) + geom_bar(stat="identity", fill="darkgreen") + ylab("Times purchased") + xlab("Product") + theme_bw() + coord_flip()
  return(p)
}

client_profile <- function(sales, client_id){
  sales %>% filter(KLIENDIKAARDIKOOD==client_id) %>% group_by(TOOTENIMI) %>% summarise(n=n()) %>% arrange(-n) %>% top_n(20) -> client_data
  p <- ggplot(client_data, aes(x=reorder(TOOTENIMI, n), y=n)) + geom_bar(stat="identity", fill="darkgreen") + ylab("Times purchased") + xlab("Product") + theme_bw() + coord_flip()
  return(p)
}

# predict_product <- function(recc_model, customer_id, product_item, n, bin) {
#   # product_item is model matrix
#   # get the customer profile from the customer-item table
#   c_key = which(product_item@data@Dimnames[[1]]==customer_id)
#   cust_products <- product_item[c_key,]
#   #if(bin){
#   #  sp_products <- as(cust_products, "binaryRatingMatrix")
#   #}
#   #else{
#   #  sp_products <- as(cust_products, 'realRatingMatrix')
#   #}
#   # perform the prediction
#   recom <- predict(recc_model, cust_products, type="topNList", n=n)
#   return(as(recom, "list"))
# }

# Function for retrieving suggestions
predict_product <- function(model, customer, n_recommended = 3, sales_mat, sales_mat_bin) {
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

recommend_client <- function(model, client_id, nr_products, products, sales_mat, sales_mat_bin){
  recc_prod = predict_product(model=model, customer = client_id, n_recommended = nr_products, sales_mat=sales_mat, sales_mat_bin=sales_mat_bin)
  res = products[recc_prod[[1]], c("TOOTENIMI", "SEGNIMI", "ARNIMI", "PRNIMI")]
  rownames(res) = NULL
  return(res)
}

