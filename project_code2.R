# tail -n +2 Sales10.txt > Sales10t.txt
# echo 'TSEKKID,KP,TOOTEID,KAMPID,KLIENDIKAARDIKOOD,KOGUS' | cat - Sales10t.txt > temp && mv temp Sales10t.txt
# Võibolla teha kaks eraldi lahendust, esiteks selekteerida ka need, kes ostavad tihti soodukaid ja 
# neile reklaamida rohkem. Lisaks mis kategooriaid nad just eelistavad. Või jagada kliendikaardiomanikud
# katoegooriatesse, mis soovitusi and just eelistatult saavad

library(data.table)
library(dplyr)
library(caret)
library(stringr)
library(reshape2)

##### data #####
# Big data
sales <- read.table("Sales_all_n.txt", sep = ",", header = T, skipNul = T)
head(sales)

# Products info
products <- read.csv2("products.csv")
products <- data.table(products)

### test data ###
sales10kt <- read.table("Sales10ktn.txt", sep = ",", header = T, skipNul = T)
sales10kt <- data.table(sales10kt)
sum(is.na(sales10kt$KLIENDIKAARDIKOOD))
# 2071
sales10kt <- sales10kt[!is.na(sales10kt$KLIENDIKAARDIKOOD),]
sum(is.na(sales10kt$KLIENDIKAARDIKOOD))

######## Andmete ettevalmistamine #########

# Kuna meie eesmärk on teha soovitussüsteemi, siis esiteks eemaldan kõik inimesed, kellel pole kliendikaarti
sum(is.na(sales$KLIENDIKAARDIKOOD))
# 9614188
sales <- sales[!is.na(sales$KLIENDIKAARDIKOOD),]
sum(is.na(sales10kt$KLIENDIKAARDIKOOD))

# Nüüd on vaja teha one hot encoding ja grupeerida inidiviidid (siis kliendikaardi koodi järgi)
# Teine variant oleks muidugi grupeerida tsekkide järgi ja vaadata, mida võiks soovitada koos osta

# Funktsioon one hot encodingu jaoks 
# NB! evals ja arguments võib segadusse ajada, asja mõte on, et alguses
# käsitletakse argumente sõnedena, aga hiljem, data.table "by" argumendi jaoks peab see tähistama
# muutujat, sest nii antakse tulpade nimesid ette. Siis enne data.tabeli funktsionaalsue kasutamist teeme
# sellest muutuja

dummy_function <- function(data, formula, colname) {
  require(caret)
  require(data.table)
  arguments <- as.list(match.call())
  data <-  as.data.frame(apply(data, 2, as.factor))
  dmy <- dummyVars(formula, data = data)
  data2 <- data.frame(predict(dmy, newdata = data))
  data <- cbind(data[,1], data2)
  colnames(data)[1] <-  colname
  colnames(data)[2:ncol(data)] <- gsub("(\\D+)", "X",
                                       colnames(data)[2:ncol(data)])
  data <- data.table(data)
  by_arg = eval(arguments$colname, data)
  data <- data[, lapply(.SD, sum), by=by_arg]
  return(data)
}

# Esiteks one hot encoding tsekkide järgi
sales10kt_tsekk <- sales10kt %>%
  mutate(ts_kp_kk = paste(TSEKKID, KP, KLIENDIKAARDIKOOD, sep = "|")) %>%
  select(ts_kp_kk, TOOTEID)

sales10kt_tsekk <- dummy_function(sales10kt_tsekk, "~ TOOTEID", "ts_kp_kk")

# Ja siis kliendikaartide järgi
sales10kt_kkaart <- sales10kt %>%
  select(KLIENDIKAARDIKOOD, TOOTEID)

sales10kt_kkaart <- dummy_function(sales10kt_kkaart, "~ TOOTEID", "KLIENDIKAARDIKOOD")


#############################################################################
#### See on lihtsalt tõestuseks, et mu kirjutatud funkstioon tõepoolest töötab!
##### Kontroll, kas TOOTEID-d ja TSEKKID-d klappivad ######
#### TSEKKID ######
# Lihsalt 10 reaga tabel, milles iga tseki kohta on 2 toodet
kontroll_tsekk <- head(sales10kt_tsekk[rowSums(sales10kt_tsekk[, 2:ncol(sales10kt_tsekk)]) == 2, ], 10)
kontroll_tsekk <- as.data.frame(kontroll_tsekk)
# Võtan välja tulbad, mille summa on üle 1, +1 on seal sellepärast, et alustasin 2. pos 
# ja seega vektor on 1 võrra nihkes
kontroll_tsekk <- kontroll_tsekk[, c(1, grep(TRUE, colSums(kontroll_tsekk[, 2:ncol(kontroll_tsekk)]) > 0) +1)]
kontroll_tsekk <- melt(kontroll_tsekk) %>%
  filter(value > 0) %>%
  group_by(ts_kp_kk) %>%
  summarise(products = paste(variable, collapse = "|")) %>%
  mutate(tsekk = str_split(ts_kp_kk, "\\|", simplify = T)[, 1]) 

# ja nüüd võrdlen originaaliga, kas tooted ja tsekid klapivad
kontroll_original <- sales10kt[sales10kt$TSEKKID %in% kontroll_tsekk$tsekk,] %>%
  select(TSEKKID, TOOTEID) %>%
  group_by(TSEKKID) %>%
  summarise(products_original = paste(TOOTEID, collapse = "|"))
kontroll_tsekk <- cbind(kontroll_tsekk, kontroll_original)
# Jep, õigele tsekile vastavad õiged tooted

###### KLIENDIKAARDIKOOD ######
# Lihsalt 10 reaga tabel, milles iga tseki kohta on 2 toodet
kontroll_kkaart <- head(sales10kt_kkaart[rowSums(sales10kt_kkaart[, 2:ncol(sales10kt_kkaart)]) == 2, ], 10)
kontroll_kkaart <- as.data.frame(kontroll_kkaart)
# Võtan välja tulbad, mille summa on üle 1, +1 on seal sellepärast, et alustasin 2. pos 
# ja seega vektor on 1 võrra nihkes
kontroll_kkaart <- kontroll_kkaart[, c(1, grep(TRUE, colSums(kontroll_kkaart[, 2:ncol(kontroll_kkaart)]) > 0) +1)]
kontroll_kkaart <- melt(kontroll_kkaart) %>%
  filter(value > 0) %>%
  group_by(KLIENDIKAARDIKOOD) %>%
  summarise(products = paste(variable, collapse = "|"))

# ja nüüd võrdlen originaaliga, kas tooted ja tsekid klapivad
kontroll_original <- sales10kt[sales10kt$KLIENDIKAARDIKOOD %in% kontroll_kkaart$KLIENDIKAARDIKOOD,] %>%
  select(KLIENDIKAARDIKOOD, TOOTEID) %>%
  group_by(KLIENDIKAARDIKOOD) %>%
  summarise(products_original = paste(TOOTEID, collapse = "|"))
kontroll_original <- kontroll_original[match(kontroll_kkaart$KLIENDIKAARDIKOOD, kontroll_original$KLIENDIKAARDIKOOD),]
kontroll_kkaart <- cbind(kontroll_kkaart, kontroll_original)
rm(kontroll_original)
# Ka sama!
##############################################################################################
