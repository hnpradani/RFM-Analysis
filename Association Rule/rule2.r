setwd("D:/Jupyter/isfest/category2")
library('arules')
library('arulesViz')

# Load Data
df_aksesoris_wanita <- read.csv('Aksesoris Wanita.csv', head = TRUE, as.is = T)
df_bahan_makanan <- read.csv('Bahan Makanan.csv', head=TRUE, as.is = T)
df_fresh_food <- read.csv('Fresh Food.csv', head = TRUE, as.is = T)
df_kebersihan_diri <- read.csv('Kebersihan Diri.csv', head = TRUE, as.is = T)
df_makanan_instan <- read.csv('Makanan Instan.csv', head = TRUE, as.is = T)
df_makanan_kaleng <- read.csv('Makanan Kaleng.csv', head = TRUE, as.is = T)
df_minuman_ringan <- read.csv('Minuman Ringan.csv', head = TRUE, as.is = T)
df_pakaian_muslim_wanita <- read.csv('Pakaian Muslim Wanita.csv', head = TRUE, as.is = T)
df_pakaian_pria <- read.csv('Pakaian Pria.csv', head = TRUE, as.is = T)
df_pakaian_tidur_wanita <- read.csv('Pakaian Tidur Wanita.csv', head = TRUE, as.is = T)
df_pakaian_wanita <- read.csv('Pakaian Wanita.csv', head = TRUE, as.is = T)
df_vitamin <- read.csv('Vitamin.csv', head = TRUE, as.is = T)

df_pakaian <- rbind(df_pakaian_pria, df_pakaian_wanita)
df_pakaian <- rbind(df_pakaian, df_pakaian_muslim_wanita)
df_pakaian <- rbind(df_pakaian, df_pakaian_tidur_wanita)
df_pakaian <- rbind(df_pakaian, df_aksesoris_wanita)

df_makanan <- rbind(df_makanan_kaleng, df_makanan_instan)
df_makanan <- rbind(df_makanan, df_minuman_ringan)
df_makanan <- rbind(df_makanan, df_vitamin)

df_bahan <- rbind(df_bahan_makanan, df_fresh_food)


tr_pakaian <- as(split(df_pakaian[,"desc_product"], df_pakaian[,"order_id"]), "transactions")
inspect(head(tr_pakaian))

fi_pakaian <- eclat (tr_pakaian, parameter = list(supp = 0.01, maxlen = 15))
inspect(fi_pakaian)

rule_pakaian <- apriori(tr_pakaian, parameter = list(support = 0.00005))

rule_pakaian <- sort(rule_pakaian, by='lift', decreasing = TRUE)
inspect(head(rule_pakaian))

top10pk <- tail(rule_pakaian, n = 5, by = "lift")
plot(top10pk, method = "graph",  engine = "htmlwidget")

ifpk = rev(tail(sort(itemFrequency(tr_pakaian, type='absolute')), 10))
par(mar=c(10,4,4,2))
barplot(ifpk, las=2, cex.names=0.6)

pi_pk <- data.frame(
  item = c("QUEEN CEFA BRACELET LEATHER", "ANNA FAITH LEGGING GLOSSY",
           "EMBA SHORT PANT INATH TWO", "RIDER CELANA DEWASA SPANDEX ANTI BAKTERI R325BW",
           "SHEW BLOUSE MELLY"),
  num = c(438, 366, 334, 310, 270)
)
p <- ggplot(pi_pk, aes(x = item, y = num))+
  geom_col(width = 0.7) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))

p + coord_flip()



tr_pakaian_2 <- tr_pakaian[which(size(tr_pakaian) > 2)]
inspect(head(tr_pakaian_2, 10))

fi_pakaian_2 <- eclat (tr_pakaian_2, parameter = list(supp = 0.01, maxlen = 15))
inspect(fi_pakaian_2)

rule_pakaian_2 <- apriori(tr_pakaian_2, parameter = list(support = 0.0001))

rule_pakaian_2 <- sort(rule_pakaian_2, by='confidence', decreasing = TRUE)
inspect(head(rule_pakaian_2))

top10subRules <- head(rule_pakaian_2, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")

rule3 <- apriori(tr_pakaian, parameter = list(supp=0.00001, conf=0.8,maxlen=10))
rule3



tr_makanan <- as(split(df_makanan[,"desc_product"], df_makanan[,"order_id"]), "transactions")
inspect(head(tr_makanan))

fi_makanan <- eclat (tr_makanan, parameter = list(supp = 0.005, maxlen = 15))
inspect(fi_makanan)

rule_makanan <- apriori(tr_makanan, parameter = list(support = 0.000035))
rule_makanan <- sort(rule_makanan, by='confidence', decreasing = TRUE)
inspect(head(rule_makanan))

top10makanan <- head(rule_makanan, n = 5, by = "lift")
plot(top10makanan, method = "graph",  engine = "htmlwidget")

tr_makanan_2 <- tr_makanan[which(size(tr_makanan) > 2)]
inspect(head(tr_makanan_2, 10))

fi_makanan_2 <- eclat (tr_makanan_2, parameter = list(supp = 0.01, maxlen = 15))
inspect(fi_makanan_2)

rule_makanan_2 <- apriori(tr_makanan_2, parameter = list(support = 0.0001))

rule_makanan_2 <- sort(rule_makanan_2, by='confidence', decreasing = TRUE)
inspect(head(rule_makanan_2))

top10makanan_2 <- head(rule_makanan_2, n = 5, by = "lift")
plot(top10makanan_2, method = "graph",  engine = "htmlwidget")

ifmk = rev(tail(sort(itemFrequency(tr_makanan, type='absolute')), 10))
ifmk
pi_mk <- data.frame(
  item = c("Super Bubur Instant Bergizi Ayam 45G", "Blackmores Odourless Fish Oil 1000 200's",
           "Big Soft Drink Strawberry 3.1L", "Frestea Minuman Teh Lychee 500Ml",
           "Proman Minuman Energi 200Ml"),
  num = c(186, 183, 177, 171, 167)
)
p <- ggplot(pi_mk, aes(x = item, y = num))+
  geom_col(width = 0.7) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))

p + coord_flip()









tr_bahan <- as(split(df_bahan[,"desc_product"], df_bahan[,"order_id"]), "transactions")
inspect(head(tr_bahan))

fi_bahan <- eclat (tr_bahan, parameter = list(supp = 0.005, maxlen = 15))
inspect(fi_bahan)

rule_bahan <- apriori(tr_bahan, parameter = list(support = 0.00001))
rule_bahan <- sort(rule_bahan, by='confidence', decreasing = TRUE)
inspect(head(rule_bahan))

top10bahan <- head(rule_bahan, n = 5, by = "lift")
plot(top10bahan, method = "graph",  engine = "htmlwidget")

ifbm = rev(tail(sort(itemFrequency(tr_bahan, type='absolute')), 10))
ifbm
pi_bm <- data.frame(
  item = c("Sasa Bumbu Masak 250G", 
           "Ajinomoto Bumbu Nasi Goreng Sajiku Ayam 20G",
           "Bonanza Daging Slice Gyudon 300g", 
           "Adib Dory Fillet 1Kg",
           "Indomaret Nata De Coco Selasih Leci 200G"),
  num = c(178, 177, 176, 174, 170)
)
p <- ggplot(pi_bm, aes(x = item, y = num))+
  geom_col(width = 0.7) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))

p + coord_flip()




tr_kebersihan <- as(split(df_kebersihan_diri[,"desc_product"], 
                          df_kebersihan_diri[,"order_id"]), "transactions")

tr_kebersihan <- tr_kebersihan[which(size(tr_kebersihan) > 2)]
inspect(head(tr_kebersihan))

fi_kebersihan <- eclat (tr_kebersihan, parameter = list(supp = 0.005, maxlen = 15))
inspect(fi_kebersihan)

rule_kebersihan <- apriori(tr_kebersihan, parameter = list(support = 0.000025))
rule_kebersihan <- sort(rule_kebersihan, by='confidence', decreasing = TRUE)
inspect(head(rule_kebersihan))

top10kebersihan <- head(rule_kebersihan, n = 5, by = "lift")
plot(top10kebersihan, method = "graph",  engine = "htmlwidget")

itemFrequencyPlot(tr_kebersihan,type="absolute", 
                  topN=10, main="Jumlah Frekuensi Item")


ifk = rev(tail(sort(itemFrequency(tr_kebersihan, type='absolute')), 10))
ifk
pi_k <- data.frame(
  item = c("Vaseline Lotion Healthy White Uv Lightening 200Ml", 
           "Natur-E Hand Body Lotion 245Ml",
           "Antis Pembersih Tangan Antiseptik Spray Jeruk Nipis 55Ml", 
           "Saniter Sanitizing Handwash 250Ml",
           "Laurier Panty Liners 40'S Cleanfresh Non Parfumed"),
  num = c(200, 175, 174, 172, 171)
)
p <- ggplot(pi_k, aes(x = item, y = num))+
  geom_col(width = 0.7) + scale_x_discrete(labels = function(x) str_wrap(x, width = 25))

p + coord_flip()
