library(rvest)
library(stringr)
library(magrittr)
library(dplyr)

D05<-read.csv("D05.csv")
D05<-D05[,c(-1)]
# D05<-D05[!str_detect(D05$X.1,"dg:"),]
D05<-D05[!str_detect(D05$X1,"[A-C]"),]
D05<-D05[!str_detect(D05$X1,"[E-Z]"),]
D05<-D05[str_length(D05$X1)>3,]


for(i in 1:nrow(D05)){
  for(j in 1:ncol(D05)){
    if(str_detect(D05[i,j],"D")==0) D05[i,j]=""
  }
}

(D05vec<-as.vector(as.matrix(D05)))
(D05vec<-D05vec[D05vec!="" & !is.na(D05vec)])
(D05vec<-unique(D05vec))

(URL_D05<-paste("http://www.kegg.jp/medicus-bin/similar_product?kegg_drug=",D05vec,sep=""))

INFO_D05<-list()
tbl_D05<-list()

for(i in 1:length(URL_D05)){
  INFO_D05[[i]]<-read_html(URL_D05[i])
  tbl_D05[[i]]<-INFO_D05[[i]] %>% html_table(fill=T)
}

(tbl_D05_2<-lapply(tbl_D05,"[",2))



