library(rvest)

(raw1<-read_html("http://pha.medicalonline.jp/index/category?from=tmenu&catkind=0&catid=1-12-93-556"))
(drugs1<-raw1 %>% html_nodes(xpath="//table") %>% html_text)
(drugs1<-strsplit(drugs1,"\n"))

lapply(drugs1,"[",2)
