library(rvest)

(raw1<-read_html("http://meds.qlifepro.com/detail/3999426G1024/ƒqƒ…ƒ~ƒ‰”ç‰º’‚S‚O‚‚‡ƒVƒŠƒ“ƒW‚OD‚W‚‚k"))
(drugs1<-raw1 %>% html_nodes("body h3"))
(drugs1<-strsplit(drugs1,"\n"))

lapply(drugs1,"[",1)


