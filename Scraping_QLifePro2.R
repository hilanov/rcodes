library(rvest)

(raw1<-read_html("http://meds.qlifepro.com/disease/list/6961009"))
(drugs1<-raw1 %>% html_nodes("span.name") %>% html_text)
(drugs1<-strsplit(drugs1,"\n"))

lapply(drugs1,"[",1)


(span<-html_nodes(raw1,"a"))

class(span)
str(span)
