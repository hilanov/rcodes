(simple3<-read_html("https://ishidamotohiro.github.io/sample_check/attrs.html"))
simple3 %>% html_nodes(xpath="//p")
simple3 %>% html_nodes(xpath="//a[starts-with(@href,'https')]")
