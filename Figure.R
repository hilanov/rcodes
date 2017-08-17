library(ggplot2)
library(tidyr)
library(dplyr)

# テストデータの準備
testdata1 <- data.frame(t = seq(0, 10, 0.1)) %>%
  mutate(x1 = sin(t+1)*sqrt(t+1), x2 = sin(t)*sqrt(t)+.5)

testdata2 <- data.frame(t = seq(0, 10, 0.1)) %>%
  mutate(x1 = cos((t+1)*.7)*sqrt((t+1)*.7), x2 = cos(t*.7)*sqrt(t*.7)+.2)

testdata3 <- data.frame(t = seq(0, 10, 0.1)) %>%
  mutate(x1 = sin((t+.5)*1.2)*((t+.5)*1.2)^.3, x2 = sin(t*1.2)*(t*1.2)^.3+.3)


# 散布図の骨格を決める
p1 <- testdata1 %>%
  gather(Variable, var, -t) %>% # t以外の変数をvarにまとめ、列名をVariableにグループ変数として格納する
  ggplot(aes(x = t, y = var))

p2 <- testdata2 %>%
  gather(Variable, var, -t) %>% # t以外の変数をvarにまとめ、列名をVariableにグループ変数として格納する
  ggplot(aes(x = t, y = var))

p3 <- testdata3 %>%
  gather(Variable, var, -t) %>% # t以外の変数をvarにまとめ、列名をVariableにグループ変数として格納する
  ggplot(aes(x = t, y = var))


# colorをVariableにマッピングする
p1 + geom_line(aes(color = Variable))+
  labs(x="Time",y="x1 or x2")+ 
  geom_hline(yintercept=0)+
  annotate("text", label = "SUBJID=001", x = 2.5, y = 3, colour = "black", size = 5)+ 
  theme(legend.position = c(0.75, 0.05), legend.justification = c(1, 0))+
  ylim(-4,4)

p2 + geom_line(aes(color = Variable))+
  labs(x="Time",y="x1 or x2")+ 
  geom_hline(yintercept=0)+
  annotate("text", label = "SUBJID=002", x = 2.5, y = 2.5, colour = "black", size = 5)+ 
  theme(legend.position = c(0.75, 0.05), legend.justification = c(1, 0))+
  ylim(-4,4)

p3 + geom_line(aes(color = Variable))+
  labs(x="Time",y="x1 or x2")+ 
  geom_hline(yintercept=0)+
  annotate("text", label = "SUBJID=003", x = 2.5, y = 2.5, colour = "black", size = 5)+ 
  theme(legend.position = c(0.75, 0.05), legend.justification = c(1, 0))+
  ylim(-4,4)






