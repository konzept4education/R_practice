#回帰の復習するときはこれから始めよう。
library(tidyr)
library(readr)
library(psych)
library(tidyverse)
library(ggplot2)
library(stringr)
library(readxl)
library(magrittr)
library(maps)


#回帰復習
setwd("~/Documents/r_sample/imai/")

#データを集める
pres08 = read.csv("pres08.csv")
polls08 = read.csv("polls08.csv")

#データの差を計算
polls08$margin = polls08$Obama - polls08$McCain
pres08$margin = pres08$Obama - pres08$McCain

#-------------------------------------
#middledateが関数なのでそれをdate関数に直す。
#選挙までの日時を計算

polls08$middate = as.Date(polls08$middate)
polls08$DayToElection = as.Date("2008-11-04") - polls08$middate

#--------------------------------------
#空の値を作って、そこに名前をはめ込む
poll.pred = rep(NA, 51)
st.names = unique(polls08$state)
#↑こうすると重複なく州の名前をゲットできる。

#
# poll.pred
#AL AK AZ AR CA CO CT DC DE FL GA HI ID IL IN IA KS KY LA ME MD MA MI MN MS MO MT NE NV NH NJ NM NY NC ND OH 
#NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA 
#OK OR PA RI SC SD TN TX UT VT VA WA WV WI WY 
#NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA 
#こんな結果になるはず。

#50の州をループさせる
for (i in 1:51) {
  state.data = polls08 %>% filter(state == st.names[i])
  latest = state.data %>% filter(DayToElection == min(DayToElection))
  poll.pred[i] =  mean(latest$margin)
}

#州内で部分集合化して、平均を計算する。


#一般市民からの投票から予測する
pollsus08 = read.csv("./pollsUS08.csv")
pollsus08 = pollsus08 %>% mutate(margin = Obama - McCain)
pollsus08$middate = as.Date(pollsus08$middate)
pollsus08 = pollsus08$DayToElection = as.Date("2008-11-04") - pollsus08$middate


Obama.pred = McCain.pred = rep(NA, 90)

#部分集合を作って週の平均を出す。それを90日間の間に流し込む。
for (i in 1:90) {
  week.data = pollsus08 %>% filter(DayToElection <= (90 -i + 7) & DayToElection > (90 - i))
  Obama.pred[i] = mean(week.data$Obama)
  McCain.pred[i] = mean(week.data$McCain)
}

#グラフの作成
plot(90:1,Obama.pred, type ="b", 
     xlim =c(90,1),
     ylim = c(40,60), 
     col = "blue",
     xlab = "Days To Elections",
     ylab = "support")

lines(90:1, McCain.pred, type="b",
      col ="red")
abline(v=0)

#予測の練習

pollsus08 = read.csv("./pollsUS08.csv")
pollsus08 = pollsus08 %>% mutate(margin = Obama - McCain)
pollsus08$middate = as.Date(pollsus08$middate)
pollsus08$DayToElection = as.Date("2008-11-04") - pollsus08$middate

Obama.pred = McCain.pred = rep(NA, 90)

for (i in 1:90) {
  week.data = pollsus08 %>% filter(DayToElection <= (90 -i + 7) & DayToElection > (90 - i))
  Obama.pred[i] = mean(week.data$Obama)
  McCain.pred[i] = mean(week.data$McCain)
}

plot(90:1,Obama.pred, type ="b", 
     xlim =c(90,1),
     ylim = c(40,60), 
     col = "blue",
     xlab = "Days To Elections",
     ylab = "support")

lines(90:1, McCain.pred, type="b",
      col ="red")
abline(v=0)

#モデルの当てはまりの例え
florida = read.csv("florida.csv")
head(florida)
fit2 = lm(florida$Buchanan00 ~ florida$Perot96)
fit2summary = summary(fit2) 
#Ｒに内蔵された関数で決定係数を出してみる
fit2summary$r.squared

#決定係数を簡単に出してくれる関数を作成
R2 = function(fit) {
  resid = resid(fit) #残差
  y = fitted(fit) + resid #線形化
  TSS = sum((y - mean(y))^2) #平均からの平方和
  SSR = sum(resid^2) #残差平方和
  R2 = (TSS - SSR) / TSS #決定係数の公式
  return(R2)
}


#------残差プロット
plot(fitted(fit2), resid(fit2),
     xlim = c(0,1500), ylim = c(-750, 2500))
abline(h=0)

#外れ値が極めて大きいものがある
#これからすること。外れ値の大きいところを外して、再度当てはめをしてみる。
florida.pb = subset(florida, subset = (county != "PalmBeach"))
fit3 = lm(florida.pb$Buchanan00 ~ florida.pb$Perot96)
R2(fit3)

par(mfrow = c(1,2))
#再度残差プロット
plot(fitted(fit3), resid(fit3),
     xlim = c(0,1500), ylim = c(-750,2500))
abline(h=0)

plot(florida.pb$Perot96, florida.pb$Buchanan00)
abline(fit2, lty="dashed") #削除前
abline(fit3)

#重回帰モデル
social = read.csv("social.csv")
head(social)

#Civic dutyを基準と考える。

levels(social$messages)

#線形回帰モデル
fit = lm(primary2006 ~ messages, data = social)
summary(fit)

# Y = alpha + B1* control + B2 * Hawthorne + B3 * Neighbors
# Controlグループでは a + b = 0.315 + (-0.018) = 0.297となる

unique.message = data.frame(messages = unique(social$messages))

#予測値　この関数を使えば切片プラス、係数の計算がすぐに出来る。
predict(fit, newdata = unique.message)

#でも標本平均はtapply関数を使っても求めることが出来る。
tapply(social$primary2006, social$messages, mean)








