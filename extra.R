#Atividade 3.3 - EXTRA
#Natasha Nuto Smidt (232014852)

#Questão 1

lista <- vector("list", 1000)
caso1 <- vector("numeric", 1000)
caso2 <- vector("numeric", 1000)

media <- runif(1, min=100, max=200)
desvio <- 0.10 * media

for (i in 1:1000) {
  sample <- rnorm(n = 15, mean = media, sd = desvio)
  lista[[i]] <- sample
  
  
  ks1 <- ks.test(sample, "pnorm", mean = media, sd = desvio)
  caso1[i] <- ks1$statistic
  
  estmedia <- mean(sample)
  estdesvio <- sd(sample)
  ks2 <- ks.test(sample, "pnorm", mean = estmedia, sd = estdesvio)
  caso2[i] <- ks2$statistic
}


quartis1 <- quantile(caso1,probs = c(0.80,0.85,0.90,0.95,0.975,0.99))
quartis2 <- quantile(caso2, probs = c(0.80,0.85,0.90,0.95,0.975,0.99))


#Tabela 13 do Conover - Komolgorov-Smirnoff

conover80 <- 0.266
conover90 <- 0.304
conover95 <- 0.338
conover98 <- 0.377
conover99 <- 0.404



#Questão 2

sw <- vector("numeric", 1000)
ad <- vector("numeric", 1000)

for (i in 1:1000) {
  testesw <- shapiro.test(sample)
  sw[i] <- testesw$statistic
  
  testead <- ad.test(sample)
  ad[i] <- testead$statistic
}

#Tabela 17 do Conover - Shapiro-Wilk
swconover1 <- 0.835
swconover2 <- 0.855
swconover5 <- 0.881
swconover10 <- 0.901
swconover50 <- 0.950
swconover90 <- 0.975
swconover95 <- 0.980
swconover98 <- 0.984
swconover99 <- 0.987


#Questão 3

lista2 <- vector("list", 1000)
sw2 <- vector("numeric", 1000)
ad2 <- vector("numeric", 1000)

for (i in 1:1000) {
  sample2 <- rnorm(n = 2000, mean = media, sd = desvio)
  lista2[[i]] <- sample2
  
  testesw2 <- shapiro.test(sample2)
  sw2[i] <- testesw2$statistic
  
  testead2 <- ad.test(sample2)
  ad2[i] <- testead2$statistic
}

