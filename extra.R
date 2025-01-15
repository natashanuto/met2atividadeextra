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

#Estatísticas de Teste - Caso 1 X Caso 2

casosteste <- data.frame(
  dados = c(caso1, caso2),
  Caso = rep(c("Caso 1", "Caso 2"), each = 1000)
)

#Boxplot

ggplot(casosteste, aes(x = Caso, y = dados, fill = Caso)) +
  geom_boxplot(width = 0.5) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  scale_fill_manual(values = c("Caso 1" = "lightblue", "Caso 2" = "lightgreen")) +
  labs(x = "Caso", y = "Estatística de Teste") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("extra1.png", width = 158, height = 93, units = "mm")
  

quadro <- casosteste %>%
  group_by(Caso) %>%
  summarize(
    `Média` = round(mean(dados), 2),
    `Desvio Padrão` = round(sd(dados), 2),
    `Variância` = round(var(dados), 2),
    `Mínimo` = round(min(dados), 2),
    `1º Quartil` = round(quantile(dados, probs = .25), 2),
    `Mediana` = round(quantile(dados, probs = .5), 2),
    `3º Quartil` = round(quantile(dados, probs = .75), 2),
    `Máximo` = round(max(dados), 2)
  )

#Caso   Média `Desvio Padrão` Variância Mínimo `1º Quartil` Mediana `3º Quartil` Máximo
#<chr>  <dbl>           <dbl>     <dbl>  <dbl>        <dbl>   <dbl>        <dbl>  <dbl>
#1 Caso 1  0.22            0.07         0   0.08         0.17    0.21         0.26   0.49
#2 Caso 2  0.15            0.04         0   0.08         0.13    0.15         0.17   0.32


#Histograma

ggplot(casosteste, aes(x = dados, fill = Caso)) +
  geom_histogram(bins = 30, position = "identity", alpha = 0.6, color = "black") +
  scale_fill_manual(values = c("Caso 1" = "lightblue", "Caso 2" = "lightgreen")) +
  labs(x = "Estatística de Teste", y = "Frequência") +
  theme_minimal()

ggsave("extra2.png", width = 158, height = 93, units = "mm")

freq1<- table(cut(caso1, breaks = seq(0, 0.5, by = 0.05), right = FALSE))
freq2 <- table(cut(caso2, breaks = seq(0, 0.5, by = 0.05), right = FALSE))

table <- data.frame(
  Intervalo = rep(names(freq1), 2),
  Frequência = c(as.numeric(freq1), as.numeric(freq2)),
  Caso = rep(c("Caso 1", "Caso 2"), each = length(freq1))
)

table <- table %>%
  pivot_wider(names_from = Caso, values_from = Frequência)

#Intervalo  `Caso 1` `Caso 2`
#<chr>         <dbl>    <dbl>
#1 [0,0.05)          0        0
#2 [0.05,0.1)        7       53
#3 [0.1,0.15)      169      451
#4 [0.15,0.2)      284      394
#5 [0.2,0.25)      267       91
#6 [0.25,0.3)      154       10
#7 [0.3,0.35)       76        1
#8 [0.35,0.4)       33        0
#9 [0.4,0.45)        8        0
#10 [0.45,0.5)        2        0


#Quartis

quartis1 <- quantile(caso1,probs = c(0.80,0.85,0.90,0.95,0.975,0.99))

#80%       85%       90%       95%     97.5%       99% 
#0.2715597 0.2863787 0.3080460 0.3430673 0.3795414 0.4119051

quartis2 <- quantile(caso2, probs = c(0.80,0.85,0.90,0.95,0.975,0.99))

#80%       85%       90%       95%     97.5%       99% 
#0.1824974 0.1906661 0.2018068 0.2218212 0.2439071 0.2670454 


quartis <- data.frame(
  Quartil = rep(c("80%", "85%", "90%", "95%", "97.5%", "99%"), 2),
  Valor = c(quartis1, quartis2),
  Caso = rep(c("Caso 1", "Caso 2"), each = 6)
)

#Tabela 13 do Conover - Komolgorov-Smirnoff

conover80 <- 0.266
conover90 <- 0.304
conover95 <- 0.338
conover98 <- 0.377
conover99 <- 0.404

conover <- data.frame(
  Quartil = c("80%", "90%", "95%", "97.5%", "99%"),
  Valor = c(conover80, conover90, conover95, conover98, conover99),
  Caso = rep("Conover", 5))

data <- rbind(quartis, conover)

ggplot(data, aes(x = Quartil, y = Valor, color = Caso, group = Caso)) +
  geom_point(size = 3) +
  geom_line() +  
  labs(x = "Quartis", y = "Valor") +
  scale_color_manual(values = c("Caso 1" = "lightblue", "Caso 2" = "lightgreen", "Conover" = "red")) +
  theme_minimal()

ggsave("extra3.png", width = 158, height = 93, units = "mm")

hipotese1 <- sum(caso1 > conover95)
hipotese2 <- sum(caso2 > conover95)
naorejeita1 <- 1000 - hipotese1
naorejeita2 <- 1000 - hipotese2

hipotese <- data.frame(
  Caso = c("Caso 1", "Caso 1", "Caso 2", "Caso 2"),
  Tipo = c("Rejeitada", "Não Rejeitada", "Rejeitada", "Não Rejeitada"),
  Contagem = c(hipotese1, naorejeita1, hipotese2, naorejeita2)
)

ggplot(hipotese, aes(x = Caso, y = Contagem, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Caso",
       y = "Frequência de amostras",
       fill = "Hipótese") +
  scale_fill_manual(values = c("Rejeitada" = "lightpink", "Não Rejeitada" = "lightgoldenrod")) +
  theme_minimal()

ggsave("extra4.png", width = 158, height = 93, units = "mm")

#Caso          Tipo Contagem
#1 Caso 1     Rejeitada       57
#2 Caso 1 Não Rejeitada      943
#3 Caso 2     Rejeitada        0
#4 Caso 2 Não Rejeitada     1000


#Questão 2

sw <- vector("numeric", 1000)
ad <- vector("numeric", 1000)

for (i in 1:1000) {
  sample <- lista[[i]]
  
  testesw <- shapiro.test(sample)
  sw[i] <- testesw$statistic
  
  testead <- ad.test(sample)
  ad[i] <- testead$p.valor
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


#Hipóteses

alfa <- 0.05

hipotesesw <- sum(sw < swconover5)
hipotesead <- sum(ad <= alfa)

naorejeitasw <- 1000 - hipotesesw
naorejeitaad <- 1000 - hipotesead


hipoteses <- data.frame(
  Teste = c("Shapiro-Wilk", "Shapiro-Wilk", "Anderson-Darling", "Anderson-Darling", "Kolmogorov-Smirnov", "Kolmogorov-Smirnov"),
  Tipo = c("Rejeitada", "Não Rejeitada", "Rejeitada", "Não Rejeitada", "Rejeitada", "Não Rejeitada"),
  Contagem = c(hipotesesw, naorejeitasw, hipotesead, naorejeitaad, hipotese1, naorejeita1)
)


#Teste          Tipo Contagem
#1       Shapiro-Wilk     Rejeitada       40
#2       Shapiro-Wilk Não Rejeitada      960
#3   Anderson-Darling     Rejeitada        0
#4   Anderson-Darling Não Rejeitada     1000
#5 Kolmogorov-Smirnov     Rejeitada       57
#6 Kolmogorov-Smirnov Não Rejeitada      943


#Gráfico de barras

ggplot(hipoteses, aes(x = Teste, y = Contagem, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Teste de Normalidade",
       y = "Frequência de amostras",
       fill = "Hipótese") +
  scale_fill_manual(values = c("Rejeitada" = "lightpink", "Não Rejeitada" = "lightgoldenrod")) +
  theme_minimal()

ggsave("extra5.png", width = 158, height = 93, units = "mm")

#Questão 3

lista2 <- vector("list", 1000)
sw2 <- vector("numeric", 1000)
ad2 <- vector("numeric", 1000)

for (i in 1:1000) {
  sample2 <- rnorm(n = 2000, mean = media, sd = desvio)
  lista2[[i]] <- sample2
  
  testesw2 <- shapiro.test(sample2)
  sw2[i] <- testesw2$p.value
  
  testead2 <- ad.test(sample2)
  ad2[i] <- testead2$p.value
}


#Hipóteses

hipotesesw3 <- sum(sw2 <= alfa)
hipotesead3 <- sum(ad2 <= alfa)

naorejeitasw3 <- 1000 - hipotesesw3
naorejeitaad3 <- 1000 - hipotesead3

hipoteses3 <- data.frame(
  Teste = c("SW(n=2000)", "SW(n=2000)", "AD(n=2000)", "AD(n=2000)",
            "SW(n=15)", "SW(n=15)", "AD(n=15)", "AD(n=15)"),
  Tipo = c("Rejeitada", "Não Rejeitada", "Rejeitada", "Não Rejeitada"),
  Contagem = c(hipotesesw3, naorejeitasw3, hipotesead3, naorejeitaad3, hipotesesw, naorejeitasw, hipotesead, naorejeitaad)
)

#Teste          Tipo Contagem
#1 SW(n=2000)     Rejeitada       41
#2 SW(n=2000) Não Rejeitada      959
#3 AD(n=2000)     Rejeitada       53
#4 AD(n=2000) Não Rejeitada      947
#5   SW(n=15)     Rejeitada       40
#6   SW(n=15) Não Rejeitada      960
#7   AD(n=15)     Rejeitada        0
#8   AD(n=15) Não Rejeitada     1000

#Gráfico de barras

ggplot(hipoteses3, aes(x = Teste, y = Contagem, fill = Tipo)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(x = "Teste de Normalidade",
       y = "Frequência de amostras",
       fill = "Hipótese") +
  scale_fill_manual(values = c("Rejeitada" = "lightpink", "Não Rejeitada" = "lightgoldenrod")) +
  theme_minimal()

ggsave("extra6.png", width = 158, height = 93, units = "mm")


