#Atividade 1.3 - Métodos 2

#NOTA_LP

ggplot(amostra) +
  aes(x = NOTA_LP) +
  geom_histogram(colour = "white", fill= "#000", binwidth = 25) +
  labs(x = "Nota Língua Portuguesa", y = "Frequência Absoluta") +
  theme_minimal()
ggsave("notalp1.png", width = 158, height = 93, units = "mm")


notalp <- amostra %>%
  summarize(
    `Média` = round(mean(NOTA_LP), 2),
    `Desvio Padrão` = round(sd(NOTA_LP), 2),
    `Variância` = round(var(NOTA_LP), 2),
    `Mínimo` = round(min(NOTA_LP), 2),
    `1º Quartil` = round(quantile(NOTA_LP, probs = .25), 2),
    `Mediana` = round(quantile(NOTA_LP, probs = .5), 2),
    `3º Quartil` = round(quantile(NOTA_LP, probs = .75), 2),
    `Máximo` = round(max(NOTA_LP), 2),
    `Assimetria` = round(skewness(NOTA_LP), 2),  
    `Curtose` = round(kurtosis(NOTA_LP), 2)) 
  

#Média Desvio Padrão Variância Mínimo 1º Quartil Mediana 3º Quartil Máximo Assimetria Curtose
#254.97     49.06     2406.78   129.24  220.16   255.99     289.62  373.8      -0.06   -0.54

ggplot(amostra) +
  aes(x=factor(""), y=NOTA_LP) +
  geom_boxplot(fill=c("#000"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  scale_y_continuous(breaks = seq(100,425, by = 50)) +
  labs(x="", y="Nota Língua Portuguesa")+
  theme_minimal()
ggsave("boxplotlp1.png", width = 158, height = 93, units = "mm")

intervalolp <- cut(amostra$NOTA_LP, breaks = seq(100, 400, by = 25), right = FALSE)
frequenciaslp <- table(intervalolp)


#[100,125) [125,150) [150,175) [175,200) [200,225) [225,250) [250,275) [275,300) [300,325) [325,350) 
#0         9        51        88       125       181       183       173       115        53 
#[350,375) [375,400) 
#22         0 

#NOTA_MAT

ggplot(amostra) +
  aes(x = NOTA_MT) +
  geom_histogram(colour = "white", fill= "#000", binwidth = 25) +
  labs(x = "Nota Matemática", y = "Frequência Absoluta") +
  theme_minimal()
ggsave("notamat1.png", width = 158, height = 93, units = "mm")

notamt <- amostra %>%
  summarize(
    `Média` = round(mean(NOTA_MT), 2),
    `Desvio Padrão` = round(sd(NOTA_MT), 2),
    `Variância` = round(var(NOTA_MT), 2),
    `Mínimo` = round(min(NOTA_MT), 2),
    `1º Quartil` = round(quantile(NOTA_MT, probs = .25), 2),
    `Mediana` = round(quantile(NOTA_MT, probs = .5), 2),
    `3º Quartil` = round(quantile(NOTA_MT, probs = .75), 2),
    `Máximo` = round(max(NOTA_MT), 2),
    `Assimetria` = round(skewness(NOTA_MT), 2),  
    `Curtose` = round(kurtosis(NOTA_MT), 2)) 

#Média Desvio Padrão Variância Mínimo 1º Quartil Mediana 3º Quartil Máximo Assimetria Curtose
#254.56    48.89      2390.52   130.65  219.55    253.42     289.78  406.4       0.08   -0.22


ggplot(amostra) +
  aes(x=factor(""), y=NOTA_MT) +
  geom_boxplot(fill=c("#000"), width = 0.5) +
  guides(fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  scale_y_continuous(breaks = seq(100,425, by = 50)) +
  labs(x="", y="Nota Matemática")+
  theme_minimal()
ggsave("boxplotmt1.png", width = 158, height = 93, units = "mm")

intervalomt <- cut(amostra$NOTA_MT, breaks = seq(100, 425, by = 25), right = FALSE)
frequenciasmt <- table(intervalomt)

#[100,125) [125,150) [150,175) [175,200) [200,225) [225,250) [250,275) [275,300) [300,325) [325,350) 
#0        12        37        88       147       185       178       172       113        43 
#[350,375) [375,400) [400,425) 
#13        10         2 

#Ano de nascimento

dados_frequencia <- amostra %>%
  count(ANO_NASC) 

dados_frequencia <- dados_frequencia %>%
  filter(n != "8")

#ANO_NASC   n
#1                  8
#2  1998 ou antes   7
#3           1999  20
#4           2000  50
#5           2001 139
#6           2002 434
#7           2003 337
#8           2004   4
#9 2005 ou depois   1


ggplot(dados_frequencia) +
  aes(x=ANO_NASC, y=n, group=1) +
  geom_line(size=1.5,colour="#000") + geom_point(colour="#000",size=3) +
  labs(x="Ano de Nascimento", y="Frequência de alunos") +
  theme_minimal()

ggsave("anoasc.png", width = 158, height = 93, units = "mm")


#Raça/cor

classes <- amostra %>%
  count(RACA_COR)

classes <- classes %>%
  filter(n != "33")

#RACA_COR   n
#1                     33
#2            Amarela  39
#3             Branca 296
#4           Indígena  20
#5 Não quero declarar  50
#6              Parda 446
#7              Preta 116

ggplot(classes) +
  aes(x = fct_reorder(RACA_COR, n, .desc = TRUE), y = n) +
  geom_bar(stat = "identity", fill = "#000", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) + 
  labs(x = "Raça/Cor", y = "Frequência de alunos") +
  theme_minimal()

ggsave("cor.png", width = 158, height = 93, units = "mm")

#Localização

contagem <- amostra %>% 
  group_by(LOCALIZACAO) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 1)) %>%
  arrange(desc(LOCALIZACAO)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)

#LOCALIZACAO  Freq  Prop posicao
#<chr>       <int> <dbl>   <dbl>
#1 Urbana        891  89.1    44.6
#2 Rural         109  10.9    94.6

ggplot(contagem) +
  aes(x = factor(""), y = Prop , fill = factor(LOCALIZACAO)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) + 
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = colors, name = 'Localização')

ggsave("localizacao.png", width = 158, height = 93, units = "mm")

colors <- c("#0B3B24", "#000")

#Sexo


sexo <- amostra %>% 
  group_by(SEXO) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 1)) %>%
  arrange(desc(SEXO)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)


#A tibble: 3 × 4
#SEXO         Freq  Prop posicao
#<chr>       <int> <dbl>   <dbl>
#1 "Masculino"   484  48.4    24.2
#2 "Feminino"    490  49      72.9
#3 " "            26   2.6    98.7

sexo <- sexo %>%
  filter(Freq != "26")

ggplot(sexo) +
  aes(x = factor(""), y = Prop , fill = factor(SEXO)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) + 
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = colors1, name = 'Localização')

ggsave("sexo.png", width = 158, height = 93, units = "mm")

colors1 <- c("#F5A9F2", "#819FF7")

#Região

regiao <- amostra %>%
  count(REGIAO)

#REGIAO   n
#1 Centro-Oeste  88
#2     Nordeste 272
#3        Norte  94
#4      Sudeste 399
#5          Sul 147

ggplot(regiao) +
  aes(x = fct_reorder(REGIAO, n, .desc = TRUE), y = n) +
  geom_bar(stat = "identity", fill = "#000", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(x = "Região", y = "Frequência de alunos") +
  theme_minimal()

ggsave("regiao.png", width = 158, height = 93, units = "mm")

#Dependência Administrativa

adm <- amostra %>% 
  group_by(DEPENDENCIA_ADM) %>%
  summarise(Freq = n()) %>%
  mutate(Prop = round(100*(Freq/sum(Freq)), 1)) %>%
  arrange(desc(DEPENDENCIA_ADM)) %>%
  mutate(posicao = cumsum(Prop) - 0.5*Prop)


#DEPENDENCIA_ADM  Freq  Prop posicao
#<chr>           <int> <dbl>   <dbl>
#1 Municipal         408  40.8    20.4
#2 Federal             3   0.3    40.9
#3 Estadual          589  58.9    70.6

ggplot(adm) +
  aes(x = factor(""), y = Prop , fill = factor(DEPENDENCIA_ADM)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(
    aes(x = 1.8, y = posicao, label = paste0(Prop, "%")),
    color = "black"
  ) + 
  theme_void() +
  theme(legend.position = "top") +
  scale_fill_manual(values = colors2, name = 'Localização')

ggsave("adm.png", width = 158, height = 93, units = "mm")

colors2 <- c("#0B3B24", "#380B61", "#000")

#Computador

compuador <- amostra %>%
  count(COMPUTADOR)

#COMPUTADOR   n
#1                      17
#2             Não tem 421
#3           Sim, dois  98
#4 Sim, quatro ou mais   6
#5           Sim, três  28
#6             Sim, um 430

compuador <- compuador %>%
  filter(n != "17")

compuador$COMPUTADOR <- fct_relevel(compuador$COMPUTADOR, 
                                    "Não tem", 
                                    "Sim, um", 
                                    "Sim, dois", 
                                    "Sim, três", 
                                    "Sim, quatro ou mais")
ggplot(compuador) +
  aes(x = COMPUTADOR, y = n) +
  geom_bar(stat = "identity", fill = "#000", width = 0.7) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +  
  labs(x = "Possui computador em casa?", y = "Frequência de alunos") +
  theme_minimal()

ggsave("computador.png", width = 158, height = 93, units = "mm")
