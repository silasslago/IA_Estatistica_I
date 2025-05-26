##############################################
# TRABALHO DE IAA004 – Estatística Aplicada I
# Prof. Dr. Arno P. Schmitz 
#
# Grupo 12
#
# - Eduardo Dan Shimada 
# - Lucas Machado 
# - Filipe Antonio Mariotti  
# - Gabriele Azevedo Pedroso 
# - Silas Juan Santos Lago 
#
##############################################

# Diretório de trabalho
setwd(getwd())
load("salarios.RData")

##############################################
# 1. GRÁFICOS E TABELAS
##############################################

# a) Boxplot e Histograma para "age" e "husage"

# Instalando o pacote que gera o boxplot
install.packages("car")

# Carregando o pacote boxplot
library(car)

# Gerando boxplot das esposas
Boxplot(
  ~age, 
  data = salarios, 
  id = list(method = "y"), 
  ylab = "Idade das esposas",
  main = "Boxplot - Idade das Esposas"
)

# Gerando boxplot dos maridos
Boxplot(
  ~husage, 
  data = salarios, 
  id = list(method = "y"), 
  ylab = "Idade dos maridos",
  main = "Boxplot - Idade dos Maridos"
)

# Gerando boxplot das esposas e maridos
Boxplot(
  ~husage + ~age, 
  data = salarios, 
  id = list(method = "y"), 
  ylab = "Idade dos maridos e esposas",
  main = "Boxplot Comparativo"
)

# Histograma - Idade dos Maridos
hist(
  salarios$husage, 
  main = "Histograma - Idade dos Maridos", 
  xlab = "Idade", 
  ylab = "Frequência"
)

# Histograma - Idade das Esposas
hist(
  salarios$age, 
  main = "Histograma - Idade das Esposas", 
  xlab = "Idade", 
  ylab = "Frequência"
)

# Comparação:
# As idades dos maridos são mais altas em média.
# Há maior dispersão entre as idades dos homens.

# b) Tabela de Frequência para "age" e "husage"

# Instalar e carregar pacote
install.packages("fdth")
library(fdth)

# Tabela - Idade dos Maridos
freq_marido <- fdt(salarios$husage)
print(freq_marido)

# Tabela - Idade das Esposas
freq_esposa <- fdt(salarios$age)
print(freq_esposa)

# Comparação:
# Podemos verificar que a faixa etária dos homens é maior em relação às das esposas, onde a idade dos homens varia entre 18 anos e 81 anos enquanto das mulheres varia entre 17 anos e 56 anos.  
# Nota-se também que a moda dos homens e mulheres são quase da mesma idade, onde os homens apresentam maior frequência na idade de 38 anos, com valor f = 917, 
# já nas mulheres apresenta maior frequência na idade de 35 anos, com valor f = 624. 

##############################################
# 2. MEDIDAS DE POSIÇÃO E DISPERSÃO
##############################################

# a) Média, Mediana e Moda

# Média
mean(salarios$husage)  # Marido
mean(salarios$age)     # Esposa

# Mediana
median(salarios$husage)
median(salarios$age)

# Moda
table(salarios$husage)
subset(table(salarios$husage), table(salarios$husage) == max(table(salarios$husage)))

table(salarios$age)
subset(table(salarios$age), table(salarios$age) == max(table(salarios$age)))

# Comparação entre maridos e esposas

# Cálculo de medidas
media_h <- mean(salarios$husage)
media_m <- mean(salarios$age)
mediana_h <- median(salarios$husage)
mediana_m <- median(salarios$age)
moda_h <- as.numeric(names(which.max(table(salarios$husage))))
moda_m <- as.numeric(names(which.max(table(salarios$age))))

# Diferenças percentuais
media_diff <- ((media_h / media_m) - 1) * 100
mediana_diff <- ((mediana_h / mediana_m) - 1) * 100
moda_diff <- ((moda_h / moda_m) - 1) * 100

# Impressão formatada
cat("
--- COMPARAÇÃO ENTRE MARIDOS E ESPOSAS ---
")
cat("Média   - Marido:", round(media_h, 2), "| Esposa:", round(media_m, 2),
    "| Diferença:", round(media_diff, 2), "%
")
cat("Mediana - Marido:", mediana_h,        "| Esposa:", mediana_m,
    "| Diferença:", round(mediana_diff, 2), "%
")
cat("Moda    - Marido:", moda_h,           "| Esposa:", moda_m,
    "| Diferença:", round(moda_diff, 2), "%
")

# b) Variância, Desvio Padrão e Coeficiente de Variação

# Variância
var(salarios$husage)
var(salarios$age)

# Desvio padrão
sd(salarios$husage)
sd(salarios$age)

# Coeficiente de Variação
media_h <- mean(salarios$husage)
media_m <- mean(salarios$age)
dp_h <- sd(salarios$husage)
dp_m <- sd(salarios$age)

cv_h <- (dp_h / media_h) * 100
cv_m <- (dp_m / media_m) * 100

cv_h
cv_m

# Comparação automatizada entre maridos e esposas (Dispersão)

# Cálculo de variâncias
var_h <- var(salarios$husage)
var_m <- var(salarios$age)
var_diff <- ((var_h / var_m) - 1) * 100

# Cálculo de desvios padrão
sd_h <- sd(salarios$husage)
sd_m <- sd(salarios$age)
sd_diff <- ((sd_h / sd_m) - 1) * 100

# Cálculo de coeficientes de variação
cv_h <- (sd_h / media_h) * 100
cv_m <- (sd_m / media_m) * 100
cv_diff <- ((cv_h / cv_m) - 1) * 100

# Impressão formatada
cat("
--- COMPARAÇÃO DE DISPERSÃO ---
")
cat("Variância - Marido:", round(var_h, 2), "| Esposa:", round(var_m, 2),
    "| Diferença:", round(var_diff, 2), "%
")
cat("Desvio Padrão - Marido:", round(sd_h, 2), "| Esposa:", round(sd_m, 2),
    "| Diferença:", round(sd_diff, 2), "%
")
cat("Coef. Variação - Marido:", round(cv_h, 2), "% | Esposa:", round(cv_m, 2), "%",
    "| Diferença:", round(cv_diff, 2), "%
")

##############################################
# 3. TESTES PARAMÉTRICOS E NÃO PARAMÉTRICOS
##############################################

# a) Testar se as médias ou medianas das variáveis "age" e "husage" são iguais
# De acordo com o enunciado, é necessário verificar se as distribuições são normais
# para então escolher entre o teste t (paramétrico) ou o teste de Mann-Whitney (não-paramétrico)

# Instalar e carregar pacote
library(nortest)

tabela_aux_esposa <- data.frame(idade = salarios$age)
tabela_aux_esposa$group <- "esposa"

tabela_aux_marido <- data.frame(idade = salarios$husage)
tabela_aux_marido$group <- "marido"

nova_tabela <- rbind(tabela_aux_esposa, tabela_aux_marido)

# Cálculo da média da idade entre as esposas e maridos juntos
mean(nova_tabela$idade)

library(dplyr)
group_by(nova_tabela, group) %>% summarise(
  count = n(),
  mean = mean(idade, na.rm = TRUE),
  sd = sd(idade, na.rm = TRUE)
)

# Teste de normalidade (Anderson-Darling)
cat("
--- TESTE DE NORMALIDADE (Anderson-Darling) ---
")

normalidade_h <- ad.test(salarios$husage)
normalidade_m <- ad.test(salarios$age)

cat("p-valor Marido:", format.pval(normalidade_h$p.value, digits = 4), "
")
cat("p-valor Esposa:", format.pval(normalidade_m$p.value, digits = 4), "
")

if (normalidade_h$p.value < 0.05 | normalidade_m$p.value < 0.05) {
  cat("Conclusão: Pelo menos uma das variáveis não segue distribuição normal.
")
  cat("Utilizaremos o teste não-paramétrico de Mann-Whitney.
")
} else {
  cat("Conclusão: Ambas as variáveis seguem distribuição normal. Poderíamos usar o teste t.
")
}

# Se p-valor < 0.05 → distribuição não normal → usar teste de Mann-Whitney
# H0: Ambas as variáveis seguem Distribuição Normal. 
# Ha: Uma, ou ambas, as variáveis não seguem Distribuição Normal. 
# Confirmando o que foi analisado pelos histogramas, as distribuições de idade não seguem uma distribuição Normal nos grupos. 
# Seu p-valor sendo 2.2e-16, que é estritamente menor que 0.05. Dessa forma, rejeitamos a hipótese H0 de que a distribuição é Normal 
# e seguimos adiante com Ha e com o Teste Não Paramétrico “U” de Mann-Whitney. 


# Teste de Mann-Whitney (não-paramétrico)
#
# H0: A mediana da idade de homens e mulheres é estatisticamente igual 
# Ha: A mediana da idade de homens e mulheres NÃO é estatisticamente igual 

mann_whitney <- wilcox.test(salarios$husage, salarios$age, alternative = "two.sided", conf.int = TRUE)

cat("
--- RESULTADO TESTE DE MANN-WHITNEY ---
")
cat("Estatística W:", round(mann_whitney$statistic, 2), "
")
cat("Valor-p:", format.pval(mann_whitney$p.value, digits = 4), "
")
cat("Intervalo de confiança para a diferença das medianas:
")
print(mann_whitney$conf.int)

if (mann_whitney$p.value < 0.05) {
  cat("
Conclusão: As idades dos maridos e esposas são estatisticamente diferentes (p < 0.05).
")
} else {
  cat("
Conclusão: Não há diferença estatística entre as idades (p ≥ 0.05).
")
}
# Isto é, H0: A mediana da idade de homens e mulheres é estatisticamente igual é rejeitada pois seu p-valor é 2.2e-16, que é estritamente menor que 0.05.
# Com um intervalo de confiança de 95% - [2.000033, 3.000024]. 
# Logo, aceitamos Ha: A mediana da idade de homens e mulheres NÃO é estatisticamente igual. 

#

##############################################
# FIM DO SCRIPT
##############################################
