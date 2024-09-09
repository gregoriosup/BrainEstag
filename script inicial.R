###pacotes###


###dados####

dados <- read.table("clipboard", header = TRUE, sep = "\t")

dados$F2 <- as.numeric(gsub(",", ".", dados$F2))
dados$desin_ext <- as.numeric(gsub(",", ".", dados$desin_ext))
dados$lactose <- as.numeric(gsub(",", ".", dados$lactose))
dados$pvp <- as.numeric(gsub(",", ".", dados$pvp))
dados$desin_int <- as.numeric(gsub(",", ".", dados$desin_int))


####modelo####

mod <- lm(F2 ~ desin_int+pvp+lactose,data=dados)

summary(mod)

shapiro.test(residuals(mod))
plot(mod)

# Criar um data frame com combinações de variáveis
variaveis <- expand.grid(
  desin_int = seq(5, 25, by = 1),
  pvp = seq(1, 4, by = 0.5),
  lactose = seq(20,40,by=1)
)

# Prever F2 para todas as combinações
variaveis$F2_previsto <- predict(mod, newdata = variaveis)

# Encontrar a combinação que maximiza F2
linha_maxima <- variaveis[which.max(variaveis$F2_previsto), ]

# Exibir os resultados
print("Valores ótimos que maximizam F2:")
print(linha_maxima)







#####tentativas de melhorar o modelo SEM SUCESSO####

# Filtra os valores de F2_previsto para garantir que sejam entre 0 e 100
variaveis_filtradas <- variaveis[variaveis$F2_previsto <= 100 & variaveis$F2_previsto > 0, ]

# Verifica se há dados filtrados
if (nrow(variaveis_filtradas) > 0) {
  # Obtém a linha com o valor máximo de F2_previsto dentro do limite
  linha_maxima <- variaveis_filtradas[which.max(variaveis_filtradas$F2_previsto), ]
} else {
  # Se não houver dados válidos, defina a linha máxima como NA
  linha_maxima <- NA
  # ou outro tratamento apropriado
}

print(linha_maxima)


###cross valid

install.packages("boot")

library(boot)

# Ajuste do modelo linear
modelo <- glm(F2 ~ desin_intabs+pvpabs+lactoseabs,data=dados)

# Realizar validação cruzada com 10 folds
cv_resultado <- cv.glm(dados, modelo, K = 10)

# Mostrar erro médio da validação cruzada
print(cv_resultado$delta)

# Revertendo a transformação logit para o intervalo [0, 100]
Y_pred <- (exp(0.020) / (1 + exp(0.020)))*100
Y_pred <- Y_pred * 100  # Convertendo de volta para a escala de 0 a 100

nova_observacao <- data.frame(desin_intabs = 188, pvpabs = 20, lactoseabs = 327)
previsao <- predict(mod, nova_observacao)
