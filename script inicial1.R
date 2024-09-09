###pacotes###


###dados####

dados <- read.table("clipboard", header = TRUE, sep = "\t")

dados$desin_intabs <- dados$desin_int*900/100
dados$desin_extabs <- dados$desin_ext*900/100
dados$lactoseabs <- dados$lactose*900/100
dados$pvpabs <- dados$pvp*900/100


dados$F2 <- as.numeric(gsub(",", ".", dados$F2))
dados$F2log <- log((dados$F2/100)/(1-(dados$F2/100)))

dados$desin_ext <- as.numeric(gsub(",", ".", dados$desin_ext))
dados$lactose <- as.numeric(gsub(",", ".", dados$lactose))
dados$pvp <- as.numeric(gsub(",", ".", dados$pvp))
dados$desin_int <- as.numeric(gsub(",", ".", dados$desin_int))


####modelo####

mod <- lm(F2 ~ desin_intabs+pvpabs+lactoseabs,data=dados)

summary(mod)

shapiro.test(residuals(mod))
plot(mod)

# Criar um data frame com combina��es de vari�veis
variaveis <- expand.grid(
  desin_intabs = seq(50, 300, by = 1),
  pvpabs = seq(10, 40, by = 0.5),
  lactoseabs = seq(150,500,by=1)
)

# Prever F2 para todas as combina��es
variaveis$F2_previsto <- predict(mod, newdata = variaveis)

# Encontrar a combina��o que maximiza F2
linha_maxima <- variaveis[which.max(variaveis$F2_previsto <= 100 & variaveis$F2_previsto >0), ]

# Exibir os resultados
print("Valores �timos que maximizam F2:")
print(linha_maxima)

# Filtra os valores de F2_previsto para garantir que sejam entre 0 e 100
variaveis_filtradas <- variaveis[variaveis$F2_previsto <= 100 & variaveis$F2_previsto > 0, ]

# Verifica se h� dados filtrados
if (nrow(variaveis_filtradas) > 0) {
  # Obt�m a linha com o valor m�ximo de F2_previsto dentro do limite
  linha_maxima <- variaveis_filtradas[which.max(variaveis_filtradas$F2_previsto), ]
} else {
  # Se n�o houver dados v�lidos, defina a linha m�xima como NA
  linha_maxima <- NA
  # ou outro tratamento apropriado
}

print(linha_maxima)


###cross valid

install.packages("boot")

library(boot)

# Ajuste do modelo linear
modelo <- glm(F2 ~ desin_intabs+pvpabs+lactoseabs,data=dados)

# Realizar valida��o cruzada com 10 folds
cv_resultado <- cv.glm(dados, modelo, K = 10)

# Mostrar erro m�dio da valida��o cruzada
print(cv_resultado$delta)

# Revertendo a transforma��o logit para o intervalo [0, 100]
Y_pred <- (exp(0.020) / (1 + exp(0.020)))*100
Y_pred <- Y_pred * 100  # Convertendo de volta para a escala de 0 a 100

nova_observacao <- data.frame(desin_intabs = 188, pvpabs = 20, lactoseabs = 327)
previsao <- predict(mod, nova_observacao)
