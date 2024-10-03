####ANÁLISE DE COMPONENTES PRINCIPAIS####

####pacotes####

library("readxl")
library("ggplot2")

####dados####

dados <- read_xlsx("Dados Fenofibrato.xlsx", sheet = "pca_dumping")
dados_semref <- na.omit(dados[-1,])
dados_num <- dados[-1,-c(1,2,3,32)]
dados_esc <- scale(dados_num)
dados_esc_na <- na.omit(dados_esc)
str(dados_num)
dados_fatores <- dados_esc[,c(25,26,27,28)]
  
####analise####

pca_res <- prcomp(dados_esc_na, center = T, scale. = T)
summary(pca_res)

pca_res$x #scores
pca_res$rotation #loadings

####gráficos####

#grafico de scores

scores <- as.data.frame(pca_res$x)

scores$id <- dados_semref$ID
str(scores)

ggplot(scores, aes(x = PC1, y = PC2, label = id)) +
  geom_point() +
  geom_text(vjust = -0.5, hjust = 0.5) +
  ggtitle("PCA - Componentes Principais 1 e 2 com Identificações")+
  theme_light()


#grafico de loadings
# Extraindo os loadings
loadings <- as.data.frame(pca_res$rotation)

# Adicionando os nomes das variáveis
loadings$variable <- rownames(loadings)

# Usando ggplot para plotar os loadings no PC1 e PC2 de todas as variaveis
library(ggplot2)
ggplot(loadings, aes(x = PC1, y = PC2, label = variable)) +
  geom_point() +
  geom_text(vjust = 1.5, hjust = 0.5) + 
  labs(title = "Loadings no Espaço PC1 vs PC2") +
  xlab("PC1") +
  ylab("PC2") +
  theme_light()



loadings_fatores <- loadings[c(25:36),]

ggplot(loadings_fatores, aes(x = PC1, y = PC2, label = variable)) +
  geom_point() +
  geom_text(vjust = 1.5, hjust = 0.5) + 
  labs(title = "Loadings no Espaço PC1 vs PC2") +
  xlab("PC1") +
  ylab("PC2") +
  theme_light()
+
  ,

