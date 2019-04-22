#=====================================
# AnAlise fatorial e RegressAo Linear: 
# a importancia da aplicabilidade conjunta 
# de duas tecnicas de analise de dados
#====================================
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
#-----------------------------------
# FERNANDO CASALUNGA - FERNANDOCASALUNGA@GMAIL.COM
# RECIFE 2018 - DEZEMBRO
#====================================
# Analise de componentes principais e fatorial #
#====================================

# Acesso a biblioteca #
if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(foreign) == F) install.packages('foreign'); require(foreign)
if(require(FactoMineR) == F) install.packages('FactoMiner'); require(FactoMineR)
if(require(stats) == F) install.packages('stats'); require(stats)
if(require(reshape2) == F) install.packages('reshape2'); require(reshape2)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)


# Carrega base de dados QoG.csv #
data <-read.csv("qog_std_cs_jan16.csv")

#### Matriz de correlacao das variaveis independentes
analysis_data_c <- data[c("eiu_cl", "wvs_confgov", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                          "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data_c <- na.omit(analysis_data_c)

# Generate correlation matrix - fh_cl
cormat <- cor(analysis_data_c[, -c(1:4)])
melted_cormat <- melt(cormat)

# Correlation matrix graph - fh_cl
ggplot(data = melted_cormat, 
       aes(x=`Var1`, y=`Var2`, fill=value)) +
  geom_tile()

#========
# MODELOS 1 e 2 utilizam a variavel dependente discreta - eiu_cl para mensurar as liberdades civis  (1 menos livre - 10 mais livre)
# Modelo 1 - com variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data <- data[c("eiu_cl", "wvs_confgov", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                        "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data <- na.omit(analysis_data)


# Modelo 2 - sem variavel wvs_confgov
# Definindo as variaveis para compor os fatores
analysis_data_2 <- data[c("eiu_cl", "wef_fgo", "diat_ati", "wbgi_cce", "wef_ebf", "wef_pr", "wbgi_pse", 
                          "eiu_fog", "fe_cultdiv", "fe_etfra", "al_language")]

# Retira os Dados Omissos da Base da Dados
analysis_data_2 <- na.omit(analysis_data_2)


# Gerar matriz de correlacao 
cormat <- cor(analysis_data[, -c(1:4)])
melted_cormat <- melt(cormat)

# Grafico da matriz de correlacao 
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile()

#=========================================
# Contruindo a Analise Fatorial (PCA)
#=======================================
# Modelo 1
pca1 <- princomp(analysis_data[, -c(1:4)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca1)

# Carregamento dos componentes principais
loadings(pca1)

# Verificando Quantos fatores devemos utilizar
plot(pca1) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 1
analysis_data$fac1 <- pca1$scores[,1]
analysis_data$fac2 <- pca1$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data$fac1, analysis_data$fac2)

#Scree plot of eigenvalues
screeplot(pca1, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca1$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax <- factanal(analysis_data[, -c(1:4,13:14)],factors=2, rotation="varimax", scores="regression")

# Descritivo dos fatores 1 e 2 
summary(analysis_data$fac1)
summary(analysis_data$fac2)

# Biplot do valor das variaveis
biplot(pca1)

#=======================
# Modelo 2 
pca2 <- princomp(analysis_data_2[, -c(1:3)], scores=TRUE, cor=TRUE)
# Sumario do PCA
summary(pca2)

# Carregamento dos componentes principais
loadings(pca2)

# Verificando Quantos fatores devemos utilizar
plot(pca2) # cotovelo em 2 fatores

# Selecionando os 2 fatores - modelo 1
analysis_data_2$fac1 <- pca2$scores[,1]
analysis_data_2$fac2 <- pca2$scores[,2]

# Grafico Bidimensional dos Fatores
plot(analysis_data_2$fac1, analysis_data_2$fac2)

#Scree plot of eigenvalues
screeplot(pca2, type="line", main="Scree Plot")
abline(1,0, lty=2)

# Scores dos primeiros components
pca2$scores[1 : 10]

# Rotacao varimax(pca1$rotation) 
# Analise fatorial 
fatorial_varimax_2 <- factanal(analysis_data_2[, -c(1:3,12:13)],factors=2, rotation="varimax", scores="regression")

# Biplot do valor das variaveis
biplot(pca2)

#=============================
# Salvar banco de dados da matriz de correlacao
# Matriz de correlacao eiu_cl
write.table(analysis_data_c, "analysis_data_c.csv", sep = ";", row.names = F)

# Salvar banco de dados dos modelos 1 e 2
# modelo 1
write.table(analysis_data, "analysis_data.csv", sep = ";", row.names = F)

# modelo 2
write.table(analysis_data_2, "analysis_data_2.csv", sep = ";", row.names = F)
