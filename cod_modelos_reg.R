#==================================================
# Analise fatorial e Regressao Linear: 
# a importancia da aplicabilidade conjunta 
# de duas tecnicas de analise de dados
#==================================================
# UNIVERSIDADE FEDERAL DE PERNAMBUCO
#-------------------------------------------------
# FERNANDO CASALUNGA - FERNANDOCASALUNGA@GMAIL.COM
# RECIFE 2018 - DEZEMBRO
#==================================================
# Estatistica descritiva e modelo de regressao
# =================================================

# Acesso a biblioteca
if(require(stargazer) == F) install.packages('stargazer'); require(stargazer)
if(require(readr) == F) install.packages('readr'); require(readr)
if(require(ggplot2) == F) install.packages('ggplot2'); require(ggplot2)
if(require(car) == F) install.packages('car'); require(car)
if(require(MASS) == F) install.packages('MASS'); require(MASS)
if(require(xtable) == F) install.packages('xtable'); require(xtable)
if(require(lmtest) == F) install.packages('lmtest'); require(lmtest)

# Importando banco de dados em formato .csv - modelo 0
library(readr)
analysis_data_c <- read_delim("analysis_data_c.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)


# Modelo de regressao linear multivariado 0 - sem variaveis latentes -
modelo_0 <- lm(eiu_cl ~ wvs_confgov +
                 wef_fgo + 
                 diat_ati + 
                 wbgi_cce + 
                 wef_ebf + 
                 wef_pr +
                 wbgi_pse + 
                 eiu_fog + 
                 fe_cultdiv + 
                 fe_etfra + 
                 al_language,
               data = analysis_data_c)

summary(modelo_0)

# Visualizacao do modelo 0
stargazer(modelo_0, type = "text", title = "Resultados do modelo 0", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(model_0) 
IC <-  confint(model_0, level=0.95) 
y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude of Coefficients", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i", main="Confidence Intervals Model Zero")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 1
analysis_data_c$Preditos <- predict(modelo_0)
analysis_data_c$Residuo <- analysis_data_c$eiu_cl - analysis_data_c$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data_c$Residuo)

# Grafico de dispersao dos residuos padronizados do modelo 1
plot(analysis_data_c$Preditos, analysis_data_c$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_0)

### Teste para Colinearidade  ###
vif(modelo_0) # variance inflation factors 
sqrt(vif(modelo_0)) > 3
sqrt(vif(modelo_0)) < 1

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_0)

# Correlacao entre a variavel dependente e variaveis independentes institucionais
cor(eiu_cl, wvs_confgov, wef_fgo, 
    diat_ati, wbgi_cce, wef_ebf,
    wef_pr, wbgi_pse, data = analysis_data_c)

# Correlacao entre a variavel dependente e variaveis independentes sociais
cor(eiu_cl, eiu_fog, fe_cultdiv, 
    fe_etfra, al_language, data = analysis_data_c)

### Teste para variancia do erro nao constante
ncvTest(modelo_0)

### Teste de Nao-linearidade ###
# componente somado aos residuos  
crPlots(modelo_0)

### Avaliando distribuição dos resíduos ###
# Histograma de dispersao dos residuos modelo 1
hist(analysis_data_c$Residuo, freq=FALSE, ylim=c(0,0.6), main="Distribuicao dos residuos padronizados 0")
xfit <- seq(min(analysis_data_c$Residuo),max(analysis_data_c$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

###############################################
# Modelos 1 e 2 utilizando variáveis latentes #
###############################################

# Importando banco de dados em formato .csv - modelo 1
library(readr)
analysis_data <- read_delim("analysis_data.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)

# Estatistica descritiva das variaveis independentes do modelo de regressao 1
summary(analysis_data$wvs_confgov)
summary(analysis_data$wef_fgo)
summary(analysis_data$diat_ati)
summary(analysis_data$fac1)
summary(analysis_data$fac2)

# Estatistica descritia da variavel dependente do modelo de regressao 1
summary(analysis_data$eiu_cl)

# Modelo de regressao linear multivariado 1
modelo_1 <- lm(eiu_cl ~ wvs_confgov +
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data)

summary(modelo_1)


# Visualizacao do modelo 1
stargazer(modelo_1, type = "text", title = "Resultados do modelo 1", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_1) 
IC <-  confint(modelo_1, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


# Grafico de dispersao do modelo 1
ggplot(analysis_data, aes(x = fac1, y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


# Grafico de dispersao do modelo 1 com fac1 logaritimizado
ggplot(analysis_data, aes(x = log(fac1), y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 1
analysis_data$Preditos <- predict(modelo_1)
analysis_data$Residuo <- analysis_data$eiu_cl - analysis_data$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data$Residuo)

# Grafico de dispersao dos residuos padronizados do modelo 1
plot(analysis_data$Preditos, analysis_data$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_1)

### Teste para Colinearidade  ###
vif(modelo_1) # variance inflation factors 
sqrt(vif(modelo_1)) > 3
sqrt(vif(modelo_1)) < 1

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_1)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data$eiu_cl, analysis_data$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data$eiu_cl, analysis_data$fac2)

### Teste para variancia do erro nao constante
ncvTest(modelo_1)

### Teste de Nao-linearidade ###
# componente somado aos residuos  
crPlots(modelo_1)

### Avaliando distribuição dos resíduos ###
# Histograma de dispersao dos residuos modelo 1
hist(analysis_data$Residuo, freq=FALSE, ylim=c(0,0.6), main="Distribuicao dos residuos padronizados 1")
xfit <- seq(min(analysis_data$Residuo),max(analysis_data$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

#=============================================

# Importando banco de dados em formato .csv - modelo 2
library(readr)
analysis_data_2 <- read_delim("analysis_data_2.csv", 
                              ";", escape_double = FALSE, trim_ws = TRUE)

# Modelo de regressao linear multivariado 2
modelo_2 <- lm(eiu_cl ~
                 wef_fgo + 
                 diat_ati + 
                 fac1 + 
                 fac2 , 
               data = analysis_data_2)

summary(modelo_2)

# Visualizacao dos modelos 0, 1 e 2
stargazer(modelo_0, modelo_1, modelo_2, type = "text", title = "Resultados do modelo 1", style = "ajps", p.auto=FALSE)


# Grafico para a representaco dos intervalos de confianca dos coeficientes etimados
par(mfrow=c(1,1))
betas <- coefficients(modelo_2) 
IC <-  confint(modelo_2, level=0.95) 

y.axis <- seq(from=1, to=length(betas))
plot(betas, y.axis, type="p", pch=19, xlab="Magnitude dos Coeficientes", ylab="", 
     axes=F, xlim=c(min(IC-.4), max(IC+.4)), ylim=c(min(y.axis-.2), max(y.axis+.2)), 
     cex=1,yaxs="i",xaxs="i")
segments(IC[,1], y.axis, IC[,2], y.axis)
axis(1, at=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), 
     labels=seq(round(min(IC-.9)), round(max(IC+.9)), by=0.1), tick=T, cex.axis=1, 
     mgp=c(2,.7,0))
axis(2, at=y.axis, label=names(betas), las=1, tick=T, line=-.5, cex.axis=1, 
     mgp=c(2,.7,0))
abline(v=0, lty=2, col="red")


# Grafico de dispersao do modelo 2
ggplot(analysis_data_2, aes(x = fac1, y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


# Grafico de dispersao do modelo 2 com fac1 logaritimizado
ggplot(analysis_data_2, aes(x = log(fac1), y = eiu_cl)) +
  geom_point()+ 
  geom_smooth(method='lm')


### Avaliando homocedasticidade ###
# Analise dos Residuos do modelo de regressao linear multivariado 2
analysis_data_2$Preditos <- predict(modelo_2)
analysis_data_2$Residuo <- analysis_data_2$eiu_cl - analysis_data_2$Preditos

# Teste do valor medio do termo de erro igual a zero
summary(analysis_data_2$Residuo)

# Grafico de dispersao dos residuos modelo 2
plot(analysis_data_2$Preditos, analysis_data_2$Residuo, pch=21, bg="red", col="red")
abline(0,0, lty = 2)

# plot residuos padronizados e valores preditos
spreadLevelPlot(modelo_2)

### Teste para Colinearidade 
vif(modelo_2) # variance inflation factors 
sqrt(vif(modelo_2)) > 3
sqrt(vif(modelo_2)) < 1

### Teste para autocorrelacao dos erros 
durbinWatsonTest(modelo_2)

# Correlacao entre a variavel dependente e o fator 1 - variavel independente construida
cor(analysis_data_2$eiu_cl, analysis_data_2$fac1)

# Correlacao entre a variavel dependente e o fator 2 - variavel independente construida
cor(analysis_data_2$eiu_cl, analysis_data_2$fac2)

### Teste para variancia do erro nao constante
ncvTest(modelo_2)

### Teste de Nao-linearidade 
# componente somado aos residuos  
crPlots(modelo_2)

# Histograma de dispersao dos residuos modelo 2
hist(analysis_data_2$Residuo, freq=FALSE, ylim=c(0,0.6), main="Distribuicao dos residuos padronizados 2")
xfit <- seq(min(analysis_data_2$Residuo),max(analysis_data_2$Residuo),length=40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

#============================================






