## Title: Investor Sentiment and Anomalies in Brazilian Market
##
## Version: 0.0.1
##
## Description: Script to compute que Investor Sentiment Index of the brazilian
## market.                       
## 

## CONTENTS ## #################################################################
## 1. SETTINGS
## 2. GET DATA AND CLEAN
## 3. INVESTOR SENTIMENT INDEX
## 4. CONSTRUCT PORTFOLIOS
## 5. PRICING MODELS
## 6. INVESTOR SENTIMENT AND ANOMALIES
##

#' 

## 1. SETTINGS ## ##############################################################
## Definir parâmetros
## 
## SOBRE O PERIODO:
##
## Periodo momento (jul do ano n-1 a mai do ano n)
## Para calculo do das Carteiras: precos  de jun/(n  ):jun/(n+1) (13 meses)
##                                retorno de jul/(n  ):jun/(n+1) (12 meses)
##
## Para calculo do Fator Momento: precos  de jun/(n-1):mai/(n  ) (12 meses)
##                                retorno de jul/(n-1):mai/(n  ) (11 meses)
##

## Definir Parametros / Set Parameters -----------------------------------------
setwd("C:/Dropbox/investorSentiment") # Pasta de Trabalho / Working Directory

PERIOD.n <- 1999 # Ano Inicial
PERIOD.N <- 2014 # Ano Final

## Periodo Mensal / Monthly Period
PERIOD.XTS <- paste(PERIOD.n,"-06/", PERIOD.N, "-06", sep="")
#                   JUN/n        a     JUN/N    (Ex. 1999-06/2014-06)

## Periodo Anual (Dados de negociacoes) / Yearly  Period
PERIOD.JUN <- paste(PERIOD.n+1,"-06/", PERIOD.N-1, "-06", sep="")
#                   JUN/(n+1)    a    JUN/(N-1) (Ex. 2000-06/2013-06)

## Periodo Anual (Dados de contabeis) / Yearly  Period
PERIOD.DEZ <- paste(PERIOD.n,"-12/", PERIOD.N-2, "-12", sep="")
#                      DEZ/n     a    DEZ/(N-2) (Ex. 1999-12/2012-12)

## Instalar pacotes / Install packages -----------------------------------------
ip <- installed.packages()
pkg <- "dynlm"     ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "lubridate" ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "Quandl"    ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "TTR"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "XML"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "xts"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
rm(list=c("ip","pkg"))

## Carregar pacotes / Load packages --------------------------------------------
library("dynlm")
library("lubridate")
library("Quandl") ; Quandl.auth("WP2rt8HsRo3kjWsRkLY5")
library("xts")
library("TTR")
library("XML")

## Executar minhas funçoes / Run my functions
source("R/functions.R")

## 2. LOAD DATA AND CLEAN ## ###################################################
##
## Importar e limpar dados / Load and Clean Data
##

## 2.1 Importar Dados / Load Data # ============================================

### Importar Dados Online / Load Online Data # ---------------------------------

### BAIXAR E SALVAR DADOS LOCALMENTE ###
# # Emissoes de Acoes Iniciais - IPO (Fonte: Site CVM)
# dbCVM_IPO <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaIPOnaCVM)
# write.table(dbCVM_IPO, "Data/IPOs.csv", quote=F, sep=";", row.names=F)
# # Emissoes de Acoes Subsequentes (Fonte: Site CVM)
# dbCVM_SUB <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaSubsequentesnaCVM)
# write.table(dbCVM_SUB, "Data/SUBs.csv", quote=F, sep=";", row.names=F)
# # Emissoes de Dividas (Fonte: Site CVM)
# dbCVM_DEB <- coletaVariosAnosCVM(PERIOD.n:PERIOD.N, coletaDEBnaCVM)
# write.table(dbCVM_DEB, "Data/DEBs.csv", quote=F, sep=";", row.names=F)

### LER DADOS BAIXADOS E SALVOS LOCALMENTE ###
# Emissoes de Acoes Iniciais - IPO (Fonte: Site CVM)
dbCVM_IPO <- read.table("Data/IPOs.csv", header=T,
                        sep=";", stringsAsFactors = F)
dbCVM_IPO$data <- as.Date(dbCVM_IPO$data)
# Emissoes de Acoes Subsequentes (Fonte: Site CVM)
dbCVM_SUB <- read.table("Data/SUBs.csv", header=T,
                        sep=";",stringsAsFactors = F)
dbCVM_SUB$data <- as.Date(dbCVM_SUB$data)
# Emissoes de Dividas (Fonte: Site CVM)
dbCVM_DEB <- read.table("Data/DEBs.csv", header=T,
                        sep=";", stringsAsFactors = F)
dbCVM_DEB$data <- as.Date(dbCVM_DEB$data)

### Ler Dados em CSV / Load CSV Data # -----------------------------------------

## Carregando matriz de precos / Stock Prices
mPrices <- importaBaseCSV("Input/mPrices.csv", PERIOD.XTS)

## Importar Valor de Mercado da Empresa / Read Market Value of the Firm
mMVfirm <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.XTS, ignora=1)

## Importar Patrimonio Liquido / Read Book Firm
yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", PERIOD.XTS, formato="%Y",
                            ignora=0)

## Importar Valor de Mercado da Classe (para ponderacao)
mMVclass <- importaBaseCSV("Input/mMarketValue.csv", PERIOD.XTS, ignora=1)

## Importar Volume medio em reais ultimos 12 meses
mVolume <- importaBaseCSV("Input/mVolume.csv", PERIOD.XTS, ignora=1)

## Importar Indice de Negociabilidade da Bovespa / Bovespa Negociability Index
mNegociab <- importaBaseCSV("Input/mNegociabilidade.csv", PERIOD.XTS)

## 2.2 Calcular Variaveis / Compute Variables # ================================

## Valor de Mercado da Classe em JUNHO
yMVclassJun <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="jun"),]

## Valor de Merdado da Classe em DEZEMBRO
yMVclassDez <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="dez"),]

## Valor de Mercado da Empresa JUNHO
yMVfirmJun <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="jun"),]

## Valor de Mercado da Empresa DEZEMBRO
yMVfirmDez <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="dez"),]

## Book-to-Market
yBM <- yBookFirm / yMVfirmDez

## Indice de Negociabilidade da Bovespa / Bovespa Negociability Index
yNegociab <- mNegociab[(months(as.Date(rownames(mNegociab)), T)=="jun"),]

## Calcular Retornos Logaritimos Mensais / Compute Logarithmic Monthly Return
pXTS     <- as.xts(mPrices)
mReturns <- as.data.frame( diff(log(pXTS), lag=1) ) ; rm(pXTS)

## Calcular Variavel Momento
yMomentum <- computeMomentum (mReturns)

## Volume Médio de Negociação em Reais nos últimos 12 meses (mes base: JUNHO)
yVolumeJun <- mVolume[(months(as.Date(rownames(mVolume)), T)=="jun"),]

## Volume Médio de Negociação em Reais nos últimos 12 meses (mes base: DEZ)
yVolumeDez <- mVolume[(months(as.Date(rownames(mVolume)), T)=="dez"),]

## 2.2 Filtrar Amostra / Filter Sample # =======================================

## Calcular Amostra Inicial
ySample0 <- initialSample(mPrices) ; rownames(ySample0) <- rownames(yMVclassJun)

## Filtrar de Empresas Nao Financeiras
ySample1 <- filterNoFinancial(ySample0, "Input/dbStocks.csv")

## Filtrar ações com cotações 24 meses consecutivos
ySample2 <- filterNo24months(mPrices, ySample1)

## Filtro Valor de Mercado em 30/06/n e 31/12/n-1
ySample3 <- ySample2 # Cria matriz de controle da amostra a partir da ultima
ySample3[-1,][ (yMVfirmDez <= 0) ] <- FALSE # Falso p/ valores n positivos em n-1
ySample3[-1,][ is.na(yMVfirmDez) ] <- FALSE # Falso p/ valores invalidos em n-1
ySample3[ (yMVfirmJun <= 0) ] <- FALSE
ySample3[ is.na(yMVfirmJun) ] <- FALSE

## Filtro Patrimonio Liquido
ySample4 <- ySample3 # Cria matriz de controle da amostra a partir da ultima
ySample4[-1,][ (yBookFirm <= 0) ] <- FALSE # Falso p/ valores n positivos em n-1
ySample4[-1,][ is.na(yBookFirm) ] <- FALSE # Falso p/ valores invalidos em n-1

## Filtrar Indice de Negociabilidade maior que 0,01
ySample5 <- ySample4
ySample5[yNegociab < 0.001] <- F ; ySample5[is.na(yNegociab)] <- F

## Amostra Final / Final Sampe
ySample <- ySample5
sampleReportAll(ySample0, ySample1, ySample2, ySample3, ySample4, ySample5)

## 2.3 Limpar Dados / Clean Data # =============================================

yMVfirmJun  <- cleanData(yMVfirmJun,  ySample)
yMVfirmDez  <- cleanData(yMVfirmDez,  ySample, LAG=1)
yMVclassJun <- cleanData(yMVclassJun, ySample)
yMVclassDez <- cleanData(yMVclassDez, ySample, LAG=1)
yVolumeJun  <- cleanData(yVolumeJun,  ySample)
yVolumeDez  <- cleanData(yVolumeDez,  ySample, LAG=1)
yBM         <- cleanData(yBM,         ySample, LAG=1)
yMomentum   <- cleanData(yMomentum,   ySample)
rownames (yMomentum) <- rownames(yVolumeJun)

## 3. INVESTOR SENTIMENT INDEX ## ##############################################
## 3. Índice de Sentimento
## 3.1. Ler/Calcular Proxies
## Temporalidade das Proxies: Selecionar proxies que serão defasadas
## 3.2. Índice de Sentimento não Ortogonalizado
## 3.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  

## 3.1 Read/Compute Proxies # ==================================================

## Periodo p/ Proxies Sentimento
PERIOD.PRX <- paste(PERIOD.n,"-01/", PERIOD.N, "-06", sep="")
#                      JAN/n     a        JUN/N (Ex. 1999-01/2014-06)

## Calcular NIPO
prx_NIPO <- calcularNIPO(dbCVM_IPO)

## Calcular S
prx_S <- calcularS(dbCVM_IPO, dbCVM_SUB, dbCVM_DEB)

# Tratar os dados faltantes
library(TTR)
# Substituindo valores zerados pela medias dos ultimos meses
prx_S$A[3]              <- SMA(prx_S$A,   2)[prx_S$A==0][1]
prx_S$A[prx_S$A==0]     <- EMA(prx_S$A,   6)[prx_S$A==0]
prx_S$DEB[prx_S$DEB==0] <- EMA(prx_S$DEB, 6)[prx_S$DEB==0]
# http://www.fmlabs.com/reference/default.htm?url=ExpMA.htm
# Recalculando S
prx_S$Issues <- prx_S$A / ( prx_S$A + prx_S$DEB)
S <- prx_S ; S$A <- NULL ; S$DEB <- NULL

prx_S <- as.data.frame(as.xts(prx_S)[PERIOD.PRX])

## Calcular TURN
prx_TURN <- calcularTURN ("Input/mNegociabilidade.csv",
                          "Input/mQN.csv",
                          #"Input/mQT.csv",
                          "Input/mQTOutStanding.csv",
                          PERIOD.PRX, 1, 0.01)

## plot(ts(prx_TURN$dTURN, start(1999,1), frequency=12))

## Calcular PVOL # -------------------------------------------------------------

## Calcula a proxy PVOL - Premio Volatilidade
## 
## INPUTS
##
## require(lubridate)
MV  <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.PRX, ignora=1)[,1:1108]
tmp <- quarter(rownames(MV))!=c(quarter(rownames(MV))[-1],TRUE)
qMVfirm <- MV[tmp,] ; rm(MV)

qBookFirm <- importaBaseCSV("Input/qBookFirm.csv", PERIOD.PRX, ignora=1)[,1:1108]
qBookFirm[qBookFirm<=0]     <- NA
qMVfirm[qBookFirm<=0]       <- NA
qBookFirm[is.na(qBookFirm)] <- NA
qMVfirm[is.na(qBookFirm)]   <- NA
qBookFirm[is.na(qMVfirm)]   <- NA
qMVfirm[is.na(qMVfirm)]     <- NA

qMB <- as.data.frame(mapply( function(mkt,book) { mkt/book },
                             as.data.frame((qMVfirm)),
                             as.data.frame((qBookFirm))) )
rownames(qMB) <- rownames(qMVfirm)

DP12m    <- importaBaseCSV("Input/mDP12m.csv", PERIOD.PRX, ignora = 1)[,1:1108]
tmp <- quarter(rownames(DP12m))!=c(quarter(rownames(DP12m))[-1],TRUE)
qDP12m <- DP12m[tmp,] ; rm(DP12m)

# Valor de Mercado da Classe p/ ponderacao
MV  <- importaBaseCSV("Input/mMarketValue.csv", PERIOD.PRX, ignora=1)
tmp <- quarter(rownames(MV))!=c(quarter(rownames(MV))[-1],TRUE)
qMVclass <- MV[tmp,1:108] ; rm(MV)

# Media Ponderada pelo VM do MB das acoes de alta volatilidade (High Volatility)
A  <- as.data.frame(t(portfolioSelectAssets(qDP12m, 3, 3)))
MB <- as.data.frame(t(qMB))
V  <- as.data.frame(t(qMVclass))
MBhv <- mapply(function(mb,v,a) { sum(mb[a]*v[a]/sum(v[a], na.rm=T), na.rm=T) }
               , MB, V, A )

# Media Ponderada pelo VM do MB das acoes de baixa volatilidade (Low Volatility)
A  <- as.data.frame(t(portfolioSelectAssets(qDP12m, 3, 1)))
MBlv <- mapply(function(mb,v,a) { sum(mb[a]*v[a]/sum(v[a], na.rm=T), na.rm=T) }
               , MB, V, A )
rm(list=c("A","MB","V"))

# Calcular PVOL
prx_PVOL <- as.data.frame(MBhv)
prx_PVOL$MBlv <- MBlv
prx_PVOL$PVOL <- log(MBhv/MBlv)    
prx_PVOL <- prx_PVOL[sort(rep(1:nrow(prx_PVOL),3)),]
rownames(prx_PVOL) <- rownames(prx_TURN)
rm(list=c("MBlv","MBhv"))

## Organizar / Importar Proxies # ----------------------------------------------

# # Codigo Antigo: Importar base de proxies calculadas pelo Excel
# mProxies   <- read.table ("Input/mProxies.csv",          # Read data
#                           header = T, sep=";", dec=",",
#                           row.names=1)
# mProxies <- mProxies[!is.na(mProxies$NIPO_lagged),]

mProxies <- merge(prx_NIPO, prx_S, by = "row.names", all.y=T)
mProxies$CVM[is.na(mProxies$CVM)] <- 0
mProxies$A <- NULL ; mProxies$DEB <- NULL
rownames(mProxies) <- mProxies$Row.names ; mProxies$Row.names <- NULL
colnames(mProxies) <- c("NIPO", "S")

mProxies <- merge(mProxies, prx_TURN, by = "row.names", all.y=T)
mProxies$QT <- mProxies$QN <- mProxies$TURN <- NULL
rownames(mProxies) <- mProxies$Row.names ; mProxies$Row.names <- NULL
colnames(mProxies) <- c("NIPO", "S", "TURN")

mProxies <- merge(mProxies, prx_PVOL, by = "row.names", all.y=T)
mProxies$MBhv <- mProxies$MBlv <- NULL
rownames(mProxies) <- mProxies$Row.names ; mProxies$Row.names <- NULL

# mProxies      <- as.data.frame(prx_S$Issues) ; colnames(mProxies) <- "S"
# mProxies$NIPO <- c(prx_NIPO$CVM,rep(0,6))
# mProxies$TURN <- prx_TURN$dTURN
# mProxies$PVOL <- prx_PVOL$PVOL
# rownames(mProxies) <- rownames(prx_TURN)

LAG <- 12
mProxies$Slag <- c(rep(NA,LAG),mProxies$S[1:(nrow(mProxies)-LAG)])
mProxies$NIPOlag <- c(rep(NA,LAG),mProxies$NIPO[1:(nrow(mProxies)-LAG)])
mProxies$TURNlag <- c(rep(NA,LAG),mProxies$TURN[1:(nrow(mProxies)-LAG)])
mProxies$PVOLlag <- c(rep(NA,LAG),mProxies$PVOL[1:(nrow(mProxies)-LAG)])

# ## Plotar todas as Proxies
# # plot(ts(mProxies, start(1999,1), frequency=12))

## Correlations
#   as.dist(round(cor(mProxies, use="everything"),2)) # c/ Na
as.dist(round(cor(mProxies, use="na.or.complete"),2)) # s/ NA

## 3.2 First Step # ============================================================
# Estimating first component of all proxies and their lags and choose the best
mProxies <- mProxies[!is.na(mProxies$Slag),]
PCAstep1 <- prcomp(mProxies, scale=T)

round(cor(PCAstep1$x[,"PC1"],mProxies),2)         # The correlations
mBestProxies <- chooseLAG(mProxies);rm(chooseLAG) # Choosing LAGs...
colnames(mBestProxies)                            # Best proxies
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)     # Correlation with PC1
as.dist(round(cor(mBestProxies),2))               # Correlations between them

## 3.3 Second Step # ===========================================================
# Estimating first component of the best proxies

PCAstep2 <-prcomp(mBestProxies, scale=T)

cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]) # Correlation with PC1 of the 1º step
summary(PCAstep2)                          # Proportion of Variance
PCAstep2$rotation[,"PC1"] # Not orthogonalized index (osb.: not important)

## 3.4 Third Step # ============================================================
# Estimate orthogonilized proxies by the regression all raw proxies

## PIB (% Change)
## https://www.quandl.com/BCB/4380-GDP-monthly-current-prices-R-million
data_inicial <- as.Date(paste(PERIOD.n,"-12-31", sep=""))
PIB <- Quandl("BCB/4380",
              trim_start=data_inicial, trim_end="2014-06-30",
              transformation="rdiff", sort="asc")



## OECD Dummy (Recession)
## https://www.quandl.com/FRED/BRARECM
data_inicial <- as.Date(paste(PERIOD.n+1,"-01-01", sep=""))
RECESS <- Quandl("FRED/BRARECM",
                 trim_start=data_inicial, trim_end="2014-06-30", sort="asc")

mMacroeconomics <- data.frame(PIB=PIB$Value, RECESS=RECESS$Value,
                              row.names=PIB$Date)

rm(list = c("PIB", "RECESS", "data_inicial"))

## Consumo (% Change)

# # Read macroeconomics variables
# mMacroeconomics   <- read.table ("Input/mMacroeconomics.csv",   header = T, 
#                                  sep=";", dec=",", na.strings="-", row.names=1)
# # Date Filter
# x <- as.Date(rownames(mMacroeconomics), format="%d/%m/%Y")
# mMacroeconomics <-  mMacroeconomics[(x >= as.Date("2001-01-01") &
#                                          x <= as.Date("2013-12-01")),]
# rm(x)
# ## https://www.quandl.com/FRED/BRARECM
# # dummy SELIC igual a 1 quando a taxa cai em rela??o ao m?s anterior
# dSELIC <- c(0,as.numeric(embed(mMacroeconomics$SELIC,2)[,1] <= 
#                                  embed(mMacroeconomics$SELIC,2)[,2]
# ))
# # dummy PIB igual a 1 quando o PIB sobe em rela??o ao m?s anterior
# dPIB   <- c(0,as.numeric(embed(mMacroeconomics$PIB,2)[,1] >=
#                                  embed(mMacroeconomics$PIB,2)[,2]
# ))
# # Retirando a série da Selic e deixando só a do PIB
# mMacroeconomics$SELIC <- NULL
# # Acrescentando o dPIB e o dSELIC
# mMacroeconomics <-cbind(mMacroeconomics, dPIB, dSELIC)
# rm(list=c("dPIB","dSELIC"))

# Estimando Proxies Ortogonalizada
mProxiesOrtog <- mBestProxies
for ( i in 1:ncol(mProxiesOrtog)) {
        mProxiesOrtog[,i] <- lm(mBestProxies[,i] ~ data.matrix(mMacroeconomics))$residuals
}
rm(i)

# ## Plotar todas as mProxiesOrtog
# # plot(ts(mProxiesOrtog, start(1999,1), frequency=12))

# Estimando Componentes Principais da Terceira Etapa
PCAstep3 <-prcomp(mProxiesOrtog, scale=T)

# Estimando Componentes Principais da Terceira Etapa
PCAstep3 <-prcomp(mProxiesOrtog, scale=T)
# PCAstep3 <- princomp(mProxiesOrtog, scores=T, cor=T) # Metodo alternativo

# Verificando correlacao com o primeiro indice
cor(PCAstep2$x[,"PC1"],PCAstep3$x[,"PC1"])

# Percentual explicado da variancia
summary(PCAstep3)
# summary(princomp(mProxiesOrtog, scores=T, cor=T)) # Metodo alternativo

# Scree plot of eigenvalues
screeplot(PCAstep3, type="line", main="Scree Plot Sentimento Ortogonalizado")

PCAstep3$rotation[,"PC1"] * (-1) # Equacao do Indice de Sent. Ortogonalizado
Sent <- ts(PCAstep3$x[,"PC1"]*(-1), start=c(2000,1), frequency=12)
SentNO <- ts(PCAstep2$x[,"PC1"], start=c(2000,1), frequency=12)
## Plotar Sentimento e SentimentoNO (Não Ortogonalizado)
plot(SentNO, col="gray", lty="dashed")
lines(Sent, col="blue")

abline(h = 0, lty = 2)

# ## 4. INVESTOR SENTIMENT AND ANOMALIES ## 
# ## Sentimento do Investidor e Anomalias
# ## 4.1. Análise das Médias após períodos de Sentimento Alto e Baixo
# ## 4.2. Modelos Econométricos
# ## 4.1 Extremos e sentimento defasado
# ## 4.2 Extremos, sentimeto defasado e fatores de risco
# ## 4.3 Extremos, dummys
# 
# # TESTE INDICE
# LAG <- 12
# summary(lm(seriePortBM1$rVW[(1+LAG):156]  ~ PCAstep3$x[,"PC1"][1:(156-LAG)]))
# length(seriePortBM1$rVW[13:156])
# length(PCAstep3$x[,"PC1"][1:144])
plot(Sent, main="Sentiment", ylab=NULL, col="blue")

## 5. PRICING MODELS ## #######################################################
## 5.1 Serie do Retorno do Ativos Livre de Risco
## 5.2 Serie do Retorno da Carteira de Mercado
## 5.3 Construir Carteiras por Fatores
## 5.3 Fator Tamanho (Interação Tamanho e BM)
## 5.4 Fator BM      (Interação BM e Tamanho)
## 5.5 Serie de retorno dos demais fatores (MOM, LIQ)
##       Fator Momento
##       Fator Liquidez

## 5.1 Ativos Livre de Risco # =================================================
## Ativo Livre de Risco (download da Serie SELIC do Banco Central)
Rf <- riskFreeRate(PERIOD.n, PERIOD.N)

## 5.2 Carteira de Mercado # ===================================================

## Carteira de Mercado (Calculada)
MKT <- ts(portfolioSerie(mReturns, mMVclass, ySample)$rVW,
          start=c(PERIOD.n+1,07), frequency=12)

# ## Carteira de Mercado (Ibovespa)
# MKT  <- Quandl("BCB/7845", type="ts", collapse="monthly", sort="asc",
#                 # transformation="rdiff",
#                 trim_start=data_inicial-1, trim_end=data_final)
# MKT <- diff(log(MKT),1)

MKT <- MKT-Rf

## 5.2 Carteira de Mercado # ===================================================

## Carteiras por Fator
assetsF_Size_S <- portfolioSelectAssets(yMVclassJun,2,1) * 1 # Small
assetsF_Size_B <- portfolioSelectAssets(yMVclassJun,2,2) * 1 # Big   

assetsF_BM_H <- portfolioSelectAssets(yBM,3,1) * 1 # Value  (High BM)
assetsF_BM_N <- portfolioSelectAssets(yBM,3,2) * 1 # Neutral
assetsF_BM_L <- portfolioSelectAssets(yBM,3,3) * 1 # Growth (Low BM)

## Carteiras a partir da Interação
assetsF_SH <- assetsF_Size_S[-1,] * assetsF_BM_H # Small Value (High BM)
assetsF_SN <- assetsF_Size_S[-1,] * assetsF_BM_N # Small Neutral
assetsF_SL <- assetsF_Size_S[-1,] * assetsF_BM_L # Small Growth (Low BM)
assetsF_BH <- assetsF_Size_B[-1,] * assetsF_BM_H # Big Value (High BM)
assetsF_BN <- assetsF_Size_B[-1,] * assetsF_BM_N # Big Neutral
assetsF_BL <- assetsF_Size_B[-1,] * assetsF_BM_L # Big Growth (Low BM)
assetsF_SH <- apply(assetsF_SH, 2, function(x) as.logical(x) ) # Small Value (High BM)
assetsF_SN <- apply(assetsF_SN, 2, function(x) as.logical(x) ) # Small Neutral
assetsF_SL <- apply(assetsF_SL, 2, function(x) as.logical(x) ) # Small Growth (Low BM)
assetsF_BH <- apply(assetsF_BH, 2, function(x) as.logical(x) ) # Big Value (High BM)
assetsF_BN <- apply(assetsF_BN, 2, function(x) as.logical(x) ) # Big Neutral
assetsF_BL <- apply(assetsF_BL, 2, function(x) as.logical(x) ) # Big Growth (Low BM)
rownames(assetsF_SH) <- rownames(yBM)
rownames(assetsF_SN) <- rownames(yBM)
rownames(assetsF_SL) <- rownames(yBM)
rownames(assetsF_BH) <- rownames(yBM)
rownames(assetsF_BN) <- rownames(yBM)
rownames(assetsF_BL) <- rownames(yBM)

## Retornos
portF_SH <- portfolioSerie(mReturns, mMVclass, assetsF_SH)
portF_SN <- portfolioSerie(mReturns, mMVclass, assetsF_SN)
portF_SL <- portfolioSerie(mReturns, mMVclass, assetsF_SL)
portF_BH <- portfolioSerie(mReturns, mMVclass, assetsF_BH)
portF_BN <- portfolioSerie(mReturns, mMVclass, assetsF_BN)
portF_BL <- portfolioSerie(mReturns, mMVclass, assetsF_BL)
rm(list=ls(pattern = "assetsF_"))

## Fatores de Risco # ----------------------------------------------------------

## FATOR TAMANHO (SMB)
##
## SMB = 1/3 (Small Value + Small Neutral + Small Growth)
##       - 1/3 (Big Value + Big Neutral + Big Growth)
##
SMB <-(portF_SH$rVW+portF_SN$rVW+portF_SL$rVW)/3-(portF_BH$rVW+portF_BN$rVW+portF_BL$rVW)/3

## FATOR BM (HML)
##
## HML = 1/2 (Small Value + Big Value)   - 1/2 (Small Growth + Big Growth)
##
HML <- (portF_SH$rVW + portF_BH$rVW)/2 - (portF_SL$rVW + portF_BL$rVW)/2

## 5. CONSTRUCT PORTFOLIOS ## #################################################
## 5.1 BW  Portfolios
## 5.2 SYY Portfolios

## LongShort Stategies # -------------------------------------------------------

# TAMANHO  (VM Empresa Jun)
ls_TAM <- computeLongShort(mReturns, mMVclass, yMVfirmJun, 5)

# LIQUIDEZ (Volume Medio) JUN
ls_LIQ <- computeLongShort(mReturns, mMVclass, yVolumeJun, 5)

# BM
ls_BM <- computeLongShort(mReturns, mMVclass, yBM, 5)

# MOMENTO
yMomentum <- computeMomentum (mReturns)
yMomentum <- cleanData(yMomentum, ySample1)
ls_MOM <- computeLongShort(mReturns, mMVclass, yMomentum, 5)

## 6. INVESTOR SENTIMENT AND ANOMALIES ## ######################################
##    Sentimento do Investidor e Anomalias
##
## 6.1. Análise das Médias após períodos de Sentimento Alto e Baixo
## 6.2. Modelos Econométricos
## 6.2.1 Extremos e sentimento defasado
## 6.2.2 Extremos, sentimeto defasado e fatores de risco
## 6.2.3 Extremos, dummys

# model1
# modelCAPM
# model3F
# model4F
# model5F

#== 6.1 Análise de Médias = ===================================================

#== 6.2 Predictive Regressions = ==============================================

#   6.2.1 Sentiment and Returns - ---------------------------------------------
Rf  <- log(1+SELIC/100)
MKT <- ts(portfolioSerie(mReturns, mMVclass, ySample)$rVW,
          start=c(PERIOD.n+1,07), frequency=12)
MKT <- MKT-Rf

# LAG <- 2 FF Tam.L e LIQ.L
# LAG <- 5 Tam.S e LIQ.S (CAPM e FF)
# LAG <- 6 LIQ.L 0.02 e LIQ.LS 0.09
# LAG 7 CAPM (BM.L  LIQ.L) e FF (BM.L       LIQ.L      LIQ.LS)
LAG <- 12

N <- length(Rf)
Sentiment <- as.numeric(Sent)[(1+LAG):N]

## TAMANHO (Firma em Jun)
Long      <- ls_TAM$LONG[1:(N-LAG)]
Short     <- ls_TAM$SHORT[1:(N-LAG)]
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment ))$coefficients[2,]
result_Simple <- rbind(TAM.L=Long, TAM.S=Short, TAM.LS=LongShort)
result_Simple

## BM
Long      <- ls_BM$LONG[1:(N-LAG)]
Short     <- ls_BM$SHORT[1:(N-LAG)]
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment ))$coefficients[2,]
result_Simple <- rbind(result_Simple, BM.L=Long, BM.S=Short, BM.LS=LongShort)

## MOMENTUM
Long      <- ls_MOM$LONG[1:(N-LAG)]
Short     <- ls_MOM$SHORT[1:(N-LAG)]
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment ))$coefficients[2,]
result_Simple <- rbind(result_Simple, MOM.L=Long,MOM.S=Short,MOM.LS=LongShort)

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(N-LAG)]
Short     <- ls_LIQ$SHORT[1:(N-LAG)]
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment ))$coefficients[2,]
result_Simple <- rbind(result_Simple, LIQ.L=Long,LIQ.S=Short,LIQ.LS=LongShort)

rm(list=c("Long", "Short", "LongShort"))

#   6.2.2 Sentiment and Pricing Models - ---------------------------------------

## CAPM # ----------------------------------------------------------------------
Rf  <-  Rf[1:(N-LAG)]
MKT <- MKT[1:(N-LAG)]

## TAMANHO firm Jun
Long      <- ls_TAM$LONG[1:(N-LAG)] - Rf
Short     <- ls_TAM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]
result_CAPM <- rbind(TAM.L=Long, TAM.S=Short, TAM.LS=LongShort)

## BM
Long      <- ls_BM$LONG[1:(N-LAG)] - Rf
Short     <- ls_BM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]
result_CAPM <- rbind(result_CAPM, BM.L=Long, BM.S=Short, BM.LS=LongShort)

## MOMENTUM
Long      <- ls_MOM$LONG[1:(N-LAG)] - Rf
Short     <- ls_MOM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]
result_CAPM <- rbind(result_CAPM, MOM.L=Long,MOM.S=Short,MOM.LS=LongShort)

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(N-LAG)] - Rf
Short     <- ls_LIQ$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]
result_CAPM <- rbind(result_CAPM, LIQ.L=Long,LIQ.S=Short,LIQ.LS=LongShort)

## FF1993 # --------------------------------------------------------------------
# Sent.Long.Beta        <- lm(Long.Beta  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Beta       <- lm(Short.Beta ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Size        <- lm(Long.Size  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Size       <- lm(Short.Size ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Liquidity   <- lm(Long.Liquidity  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Liquidity  <- lm(Short.Liquidity ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.BM          <- lm(Long.BM  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.BM         <- lm(Short.BM ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)

SMB <- SMB[1:(N-LAG)]
HML <- HML[1:(N-LAG)]

## TAMANHO firm Jun
Long      <- ls_TAM$LONG[1:(N-LAG)] - Rf
Short     <- ls_TAM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
result_FF <- rbind(TAM.L=Long, TAM.S=Short, TAM.LS=LongShort)

## BM
Long      <- ls_BM$LONG[1:(N-LAG)] - Rf
Short     <- ls_BM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
result_FF <- rbind(result_FF, BM.L=Long, BM.S=Short, BM.LS=LongShort)

## MOMENTUM
Long      <- ls_MOM$LONG[1:(N-LAG)] - Rf
Short     <- ls_MOM$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
Short <- summary(lm( Short    ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
result_FF <- rbind(result_FF, MOM.L=Long,MOM.S=Short,MOM.LS=LongShort)

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(N-LAG)] - Rf
Short     <- ls_LIQ$SHORT[1:(N-LAG)] - Rf
LongShort <- (Long - Short)
Long  <- Long  <- summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
Short <- summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
LongShort <- summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
result_FF <- rbind(result_FF, LIQ.L=Long,LIQ.S=Short,LIQ.LS=LongShort)

result_Simple
result_CAPM
result_FF

t(result_Simple[(result_Simple[,4]<=0.1),])[3:4,]
t(result_CAPM[(result_CAPM[,4]<=0.1),])[3:4,]
t(result_FF[(result_FF[,4]<=0.1),])[3:4,]

rm(list=ls(pattern = "ls_"))

#   6.2.2 Sentiment High and Low - ---------------------------------------------

# Dummy.Long.Beta       <- lm(Long.Beta  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Beta      <- lm(Short.Beta ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.BM         <- lm(Long.BM  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.BM        <- lm(Short.BM ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Size       <- lm(Long.Size  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Size      <- lm(Short.Size ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Liquidity  <- lm(Long.Liquidity  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Liquidity <- lm(Short.Liquidity ~ dH+dL+MKT+SMB+HML+MOM+LIQ)

# --- ANOTAÇÕES --- ------------------------------------------------------------

#' RESULTADOS

## AMOSTRA INICIAL E APOS OS FILTROS # -----------------------------------------
F1 <- sampleReport(ySample0, ySample1)
F2 <- sampleReport(ySample0, ySample2)[,2:3]
F3 <- sampleReport(ySample0, ySample3)[,2:3]
F4 <- sampleReport(ySample0, ySample4)[,2:3]
F5 <- sampleReport(ySample0, ySample5)[,2:3]
colnames(F1) <- c("A.I.","Filtro 1","%")
colnames(F2) <- c("Filtro 2","%")
colnames(F3) <- c("Filtro 3","%")
colnames(F4) <- c("Filtro 4","%")
colnames(F5) <- c("Filtro 5","%")
# F1: Filtro de Empresas Nao Financeiras
# F2: FILTRO DE 24 MESES
# F3: Filtro Bovespa Negociability Index
# F4: Filtro Valor de Mercado em 30/06 e 31/12
# F5: Filtro Patrimonio Liquido
cbind(F1, F2, F3, F4, F5) ; rm(list=c("F1","F2","F3","F4","F5"))

## INDICE DE SENTIMENTO RESULTADOS # -------------------------------------------
as.dist(round(cor(mProxies),2))                      # Verificando correlação entre as proxies
round(cor(PCAstep1$x[,"PC1"],mProxies),2)            # Correlação das Proxies com 1ª Componente da 1ª Etapa
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)        # Correlação Proxies Escolhidas c/ 1ª Componente da 1ª Etapa
cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]) * (-1)    # Verificando correlacao com o primeiro indice
summary(PCAstep2)                                    # Percentual explicado da variancia
PCAstep2$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Nao Ortogonalizado
as.dist(round(cor(mBestProxies),2))                  # Correlação Proxies Escolhidas
round(cor(PCAstep2$x[,"PC1"],mBestProxies),2) * (-1) # Correlação Proxies Escolhidas c/ 1ª Componente da 2ª Etapa
cor(PCAstep2$x[,"PC1"],PCAstep3$x[,"PC1"])           # Correlação do Indice da 3ª etapa com o da 2ª etapa
summary(PCAstep3)                                    # Percentual explicado da variancia
PCAstep3$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Ortogonalizado

## Quintis conforme Anomalias # ------------------------------------------------
allQuintiles(yMVfirmJun,  mReturns, mMVclass) ## TAMANHO  (VM Empresa Jun)
allQuintiles(yMVclassJun, mReturns, mMVclass) ## TAMANHO  (VM Classe  Jun)
allQuintiles(yVolumeJun, mReturns, mMVclass)  ## LIQUIDEZ (Volume Medio) JUN
allQuintiles(yBM, mReturns, mMVclass)         ## BM
allQuintiles(yMomentum, mReturns, mMVclass)   ## MOMENTO