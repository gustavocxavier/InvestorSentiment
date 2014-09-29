## Title: Investor Sentiment and Anomalies in Brazilian Market
##
## Version: 0.0.1
##
## Description: Script to compute que Investor Sentiment Index of the brazilian
## market.                       
## 

## CONTENTS ## ################################################################
## 1. SETTINGS
## 2. GET DATA AND CLEAN
## 3. INVESTOR SENTIMENT INDEX
## 4. CONSTRUCT PORTFOLIOS
## 5. PRICING MODELS
## 6. INVESTOR SENTIMENT AND ANOMALIES
##

## COISAS PRA FAZER AINDA ## ##################################################
## - Indice de Sentimento totalmente Calculado no R
## - Retorno da Carteira de Mercado
## - VM Empresa qnd ON e PN / VM Classe qnd so uma classe na amostra
## - Funcao LongShortSeries
## - FILTRO Bovespa Negociability Index
## - FAZER UM FILTRO DE DATA PRA mProxies em breve

#' 

## 1. SETTINGS ## #############################################################

## Instalar e carregar bibliotecas / Install and load packages
if ( !( "xts" %in% installed.packages() ) ) { install.packages("xts") }
library(xts)

if ( !( "lubridate" %in% installed.packages() ) ) { install.packages("lubridate") }
library(lubridate)

## Definir Parametros / Set Parameters
setwd("C:/Dropbox/investorSentiment") # Pasta de Trabalho / Working Directory
START        <- as.Date("2000-06-01") # Data Inicial / Initial Date
END          <- as.Date("2014-07-31") # Data Final / Final Date
PERIOD.XTS   <- "2000-06/2014-07"     # Periodo / Period
# Amostra de M&O(2011): jun/95 a jun/08

## Rodar Minhas Funçoes / Run my functions
source("R/functions.R")

## 2. GET DATA AND CLEAN ## ##################################################
# Carregar e limpar dados / Get Data and Clean

#== 2.1 Read Data = ==========================================================

# Carregando matriz de precos / Stock Prices
mPrices            <- read.table ("Input/mPrices.csv", header = T, sep=";",
                                  dec=",", skip=0, row.names=1, na.strings="-",
                                  stringsAsFactors=F)
row.names(mPrices) <- as.Date(row.names(mPrices), format="%d/%m/%Y")

# Filtrando Periodo
mPrices.xts        <- as.xts(mPrices, descr="MONTHLY PRICES")[PERIOD.XTS]
mPrices            <- data.frame(as.matrix(mPrices.xts)) ; rm(mPrices.xts)

#== 2.2 Initial Sample = ======================================================
# Initial Sample (1 para todos os anos que houve cotacao)
mSample0                  <- as.matrix(mPrices)
mSample0[!is.na(mPrices)] <- 1
mSample0[is.na(mPrices)]  <- 0
ySample0                  <- as.matrix(apply.yearly(as.xts(mSample0), mean))
rm(mSample0)
ySample0[ySample0>0]      <- 1

#   2.2.1 Filtro de Empresas Nao Financeiras - --------------------------------

ySample1 <- filterNoFinancial(ySample0, "Input/dbStocks.csv")

#   2.2.2 FILTRO DE 24 MESES - ------------------------------------------------

ySample2 <- filterNo24months(mPrices, ySample0) * ySample1

#   2.2.3 Filtro Bovespa Negociability Index - --------------------------------

mNegociab <- read.table ("Input/mNegociabilidade.csv", header = T, sep=";",
                         dec=",", skip=0, row.names=1, na.strings="-",
                         stringsAsFactors=F)
rownames(mNegociab) <- as.Date(rownames(mNegociab),"%d/%m/%Y")

# Filtrando Periodo
mNegociab.xts <- as.xts(mNegociab)[PERIOD.XTS]
mNegociab     <- data.frame(as.matrix(mNegociab.xts)); rm(mNegociab.xts)

yNegociab <- mNegociab[(months(as.Date(rownames(mNegociab)), T)=="jun"),]

ySample3 <- ySample2 # Cria matriz de controle da amostra a partir da ultima
ySample3[!is.na(ySample3)] <- 1 # Atribui temporariamente 1 a todos os campos

# Atribui 0 para os valores que nao satisfazem a condicao desejada em jun/n
ySample3[is.na(yNegociab)]   <- 0 # Valores invexistentes em jun na base
ySample3[(yNegociab < 0.001)] <- 0 # Valores abaixo de 0.01 em jun na base

ySample3 <- ySample3 * ySample2 # Interagem com atendem ao filtro anterior

#   2.2.4 Filtro Valor de Mercado em 30/06 e 31/12 - --------------------------

# Read Market Value of the Firm
mMVfirm <- read.table ("Input/mMarketValueFirm.csv", header = T, sep=";", dec=",",
                       skip=1, row.names=1, na.strings="-", stringsAsFactors=F)
rownames(mMVfirm) <- as.Date(rownames(mMVfirm),"%d/%m/%Y")

# Filtrando Periodo
mMVfirm.xts <- as.xts(mMVfirm)[PERIOD.XTS]
mMVfirm     <- data.frame(as.matrix(mMVfirm.xts)); rm(mMVfirm.xts)
# Cria matriz apenas com os valores de Dez e apenas com valores de Jun
yMVfirmJun <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="jun"),]
yMVfirmDez <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="dez"),]

ySample4 <- ySample3 # Cria matriz de controle da amostra a partir da ultima
ySample4[!is.na(ySample3)] <- 1 # Atribui temporariamente 1 a todos os campos

# Atribui 0 para os valores que nao satisfazem a condicao desejada em dez/n-1
ySample4[-1,][yMVfirmDez <= 0]   <- 0 # Valores zerados em dez/n-1 na base
ySample4[-1,][is.na(yMVfirmDez)] <- 0 # Valores invexistentes em dez/n-1 na base

# Atribui 0 para os valores que nao satisfazem a condicao desejada em jun/n
ySample4[(yMVfirmJun <= 0)] <- 0 # Valores zerados em jun na base
ySample4[is.na(yMVfirmJun)] <- 0 # Valores invexistentes em jun na base

ySample4 <- ySample4 * ySample3 # Interagem com atendem ao filtro anterior

#   2.2.5 Filtro Patrimonio Liquido - -----------------------------------------

yBookFirm <- read.table ("Input/yBookFirm.csv", header = T, sep=";",
                         dec=",", skip=0,
                         row.names=1, na.strings="-", stringsAsFactors=F)
row.names(yBookFirm) <- as.Date(paste(rownames(yBookFirm),"-12-01", sep=""),
                                "%Y-%m-%d")
yBookFirm            <- as.xts(yBookFirm, descr='Patrimonio Liquido')
yBookFirm            <- as.data.frame(as.matrix(yBookFirm[PERIOD.XTS]))

ySample5 <- filterPositiveBook(ySample4, yBookFirm) * ySample4

#== 2.3 Final Sample = ========================================================

# Salvar Amostra Final em um data frame lógico
ySample <- data.frame(apply( ySample4, 2, as.logical ),
           row.names=rownames(ySample4))

## RELATORIO
F1 <- sampleReport(ySample0, ySample1)
F2 <- sampleReport(ySample0, ySample2)[,2:3]
F3 <- sampleReport(ySample0, ySample3)[,2:3]
F4 <- sampleReport(ySample0, ySample4)[,2:3]
F5 <- sampleReport(ySample0, ySample5)[,2:3]
colnames(F1) <- c("Initial","(Filtro","1)")
colnames(F2) <- c("(Filtro","2)")
colnames(F3) <- c("(Filtro","3)")
colnames(F4) <- c("(Filtro","4)")
colnames(F5) <- c("(Filtro","5)")
# F1: Filtro de Empresas Nao Financeiras
# F2: FILTRO DE 24 MESES
# F3: Filtro Bovespa Negociability Index
# F4: Filtro Valor de Mercado em 30/06 e 31/12
# F5: Filtro Patrimonio Liquido
cbind(F1, F2, F3, F4, F5) ; rm(list=c("F1","F2","F3","F4","F5"))

## 3. INVESTOR SENTIMENT INDEX ## #############################################
## 3. Índice de Sentimento
## 3.1. Temporalidade das Proxies: Selecionar proxies que serão defasadas
## 3.2. Índice de Sentimento não Ortogonalizado
## 3.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  

#== 3.1 Read/Compute Proxies = ===============================================

mProxies   <- read.table ("Input/mProxies.csv",          # Read data
                          header = T, sep=";", dec=",",
                          row.names=1)

# TO DO: FAZER UM FILTRO DE DATA PRA mProxies em breve
mProxies <- mProxies[!is.na(mProxies$NIPO_lagged),]

#as.dist(round(cor(mProxies, use="na.or.complete"),2))    # Correlations s/ NA
as.dist(round(cor(mProxies, use="everything"),2))         # Correlations c/ Na

#== 3.2 First Step = ==========================================================
# Estimating first component of all proxies and their lags and choose the best

PCAstep1 <- prcomp(mProxies, scale=T)

round(cor(PCAstep1$x[,"PC1"],mProxies),2)         # The correlations
mBestProxies <- chooseLAG(mProxies);rm(chooseLAG) # Choosing LAGs...
colnames(mBestProxies)                            # Best proxies
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)     # Correlation with PC1
as.dist(round(cor(mBestProxies),2))               # Correlations between them

#== 3.3 Second Step = =========================================================
# Estimating first component of the best proxies

PCAstep2 <-prcomp(mBestProxies, scale=T)

cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]) # Correlation with PC1 of the 1º step
summary(PCAstep2)                          # Proportion of Variance
PCAstep2$rotation[,"PC1"] # Not orthogonalized index (osb.: not important)

#== 3.4 Third Step = ==========================================================
# Estimate orthogonilized proxies by the regression all raw proxies

# Read macroeconomics variables
mMacroeconomics   <- read.table ("Input/mMacroeconomics.csv",   header = T, 
                                 sep=";", dec=",", na.strings="-", row.names=1)

# Date Filter
x <- as.Date(rownames(mMacroeconomics), format="%d/%m/%Y")
mMacroeconomics <-  mMacroeconomics[(x >= as.Date("2001-01-01") &
                                         x <= as.Date("2013-12-01")),]
rm(x)

# dummy SELIC igual a 1 quando a taxa cai em rela??o ao m?s anterior
dSELIC <- c(0,as.numeric(embed(mMacroeconomics$SELIC,2)[,1] <= 
                                 embed(mMacroeconomics$SELIC,2)[,2]
))

# dummy PIB igual a 1 quando o PIB sobe em rela??o ao m?s anterior
dPIB   <- c(0,as.numeric(embed(mMacroeconomics$PIB,2)[,1] >=
                                 embed(mMacroeconomics$PIB,2)[,2]
))

# Retirando a série da Selic e deixando só a do PIB
mMacroeconomics$SELIC <- NULL
# Acrescentando o dPIB e o dSELIC
mMacroeconomics <-cbind(mMacroeconomics, dPIB, dSELIC)
rm(list=c("dPIB","dSELIC"))

# Estimando Proxies Ortogonalizada
mProxiesOrtog <- mBestProxies
for ( i in 1:ncol(mProxiesOrtog)) {
        mProxiesOrtog[,i] <- lm(mBestProxies[,i] ~ data.matrix(mMacroeconomics))$residuals
}
rm(i)

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
Sent <- PCAstep3$x[,"PC1"]

# Resultados Indice de Sentimento

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
plot(ts(Sent, start=c(2001,1), end=c(2013,12), frequency=12), main="Sentiment", ylab=NULL)


## 4. CONSTRUCT PORTFOLIOS ## #################################################
## 4. Portfolios
## 4.1 Construir Carteiras
##       portfolioAssets cria_matriz_carteira - retorna dCriterio
## 4.2 Interação de Carteiras
##       portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 4.3 Retorno das Carteiras
##       portfolioSerie - retorna ...
## 4.1 Construcao (MM utilizou 5 carteiras)
## 4.1.1 portfolioRange
## 3.1.2 portfolioAssets (cria_matriz_carteira - retorna dCriterio)
## 4.1.3 portfolioSerie  (rEW, rVW, n, xVM, xC)
##       (CALCULAR MOM, SIZ E LIQ E COMPARAR COM MM)
## 4.2   Pricing Model
## 4.2.1 Série de Retorno da Carteira de Mercado 
##       (COMPARAR COM MM O CAPM)
## 4.2.2 portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 4.2.3 Serie de retorno dos fatores FF (SMB, HML)
##       (COMPARAR BM SIMPLES COM MM, retorno merdio e capm)
##       (COMPARAR FF COM MM)
## 4.2.4 Serie de retorno dos demais fatores (MOM, LIQ)

### Compute Logarithmic Return
pXTS <- as.xts(mPrices)
mReturns <- as.data.frame( diff(log(pXTS), lag=1) ) ; rm(pXTS)

#== TAMANHO === ===============================================================

yMVfirmJun <- cleanData(yMVfirmJun, ySample)

# Valor de Mercado da Classe para ponderacao
mMVclass <- read.table ("Input/mMarketValue.csv", header = T, sep=";", dec=",",
                        skip=1, row.names=1, na.strings="-", stringsAsFactors=F)
rownames(mMVclass) <- as.Date(rownames(mMVclass),"%d/%m/%Y")

# Filtrando Periodo
mMVclass.xts <- as.xts(mMVclass, descr="MONTHLY PRICES")[PERIOD.XTS]
mMVclass     <- data.frame(as.matrix(mMVclass.xts)) ; rm(mMVclass.xts)

allQuintiles(yMVfirmJun, mReturns, mMVclass)

# Testando c/ MV class ------------------------------------------------------
## Cria matriz apenas com os valores de Dez e apenas com valores de Jun
yMVclassJun <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="jun"),]
## yMVclassDez <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="dez"),]
yMVclassJun <- cleanData(yMVclassJun, ySample)
allQuintiles(yMVclassJun, mReturns, mMVclass)

#== LIQUIDEZ === ============================================================

# Valor de Mercado da Classe para ponderacao
mVolume <- read.table ("Input/mVolume.csv", header = T, sep=";", dec=",",
                       skip=1, row.names=1, na.strings="-", stringsAsFactors=F)
rownames(mVolume)  <- as.Date(rownames(mVolume),"%d/%m/%Y")

# Filtrando Periodo
mVolume.xts <- as.xts(mVolume, descr='Volume em Reais')[PERIOD.XTS]
mVolume     <- data.frame(as.matrix(mVolume.xts)) ; rm(mVolume.xts)

# Cria matriz apenas com valores de Jun
yVolume <- mVolume[(months(as.Date(rownames(mVolume)), T)=="jun"),]

yVolume            <- cleanData(yVolume,    ySample)

allQuintiles(yVolume, mReturns, mMVclass)

#== BM === ==================================================================
# TO DO: Calcular BM da classe

# yBM <- yBookFirm / yMVfirmDez
# 
# allQuintiles(yBM, mReturns, mMVclass)

#== MOMENTO === =============================================================

yReturns <- period.apply(mReturns,endpoints(mVolume,'years'), sum)
yReturns <- apply.yearly(mReturns, mean)
yReturns <- cleanData(yReturns, ySample)

allQuintiles(yReturns, mReturns, mMVclass)

#== Returns === =============================================================

# Compute Returns
# tempPrices  <- importaBaseCSV("Input/mPrices.csv", (START-31), END)
# mReturns    <- as.data.frame(diff(log(as.matrix(mPrices)))) ; rm(tempPrices)
# 
# # Read Market Value
# mMarketValue     <- importaBaseCSV("Input/mMarketValue.csv",
#                                    START, END, pula_linha=1)
# 
# # Convert monthly data to yearly
# yMarketValue <- mMarketValue[dateIndex$M==12,]
# 
# yMarketValue[ySample==0]      <- NA
# yMarketValue[yMarketValue==0] <- NA
# yBookFirm[ySample==0]         <- NA
# 
# # Compute Book-to-market
# yBM <- yBookFirm / yMarketValue
# length(yBookFirm[yBookFirm==0])
# yBM[yMarketValue==0] <- NA
# yBM[yBM==+Inf]
# sum(yBookFirm==0, na.rm=T)
# sum(yMarketValue==0, na.rm=T)
# sum(yBM<=-Inf, na.rm=T)
# 
# head(which(yBookFirm!=0, arr.ind=T))
# yBookFirm[,1:2]
# 
# ##Teste Range
# portfolioRange(yMarketValue[2,],2,1)
# portfolioRange(yBM,3,1) # Verificar o -Inf e o +Inf
# 
# # szS szB bmH bmN bmL SH SN SL BN BL
# AssetsSize_S <- portfolioAssets2(yMarketValue,2,1)  # Small
# AssetsSize_B <- portfolioAssets2(yMarketValue,2,2)  # Big   
# 
# AssetsBM_H <- portfolioAssets2(yBM,3,1)             # Value (High BM)
# AssetsBM_N <- portfolioAssets2(yBM,3,2)             # Neutral
# AssetsBM_L <- portfolioAssets2(yBM,3,3)             # Growth (Low BM)
# 
# AssetsSH <- AssetsSize_S * AssetsBM_H # Small Value (High BM)
# AssetsSN <- AssetsSize_S * AssetsBM_N # Small Neutral
# AssetsSL <- AssetsSize_S * AssetsBM_L # Small Growth (Low BM)
# AssetsBH <- AssetsSize_B * AssetsBM_H # Big Value (High BM)
# AssetsBN <- AssetsSize_B * AssetsBM_N # Big Neutral
# AssetsBL <- AssetsSize_B * AssetsBM_L # Big Growth (Low BM)
# 
# AssetsSH <- apply(AssetsSH, 1, function(x) as.logical(x) ) # Small Neutral
# AssetsSN <- apply(AssetsSN, 2, function(x) as.logical(x) ) # Small Neutral
# AssetsSL <- apply(AssetsSL, 2, function(x) as.logical(x) ) # Small Growth (Low BM)
# AssetsBH <- apply(AssetsBH, 2, function(x) as.logical(x) ) # Big Value (High BM)
# AssetsBN <- apply(AssetsBN, 2, function(x) as.logical(x) ) # Big Neutral
# AssetsBL <- apply(AssetsBL, 2, function(x) as.logical(x) ) # Big Growth (Low BM)
# AssetsSH[1:5,1:5]
# 
# rownames(AssetsSH) <- rownames(yBM)
# rownames(AssetsSN) <- rownames(yBM)
# rownames(AssetsSL) <- rownames(yBM)
# rownames(AssetsBH) <- rownames(yBM)
# rownames(AssetsBN) <- rownames(yBM)
# rownames(AssetsBL) <- rownames(yBM)
# 
# # ...........................
# 
# interactPortfolios <- function (x, y) {
#         # Criando tabela
#         tabela <- x
#         for (i in 1:nrow(tabela)) {
#                 tabela[i,] <- as.logical(x[i,] * y[i,])
#         }
#         return(tabela)
# }
# 
# C <- A[c(1,2,1,3,5),c(1,2,3,4,5,1)]
# B
# D <- B*C
# apply(D, 2, function(x) as.logical(x) )
# 
# # ........ Procurar no stack overflow como transformar data.frame 1 o em logico
# 
# # HML = 1/2 (Small Value + Big Value) - 1/2 (Small Growth + Big Growth)
# FactorHML <- 1/2*(AssetsSH)
# 
# # SMB = 1/3 (Small Value + Small Neutral + Small Growth)
# #       - 1/3 (Big Value + Big Neutral + Big Growth)
# PortfolioSMB <- 
#         PortfolioHML
# debug(portfolioSerie)
# serieSmallValue <- portfolioSerie(mReturns, mMarketValue,AssetsSH)
# warnings()
# head(AssetsSH[,1:5])
# head(mReturns[,1:5])
# tail(mReturns[,1:5])
# tail(AssetsSH[,1:5])
# 
# serieSmallValue <- portfolioSerie(mReturns, mMarketValue,AssetsSH)
# 
# seriePortBM1 <- portfolioSerie(mReturns, mMarketValue, portfolioAssets2(yBM,5,1))
# 
# # TESTE INDICE
# LAG <- 12
# summary(lm(seriePortBM1$rWV[(1+LAG):156]  ~ PCAstep3$x[,"PC1"][1:(156-LAG)]))
# 
# 
# length(seriePortBM1$rWV[13:156])
# length(PCAstep3$x[,"PC1"][1:144])
# 
# 
# # _____________________________________________________________________________
# # TESTE UTILIZANDO DADOS REAIS
# 
# nAtivos <- 1108
# 
# precins  <- mPrices[12:36,1:nAtivos]
# retornin <- diff(log(as.matrix(precins)))
# valorzin <- mMarketValue[13:36,1:nAtivos]
# criterin <- yBookFirm[1:2,1:nAtivos]
# criterin <- yBookFirm[1:2,1:nAtivos] / valorzin[c(12,24),]
# 
# #portfolioAssets2(criterin,3,1)
# #portfolioAssets2(criterin,3,3)
# #portfolioSerie(retornin, valorzin, portfolioAssets2(criterin,5,1))
# 
# #rm(list=c("precins", "retornin", "criterin", "valorzin", "nAtivos"))
# 
# ..............................................................................
# 
# ..............................................................................

# _____________________________________________________________________________
## TESTE EM VETORES
#
#portfolioRange(yNegociabilidade[1,],5,5)
#portfolioAssets(yNegociabilidade[1,],5,1)
# _____________________________________________________________________________
## TESTE EM MATRIZ
## SIMULANDO VALORES
#retornin <- matrix(rnorm((12*3*5),0,0.3), ncol=5, nrow=(12*3))
#criterin <- matrix(round( rnorm((3*5),10,5) ,0), ncol=5, nrow=3)
#valorzin <- matrix(round(rnorm((12*3*5),100,50),0), ncol=5, nrow=(12*3))
#
#portfolioSerie(retornin,
#               valorzin,
#               portfolioAssets2(criterin,5,1)
#)
#rm(list=c("retornin", "criterin", "valorzin"))
# ______________________________________________________________________________
# TESTE UTILIZANDO A FUNÇÃO APPLY
#
#wPortfolio <- t(         apply(valorzin,
#                               MARGIN=1, function (x) (x/sum(x, na.rm=T)) ) )
#rPortfolio <- as.matrix( apply(retornin*wPortfolio,
#                               MARGIN=1, sum, na.rm=T                     ) )
#
#



## 5. PRICING MODELS ## #######################################################
## 5.1 Ativos Livre de Risco
## 5.2 Carteiras de Mercado
## 5.3 Fator Tamanho
## 5.4 Fator BM
## 5.5 Outros fatores

# # ### PRICING MODEL ### 
# 
# # szS szB bmH bmN bmL SH SN SL BN BL
# AssetsSize_S <- portfolioAssets2(yMV,2,1)  # Small
# AssetsSize_B <- portfolioAssets2(yMV,2,2)  # Big   
# 
# AssetsBM_H <- portfolioAssets2(yBM,3,1)    # Value (High BM)
# AssetsBM_N <- portfolioAssets2(yBM,3,2)    # Neutral
# AssetsBM_L <- portfolioAssets2(yBM,3,3)    # Growth (Low BM)
# 
# AssetsSH <- AssetsSize_S * AssetsBM_H # Small Value (High BM)
# AssetsSN <- AssetsSize_S * AssetsBM_N # Small Neutral
# AssetsSL <- AssetsSize_S * AssetsBM_L # Small Growth (Low BM)
# AssetsBH <- AssetsSize_B * AssetsBM_H # Big Value (High BM)
# AssetsBN <- AssetsSize_B * AssetsBM_N # Big Neutral
# AssetsBL <- AssetsSize_B * AssetsBM_L # Big Growth (Low BM)
# 
# AssetsSH <- apply(AssetsSH, 1, function(x) as.logical(x) ) # Small Neutral
# AssetsSN <- apply(AssetsSN, 2, function(x) as.logical(x) ) # Small Neutral
# AssetsSL <- apply(AssetsSL, 2, function(x) as.logical(x) ) # Small Growth (Low BM)
# AssetsBH <- apply(AssetsBH, 2, function(x) as.logical(x) ) # Big Value (High BM)
# AssetsBN <- apply(AssetsBN, 2, function(x) as.logical(x) ) # Big Neutral
# AssetsBL <- apply(AssetsBL, 2, function(x) as.logical(x) ) # Big Growth (Low BM)
# AssetsSH[1:5,1:5]
# 
# rownames(AssetsSH) <- rownames(yBM)
# rownames(AssetsSN) <- rownames(yBM)
# rownames(AssetsSL) <- rownames(yBM)
# rownames(AssetsBH) <- rownames(yBM)
# rownames(AssetsBN) <- rownames(yBM)
# rownames(AssetsBL) <- rownames(yBM)
# 
# # ...........................
# 
# interactPortfolios <- function (x, y) {
#     # Criando tabela
#     tabela <- x
#     for (i in 1:nrow(tabela)) {
#         tabela[i,] <- as.logical(x[i,] * y[i,])
#     }
#     return(tabela)
# }
# 
# 
# # HML = 1/2 (Small Value + Big Value) - 1/2 (Small Growth + Big Growth)
# FactorHML <- 1/2*(AssetsSH)
# 
# # SMB = 1/3 (Small Value + Small Neutral + Small Growth)
# #       - 1/3 (Big Value + Big Neutral + Big Growth)
# PortfolioSMB <- 
#     PortfolioHML
# debug(portfolioSerie)
# serieSmallValue <- portfolioSerie(mReturns, mMVclass,AssetsSH)
# warnings()
# head(AssetsSH[,1:5])
# head(mReturns[,1:5])
# tail(mReturns[,1:5])
# tail(AssetsSH[,1:5])
# 
# serieSmallValue <- portfolioSerie(mReturns, mMVclass,AssetsSH)
# 
# seriePortBM1 <- portfolioSerie(mReturns, mMVclass, portfolioAssets2(yBM,5,1))
# 



## 6. INVESTOR SENTIMENT AND ANOMALIES ## #####################################
##    Sentimento do Investidor e Anomalias
##
## 6.1. Análise das Médias após períodos de Sentimento Alto e Baixo
## 6.2. Modelos Econométricos
## 6.2.1 Extremos e sentimento defasado
## 6.2.2 Extremos, sentimeto defasado e fatores de risco
## 6.2.3 Extremos, dummys

#== 6.1 Análise de Médias = ===================================================

#== 6.2 Predictive Regressions = ==============================================

#   6.2.1 Sentiment and Returns - ---------------------------------------------

# Sent.Long.Beta        <- lm(Long.Beta  ~ SENT[_n-1])
# Sent.Short.Beta       <- lm(Short.Beta ~ SENT[_n-1])
# Sent.Long.Size        <- lm(Long.Size  ~ SENT[_n-1])
# Sent.Short.Size       <- lm(Short.Size ~ SENT[_n-1])
# Sent.Long.Liquidity   <- lm(Long.Liquidity  ~ SENT[_n-1])
# Sent.Short.Liquidity  <- lm(Short.Liquidity ~ SENT[_n-1])
# Sent.Long.BM          <- lm(Long.BM  ~ SENT[_n-1])
# Sent.Short.BM         <- lm(Short.BM ~ SENT[_n-1])

#   6.2.2 Sentiment and Pricing Models - --------------------------------------

# Sent.Long.Beta        <- lm(Long.Beta  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Beta       <- lm(Short.Beta ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Size        <- lm(Long.Size  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Size       <- lm(Short.Size ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Liquidity   <- lm(Long.Liquidity  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Liquidity  <- lm(Short.Liquidity ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.BM          <- lm(Long.BM  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.BM         <- lm(Short.BM ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)

#   6.2.2 Sentiment High and Low - --------------------------------------------

# Dummy.Long.Beta       <- lm(Long.Beta  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Beta      <- lm(Short.Beta ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.BM         <- lm(Long.BM  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.BM        <- lm(Short.BM ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Size       <- lm(Long.Size  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Size      <- lm(Short.Size ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Liquidity  <- lm(Long.Liquidity  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Liquidity <- lm(Short.Liquidity ~ dH+dL+MKT+SMB+HML+MOM+LIQ)

# --- ANOTAÇÕES --- -----------------------------------------------------------

### Teste replicação M&O (2011)
# OK 1 - tratar amostra
# OK 2 - calcular retornos
# OK 3 - Calcular 5 carteiras p/ cada anomalia MOM, SIZ, LIQ
# OK      Rebalanceamento: em JUNHO de acordo com variavel de interesse
# OK      * Todos retornos mensais ponderado pelo valor de mercado
#         Primeiras Anomalias: MOM, SIZ, LIQ (Depois BM, FC/P, L/P, ALAV)
#         ANALIZAR RETORNO MÉDIO COM TRABALHO DE MM
# OK 4 - Calcular BM (simples)
# 5 - Calcular MKT
# 7 - Comparar resultados CAPM
#         Calcular Série Carteira de Mercado
# 8 - Calcular FF Factors
#         Calcular Série SMB
#         Calcular Série HML
# 9 - Comparar resultados FF
# 10- Fazer todos os fatores juntos
#         Calcular Série LIQ
#         Calcular Série MOM

### Outras informacoes:
# variavel de interesse = criterio = estrategia
#
