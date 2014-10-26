## Title: Investor Sentiment and Anomalies in Brazilian Market
##
## Version: 0.0.1
##
## Description: Script to compute que Investor Sentiment Index of the brazilian
## market.                       
## 

## CONTENTS ## #################################################################
## 1. SETTINGS
## 2. LOAD DATA AND CLEAN
## 3. INVESTOR SENTIMENT INDEX
## 4. PRICING MODELS
## 5. CONSTRUCT PORTFOLIOS
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
pkg <- "zoo"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "dynlm"     ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "lubridate" ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "Quandl"    ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "TTR"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "XML"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "xts"       ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "lmtest"    ; if ( !(pkg %in% ip) ) { install.packages(pkg) }
pkg <- "sandwich"  ; if ( !(pkg %in% ip) ) { install.packages(pkg) }

rm(list=c("ip","pkg"))

## Carregar pacotes / Load packages --------------------------------------------
library("zoo")
library("dynlm")
library("lubridate")
library("Quandl") ; Quandl.auth("WP2rt8HsRo3kjWsRkLY5")
library("xts")
library("TTR")
library("XML")
library("lmtest")
library("sandwich")

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

f_PortfReturns  <- filterPortfReturns(mReturns, PERIOD.n, PERIOD.N)
f_MVclass       <- filterMonthMV(mMVclass, PERIOD.n, PERIOD.N)
f_MOM           <- filterMOM(mReturns, PERIOD.n, PERIOD.N)
f_MVjun         <- filterNA(mMVfirm, PERIOD.n, PERIOD.N, "jun")
f_MVdez         <- filterNA(mMVfirm, PERIOD.n, PERIOD.N, "dez")
f_BookNA        <- filterNA(yBookFirm, PERIOD.n, PERIOD.N, "dez")
f_BookPositive  <- filterGreaterThan(yBookFirm, 0, PERIOD.n, PERIOD.N, "dez")
f_IN            <- filterGreaterThan(yNegociab, 0.01, PERIOD.n, PERIOD.N, "jun")

## Calcular Amostra Inicial
ySample0 <- initialSample(mPrices) ; rownames(ySample0) <- rownames(yMVclassJun)

## Filtrar de Empresas Nao Financeiras
ySample1 <- filterNoFinancial(ySample0, "Input/dbStocks.csv")
f_NoFinancial <- ySample1

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

## Periodo p/ Proxies Sentimento. Ex.: JAN/n a JUN/N (Ex. 1999-01/2014-06)
PERIOD.PRX <- paste(PERIOD.n,"-01/", PERIOD.N, "-06", sep="")

## Calcular NIPO
prx_NIPO <- calcularNIPO(dbCVM_IPO)

## Calcular S
prx_S <- calcularS(dbCVM_IPO, dbCVM_SUB, dbCVM_DEB)
library(TTR)
# OPÇÃO 1 Tratar os dados faltantes
# Substituindo valores zerados pela medias dos ultimos meses
prx_S$A[3]              <- SMA(prx_S$A,   2)[prx_S$A==0][1]
prx_S$A[prx_S$A==0]     <- EMA(prx_S$A,   6)[prx_S$A==0]
prx_S$DEB[prx_S$DEB==0] <- EMA(prx_S$DEB, 6)[prx_S$DEB==0]
# http://www.fmlabs.com/reference/default.htm?url=ExpMA.htm
prx_S$Issues <- prx_S$A / ( prx_S$A + prx_S$DEB ) # Recalculando S
# Aplicando uma media movel
prx_S$Issues <- c(prx_S$Issues[1:5],SMA(prx_S$Issues,6)[-(1:5)])
prx_S <- as.data.frame(as.xts(prx_S)[PERIOD.PRX])

## Calcular TURN
prx_TURNq <- calcularTURNqtd ("Input/mNegociabilidade.csv",
                             "Input/mQN.csv",
                              #"Input/mQT.csv",
                              "Input/mQTOutStanding.csv",
                              PERIOD.PRX, lagDetrend=4, Liq=0.01)
# plot(as.xts( ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=12) ))

prx_TURNv <- calcularTURN ("Input/mNegociabilidade.csv",
                          "Input/mVolFinanNoMes.csv",
                          "Input/mMarketValue.csv",
                          PERIOD.PRX, lagDetrend=4, Liq=0.01)
# plot(as.xts( ts(prx_TURNv$TURN, start=c(PERIOD.n,1), frequency=12) ))
# plot(as.xts( ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=12) ))


# plot(as.xts( ts(prx_NIPO$CVM, start=c(PERIOD.n,1), frequency=12)))
# plot(as.xts( ts(prx_S$Issues, start=c(PERIOD.n,1), frequency=12) ))

# plot(as.xts( ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=12) ))
# plot(as.xts( ts(prx_TURNq$dQN, start=c(PERIOD.n,1), frequency=12) ))


## Calcular PVOL
prx_PVOL <- calcularPVOL()

## Organizar / Importar Proxies # ----------------------------------------------

LAG <- 12
mProxies <- ts.intersect(
    NIPO  = ts(c(prx_NIPO$CVM,rep(0,6)) ,  start=c(PERIOD.n,1), frequency=(12)),
    S     = ts(prx_S$Issues ,  start=c(PERIOD.n,1), frequency=(12)),
    # TURNV = ts(prx_TURNv$TURN, start=c(PERIOD.n,1), frequency=(12)),
    # dTURNV = ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12)),
    dTURNQ = ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=(12)),
    # QN   = ts(prx_TURNq$dQN,   start=c(PERIOD.n,1), frequency=(12)),
    PVOL = ts(prx_PVOL$PVOL,  start=c(PERIOD.n,1), frequency=(12)),
    NIPOlag = lag(ts(prx_NIPO$CVM,   start=c(PERIOD.n,1), frequency=(12)), -LAG),
    Slag    = lag(ts(prx_S$Issues,   start=c(PERIOD.n,1), frequency=(12)), -LAG),
    # TURNVlag = lag(ts(prx_TURNv$TURN, start=c(PERIOD.n,1), frequency=(12)), -LAG),
    # dTURNVlag = lag(ts(prx_TURNv$dTURN, start=c(PERIOD.n,1), frequency=(12)), -LAG),
    dTURNQlag = lag(ts(prx_TURNq$dTURN, start=c(PERIOD.n,1), frequency=(12)), -LAG),
    # QNlag   = lag(ts(prx_TURNq$dQN,   start=c(PERIOD.n,1), frequency=(12)), -LAG),
    PVOLlag = lag(ts(prx_PVOL$PVOL,  start=c(PERIOD.n,1), frequency=(12)), -LAG),
    dframe = T)
row.names(mProxies) <- as.character(as.Date(index(as.xts(mProxies[,1]))))
## Correlations
#   as.dist(round(cor(mProxies, use="everything"),2)) # c/ Na
as.dist(round(cor(mProxies, use="na.or.complete"),2)) # s/ NA

## 3.2 First Step # ============================================================
# Estimating first component of all proxies and their lags and choose the best
# mProxies <- mProxies[!is.na(mProxies$Slag),]
PCAstep1 <- prcomp(mProxies, scale=T, center = TRUE)

round(cor(PCAstep1$x[,"PC1"],mProxies),2) * (-1)     # The correlations
mBestProxies <- chooseLAG(mProxies)                  # Choosing LAGs...
colnames(mBestProxies)                               # Best proxies
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2) * (-1) # Correlation with PC1
as.dist(round(cor(mBestProxies),2))                  # Correlations between them

## 3.3 Second Step # ===========================================================

## Estimating first component of the best proxies
PCAstep2 <-prcomp(mBestProxies, scale=T, center = TRUE)

## Correlation with PC1 of the 1º step
abs(cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]))

# ## Proportion of Variance
summary(PCAstep2)

## Not orthogonalized index (osb.: not important)
PCAstep2$rotation[,"PC1"]

## 3.4 Third Step # ============================================================
# Estimate orthogonilized proxies by the regression all raw proxies

## PIB (% Change)
## https://www.quandl.com/BCB/4380-GDP-monthly-current-prices-R-million
data_inicial <- as.Date(paste(PERIOD.n,"-12-31", sep=""))
PIB <- Quandl("BCB/4380",
              trim_start=data_inicial, trim_end="2014-06-30",
              transformation="rdiff",
              sort="asc")



## OECD Dummy (Recession)
## https://www.quandl.com/FRED/BRARECM
data_inicial <- as.Date(paste(PERIOD.n+1,"-01-01", sep=""))
RECESS <- Quandl("FRED/BRARECM",
                 trim_start=data_inicial, trim_end="2014-06-30", sort="asc")

mMacroeconomics <- data.frame(PIB=PIB$Value, RECESS=RECESS$Value,
                              row.names=PIB$Date)

rm(list = c("PIB", "RECESS", "data_inicial"))

# Estimando Proxies Ortogonalizada
mProxiesOrtog <- ortogonalizeProxies(mBestProxies, mMacroeconomics)

# ## Plotar todas as mProxiesOrtog
# plot(ts(mProxiesOrtog, start(1999,1), frequency=12))

# Estimando Componentes Principais da Terceira Etapa
PCAstep3 <-prcomp(mProxiesOrtog, scale=T, center = TRUE)

# Verificando correlacao com o primeiro indice
abs(cor(PCAstep2$x[,"PC1"],PCAstep3$x[,"PC1"]))

# Percentual explicado da variancia
summary(PCAstep3)

# Scree plot of eigenvalues
screeplot(PCAstep3, type="line", main="Scree Plot Sentimento Ortogonalizado")

PCAstep3$rotation[,"PC1"] # Equacao do Indice de Sent. Ortogonalizado

Sentiment <- ts(PCAstep3$x[,"PC1"], start=c(2000,1), frequency=12)
SentNO <- ts(PCAstep2$x[,"PC1"], start=c(2000,1), frequency=12)

## Plotar Sentimento e SentimentoNO (Não Ortogonalizado)
plot(SentNO, col="dark red", lty="dashed")
lines(Sentiment, col="blue")
abline(h = 0, lty = 3)


## 4. PRICING MODELS ## #######################################################
## 5.1 Serie do Retorno do Ativos Livre de Risco
## 5.2 Serie do Retorno da Carteira de Mercado
## 5.3 Construir Carteiras por Fatores
## 5.3 Fator Tamanho (Interação Tamanho e BM)
## 5.4 Fator BM      (Interação BM e Tamanho)
## 5.5 Serie de retorno dos demais fatores (MOM, LIQ)
##       Fator Momento
##       Fator Liquidez

## 4.1 Ativos Livre de Risco # =================================================
## Ativo Livre de Risco (download da Serie SELIC do Banco Central)
Rf   <- riskFreeRate(PERIOD.n, PERIOD.N)

## 4.2 Carteira de Mercado # ===================================================

## Carteira de Mercado (Calculada)

# Filtro p/ calculo da carteira de Mercado
f_MKT <- data.frame(lapply(f_PortfReturns * f_MVclass * f_NoFinancial,
                           as.logical), row.names=rownames(f_MVclass))

# Transformando a classe do valor de mercado de integer p/ numerico
# por causa da memoria
mMVclass <- data.frame(lapply(mMVclass, as.numeric ), row.names=rownames(mMVclass))

tmp <- portfolioSerie2(mReturns, mMVclass, f_MKT) ; rm(f_MKT)
MKT <- ts(tmp$rVW, start=c(PERIOD.n,07), frequency=12) ; rm(tmp)

## Carteira de Mercado (Ibovespa)
IBV  <- Quandl("BCB/7845", type="ts", collapse="monthly", sort="asc",
               #               transformation="rdiff",
               trim_start=as.Date(paste(PERIOD.n+1,"-06-01", sep="")),
               trim_end=as.Date(paste(PERIOD.N,"-06-01", sep="")))
IBV <- diff(log(IBV),1)

#' Correlação entre a carteira calculada e o IBOVESPA
#' Com empresas Financeiras = 0.96
as.dist(cor(merge(MKT=as.zoo(MKT), IBOV=as.zoo(IBV), all=F)))

MKT <- MKT-Rf

## 4.2 Fatores de Risco # ======================================================

## Carteiras por Fator (aF: assets Factors)
aF_Size_S <- portfolioSelectAssets(yMVclassJun,2,1) * 1 # Small
aF_Size_B <- portfolioSelectAssets(yMVclassJun,2,2) * 1 # Big   

aF_BM_H <- portfolioSelectAssets(yBM,3,1) * 1 # Value  (High BM)
aF_BM_N <- portfolioSelectAssets(yBM,3,2) * 1 # Neutral
aF_BM_L <- portfolioSelectAssets(yBM,3,3) * 1 # Growth (Low BM)

aF_MOM_W <- portfolioSelectAssets(yMomentum,3,1) * 1 # Value  (Wins)
aF_MOM_L <- portfolioSelectAssets(yMomentum,3,3) * 1 # Growth (Loss)

## Carteiras a partir da Interação
##
## Conforme French Site
## http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html
##
aF_SH <- aF_Size_S[-1,] * aF_BM_H # Small Value (High BM)
aF_SN <- aF_Size_S[-1,] * aF_BM_N # Small Neutral
aF_SL <- aF_Size_S[-1,] * aF_BM_L # Small Growth (Low BM)
aF_BH <- aF_Size_B[-1,] * aF_BM_H # Big Value (High BM)
aF_BN <- aF_Size_B[-1,] * aF_BM_N # Big Neutral
aF_BL <- aF_Size_B[-1,] * aF_BM_L # Big Growth (Low BM)
aF_SH <- data.frame(lapply(aF_SH, as.logical), row.names=rownames(yBM))
aF_SH <- apply(aF_SH, 2, function(x) as.logical(x) ) # Small Value (High BM)
aF_SN <- apply(aF_SN, 2, function(x) as.logical(x) ) # Small Neutral
aF_SL <- apply(aF_SL, 2, function(x) as.logical(x) ) # Small Growth (Low BM)
aF_BH <- apply(aF_BH, 2, function(x) as.logical(x) ) # Big Value (High BM)
aF_BN <- apply(aF_BN, 2, function(x) as.logical(x) ) # Big Neutral
aF_BL <- apply(aF_BL, 2, function(x) as.logical(x) ) # Big Growth (Low BM)
aF_SWI <- (aF_Size_S * aF_MOM_W)[-1,] # Small Win
aF_SLO <- (aF_Size_S * aF_MOM_L)[-1,] # Small Los
aF_BWI <- (aF_Size_B * aF_MOM_W)[-1,] # Big Win
aF_BLO <- (aF_Size_B * aF_MOM_L)[-1,] # Big Los
aF_SWI <- apply(aF_SWI, 2, function(x) as.logical(x) )
aF_SLO <- apply(aF_SLO, 2, function(x) as.logical(x) )
aF_BWI <- apply(aF_BWI, 2, function(x) as.logical(x) )
aF_BLO <- apply(aF_BLO, 2, function(x) as.logical(x) )

rownames(aF_SH) <- rownames(yBM)
rownames(aF_SN) <- rownames(yBM)
rownames(aF_SL) <- rownames(yBM)
rownames(aF_BH) <- rownames(yBM)
rownames(aF_BN) <- rownames(yBM)
rownames(aF_BL) <- rownames(yBM)
rownames(aF_SWI) <- rownames(yBM)
rownames(aF_SLO) <- rownames(yBM)
rownames(aF_BWI) <- rownames(yBM)
rownames(aF_BLO) <- rownames(yBM)

## Retornos
portF_SH <- portfolioSerie(mReturns, mMVclass, assetsF_SH)
portF_SN <- portfolioSerie(mReturns, mMVclass, assetsF_SN)
portF_SL <- portfolioSerie(mReturns, mMVclass, assetsF_SL)
portF_BH <- portfolioSerie(mReturns, mMVclass, assetsF_BH)
portF_BN <- portfolioSerie(mReturns, mMVclass, assetsF_BN)
portF_BL <- portfolioSerie(mReturns, mMVclass, assetsF_BL)
portF_SWI <- portfolioSerie(mReturns, mMVclass, assetsF_SWI)
portF_SLO <- portfolioSerie(mReturns, mMVclass, assetsF_SLO)
portF_BWI <- portfolioSerie(mReturns, mMVclass, assetsF_BWI)
portF_BLO <- portfolioSerie(mReturns, mMVclass, assetsF_BLO)
rm(list=ls(pattern = "aF_"))

## Fatores de Risco # ----------------------------------------------------------

## FATOR TAMANHO (SMB)
##
## SMB = 1/3 (Small Value + Small Neutral + Small Growth)
##       - 1/3 (Big Value + Big Neutral + Big Growth)
##
SMB <-(portF_SH$rVW+portF_SN$rVW+portF_SL$rVW)/3-(portF_BH$rVW+portF_BN$rVW+portF_BL$rVW)/3
SMB <-ts(SMB, start=c(PERIOD.n,7), frequency=12)

## FATOR BM (HML)
##
## HML = 1/2 (Small Value + Big Value)   - 1/2 (Small Growth + Big Growth)
##
HML <- (portF_SH$rVW + portF_BH$rVW)/2 - (portF_SL$rVW + portF_BL$rVW)/2
HML <- ts(HML, start=c(PERIOD.n,7), frequency=12)

## FATOR MOMENTO (UMD)
##
## Mom =    1/2 (Small High + Big High) - 1/2(Small Low + Big Low).	
##
UMD <- (portF_SWI$rVW + portF_BWI$rVW)/2 - (portF_SLO$rVW + portF_BLO$rVW)/2
UMD <- ts(UMD, start=c(PERIOD.n,7), frequency=12)

# ## Pequeno Teste dos Fatores
# summary(lm(portF_SH$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_SN$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_SL$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BH$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BN$rVW-Rf ~ MKT + SMB + HML))
# summary(lm(portF_BL$rVW-Rf ~ MKT + SMB + HML))
# 
NW(lm(portF_SH$rVW-Rf ~ MKT + SMB + HML + UMD))
NW(lm(portF_SN$rVW-Rf ~ MKT + SMB + HML + UMD))
NW(lm(portF_SL$rVW-Rf ~ MKT + SMB + HML + UMD))
NW(lm(portF_BH$rVW-Rf ~ MKT + SMB + HML + UMD))
NW(lm(portF_BN$rVW-Rf ~ MKT + SMB + HML + UMD))
NW(lm(portF_BL$rVW-Rf ~ MKT + SMB + HML + UMD))

## 5. CONSTRUCT PORTFOLIOS ## ##################################################
## 5.1 BW  Portfolios
## 5.2 SYY Portfolios

## LongShort Stategies # -------------------------------------------------------

## TAMANHO  (VM Empresa Jun)
mMVfirm <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.XTS, ignora=1)
yMVfirmJun <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="jun"),]
yMVfirmJun <- cleanData(yMVfirmJun, f_PortfReturns)
yMVfirmJun <- cleanData(yMVfirmJun, f_MVclass)
yMVfirmJun <- cleanData(yMVfirmJun, f_NoFinancial)
ls_TAM <- computeLongShort(mReturns, mMVclass, yMVfirmJun/1000000, 10, 1, 10, Rf)
# ls_TAM <- computeLongShort(mReturns, mMVclass, yMVfirmJun/1000000,  5, 1,  5, 0)
# allQuintiles(yMVfirmJun, mReturns, mMVclass)

## MOMENTO
## Obs.: Momento so funcionou com decis
yMomentum <- computeMomentum (mReturns)
rownames(yMomentum) <- sub("-05", "-06", rownames(yMomentum))
yMomentum <- cleanData(yMomentum, f_PortfReturns)
yMomentum <- cleanData(yMomentum, f_MVclass)
yMomentum <- cleanData(yMomentum, f_NoFinancial)
# yMomentum <- cleanData(yMomentum, f_MVdez) # Filtrando ou nao, da o mesmo resultado
ls_MOM <- computeLongShort(mReturns, mMVclass, yMomentum, 10, 1, 10, Rf)
# ls_MOM <- computeLongShort(mReturns, mMVclass, yMomentum,  5, 1,  5, 0)
# allQuintiles(yMomentum, mReturns, mMVclass)

## VOLATILIDADE
mDP12m <- importaBaseCSV("Input/mDP12m.csv", PERIOD.XTS, ignora = 1)
yDP12m <- mDP12m[(months(as.Date(rownames(mDP12m)), T)=="jun"),] ; rm(mDP12m)
yDP12m <- cleanData(yDP12m, f_NoFinancial)
yDP12m <- cleanData(yDP12m, f_PortfReturns)
yDP12m <- cleanData(yDP12m, f_MVclass)
ls_VOL <- computeLongShort(mReturns, mMVclass, yDP12m, 10, 1, 10, Rf)
# ls_VOL <- computeLongShort(mReturns, mMVclass, yDP12m , 5, 1, 5, 0)
# allQuintiles(yDP12m, mReturns, mMVclass)

# LIQUIDEZ (Volume Medio) JUN
mVolume <- importaBaseCSV("Input/mVolume.csv", PERIOD.XTS, ignora=1)
yVolumeJun <- mVolume[(months(as.Date(rownames(mVolume)), T)=="jun"),]
yVolumeJun <- cleanData(yVolumeJun, f_NoFinancial)
yVolumeJun <- cleanData(yVolumeJun, f_PortfReturns)
yVolumeJun <- cleanData(yVolumeJun, f_MVclass)
ls_LIQ <- computeLongShort(mReturns, mMVclass, yVolumeJun , 10, 1, 10, Rf)
# ls_LIQ <- computeLongShort(mReturns, mMVclass, yVolumeJun , 5, 1, 5, 0)
# allQuintiles(yVolumeJun, mReturns, mMVclass)

## BM
## Obs.: So funcionou o High - Midle
mMVfirm    <- importaBaseCSV("Input/mMarketValueFirm.csv", PERIOD.XTS, ignora=1)
yMVfirmDez <- mMVfirm[(months(as.Date(rownames(mMVfirm)), T)=="dez"),]
yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", PERIOD.XTS, formato="%Y",
                            ignora=0)
yMVfirmDez <- cleanData(yMVfirmDez, f_NoFinancial, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_NoFinancial, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_PortfReturns, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_PortfReturns, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_MVclass, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_MVclass, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_BookNA, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_BookNA, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_BookPositive, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_BookPositive, LAG=1)
yMVfirmDez <- cleanData(yMVfirmDez, f_MVdez, LAG=1)
yBookFirm  <- cleanData(yBookFirm,  f_MVdez, LAG=1)
yBM <- yBookFirm / yMVfirmDez
# ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 10, 1, 10, Rf) # SYY
# ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 10, 1, 5, Rf)  # BW Mid - Low
ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 10, 6, 10, Rf) # BW High - Mid
# ls_BM  <- computeLongShort(mReturns, mMVclass, yBM, 5, 1, 5, 0)
# allQuintiles(yBM, mReturns, mMVclass)

## LUCRO/PRECO
yLP <- importaBaseCSV("Input/yLP.csv", PERIOD.XTS, formato="%Y", ignora=1)
yLP  <- padronizaBase(yLP)
yLP <- cleanData(yLP, f_NoFinancial, LAG=1)
yLP <- cleanData(yLP, f_PortfReturns, LAG=1)
yLP <- cleanData(yLP, f_MVclass, LAG=1)
yLP <- cleanData(yLP, f_BookNA, LAG=1)
yLP <- cleanData(yLP, f_BookPositive, LAG=1)
ls_LP  <- computeLongShort(mReturns, mMVclass, yLP, 10, 1, 10, Rf) # MM
# ls_LP  <- computeLongShort(mReturns, mMVclass, yLP, 5, 1, 5, 0)
# allQuintiles(yLP, mReturns, mMVclass)

## EBITDA/PRECO
yEBTDA <- importaBaseCSV("Input/yEBTDA.csv", PERIOD.XTS, formato="%Y", ignora=1)
yAT2   <- importaBaseCSV("Input/yAT2.csv", PERIOD.XTS, formato="%Y", ignora=1)
yEBTDA <- padronizaBase(yEBTDA)
yAT2   <- padronizaBase(yAT2)

yEBTDA <- cleanData(yEBTDA, f_NoFinancial, LAG=1)
yAT2   <- cleanData(yAT2,   f_NoFinancial, LAG=1)
yEBTDA <- cleanData(yEBTDA, f_PortfReturns, LAG=1)
yAT2   <- cleanData(yAT2,   f_PortfReturns, LAG=1)
yEBTDA <- cleanData(yEBTDA, f_MVclass, LAG=1)
yAT2   <- cleanData(yAT2,   f_MVclass, LAG=1)
f_AT_NA       <- filterNA(yAT2, PERIOD.n, PERIOD.N, "dez")
f_AT_Positive <- filterGreaterThan(yAT2, 0, PERIOD.n, PERIOD.N, "dez")
f_EBTDA_NA    <- filterNA(yEBTDA, PERIOD.n, PERIOD.N, "dez")
yEBTDA  <- cleanData(yEBTDA, f_AT_NA, LAG=1)
yAT2    <- cleanData(yAT2,   f_AT_NA, LAG=1)
yEBTDA  <- cleanData(yEBTDA, f_EBTDA_NA, LAG=1)
yAT2    <- cleanData(yAT2,   f_EBTDA_NA, LAG=1)
yEBTDA  <- cleanData(yEBTDA, f_AT_Positive, LAG=1)
yAT2    <- cleanData(yAT2,   f_AT_Positive, LAG=1)

yEBTDA_AT <- yEBTDA / yAT2

ls_EBTDA  <- computeLongShort(mReturns, mMVclass, yEBTDA_AT, 10, 1, 10, Rf) # MM

## ENDIVIDAMENTO
yEndiv <- importaBaseCSV("Input/yExgLP_PL.csv", PERIOD.XTS, formato="%Y", ignora=1)
yEndiv <- padronizaBase(yEndiv)
yEndiv <- cleanData(yEndiv, f_NoFinancial, LAG=1)
yEndiv <- cleanData(yEndiv, f_PortfReturns, LAG=1)
yEndiv <- cleanData(yEndiv, f_MVclass, LAG=1)
yEndiv <- cleanData(yEndiv, f_BookNA, LAG=1)
yEndiv <- cleanData(yEndiv, f_BookPositive, LAG=1)
yEndiv <- cleanData(yEndiv, filterGreaterThan(yEndiv, 0, PERIOD.n, PERIOD.N, "dez"), LAG=1)

ls_ENDIV  <- computeLongShort(mReturns, mMVclass, yEndiv, 10, 1, 10, Rf) # MM

## ROA
yLucroLiq <- importaBaseCSV("Input/yLucroLiq.csv", PERIOD.XTS, formato="%Y", ignora=1)
yAT2      <- importaBaseCSV("Input/yAT2.csv", PERIOD.XTS, formato="%Y", ignora=1)
yLucroLiq <- padronizaBase(yLucroLiq)
yAT2      <- padronizaBase(yAT2)

yLucroLiq <- cleanData(yLucroLiq, f_NoFinancial, LAG=1)
yLucroLiq <- cleanData(yLucroLiq, f_PortfReturns, LAG=1)
yLucroLiq <- cleanData(yLucroLiq, f_MVclass, LAG=1)
yAT2   <- cleanData(yAT2,   f_NoFinancial, LAG=1)
yAT2   <- cleanData(yAT2,   f_PortfReturns, LAG=1)
yAT2   <- cleanData(yAT2,   f_MVclass, LAG=1)
f_AT_NA       <- filterNA(yAT2, PERIOD.n, PERIOD.N, "dez")
f_AT_Positive <- filterGreaterThan(yAT2, 0, PERIOD.n, PERIOD.N, "dez")
f_LucroLiqNA  <- filterNA(yLucroLiq, PERIOD.n, PERIOD.N, "dez")
yLucroLiq <- cleanData(yLucroLiq, f_AT_NA, LAG=1)
yAT2      <- cleanData(yAT2,      f_AT_NA, LAG=1)
yLucroLiq <- cleanData(yLucroLiq, f_AT_Positive, LAG=1)
yAT2      <- cleanData(yAT2,      f_AT_Positive, LAG=1)
yLucroLiq <- cleanData(yLucroLiq, f_LucroLiqNA, LAG=1)
yAT2      <- cleanData(yAT2,      f_LucroLiqNA, LAG=1)

yROA <- yLucroLiq / yAT2
ls_ROA <- computeLongShort(mReturns, mMVclass, yROA, 10, 1, 10, Rf) # MM

# Combinação das Estratégias
LS <- ls_TAM
LS$LONG  <- (ls_TAM$LONG + ls_LIQ$LONG + ls_VOL$LONG + ls_BM$LONG + ls_MOM$LONG +
                 ls_EBTDA$LONG + ls_ENDIV$LONG + ls_LP$LONG + ls_ROA$LONG) / 9
LS$SHORT <- (ls_TAM$SHORT + ls_LIQ$SHORT + ls_VOL$SHORT + ls_BM$SHORT + ls_MOM$SHORT +
                 ls_EBTDA$SHORT + ls_ENDIV$SHORT + ls_LP$SHORT + ls_ROA$SHORT) / 9

# CORRELACAO DAS ESTRATEGIAS
round(as.dist(cor(data.frame(TAM   = (ls_TAM$LONG - ls_TAM$SHORT),
                             LIQ   = (ls_LIQ$LONG - ls_LIQ$SHORT),
                             VOL   = (ls_VOL$LONG - ls_VOL$SHORT),
                             BM    = (ls_BM$LONG - ls_BM$SHORT),
                             MOM   = (ls_MOM$LONG - ls_MOM$SHORT),
                             EBTDA = (ls_EBTDA$LONG - ls_EBTDA$SHORT),
                             ENDIV = (ls_ENDIV$LONG - ls_ENDIV$SHORT),
                             LP    = (ls_LP$LONG - ls_LP$SHORT),
                             ROA   = (ls_ROA$LONG - ls_ROA$SHORT),
                             TODAS = (LS$LONG - LS$SHORT)))),2)

## 6. INVESTOR SENTIMENT AND ANOMALIES ## ######################################
##    Sentimento do Investidor e Anomalias
##
## 6.1. Análise das Médias após períodos de Sentimento Alto e Baixo
## 6.2. Modelos Econométricos
## 6.2.1 Extremos e sentimento defasado
## 6.2.2 Extremos, sentimeto defasado e fatores de risco
## 6.2.3 Extremos, dummys

#== 6.1 Análise de Médias # ====================================================

## LONG-SHORT (Verificar se a Anomalia é mais forte após Alto Sentimento)
# H .:. As anomalias são mais fortes após alto sentimento do que baixo
reportAvarege("LongShort", Sentiment)

## SHORT (Espera-se que seja ainda mais negativo após alto sentimento)
# H .:. Short é mais baixa (lucrativa) apos alto sentimento do que baixo ( H-L < 0 )
reportAvarege("Short", Sentiment)

## LONG (Espera-se que o sentimento nao tenha efeito)
reportAvarege("Long", Sentiment)

# computeAvarageReturns(ls_TAM, Sent, 1)
# computeAvarageReturns(ls_LIQ, Sent, 1)
# computeAvarageReturns(ls_VOL, Sent, 1)
# computeAvarageReturns(ls_BM , Sent, 1)
# computeAvarageReturns(ls_MOM, Sent, 1)
# computeAvarageReturns(ls_EBTDA, Sent, 1)
# computeAvarageReturns(ls_ENDIV, Sent, 1)
# computeAvarageReturns(ls_LP , Sent, 1)
# computeAvarageReturns(ls_ROA, Sent, 1)
# computeAvarageReturns(LS, Sent, 1)

#== 6.2 Predictive Regressions # ===============================================
## model1, modelCAPM, model3F, model4F e model5F
##

reportRegSent(Sentiment, 1)   ## Sentiment and Returns
reportRegCAPM(Sentiment, 1)   ## CAPM
reportReg3F  (Sentiment,   1) ## FF1993
plot(as.xts(Sentiment))
# load(paste(getwd(),"/Data/", "20141022_FINAL.RData", sep=""))
# save.image(paste(getwd(),"/Data/", "20141022_FINAL.RData", sep=""))

# save.image(paste(getwd(),"/Data/", format(Sys.Date(), "%Y%m%d"),
#                   "_", format(Sys.time(),"%H%M%S"), ".RData", sep=""))

# save.image(paste(getwd(),"/Data/", format(Sys.Date(), "%Y%m%d"),
#                  "_", "QN", ".RData", sep=""))