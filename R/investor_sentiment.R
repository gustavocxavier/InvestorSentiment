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

## COISAS PRA FAZER AINDA ## ###################################################
## - Deixar filtro de data do "Rf <- importaBaseCSV" automatico
## - Simplificar modelos com funcoes
##   - Funcao LongShortSeries
## - Todas as regressoes e painel com os interceptos e o R2
## - Incluir calculo no R de todas as proxies para o Indice de Sentimento
##   - Organizar Calculo do S (Solucao Interpolacao/L)
##   - Calcular PVOL
##   - Coletar RIPO
## - Calcular variavel MOMENTO como retorno de jul/(n-1):mai/(n  )
## - VM Empresa qnd ON e PN / VM Classe qnd so uma classe na amostra
## - FAZER UM FILTRO DE DATA PRA mProxies em breve
## - Testar replicação M&O (2011)
## - Testar Anomalias: FC/P, L/P, ALAV
## - Calcular demais fatores (LIQ e MOM)

#' 

## 1. SETTINGS ## ##############################################################

## Definir Parametros / Set Parameters
setwd("C:/Dropbox/investorSentiment") # Pasta de Trabalho / Working Directory
PERIOD.XTS   <- "1999-06/2014-07"     # Periodo / Period
RETORNOS     <- 

## Instalar pacotes / Install packages
ip <- installed.packages()
if ( !("xts"       %in% ip) ) { install.packages("xts") }
if ( !("lubridate" %in% ip) ) { install.packages("lubridate") } ; rm(ip)


# if (!(require(xts, character.only=T, quietly=T))) {
#     install.packages(package)
#     library(package, character.only=T)
# }

## Carregar pacotes / Load packages
library(xts)
library(lubridate)

## Executar minhas funçoes / Run my functions
source("R/functions.R")

## 2. LOAD DATA AND CLEAN ## ###################################################
##
## Importar e limpar dados / Load and Clean Data
##

## 2.1 Importar Dados / Load Data # ============================================

### Importar Dados Online / Load Online Data # ---------------------------------
## COLETA DIRETO DO SITE DA CVM EM DESENVOLVIMENTO

# ## Baixar dados de IPO do Site da CVM
# IPOs <- coletaVariosAnosCVM(1999:2013, coletaIPOnaCVM)
# ## Baixar dados de emissão de dívidas
# DEBs <- coletaVariosAnosCVM(1999:2013, coletaDEBnaCVM)
# ## Baixar dados de emissão de açoes Subsequentes
# SUBs <- coletaVariosAnosCVM(1999:2013, coletaSubsequentesnaCVM)
# ## Transformar dados de valor em decimeal (substitui virgula por ponto)
# IPOs$valor <- as.numeric(gsub(",", ".", gsub("\\.", "",  IPOs$valor, fixed=F)))
# DEBs$valor <- as.numeric(gsub(",", ".", gsub("\\.", "",  DEBs$valor, fixed=F)))
# SUBs$valor <- as.numeric(gsub(",", ".", gsub("\\.", "",  SUBs$valor, fixed=F)))

# ## Colocar as Datas no Formato do R
# IPOs$data <- as.Date(IPOs$data, format="%d/%m/%Y")
# DEBs$data <- as.Date(DEBs$data, format="%d/%m/%Y")
# SUBs$data <- as.Date(SUBs$data, format="%d/%m/%Y")
#
# CVM.IPOs <- IPOs[order(IPOs$data, IPOs$empresa),] # Ordenar matriz de IPOs
# CVM.DEBs <- DEBs[order(DEBs$data, DEBs$empresa),] # Ordenar matriz de DEBs
# 
# ## Organizando os nomes das empresas
# IPOs$nome_antigo <- IPOs$empresa
# # IPOs$empresa <- IPOs$nome_antigo
# IPOs$empresa <- gsub("^\\** ", "", IPOs$empresa)     # Retirar "** " no comeco
# IPOs$empresa <- gsub("S\\.A\\.", "SA", IPOs$empresa) # Substituir S.A. por SA
# IPOs$empresa <- gsub("S.A", "SA", IPOs$empresa) # Substituir S.A e S/A por SA
# IPOs$empresa <- gsub("\\-", "", IPOs$empresa)   # Retirar todos os tracos
# IPOs$empresa <- gsub("  ", " ", IPOs$empresa)   # Retirar 2 espacos juntos
# IPOs$empresa <- cleanString(IPOs$empresa)       # Substituir caracteris espec.
# IPOs$empresa <- toupper(IPOs$empresa)           # Colocar em letra maiuscula
# IPOs$emp     <- substr(IPOs$empresa,1,12)       # Gerar STRING de comparacao
# IPOs$D       <- !(duplicated(IPOs$emp))         # Nao duplicados = TRUE
#
# ## Verificando Quantidade de IPO por ano 
# ano      <- substr(IPOs$data[IPOs$D],1,4)
# mes      <- substr(IPOs$data[IPOs$D],6,7)
# nipo     <- data.frame(table(ano, mes))
# nipo$ano <- as.numeric(nipo$ano)
# nipo$mes <- as.numeric(nipo$mes)
# nipo     <- nipo[order(nipo$ano,nipo$mes),]
# rownames(nipo) <- seq(as.Date("1999/1/1"), as.Date("2013/12/1"), by="month")
# nipo$ano <- NULL ; nipo$mes <- NULL ; colnames(nipo) <- "nipo"
# # nipos <- as.data.frame(cbind(CVM=nipo$nipo[25:156],BVSP=mProxies$NIPO))
# rm(list=c("ano","mes","IPOs2"))
# 
# CVM.IPOs <- IPOs ; rm(IPOs)
# CVM.DEBs <- DEBs ; rm(DEBs)
# CVM.SUBs <- SUBs ; rm(SUBs)

# S <- as.data.frame(xtabs(valor~ano+mes, CVM.S[(CVM.S$tipo=="ACOES"),]))
# tmp <- as.data.frame(xtabs(valor~ano+mes, CVM.S[(CVM.S$tipo=="DIVID"),]))$Freq
# S <- cbind(S,tmp) ; rm(tmp) ; colnames(S) <- c("Y","M","A","DEB")
# rownames(S) <- NULL ; S$Y <- as.numeric(S$Y) ; S$M <- as.numeric(S$M)
# S <- S[order(S$Y,S$M),]
# S$Issues <- S$A / ( S$A + S$DEB)

# # Guardando dados em formato txt e csv
# write.table(IPOs, "Output/IPOs.txt", sep="\t", row.names = F)
# write.table(DEBs, "Output/DEBs.txt", sep="\t", row.names = F)
# write.table(IPOs, "Output/IPOs.csv", sep=";", row.names = F)
# write.table(DEBs, file="Output/DEBs.csv", sep=";", row.names = F)
# write.csv(IPOs, file="Output/IPOsCSV.csv", row.names = F)
# write.csv(DEBs, file="Output/DEBsCSV.csv", row.names = F)
# # IPO2 <- read.table("Output/IPOs.txt", sep="\t", skip=1, col.names=c("data","empresa","tipo","valor"))

# CVM.S  <- CVM.IPOs[c("data", "valor")] ; rownames(CVM.S)  <- NULL
# CVM.S2 <- CVM.SUBs[c("data", "valor")] ; rownames(CVM.S2) <- NULL
# CVM.S3 <- CVM.DEBs[c("data", "valor")] ; rownames(CVM.S3) <- NULL
# CVM.S$tipo  <- "ACOES" ; CVM.S2$tipo <- "ACOES" ; CVM.S3$tipo <- "DIVID"
# CVM.S  <- rbind(CVM.S, CVM.S2, CVM.S3) ; rm(CVM.S2) ; rm(CVM.S3)
# CVM.S$mes <- as.numeric(substr(CVM.S$data, 6,7))
# CVM.S$ano <- as.numeric(substr(CVM.S$data, 1,4))
# xtabs(~ano+mes, CVM.S)

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
yMomentum <- period.apply(mReturns,endpoints(mReturns,'years'), mean)

## Volume Médio de Negociação em Reais nos últimos 12 meses (mes base: JUNHO)
yVolumeJun <- mVolume[(months(as.Date(rownames(mVolume)), T)=="jun"),]

## Volume Médio de Negociação em Reais nos últimos 12 meses (mes base: DEZ)
yVolumeDez <- mVolume[(months(as.Date(rownames(mVolume)), T)=="dez"),]

# 2.2 Filtrar Amostra / Filter Sample # ========================================

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
F1 <- sampleReport(ySample0, ySample1)
F2 <- sampleReport(ySample0, ySample2)[,2:3]
F3 <- sampleReport(ySample0, ySample3)[,2:3]
F4 <- sampleReport(ySample0, ySample4)[,2:3]
F5 <- sampleReport(ySample0, ySample5)[,2:3]
colnames(F1) <- c("A.I.","F 1","%")
colnames(F2) <- c("Filtro2","%")
colnames(F3) <- c("Filtro3","%")
colnames(F4) <- c("Filtro4","%")
colnames(F5) <- c("Filtro5","%")
# F1: Filtro de Empresas Nao Financeiras
# F2: FILTRO DE 24 MESES
# F3: Filtro Bovespa Negociability Index
# F4: Filtro Valor de Mercado em 30/06 e 31/12
# F5: Filtro Patrimonio Liquido
cbind(F1, F2, F3, F4, F5) ; rm(list=c("F1","F2","F3","F4","F5"))

# 2.3 Limpar Dados / Clean Data # ==============================================

yMVfirmJun  <- cleanData(yMVfirmJun,  ySample)
yMVfirmDez  <- cleanData(yMVfirmDez,  ySample, LAG=1)
yMVclassJun <- cleanData(yMVclassJun, ySample)
yMVclassDez <- cleanData(yMVclassDez, ySample, LAG=1)
yVolumeJun  <- cleanData(yVolumeJun,  ySample)
yVolumeDez  <- cleanData(yVolumeDez,  ySample, LAG=1)
yBM         <- cleanData(yBM,         ySample, LAG=1)
yMomentum   <- cleanData(yMomentum,   ySample)

rownames (yMomentum) <- rownames(yVolumeJun)

## 3. INVESTOR SENTIMENT INDEX ## #############################################
## 3. Índice de Sentimento
## 3.1. Temporalidade das Proxies: Selecionar proxies que serão defasadas
## 3.2. Índice de Sentimento não Ortogonalizado
## 3.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  

#== 3.1 Read/Compute Proxies = ===============================================

mProxies   <- read.table ("Input/mProxies.csv",          # Read data
                          header = T, sep=";", dec=",",
                          row.names=1)

DF   <- read.table ("Input/mProxies.csv",          # Read data
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
Sent <- ts(PCAstep3$x[,"PC1"], start=c(2001,1), end=c(2013,12), frequency=12)

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
plot(Sent, main="Sentiment", ylab=NULL) ; lines(Sent, col="blue")

## 4. CONSTRUCT PORTFOLIOS ## #################################################
## 4. Portfolios
## 4.1 Construir Carteiras
## 4.2 Interação de Carteiras
##       portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 4.3 Retorno das Carteiras
##       portfolioSerie - retorna ...
## 4.2.4 Serie de retorno dos demais fatores (MOM, LIQ)

## LongShort Stategies # -------------------------------------------------------
# TAMANHO  (VM Classe  Jun)
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVclassJun, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVclassJun, 5, 5))
ls_TAM <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_TAM)*100

# TAMANHO  (VM Classe  Dez)
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVclassDez, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVclassDez, 5, 5))
ls_TAMdez <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_TAMdez)*100

# TAMANHO  (VM Empresa Jun)
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVfirmJun, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVfirmJun, 5, 5))
ls_TAMfirmJun <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_TAMfirmJun)*100

# TAMANHO  (VM Empresa Dez)
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVfirmDez, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMVfirmDez, 5, 5))
ls_TAMfirmDez <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_TAMfirmDez)*100

# LIQUIDEZ (Volume Medio) JUN
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yVolumeJun, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yVolumeJun, 5, 5))
ls_LIQ <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_LIQ)*100

# LIQUIDEZ (Volume Medio) DEZ ñ
Long   <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yVolumeDez, 5, 1))
Short  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yVolumeDez, 5, 5))
ls_LIQdez <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_LIQdez)*100

# BM
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yBM, 5, 1))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yBM, 5, 5))
ls_BM <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
colMeans(ls_BM)*100

# MOMENTO
Long  <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMomentum, 5, 5))
Short <- portfolioSerie(mReturns, mMVclass, portfolioSelectAssets(yMomentum, 5, 1))
ls_MOM <- data.frame(LONG=Long$rVW, SHORT=Short$rVW, row.names=rownames(Long))
rm(list=c("Long","Short"))
colMeans(ls_MOM)*100

## 5. PRICING MODELS ## #######################################################
## 5.1 Ativos Livre de Risco
## 5.2 Carteiras de Mercado
## 5.3 Fator Tamanho
##         portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 5.4 Fator BM
## 5.5 Fator Momento
## 5.6 Fator Liquidez

## Ativo Livre de Risco
Rf <- importaBaseCSV("Input/mMacroeconomics.csv")[-(1:12),]
tmp <- as.xts(Rf)
Rf  <- as.data.frame( diff(log(tmp), lag=1) ) ; rm(tmp)

## Carteira de Mercado
MKT <- portfolioSerie(mReturns, mMVclass, ySample)$rVW

Rf  <- Rf[1+(1:length(MKT)),]
Rf  <- as.numeric(Rf$SELIC)

MKT <- MKT-Rf

## Fatores de Risco / Risk Factors
##

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

HML <- (portF_SH$rVW + portF_BH$rVW)/2 - (portF_SL$rVW + portF_BL$rVW)/2
##     1/2 (Small Value + Big Value)   - 1/2 (Small Growth + Big Growth)

SMB <-(portF_SH$rVW+portF_SN$rVW+portF_SL$rVW)/3-(portF_BH$rVW+portF_BN$rVW+portF_BL$rVW)/3
##    1/3 (Small Value + Small Neutral + Small Growth)
##        - 1/3 (Big Value + Big Neutral + Big Growth)

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
# Sent <- ts(PCAstep3$x[,"PC1"], start=c(2001,1), end=c(2013,12), frequency=12)
# L <- ts(ls_LIQ$LONG ,start=c(2000,7), end=c(2013,6), frequency=12)
LAG <- 4
Sentiment <- as.numeric(Sent)[(1+LAG):152]

## TAMANHO
Long      <- ls_TAM$LONG[1:(152-LAG)]
Short     <- ls_TAM$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## TAMANHO DEZ
Long      <- ls_TAMdez$LONG[1:(152-LAG)]
Short     <- ls_TAMdez$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## TAMANHO firm Dez
Long      <- ls_TAMfirmDez$LONG[1:(152-LAG)]
Short     <- ls_TAMfirmDez$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## TAMANHO firm Jun
Long      <- ls_TAMfirmJun$LONG[1:(152-LAG)]
Short     <- ls_TAMfirmJun$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## BM
Long      <- ls_BM$LONG[1:(152-LAG)]
Short     <- ls_BM$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## MOMENTUM
Long      <- ls_MOM$LONG[1:(152-LAG)]
Short     <- ls_MOM$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(152-LAG)]
Short     <- ls_LIQ$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

## LIQUIDITY DEZ
Long      <- ls_LIQdez$LONG[1:(152-LAG)]
Short     <- ls_LIQdez$SHORT[1:(152-LAG)]
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment ))$coefficients[2,]
summary(lm( Short     ~ Sentiment ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment ))$coefficients[2,]

rm(list=c("Long", "Short", "LongShort"))

#   6.2.2 Sentiment and Pricing Models - --------------------------------------

Rf  <-  Rf[1:(152-LAG)]
MKT <- MKT[1:(152-LAG)]

## TAMANHO
Long      <- ls_TAM$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## TAMANHO DEZ
Long      <- ls_TAMdez$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMdez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## TAMANHO firm Dez
Long      <- ls_TAMfirmDez$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMfirmDez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## TAMANHO firm Jun
Long      <- ls_TAMfirmJun$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMfirmJun$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## BM
Long      <- ls_BM$LONG[1:(152-LAG)] - Rf
Short     <- ls_BM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## MOMENTUM
Long      <- ls_MOM$LONG[1:(152-LAG)] - Rf
Short     <- ls_MOM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(152-LAG)] - Rf
Short     <- ls_LIQ$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]

## LIQUIDITY DEZ
Long      <- ls_LIQdez$LONG[1:(152-LAG)] - Rf
Short     <- ls_LIQdez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT ))$coefficients[2,]


# Sent.Long.Beta        <- lm(Long.Beta  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Beta       <- lm(Short.Beta ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Size        <- lm(Long.Size  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Size       <- lm(Short.Size ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.Liquidity   <- lm(Long.Liquidity  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Liquidity  <- lm(Short.Liquidity ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Long.BM          <- lm(Long.BM  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.BM         <- lm(Short.BM ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)

SMB <- SMB[1:(152-LAG)]
HML <- HML[1:(152-LAG)]

## TAMANHO
Long      <- ls_TAM$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## TAMANHO DEZ
Long      <- ls_TAMdez$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMdez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## TAMANHO firm Dez
Long      <- ls_TAMfirmDez$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMfirmDez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## TAMANHO firm Jun
Long      <- ls_TAMfirmJun$LONG[1:(152-LAG)] - Rf
Short     <- ls_TAMfirmJun$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## BM
Long      <- ls_BM$LONG[1:(152-LAG)] - Rf
Short     <- ls_BM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## MOMENTUM
Long      <- ls_MOM$LONG[1:(152-LAG)] - Rf
Short     <- ls_MOM$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## LIQUIDITY
Long      <- ls_LIQ$LONG[1:(152-LAG)] - Rf
Short     <- ls_LIQ$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

## LIQUIDITY DEZ
Long      <- ls_LIQdez$LONG[1:(152-LAG)] - Rf
Short     <- ls_LIQdez$SHORT[1:(152-LAG)] - Rf
LongShort <- (Long - Short)
summary(lm( Long      ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( Short     ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]
summary(lm( LongShort ~ Sentiment + MKT + SMB + HML ))$coefficients[2,]

rm(list=ls(pattern = "ls_"))

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



#' RESULTADOS

## AMOSTRA INICIAL E APOS OS FILTROS
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

# INDICE DE SENTIMENTO
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

# ## Quintis conforme Anomalias # ------------------------------------------------
allQuintiles(yMVfirmJun,  mReturns, mMVclass) ## TAMANHO  (VM Empresa Jun)
allQuintiles(yMVfirmDez, mReturns, mMVclass)  ## TAMANHO  (VM Empresa Dez)
allQuintiles(yMVclassJun, mReturns, mMVclass) ## TAMANHO  (VM Classe  Jun)
allQuintiles(yMVclassDez, mReturns, mMVclass) ## TAMANHO  (VM Classe  Dez)
allQuintiles(yVolumeJun, mReturns, mMVclass)  ## LIQUIDEZ (Volume Medio) JUN
allQuintiles(yVolumeDez, mReturns, mMVclass)  ## LIQUIDEZ (Volume Medio) DEZ ñ
allQuintiles(yBM, mReturns, mMVclass)         ## BM

require(xts)
yMomentum <- period.apply(mReturns,endpoints(mReturns,'years'), mean)
yMomentum   <- cleanData(yMomentum,   ySample2)
yMomentum <- yMomentum[-16,]
# rownames (yMomentum) <- rownames(yVolumeJun)
allQuintiles(yMomentum, mReturns, mMVclass)   ## MOMENTO
