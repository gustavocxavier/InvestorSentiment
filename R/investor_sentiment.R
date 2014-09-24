##
## Title: Investor Sentiment and Anomalies in Brazilian Market
##
## Version: 0.0.1
##
## Description: Script to compute que Investor Sentiment Index of the brazilian
## market.                       
## 

## INDICE #####################################################################
## 1.  SETTINGS
## 1.1 Install and load packages
## 1.2 My Functions
## 1.3 My Parameters <-------------- DEFINIR PARAMETROS ANTES DE RODAR O CODIGO
## 2. GET DATA AND CLEAN
## 3. INVESTOR SENTIMENT INDEX
## 4. CONSTRUCT PORTFOLIOS
## 5. PRICING MODELS
## 6. INVESTOR SENTIMENT AND ANOMALIES
##

## COISAS PRA FAZER AINDA ### #################################################
## - Indice de Sentimento totalmente Calculado no R
## - VM Empresa qnd ON e PN / VM Classe qnd so uma classe na amostra
## - Funcao p/ retornar todos os portfolios de uma vez allPortfoliosSeries
## - Funcao LongShortSeries
## - FILTRO Bovespa Negociability Index
## - FAZER UM FILTRO DE DATA PRA mProxies em breve

## 1. SETTINGS ## #############################################################

#== 1.1 Install and load packages = ===========================================

if ( !( "xts" %in% installed.packages() ) ) { install.packages("xts") }
library(xts)

#== 1.2 My Functions = ========================================================

#   1.2.1 Data Functions - ----------------------------------------------------

filterNoFinancial <- function(Sample, dbFile) {
    # Exclui as empresas financeiras
    # smpl ... Matriz de Amostra
    # file ... Arquivo onde tem a relacao de empresas
    
    db <- read.table(dbFile, dec=",", sep=";", header = T, na.strings="-",
                     stringsAsFactors=F)
    
    Sample <- reclass(Sample)
    Sample[,(db$SetorEconomatica=="Financas e Seguros")] <- 0
    
    return(Sample)
}

sampleReport <- function (s0, s1) {
    s0 <- as.xts(s0)
    Sample0 <- rowSums(as.data.frame(s0, row.names=substr(index(s0), 1, 4)))
    Sample  <- as.data.frame(Sample0)
    Sample1 <- rowSums(as.data.frame(s1, row.names=substr(index(s1), 1, 4)))
    Percent <- round(as.vector(Sample1) / as.vector(Sample0),2)
    Sample  <- cbind(Sample, as.data.frame(Sample1), Percent)
    colnames(Sample) <- c("Initial", "Final", "%")
    return(Sample)
}

filterNo24months <- function(prices, InitialSample) {
    # Descrição:
    #
    # (-) acoes que n apresentam 24 meses consecutivos
    # MM: Formação das carteiras em 01/jun
    # 12 meses antes (jun a mai) e 12 depois (jul a jun)
    # Fator Momento: retorno de jul/(n-1):mai/(n  ) (11 meses)
    #                precos  de jun/(n-1):mai/(n  ) (12 meses)
    # Carteiras:     retorno de jul/(n  ):jun/(n+1)    (12 meses)
    #                precos  de jun/(n  ):jun/(n+1) (13 meses)
    #
    # Argumentos
    #
    if (!is.xts(prices)) { prices <- as.xts(prices) }
    n <- as.numeric(format(first(index(prices)),"%Y")) # First year
    N <- as.numeric(format( last(index(prices)),"%Y")) # Last year
    NewSample <- as.matrix(InitialSample)
    NewSample[(n - (n - 1)),] <- 0 # First year equal zero
    for ( i in (n+1):(N-1) ) {
        p <- paste(i-1,"-06/",i+1,"-07", sep="") # Periodo de Interesse
        NewSample[(i+1 - n),] <- !apply(prices[p], 2, anyNA)
    }
    NewSample[(N+1 - n),] <- 0 # First year equal zero
    return(NewSample)
}

filterPositiveBook <- function(Sample, Book) {
    ##
    ## Filtrar apenas ações com patrimonio liquido positivo
    ##
    for ( i in 2:nrow(Sample)) {
        Sample[i,][( Book[(i-1),]<=0 | is.na(Book[(i-1),]) )] <- 0
    }
    #Sample[Book<=0 | is.na(Book)] <- 0
    #print("under construction")
    return(Sample)
}

asLogicalDataFrame <- function (df) {
    dfOut <- apply(df, 1, function(x) as.logical(x) )
    dfOut <- as.data.frame(t(dfOut))
    rownames(dfOut) <- rownames(df)
    colnames(dfOut) <- colnames(df)
    return(dfOut)
}

#   1.2.2 Sentiment Functions - -----------------------------------------------

chooseLAG <- function (m) {
    
    # FUNCTION TO CHOOSE THE BEST CORRELATION BETWEEN THE EACH CURRENT AND
    # LAGGED PROXIES
    # ______________________________________________________________
    # INPUT:
    #
    # m ...... Proxies Data
    #
    # ______________________________________________________________
    
    nproxies <- ncol(m)
    i <- 1
    delete <- 0
    for ( i in 1:(nproxies/2) ) {
        proxy <- cor(PCAstep1$x[,"PC1"],m)[i]
        proxy_lagged <- cor(PCAstep1$x[,"PC1"],m)[i+(nproxies/2)]
        if ( abs(proxy) < abs(proxy_lagged) ) { delete <- c(delete,-1*i) }
        if ( abs(proxy) > abs(proxy_lagged) ) { delete <- c(delete,-1*(i+(nproxies/2))) }
    }
    delete <- delete[-1]
    return(m[delete])
    
    # ______________________________________________________________
    # OUTPUT: data.frame/matrix just with the best proxies
    # ______________________________________________________________
}

#   1.2.3 Portfolio Functions - -----------------------------------------------

`%between%` <- function(x,rng) {
    
    # FUNÇÃO QUE VERIFICA SE UM VALOR ESTÁ ENTRE OS EXTREMOS DE UMA
    # SÉRIE
    # ______________________________________________________________
    # INPUT:
    #
    # x ...... Valor de interesse
    # rng .... Vetor com a série ou os extremos
    #
    # Sintaxe: x %between% rng
    #
    # ______________________________________________________________
    
    x <= max(rng,na.rm = TRUE) & x >= min(rng,na.rm = TRUE)
    
    # ______________________________________________________________
    # OUTPUT: Valor lógico
    # ______________________________________________________________
}

portfolioSelectAssets <- function (V, nPort, iPort, report=F) {
    
    ## DESCRICAO: Calcula a faixa de valor da variavel de interesse para formar
    ## um portfolio.
    ##
    ## ARGUMENTOS:
    ## V     ... Variavel de Interesse(criterio/caracteristica).
    ## nPort ... Número de portfolios
    ## iPort ... Portfolio de interesse
    ##
    
    V <- as.data.frame(V)
    dfV <- data.frame(row.names=c("MIN", "MAX"))
    for ( i in 1:nrow(V)) {
        
        # Calculando Faixa de Valores da Variavel de Interesse
        x <- c(0,seq(1:nPort)/nPort) # Sequencia de todos os quantis
        RANGE <- quantile(V[i,], x[iPort:(iPort+1)], na.rm=T) # Valor max e min
        YEAR  <- substr(rownames(as.data.frame(V)[i,]), 1, 4)
        if ( !is.na(RANGE[1]) ) {
            dfV <- (cbind(dfV, RANGE))
            # Selecionando ativos que estao na faixa de interesse naquele ano
            dCriterio <- V[i,] %between% RANGE
            dCriterio[is.na(dCriterio)] <- FALSE
            
            # ADICIONAR A UM DATA FRAME
            if ( !exists("dCriterioMatrix") ) {
                # SE FOR A TABELA NAO EXISTE, CRIA
                dCriterioMatrix <- V[1,]
                dCriterioMatrix[!is.na(dCriterioMatrix)] <- NA
                dCriterioMatrix <- dCriterio
            } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                dCriterioMatrix <- rbind(dCriterioMatrix,
                                         dCriterio)
            }
        }
    }
    #rownames(dfV) <- substr(rownames(V), 1, 4)
    
    if ( report == T ) {
        colnames(dfV) <- substr(rownames(as.data.frame(V)), 1, 4)[1:ncol(dfV)]
        dfV <- rbind(dfV,QTD=rowSums(dCriterioMatrix))
        cat(paste(iPort,"º portfolio dos ", nPort,".\n", sep=""))
        print(t(as.matrix(dfV)))
    }
    
    #     dCriterioMatrix[is.na(dCriterioMatrix)] <- 0
    
    #     # Transformar data (exemplo "1995-06-01" para "1995")
    #     rownames(dCriterioMatrix) <- substr(rownames(dCriterioMatrix), 1, 4)
    
    return(as.data.frame(dCriterioMatrix))
    # Anotações:
    # constructPortfolio <- function (strategy, nPortfolios, iPortfolio) {}
    # rebalancedPortfolios <- function ()
    # SelectStockBaskets <- function (strategy, nPort, iPort) {}
}

portfolioSerie  <- function (Return, MV, A, report=FALSE) {
    
    # INPUT
    # _________________________________________________________________
    #
    # Return .... Matriz de Retornos (Returns)
    # MV ... Matriz com os Valores de Mercado (MarketValues)
    # A .... Matriz de ativos pertecentes ao Portfolio (SelectedStocks)
    # _________________________________________________________________
    
    # Criando Matriz Indice de Data
    createDateIndex <- function() {
        # Função que cria índice de data
        # Criando de uma matriz mapa de datas
        matriz_indice <- data.frame(Date=seq(from=START,to=END,by="month"))
        
        matriz_indice <- cbind(matriz_indice,
                               M=as.numeric(substr(as.character(matriz_indice$Date),6,7)),
                               Y=as.numeric(substr(as.character(matriz_indice$Date),1,4)),
                               Q=as.numeric(substr(quarters(matriz_indice$Date),2,2))
        )
        matriz_indice <- cbind(matriz_indice,
                               Quarter=paste(matriz_indice$Q, "T", matriz_indice$Y, sep = ""),
                               nM = seq(1:length(matriz_indice$Date)),
                               nY = matriz_indice$Y+1-as.numeric(substr(as.character(START),1,4)),
                               nQ = sort(rep(1:ceiling(nrow(matriz_indice)/4),4))[1:nrow(matriz_indice)]
        )
        # inidice para selecao dos ativos do portfolio
        j2   <- matriz_indice$nM[matriz_indice$M==7][2] # Segundo julho da amostra
        Pn   <- sort(rep(1:ceiling(nrow(matriz_indice)/12),12))[1:nrow(matriz_indice)]
        Pn_1 <- c(rep(NA, j2-1), Pn[(1:(nrow(matriz_indice)-j2+1))] ) # n-1
        Pn0  <- Pn_1 + 1 # Indice p/ dados em n
        Pn   <- Pn0
        matriz_indice <- cbind(matriz_indice, Pn, Pn0, Pn_1)
        return(matriz_indice)
    }
    dateIndex <- createDateIndex()
    
    n <- dateIndex$nM[dateIndex$M==7][2] # Segundo julho da amostra
    # sort(dateIndex$nM[(dateIndex$M==5)], decreasing=T)[2] # Penultimo maio
    N <- nrow(Return)
    
    # Verificar se matriz A é com base em dados anteriores a jun (mes 6)
    if ( as.numeric(substr(rownames(A)[2], 6, 7)) <= 6 ) {
        # Baseado no ano n
        dateIndex$Pn <- dateIndex$Pn0
    } else {
        # Baseado no ano n-1
        dateIndex$Pn <- dateIndex$Pn_1        
    }
    
    Return <- as.matrix(Return) # Transformando numa matriz pra subset
    
    for (i in n:N) {
        if ( dateIndex$Pn[i] <= nrow(A) ) {
            
            # Cria vetor que diz qual ativo pertence à carteira
            ASSETS <- as.logical(A[dateIndex$Pn[i],])
            
            # Valor de Mercado total dos ativos da carteira
            marketVALUE  <- sum(MV[i,ASSETS], na.rm=T)
            
            # Quantidade de ativos na carteira
            nA  <- sum(as.numeric(ASSETS))
            
            # Media igualmente ponderada do retorno dos ativos da carteira
            rEW <- mean(Return[i,ASSETS], na.rm=T)
            
            # Media ponderada pelo valor do retorno dos ativos da carteira
            rVW <- sum (Return[i,ASSETS] * MV[i,ASSETS] / marketVALUE, na.rm=T)
            
        } else {
            # Valores n existem
            marketVALUE  <- NA
            nA           <- NA
            rEW          <- NA
            rVW          <- NA
            
        }
        if ( !exists("pSerie") ) {
            # SE FOR A TABELA NAO EXISTE, CRIA
            pSerie <- data.frame(rEW=rEW,
                                 rVW=rVW,
                                 MV=marketVALUE,
                                 nA=nA)
        } else { # SE EXISTE, APENAS ADICIONAR LINHAS
            pSerie <- rbind(pSerie,c(rEW,
                                     rVW,
                                     marketVALUE,
                                     nA))
        }
    }
    
    # ______________________________________________________________
    #
    #  OUTPUT
    # ______________________________________________________________
    #
    # rEW ... Série de retornos igualmente ponderado
    # rVW ... Série de retornos ponderado pelo valor
    # MV .... Valor de Mercado da carteira no período
    # nA .... Número de ativos da carteira no período
    # ______________________________________________________________
    
    rowNames <- rownames(as.data.frame(Return))[n:N]
    rownames(pSerie) <- rowNames
    if ( report == T ) { print(summary(pSerie)) }
    return(pSerie)
}

#---- allQuintiles --- --- ---
# allQuintiles <- function (V, nPort, R, MV) {
#     
#     ## ______________________________________________________________
#     ##
#     ## Imprime Média e Retorna a Série de todos os Portfolios
#     ##
#     ## ARGUMENTS:
#     ## V     ... Variavel de Interesse(criterio/caracteristica).
#     ## nPort ... Número de portfolios
#     ## R ....... Matriz de Retornos
#     ## MV ...... Matriz com os Valores de Mercado
#     ## ______________________________________________________________
#     for ( i in 1:nPort ) {
#         portfolioSerie(R, MV, portfolioSelectAssets(V, nPort, i))
#     }
#     ## imprimir valores medios e desvio padrão
# }

# ---- LongShortSeries --- --- ---
# LongShortSeries    <- function (strategy, nPortfolios, RET, MV) {
#      cat("rLong, mvLong, rShort, mvShort")
# }

cleanData <- function(yData,Sample) {
    # Atribui NA em todos os valores yData em n-1 qnd Sample em n for FALSE
    yData[-nrow(yData),][(Sample[-1,]==F)] <- NA
    # Atribui NA em todos os valores yData no ultimo ano
    yData[ nrow(yData),] <- NA
    return(yData)
}

## Definindo Parametros / Setting Parameters

#== 1.3 My Parameters = =======================================================
## Definindo Parametros / Setting Parameters
START        <- as.Date("2000-06-01") # Initial Date
END          <- as.Date("2014-07-31") # Final Date
PERIOD.XTS   <- "2000-06/2014-07"
# M&O(2011): jun/95 a jun/08

## 2. GET DATA AND CLEAN ## ##################################################
## Get Data and Clean
## Carregar e limpar dados

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
sampleReport(ySample0, ySample1)

#   2.2.2 FILTRO DE 24 MESES --- --------------------------------------------------

ySample2 <- filterNo24months(mPrices, ySample0) * ySample1
sampleReport(ySample0, ySample2)

# --- FILTRO Bovespa Negociability Index --- ----------------------------------
# TODO: FILTRO Bovespa Negociability Index
# mNegociabilidade <- importaBaseCSV("Input/mNegociabilidade.csv", START, END)
# 
# # Convert monthly to yearly
# yNegociabilidade <- mNegociabilidade[dateIndex$M==12,]
# row.names(yNegociabilidade) <- dateIndex$Y[dateIndex$M==12]
# 
# # Liquidity filter by negociability index
# ySampleNegociab <- ySample0
# ySampleNegociab[yNegociabilidade <= 0.01] <- 0

# --- FILTRO VALOR DE MERCADO EM 30/06 e 31/12 --- ----------------------------

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

ySample3 <- ySample2 # Cria matriz de controle da amostra a partir da ultima
ySample3[!is.na(ySample3)] <- 1 # Atribui temporariamente 1 a todos os campos

# Atribui 0 para os valores que nao satisfazem a condicao desejada em dez/n-1
ySample3[-1,][yMVfirmDez <= 0]   <- 0 # Valores zerados em dez/n-1 na base
ySample3[-1,][is.na(yMVfirmDez)] <- 0 # Valores invexistentes em dez/n-1 na base

# Atribui 0 para os valores que nao satisfazem a condicao desejada em jun/n
ySample3[(yMVfirmJun <= 0)] <- 0 # Valores zerados em jun na base
ySample3[is.na(yMVfirmJun)] <- 0 # Valores invexistentes em jun na base

ySample3 <- ySample3 * ySample2 # Interagem com atendem ao filtro anterior

sampleReport(ySample0, ySample3)

# --- FILTRO DE PATRIMONIO LIQUIDO --- ----------------------------------------

yBookFirm <- read.table ("Input/yBookFirm.csv", header = T, sep=";",
                         dec=",", skip=0,
                         row.names=1, na.strings="-", stringsAsFactors=F)
row.names(yBookFirm) <- as.Date(paste(rownames(yBookFirm),"-12-01", sep=""),
                                "%Y-%m-%d")
yBookFirm            <- as.xts(yBookFirm, descr='Patrimonio Liquido')
yBookFirm            <- as.data.frame(as.matrix(yBookFirm[PERIOD.XTS]))

# Desnecessary code?
# yPERIOD.XTS <- paste(substr(PERIOD.XTS,1,4),substr(PERIOD.XTS,9,12), sep="/")
# yBookFirm            <- yBookFirm[yPERIOD.XTS]

ySample4 <- filterPositiveBook(ySample2, yBookFirm) * ySample3
sampleReport(ySample0,ySample4)
# OBS.: AMOSTRA INICIAL MENOR DO QUE A DE M&O(2011), E A FINAL MAIOR

# Codigo Antigo
# # Read Book Value
# yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", START, END, formato="%Y")
# 
# # Positive Book Value Filter
# ySamplePositiveBook <- ySample0
# ySamplePositiveBook[ is.na(yBookFirm) ] <- 0
# ySamplePositiveBook[ yBookFirm < 0    ] <- 0

# === Final Sample === ========================================================

ySample <- asLogicalDataFrame(ySample4)

## 3. INVESTOR SENTIMENT INDEX ## #############################################
## 3. Índice de Sentimento
## 3.1. Temporalidade das Proxies: Selecionar proxies que serão defasadas
## 3.2. Índice de Sentimento não Ortogonalizado
## 3.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  

#== 3.1 Read/Compute Proxies === ===============================================

mProxies   <- read.table ("Input/mProxies.csv",          # Read data
                          header = T, sep=";", dec=",",
                          row.names=1)

# x <- as.Date(rownames(mProxies), format="%d/%m/%Y")      # Temporary variable

# TO DO: FAZER UM FILTRO DE DATA PRA mProxies em breve
mProxies <- mProxies[!is.na(mProxies$NIPO_lagged),]

#as.dist(round(cor(mProxies, use="na.or.complete"),2))    # Correlations s/ NA
as.dist(round(cor(mProxies, use="everything"),2))       # Correlations c/ Na

#== 3.2 First Step ============================================================
# Estimating first component of all proxies and their lags and choose the best

PCAstep1 <- prcomp(mProxies, scale=T)

round(cor(PCAstep1$x[,"PC1"],mProxies),2)         # The correlations
mBestProxies <- chooseLAG(mProxies);rm(chooseLAG) # Choosing LAGs...
colnames(mBestProxies)                            # Best proxies
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)     # Correlation with PC1
as.dist(round(cor(mBestProxies),2))               # Correlations between them

#== 3.3 Second Step === =========================================================
# Estimating first component of the best proxies

PCAstep2 <-prcomp(mBestProxies, scale=T)

cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]) # Correlation with PC1 of the 1º step
summary(PCAstep2)                          # Proportion of Variance
PCAstep2$rotation[,"PC1"] # Not orthogonalized index (osb.: not important)

# === Third Step === ==========================================================
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

# === Sentiment Results === ===================================================
# as.dist(round(cor(mProxies),2))                      # Verificando correlação entre as proxies
# round(cor(PCAstep1$x[,"PC1"],mProxies),2)            # Correlação das Proxies com 1ª Componente da 1ª Etapa
# round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)        # Correlação Proxies Escolhidas c/ 1ª Componente da 1ª Etapa
# cor(PCAstep1$x[,"PC1"],PCAstep2$x[,"PC1"]) * (-1)    # Verificando correlacao com o primeiro indice
# summary(PCAstep2)                                    # Percentual explicado da variancia
# PCAstep2$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Nao Ortogonalizado
# as.dist(round(cor(mBestProxies),2))                  # Correlação Proxies Escolhidas
# round(cor(PCAstep2$x[,"PC1"],mBestProxies),2) * (-1) # Correlação Proxies Escolhidas c/ 1ª Componente da 2ª Etapa
# cor(PCAstep2$x[,"PC1"],PCAstep3$x[,"PC1"])           # Correlação do Indice da 3ª etapa com o da 2ª etapa
# summary(PCAstep3)                                    # Percentual explicado da variancia
# PCAstep3$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Ortogonalizado

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


## 4. CONSTRUCT PORTFOLIOS ## #################################################
## 4. Portfolios
## 4.1 Construir Carteiras
##       portfolioAssets cria_matriz_carteira - retorna dCriterio
## 4.2 Interação de Carteiras
##       portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 4.3 Retorno das Carteiras
##       portfolioSerie - retorna ...
## 4.  PORTFOLIOS ## ##########################################################
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

mPrices.xts <- as.xts(mPrices)
mReturns <- diff(log(mPrices.xts), lag=1) # Compute Logarithmic Returns

# === TAMANHO === =============================================================

yMVfirmJun <- cleanData(yMVfirmJun, ySample)

PS5.1a <- portfolioSelectAssets(yMVfirmJun, 5, 1, report=T)
PS5.2a <- portfolioSelectAssets(yMVfirmJun, 5, 2, report=F) 
PS5.3a <- portfolioSelectAssets(yMVfirmJun, 5, 3, report=F)
PS5.4a <- portfolioSelectAssets(yMVfirmJun, 5, 4, report=F)
PS5.5a <- portfolioSelectAssets(yMVfirmJun, 5, 5, report=T)

# Valor de Mercado da Classe para ponderacao
mMVclass <- read.table ("Input/mMarketValue.csv", header = T, sep=";", dec=",",
                        skip=1, row.names=1, na.strings="-", stringsAsFactors=F)
rownames(mMVclass) <- as.Date(rownames(mMVclass),"%d/%m/%Y")

# Filtrando Periodo
mMVclass.xts <- as.xts(mMVclass, descr="MONTHLY PRICES")[PERIOD.XTS]
mMVclass     <- data.frame(as.matrix(mMVclass.xts)) ; rm(mMVclass.xts)

PS5.1r <- portfolioSerie(mReturns, mMVclass, PS5.1a)
PS5.2r <- portfolioSerie(mReturns, mMVclass, PS5.2a)
PS5.3r <- portfolioSerie(mReturns, mMVclass, PS5.3a)
PS5.4r <- portfolioSerie(mReturns, mMVclass, PS5.4a)
PS5.5r <- portfolioSerie(mReturns, mMVclass, PS5.5a, T)

data.frame(P1=c(mean(PS5.1r$rVW, na.rm=T),sd(PS5.1r$rVW, na.rm=T))*100,
           P2=c(mean(PS5.2r$rVW, na.rm=T),sd(PS5.2r$rVW, na.rm=T))*100,
           P3=c(mean(PS5.3r$rVW, na.rm=T),sd(PS5.3r$rVW, na.rm=T))*100,
           P4=c(mean(PS5.4r$rVW, na.rm=T),sd(PS5.4r$rVW, na.rm=T))*100,
           P5=c(mean(PS5.5r$rVW, na.rm=T),sd(PS5.5r$rVW, na.rm=T))*100,
           row.names=c("r","DP"))

rm(list=ls(pattern= "PS5.", all.names = TRUE))

# --- Testando c/ MV class ----------------------------------------------------
# Cria matriz apenas com os valores de Dez e apenas com valores de Jun
yMVclassJun <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="jun"),]
# yMVclassDez <- mMVclass[(months(as.Date(rownames(mMVclass)), T)=="dez"),]
yMVclassJun <- cleanData(yMVclassJun, ySample)

PS5.1a <- portfolioSelectAssets(yMVclassJun, 5, 1, report=T)
PS5.2a <- portfolioSelectAssets(yMVclassJun, 5, 2, report=F) 
PS5.3a <- portfolioSelectAssets(yMVclassJun, 5, 3, report=F)
PS5.4a <- portfolioSelectAssets(yMVclassJun, 5, 4, report=F)
PS5.5a <- portfolioSelectAssets(yMVclassJun, 5, 5, report=T)

PS5.1r <- portfolioSerie(mReturns, mMVclass, PS5.1a)
PS5.2r <- portfolioSerie(mReturns, mMVclass, PS5.2a)
PS5.3r <- portfolioSerie(mReturns, mMVclass, PS5.3a)
PS5.4r <- portfolioSerie(mReturns, mMVclass, PS5.4a)
PS5.5r <- portfolioSerie(mReturns, mMVclass, PS5.5a)

data.frame(P1=c(mean(PS5.1r$rVW, na.rm=T),sd(PS5.1r$rVW, na.rm=T))*100,
           P2=c(mean(PS5.2r$rVW, na.rm=T),sd(PS5.2r$rVW, na.rm=T))*100,
           P3=c(mean(PS5.3r$rVW, na.rm=T),sd(PS5.3r$rVW, na.rm=T))*100,
           P4=c(mean(PS5.4r$rVW, na.rm=T),sd(PS5.4r$rVW, na.rm=T))*100,
           P5=c(mean(PS5.5r$rVW, na.rm=T),sd(PS5.5r$rVW, na.rm=T))*100,
           row.names=c("r","DP"))

# /// THE END ///

# === LIQUIDEZ === ============================================================

# Valor de Mercado da Classe para ponderacao
mVolume <- read.table ("Input/mVolume.csv", header = T, sep=";", dec=",",
                       skip=1, row.names=1, na.strings="-", stringsAsFactors=F)
rownames(mVolume)  <- as.Date(rownames(mVolume),"%d/%m/%Y")

# Filtrando Periodo
mVolume.xts <- as.xts(mVolume, descr='Volume em Reais')[PERIOD.XTS]
yVolume     <- apply.yearly(mVolume.xts, mean)
mVolume     <- data.frame(as.matrix(mVolume.xts)) ; rm(mVolume.xts)

yVolume            <- cleanData(yVolume,    ySample)

PL5.1a <- portfolioSelectAssets(yVolume, 5, 1)
PL5.2a <- portfolioSelectAssets(yVolume, 5, 2) 
PL5.3a <- portfolioSelectAssets(yVolume, 5, 3)
PL5.4a <- portfolioSelectAssets(yVolume, 5, 4)
PL5.5a <- portfolioSelectAssets(yVolume, 5, 5)
PL5.1r <- portfolioSerie(mReturns, mMVclass, PL5.1a)
PL5.2r <- portfolioSerie(mReturns, mMVclass, PL5.2a)
PL5.3r <- portfolioSerie(mReturns, mMVclass, PL5.3a)
PL5.4r <- portfolioSerie(mReturns, mMVclass, PL5.4a)
PL5.5r <- portfolioSerie(mReturns, mMVclass, PL5.5a)

data.frame(P1=c(mean(PL5.1r$rVW, na.rm=T),sd(PL5.1r$rVW, na.rm=T))*100,
           P2=c(mean(PL5.2r$rVW, na.rm=T),sd(PL5.2r$rVW, na.rm=T))*100,
           P3=c(mean(PL5.3r$rVW, na.rm=T),sd(PL5.3r$rVW, na.rm=T))*100,
           P4=c(mean(PL5.4r$rVW, na.rm=T),sd(PL5.4r$rVW, na.rm=T))*100,
           P5=c(mean(PL5.5r$rVW, na.rm=T),sd(PL5.5r$rVW, na.rm=T))*100,
           row.names=c("r","DP"))

rm(list=ls(pattern= "PL5.", all.names = TRUE))

# === BM === ==================================================================
# TO DO: Calcular BM da classe

yBM <- yBookFirm / yMVfirmDez

PB5.1a <- portfolioSelectAssets(yBM, 5, 1)
PB5.2a <- portfolioSelectAssets(yBM, 5, 2) 
PB5.3a <- portfolioSelectAssets(yBM, 5, 3)
PB5.4a <- portfolioSelectAssets(yBM, 5, 4)
PB5.5a <- portfolioSelectAssets(yBM, 5, 5)
PB5.1r <- portfolioSerie(mReturns, mMVclass, PB5.1a)
PB5.2r <- portfolioSerie(mReturns, mMVclass, PB5.2a)
PB5.3r <- portfolioSerie(mReturns, mMVclass, PB5.3a)
PB5.4r <- portfolioSerie(mReturns, mMVclass, PB5.4a)
PB5.5r <- portfolioSerie(mReturns, mMVclass, PB5.5a)

data.frame(P1=c(mean(PB5.1r$rVW, na.rm=T),sd(PB5.1r$rVW, na.rm=T))*100,
           P2=c(mean(PB5.2r$rVW, na.rm=T),sd(PB5.2r$rVW, na.rm=T))*100,
           P3=c(mean(PB5.3r$rVW, na.rm=T),sd(PB5.3r$rVW, na.rm=T))*100,
           P4=c(mean(PB5.4r$rVW, na.rm=T),sd(PB5.4r$rVW, na.rm=T))*100,
           P5=c(mean(PB5.5r$rVW, na.rm=T),sd(PB5.5r$rVW, na.rm=T))*100,
           row.names=c("r","DP"))

rm(list=ls(pattern= "PB5.", all.names = TRUE))

# === MOMENTO === =============================================================

yReturns <- period.apply(mReturns,endpoints(mVolume,'years'), sum)
yReturns <- apply.yearly(mReturns, mean)
yReturns <- cleanData(yReturns, ySample)

PL5.1a <- portfolioSelectAssets(yReturns, 5, 1)
PL5.2a <- portfolioSelectAssets(yReturns, 5, 2) 
PL5.3a <- portfolioSelectAssets(yReturns, 5, 3)
PL5.4a <- portfolioSelectAssets(yReturns, 5, 4)
PL5.5a <- portfolioSelectAssets(yReturns, 5, 5)
PL5.1r <- portfolioSerie(mReturns, mMVclass, PL5.1a)
PL5.2r <- portfolioSerie(mReturns, mMVclass, PL5.2a)
PL5.3r <- portfolioSerie(mReturns, mMVclass, PL5.3a)
PL5.4r <- portfolioSerie(mReturns, mMVclass, PL5.4a)
PL5.5r <- portfolioSerie(mReturns, mMVclass, PL5.5a)

data.frame(P1=c(mean(PL5.1r$rVW, na.rm=T),sd(PL5.1r$rVW, na.rm=T))*100,
           P2=c(mean(PL5.2r$rVW, na.rm=T),sd(PL5.2r$rVW, na.rm=T))*100,
           P3=c(mean(PL5.3r$rVW, na.rm=T),sd(PL5.3r$rVW, na.rm=T))*100,
           P4=c(mean(PL5.4r$rVW, na.rm=T),sd(PL5.4r$rVW, na.rm=T))*100,
           P5=c(mean(PL5.5r$rVW, na.rm=T),sd(PL5.5r$rVW, na.rm=T))*100,
           row.names=c("r","DP"))

rm(list=ls(pattern= "PL5.", all.names = TRUE))

# === Returns === =============================================================

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
