# invSent ## ##################################################################
# 
# Title: Investor Sentiment and Anomalies in Brazilian Market
#
# Version: 0.0.1
#
# Description: Script to compute que Investor Sentiment Index of the brazilian
# market.                       
# 
# 0. SETTINGS
# 0.1 Parameters
# 0.2 Bibliotecas e Funcoes
# 1. GETTING CLEANING DATA
# 2. INVESTOR SENTIMENT INDEX
# 3. CONSTRUCT PORTFOLIOS
# 4. INVESTOR SENTIMENT AND ANOMALIES
#

## COISAS PRA FAZER AINDA ### #################################################
## - Organizar Funçoes acima
## - Indice de Sentimento Calculado no R
## - VM Empresa qnd ON e PN / VM Classe qnd so uma classe na amostra
## - Funcao p/ retornar todos os portfolios de uma vez allPortfoliosSeries
## - Funcao LongShortSeries

## 0. SETTINGS ## #############################################################

# === BIBLIOTECAS E FUNCOES === ===============================================
if ( !( "xts" %in% installed.packages() ) ) { install.packages("xts") }
library(xts)

## Definindo Parametros / Setting Parameters

# === PARAMETROS === ==========================================================
## Definindo Parametros / Setting Parameters
START        <- as.Date("1995-06-01") # Initial Date
END          <- as.Date("2009-07-31") # Final Date
PERIOD.XTS   <- "1995-06/2009-07"
# M&O(2011): jun/95 a jun/08

## 1. GETTING CLEANING DATA ## ################################################
## Get Data and Clean
## Carregar e limpar dados

# === Read Data === ===========================================================

# Carregando matriz de precos / Stock Prices
mPrices            <- read.table ("Input/mPrices.csv", header = T, sep=";",
                                  dec=",", skip=0, row.names=1, na.strings="-",
                                  stringsAsFactors=F)
row.names(mPrices) <- as.Date(row.names(mPrices), format="%d/%m/%Y")

# Filtrando Periodo
mPrices.xts        <- as.xts(mPrices, descr="MONTHLY PRICES")[PERIOD.XTS]
mPrices            <- data.frame(as.matrix(mPrices.xts)) ; rm(mPrices.xts)

# === Initial Sample === ======================================================
# --- AMOSTRA INICIAL --- -----------------------------------------------------
# Initial Sample (1 para todos os anos que houve cotacao)
mSample0                  <- as.matrix(mPrices)
mSample0[!is.na(mPrices)] <- 1
mSample0[is.na(mPrices)]  <- 0
ySample0                  <- as.matrix(apply.yearly(as.xts(mSample0), mean))
rm(mSample0)
ySample0[ySample0>0]      <- 1

# --- FILTRO EMPRESAS NAO FINANCEIRAS --- -------------------------------------

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
ySample1 <- filterNoFinancial(ySample0, "Input/dbStocks.csv")
rm(filterNoFinancial)
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
sampleReport(ySample0, ySample1)

dateIndex <- createDateIndex() # Generate date map matrix for the next cmd

ySample24m  <- filter24months(ySample0, mPrices) # Filter of 24 months

# Bovespa Negociability Index
mNegociabilidade <- importaBaseCSV("Input/mNegociabilidade.csv", START, END)

# Convert monthly to yearly
yNegociabilidade <- mNegociabilidade[dateIndex$M==12,]
row.names(yNegociabilidade) <- dateIndex$Y[dateIndex$M==12]

# Liquidity filter by negociability index
ySampleNegociab <- ySample0
ySampleNegociab[yNegociabilidade <= 0.01] <- 0

# Read Book Value
yBookFirm <- importaBaseCSV("Input/yBookFirm.csv", START, END, formato="%Y")

# Positive Book Value Filter
ySamplePositiveBook <- ySample0
ySamplePositiveBook[ is.na(yBookFirm) ] <- 0
ySamplePositiveBook[ yBookFirm < 0    ] <- 0

# === Final Sample === ========================================================

# Compute all the filters together
ySample <- ySample24m * ySampleNegociab * ySamplePositiveBook

## Generate Yearly Sample Control Matrix
mSample <- ySample[sort(rep(1:nrow(ySample),12)),] # repeat 12 times the values
# Add rows to the last incomplete year
mSample <- rbind(mSample, mSample[rep(nrow(mSample),
                                      nrow(mPrices)-nrow(mSample)), ])
row.names(mSample) <- row.names(mPrices) # Set name of the rows equal mPrices

# === Results of Sample === ===================================================
rowSums(ySample0)#[-1]                # Initial Sample

rowSums(ySample24m)[-1]          # Just the firms with 24 months of price
round(rowSums(ySample24m)[-1]/rowSums(ySample0)[-1],2)          # %

rowSums(ySamplePositiveBook)[-1] # Just the Positive book
round(rowSums(ySamplePositiveBook)[-1]/rowSums(ySample0)[-1],2) # %

rowSums(ySampleNegociab)[-1]     # Just the most liquid
round(rowSums(ySampleNegociab)[-1]/rowSums(ySample0)[-1],2)     # %

rowSums(ySample)#[-1]             # Final Sample
round(rowSums(ySample)/rowSums(ySample0),2)             # %
round(rowSums(ySample)[-1]/rowSums(ySample0)[-1],2)             # %

# The first year was not computed because the 24 months filter

# OK Negociability but not OK 24 months
rowSums(ySampleNegociab)[-1]-rowSums(ySample24m*ySampleNegociab)[-1]

# OK 24 months but not OK Negociability
rowSums(ySample24m)[-1]-rowSums(ySample24m*ySampleNegociab)[-1]



## 2. INVESTOR SENTIMENT INDEX ## #############################################
## 2. Índice de Sentimento
## 2.1. Temporalidade das Proxies: Selecionar proxies que serão defasadas
## 2.2. Índice de Sentimento não Ortogonalizado
## 2.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  

# === Read/Compute Proxies === ===============================================

mProxies   <- read.table ("Input/mProxies.csv",          # Read data
                          header = T, sep=";", dec=",",
                          row.names=1)
x <- as.Date(rownames(mProxies), format="%d/%m/%Y")      # Temporary variable
mProxies <- mProxies[(x >= START & x <= END),] ; rm(x) ; # Date filter
as.dist(round(cor(mProxies),2))                          # Correlations

# === First Step === ==========================================================
# Estimating first component of all proxies and their lags and choose the best

PCAstep1 <- prcomp(mProxies, scale=T)

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

round(cor(PCAstep1$x[,"PC1"],mProxies),2)         # The correlations
mBestProxies <- chooseLAG(mProxies);rm(chooseLAG) # Choosing LAGs...
colnames(mBestProxies)                            # Best proxies
round(cor(PCAstep1$x[,"PC1"],mBestProxies),2)     # Correlation with PC1
as.dist(round(cor(mBestProxies),2))               # Correlations between them

# === Second Step === =========================================================
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
mMacroeconomics <-  mMacroeconomics[(x >= START & x <= as.Date("2013-12-01")),]
rm(x)
                                                  # <= END

END   <- as.Date("2014-07-01") # TODO: Discover why this

# dummy SELIC igual a 1 quando a taxa cai em rela??o ao m?s anterior
dSELIC <- c(0,as.numeric(embed(mMacroeconomics$SELIC,2)[,1] <= 
                                 embed(mMacroeconomics$SELIC,2)[,2]
)
)

# dummy PIB igual a 1 quando o PIB sobe em rela??o ao m?s anterior
dPIB   <- c(0,as.numeric(embed(mMacroeconomics$PIB,2)[,1] >=
                                 embed(mMacroeconomics$PIB,2)[,2]
)
)

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



## 3. CONSTRUCT PORTFOLIOS ## #################################################
## 3. Portfolios
## 3.1 Construir Carteiras
##       portfolioAssets cria_matriz_carteira - retorna dCriterio
## 3.2 Interação de Carteiras
##       portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
## 3.3 Retorno das Carteiras
##       portfolioSerie - retorna ...

# === Functions === ===========================================================

portfolioRange <- function(CRITERIO, nPortfolios=5, portfolio=1) {
    
    # ______________________________________________________________
    # 
    # Retorna os valores máximos e mínimos para formação de um portfolio
    # Cria vetor de sequencia
    x <- c(0,seq(1:nPortfolios)/nPortfolios)
    # Salvando o valor maximo e minimo
    RANGE <- quantile(CRITERIO, x[portfolio:(portfolio+1)], na.rm=T)
    # Retorna a faixa de valor do portfolio escolhido
    
    # ______________________________________________________________
    
    return(RANGE)
}

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

portfolioAssets <- function(CRITERIO, nPortfolios=5, portfolio=1) {
        
        # CRITERIO .... Vetor de criterio
        # nPortfolios . Número de portfolios
        # iPortfolio .. Portfolio desejado
        
        # Salvando faixa de valor do portfolio escolhido
        RANGE <- portfolioRange(CRITERIO, nPortfolios, portfolio)
        
        # Cirando vetor de ativos que participam da carteira
        dCriterio <- CRITERIO %between% RANGE
        dCriterio[is.na(dCriterio)] <- FALSE
        return(as.numeric(dCriterio))
}

portfolioAssets2 <- function(CRITERIO, nPortfolios=5, portfolio=1) {
        for (i in 1:nrow(CRITERIO)) {
                # Salvando faixa de valor do portfolio escolhido
                #RANGE <- portfolioRange(CRITERIO[i,], nPortfolios, portfolio)
                
                # Cirando vetor de ativos que participam da carteira
                dCriterioVector <- portfolioAssets(CRITERIO[i,], nPortfolios, portfolio)
                #dCriterio <- CRITERIO %between% RANGE
                #dCriterio[is.na(dCriterio)] <- FALSE 
                
                # ADICIONAR A UM DATA FRAME
                if ( !exists("dCriterioMatrix") ) {
                        # SE FOR A TABELA NAO EXISTE, CRIA
                        dCriterioMatrix <- CRITERIO[1,]
                        dCriterioMatrix[!is.na(dCriterioMatrix)] <- NA
                        dCriterioMatrix <- dCriterioVector
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                        dCriterioMatrix <- rbind(dCriterioMatrix,
                                                 dCriterioVector)
                }
        }
        dCriterioMatrix[is.na(dCriterioMatrix)] <- 0
        row.names(dCriterioMatrix) <- row.names(CRITERIO)
        # RETORNAR O DATA FRAME
        return(dCriterioMatrix)
}

portfolioSerie <- function (RETURN, MV, PortfolioAssets) {
    
    # INPUT
    # ______________________________________________________________
    #
    # RETURN ...... Matriz de Retornos
    # MV .......... Matriz com os Valores de Mercado
    # PortfolioAssets ... Matriz de ativos pertecentes ao Portfolio
    # ______________________________________________________________
    
    RETURN <- as.matrix(RETURN)
    
    for (i in 1:nrow(RETURN)) {
        
        # Cria vetor que diz qual ativo pertence à carteira
        ASSETS <- as.logical(PortfolioAssets[dateIndex$nY[i],])
        
        # Valor de Mercado total dos ativos da carteira
        marketVALUE  <- sum(MV[i,ASSETS], na.rm=T)
        
        # Quantidade de ativos na carteira
        nA  <- sum(as.numeric(ASSETS))
        
        # Media igualmente ponderada do retorno dos ativos da carteira
        rEW <- mean(RETURN[i,ASSETS], na.rm=T)
        
        # Media ponderada pelo valor do retorno dos ativos da carteira
        rWV <- sum (RETURN[i,ASSETS] * MV[i,ASSETS] / marketVALUE, na.rm=T)
        
        # xC
        if ( !exists("pSerie") ) {
            # SE FOR A TABELA NAO EXISTE, CRIA
            pSerie <- data.frame(rEW=rEW,
                                 rWV=rWV,
                                 MV=marketVALUE,
                                 nA=nA)
        } else { # SE EXISTE, APENAS ADICIONAR LINHAS
            pSerie <- rbind(pSerie,c(rEW,
                                     rWV,
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
    # rWV ... Série de retornos ponderado pelo valor
    # MV .... Valor de Mercado da carteira no período
    # nA .... Número de ativos da carteira no período
    # xC .... Valor médio da característica de formação da carteira
    # ______________________________________________________________
    
    row.names(pSerie) <- row.names(RETURN)
    return(pSerie)
    
    
}

# === Returns === =============================================================

# Compute Returns
tempPrices  <- importaBaseCSV("Input/mPrices.csv", (START-31), END)
mReturns    <- as.data.frame(diff(log(as.matrix(mPrices)))) ; rm(tempPrices)

# Read Market Value
mMarketValue     <- importaBaseCSV("Input/mMarketValue.csv",
                                   START, END, pula_linha=1)

# Convert monthly data to yearly
yMarketValue <- mMarketValue[dateIndex$M==12,]

yMarketValue[ySample==0]      <- NA
yMarketValue[yMarketValue==0] <- NA
yBookFirm[ySample==0]         <- NA

# Compute Book-to-market
yBM <- yBookFirm / yMarketValue
length(yBookFirm[yBookFirm==0])
yBM[yMarketValue==0] <- NA
yBM[yBM==+Inf]
sum(yBookFirm==0, na.rm=T)
sum(yMarketValue==0, na.rm=T)
sum(yBM<=-Inf, na.rm=T)

head(which(yBookFirm!=0, arr.ind=T))
yBookFirm[,1:2]

##Teste Range
portfolioRange(yMarketValue[2,],2,1)
portfolioRange(yBM,3,1) # Verificar o -Inf e o +Inf

# szS szB bmH bmN bmL SH SN SL BN BL
AssetsSize_S <- portfolioAssets2(yMarketValue,2,1)  # Small
AssetsSize_B <- portfolioAssets2(yMarketValue,2,2)  # Big   

AssetsBM_H <- portfolioAssets2(yBM,3,1)             # Value (High BM)
AssetsBM_N <- portfolioAssets2(yBM,3,2)             # Neutral
AssetsBM_L <- portfolioAssets2(yBM,3,3)             # Growth (Low BM)

AssetsSH <- AssetsSize_S * AssetsBM_H # Small Value (High BM)
AssetsSN <- AssetsSize_S * AssetsBM_N # Small Neutral
AssetsSL <- AssetsSize_S * AssetsBM_L # Small Growth (Low BM)
AssetsBH <- AssetsSize_B * AssetsBM_H # Big Value (High BM)
AssetsBN <- AssetsSize_B * AssetsBM_N # Big Neutral
AssetsBL <- AssetsSize_B * AssetsBM_L # Big Growth (Low BM)

AssetsSH <- apply(AssetsSH, 1, function(x) as.logical(x) ) # Small Neutral
AssetsSN <- apply(AssetsSN, 2, function(x) as.logical(x) ) # Small Neutral
AssetsSL <- apply(AssetsSL, 2, function(x) as.logical(x) ) # Small Growth (Low BM)
AssetsBH <- apply(AssetsBH, 2, function(x) as.logical(x) ) # Big Value (High BM)
AssetsBN <- apply(AssetsBN, 2, function(x) as.logical(x) ) # Big Neutral
AssetsBL <- apply(AssetsBL, 2, function(x) as.logical(x) ) # Big Growth (Low BM)
AssetsSH[1:5,1:5]

rownames(AssetsSH) <- rownames(yBM)
rownames(AssetsSN) <- rownames(yBM)
rownames(AssetsSL) <- rownames(yBM)
rownames(AssetsBH) <- rownames(yBM)
rownames(AssetsBN) <- rownames(yBM)
rownames(AssetsBL) <- rownames(yBM)

# ...........................

interactPortfolios <- function (x, y) {
        # Criando tabela
        tabela <- x
        for (i in 1:nrow(tabela)) {
                tabela[i,] <- as.logical(x[i,] * y[i,])
        }
        return(tabela)
}

C <- A[c(1,2,1,3,5),c(1,2,3,4,5,1)]
B
D <- B*C
apply(D, 2, function(x) as.logical(x) )

# ........ Procurar no stack overflow como transformar data.frame 1 o em logico

# HML = 1/2 (Small Value + Big Value) - 1/2 (Small Growth + Big Growth)
FactorHML <- 1/2*(AssetsSH)

# SMB = 1/3 (Small Value + Small Neutral + Small Growth)
#       - 1/3 (Big Value + Big Neutral + Big Growth)
PortfolioSMB <- 
        PortfolioHML
debug(portfolioSerie)
serieSmallValue <- portfolioSerie(mReturns, mMarketValue,AssetsSH)
warnings()
head(AssetsSH[,1:5])
head(mReturns[,1:5])
tail(mReturns[,1:5])
tail(AssetsSH[,1:5])

serieSmallValue <- portfolioSerie(mReturns, mMarketValue,AssetsSH)

seriePortBM1 <- portfolioSerie(mReturns, mMarketValue, portfolioAssets2(yBM,5,1))

# TESTE INDICE
LAG <- 12
summary(lm(seriePortBM1$rWV[(1+LAG):156]  ~ PCAstep3$x[,"PC1"][1:(156-LAG)]))


length(seriePortBM1$rWV[13:156])
length(PCAstep3$x[,"PC1"][1:144])


# _____________________________________________________________________________
# TESTE UTILIZANDO DADOS REAIS

nAtivos <- 1108

precins  <- mPrices[12:36,1:nAtivos]
retornin <- diff(log(as.matrix(precins)))
valorzin <- mMarketValue[13:36,1:nAtivos]
criterin <- yBookFirm[1:2,1:nAtivos]
criterin <- yBookFirm[1:2,1:nAtivos] / valorzin[c(12,24),]

#portfolioAssets2(criterin,3,1)
#portfolioAssets2(criterin,3,3)
#portfolioSerie(retornin, valorzin, portfolioAssets2(criterin,5,1))

#rm(list=c("precins", "retornin", "criterin", "valorzin", "nAtivos"))

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

## PRICING MODEL ## ###########################################################
## 3. Fatores de Risco
## 3.1 Fator de Mercado
## 3.2 Construir Carteiras
## 3.3 Interagir Carteiras
## 3.4 Retorno das Carteiras Ponderado pelo Valor


## 4. INVESTOR SENTIMENT AND ANOMALIES ## #####################################
## Sentimento do Investidor e Anomalias
## 4.1. Análise das Médias após períodos de Sentimento Alto e Baixo
## 4.2. Modelos Econométricos
## 4.1 Extremos e sentimento defasado
## 4.2 Extremos, sentimeto defasado e fatores de risco
## 4.3 Extremos, dummys

# === Análise de Médias === ===================================================

# === Predictive Regressions === ==============================================

# Sent.Long.Beta   <- lm(Long.Beta  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Beta  <- lm(Short.Beta ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Beta  <- lm(Long.Beta  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Beta <- lm(Short.Beta ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# 
# Sent.Long.Size   <- lm(Long.Size  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Size  <- lm(Short.Size ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Size  <- lm(Long.Size  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Size <- lm(Short.Size ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# 
# Sent.Long.BM   <- lm(Long.BM  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.BM  <- lm(Short.BM ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.BM  <- lm(Long.BM  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.BM <- lm(Short.BM ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# 
# Sent.Long.Liquidity   <- lm(Long.Liquidity  ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Sent.Short.Liquidity  <- lm(Short.Liquidity ~ SENT[_n-1]+MKT+SMB+HML+MOM+LIQ)
# Dummy.Long.Liquidity  <- lm(Long.Liquidity  ~ dH+dL+MKT+SMB+HML+MOM+LIQ)
# Dummy.Short.Liquidity <- lm(Short.Liquidity ~ dH+dL+MKT+SMB+HML+MOM+LIQ)