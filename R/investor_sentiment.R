# invSent ## ##################################################################
# 
# Title: Investor Sentiment and Anomalies in Brazilian Market
#
# Version: 0.0.1
#
# Description: Script to compute que Investor Sentiment Index of the brazilian
# market.                       
# 
# investor_sentiment_and_anomalies.R
#

## SETTINGS ## ################################################################
## Setting Parameters
## Definindo Parametros

START <- as.Date("2001-01-01") # Initial Date
END   <- as.Date("2013-12-31") # Final Date

## GETTING DATA AND CLEANING ## ###############################################
## Get Data and Clean
## Carregar e limpar dados

# Functions == ================================================================

importaBaseCSV <- function(arquivo, doDia, ateDia,
                           formato="%d/%m/%Y", pula_linha=0, financeiras=F) {
    
    # Function to import Brazilian Data
    # Funcao para carregar dados Economatica
    
    # Importanto matriz de precos mensais
    tabela <- read.table (arquivo, header = T, sep=";", dec=",",
                          row.names=1, skip=pula_linha,
                          na.strings="-", stringsAsFactors=F)
    
    # Retirando as empresas financeiras (coluna 1109(ABCB11) a 1224)
    if (financeiras == F ) { tabela     <- tabela[c(-1109:-1224)] }
    
    # TODO: Fazer a seleção de empresas financeiras e nao financeiras
    # automatica.
    
    # Filtrando a data em matriz mensal
    tabela <-  tabela[(as.Date(rownames(tabela), format=formato) >= doDia
                       &
                           as.Date(rownames(tabela), format=formato) <= ateDia)
                      ,]
    return(tabela)
}

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
    
    return(matriz_indice)
}

filter24months <- function (sample1, prices) {
    # Function to filter the stocks that have 24 months of consecutive prices
    newSample <- sample1
    for ( j in seq_len(ncol(prices)) ) {
        # Fazer essa rotina para coluna j
        for ( i in seq_len(nrow(prices)) ) {
            # Fazer essa rotina para cada linha i da coluna j
            # Verificar se a linha i est? entre START+1 e END-1
            if ( i<=12 ) {
                newSample[as.numeric(dateIndex$nY[i]),j] <- 0
            }
            else if( i>floor(nrow(dateIndex)/12)*12 ) {
                # nao faz nada
            }
            # Verificar se a linha i corresponde ao mes de junho
            else if ( as.numeric(dateIndex$M[i])==6 ) {
                # Verifica se tem preÃ§o nos 24 meses consecutivos
                if (sum(!is.na(prices[(i-12):(i+12),j])) != 25) {
                    # E atribui 0 na matriz de controle da amostra
                    newSample[as.numeric(dateIndex$nY[i]),j] <- 0
                }
            }
        }
    }
    return(newSample)
}

# Read Data == ================================================================

# Stock Prices
mPrices     <- importaBaseCSV("Input/mPrices.csv", START, END)

# Initial Sample == ===========================================================

# Initial Sample (1 para todos os anos que houve cotacao)
ySample0 <- importaBaseCSV("Input/ySample0.csv", START, END, formato="%Y")

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

# Final Sample == =============================================================

# Compute all the filters together
ySample <- ySample24m * ySampleNegociab * ySamplePositiveBook

## Generate Yearly Sample Control Matrix
mSample <- ySample[sort(rep(1:nrow(ySample),12)),] # repeat 12 times the values
# Add rows to the last incomplete year
mSample <- rbind(mSample, mSample[rep(nrow(mSample),
                                      nrow(mPrices)-nrow(mSample)), ])
row.names(mSample) <- row.names(mPrices) # Set name of the rows equal mPrices

# Results of Sample == ========================================================
rowSums(ySample0)[-1]            # Initial Sample

rowSums(ySample24m)[-1]          # Just the firms with 24 months of price
round(rowSums(ySample24m)[-1]/rowSums(ySample0)[-1],2)          # %

rowSums(ySamplePositiveBook)[-1] # Just the Positive book
round(rowSums(ySamplePositiveBook)[-1]/rowSums(ySample0)[-1],2) # %

rowSums(ySampleNegociab)[-1]     # Just the most liquid
round(rowSums(ySampleNegociab)[-1]/rowSums(ySample0)[-1],2)     # %

rowSums(ySample)[-1]             # Final Sample
round(rowSums(ySample)[-1]/rowSums(ySample0)[-1],2)             # %

# The first year was not computed because the 24 months filter

# OK Negociability but not OK 24 months
rowSums(ySampleNegociab)[-1]-rowSums(ySample24m*ySampleNegociab)[-1]

# OK 24 months but not OK Negociability
rowSums(ySample24m)[-1]-rowSums(ySample24m*ySampleNegociab)[-1]

# -----------------------------------------------------------------------------

## INVESTOR SENTIMENT ## ######################################################
## 1. Índice de Sentimento
# 1.1. Temporalidade das Proxies: Selecionar proxies que serão defasadas
# 1.2. Índice de Sentimento não Ortogonalizado
# 1.3. Índice de Sentimento Ortogonalizado à variáveis macroeconômicas  
# _____________________________________________________________________________

# Investor Sentiment Index #####################################################

########## Carregando matriz de proxies
mProxies   <- read.table ("Input/mProxies.csv",   header = T, 
                          sep=";", dec=",", na.strings="-", row.names=1)

# Filtrando a data em matriz mensal
mProxies <-  mProxies[(as.Date(rownames(mProxies), format="%d/%m/%Y") >= START
                       &
                               as.Date(rownames(mProxies), format="%d/%m/%Y") <= END ),]

# Verificando correla??o entre as proxies
as.dist(round(cor(mProxies),2))

# _____________________________________________________________________________
## COLETA DIRETO DO SITE DA CVM EM DESENVOLVIMENTO
##
# source("MyFuns/coletaDEBnaCVM.R")
# source("MyFuns/coletaIPOnaCVM.R")
# source("MyFuns/coletaVariosAnosCVM.R")
#
## Baixando dados de IPO
#IPOs <- coletaVariosAnosCVM(1999:2013, coletaIPOnaCVM)
## Baixando dados de emissão de dívidas
#DEBs <- coletaVariosAnosCVM(1999:2013, coletaDEBnaCVM)
## Guardando dados em formato txt e csv
#write.table(IPOs, "Output/IPOs.txt", sep="\t", row.names = F)
#write.table(DEBs, "Output/DEBs.txt", sep="\t", row.names = F)
#write.table(IPOs, "Output/IPOs.csv", sep=";", row.names = F)
#write.table(DEBs, file="Output/DEBs.csv", sep=";", row.names = F)
## write.csv(IPOs, file="Output/IPOsCSV.csv", row.names = F)
## write.csv(DEBs, file="Output/DEBsCSV.csv", row.names = F)
## IPO2 <- read.table("Output/IPOs.txt", sep="\t", skip=1, col.names=c("data","empresa","tipo","valor"))
## identical(IPOs, IPO2)
#
## --- FALTA AGORA TRATAR OS DADOS ---
# transformaDecimal <- function (vetor) {
#        vetor <- gsub(".", "",  vetor, fixed=T)
#        vetor <- gsub(",", ".", vetor, fixed=T)
#        return(vetor)
# }
# transformaDecimal(IPOs$valor)
# _____________________________________________________________________________


########## Sentiment - ACP Step 1

# Estimando primeira componente da primeira etapa
step1.pca <- prcomp(mProxies, scale=T)

# Funcao para excluir menor correlacao
escolheDefasagem <- function (m) {
        nproxies <- ncol(m)
        i <- 1
        delete <- 0
        for ( i in 1:(nproxies/2) ) {
                proxy <- cor(step1.pca$x[,"PC1"],m)[i]
                proxy_lagged <- cor(step1.pca$x[,"PC1"],m)[i+(nproxies/2)]
                if ( abs(proxy) < abs(proxy_lagged) ) { delete <- c(delete,-1*i) }
                if ( abs(proxy) > abs(proxy_lagged) ) { delete <- c(delete,-1*(i+(nproxies/2))) }
        }
        delete <- delete[-1]
        return(m[delete])
}

# Escolhendo defasagem
round(cor(step1.pca$x[,"PC1"],mProxies),2)
mBestProxies <- escolheDefasagem(mProxies)

# Proxies escolhidas
colnames(mBestProxies)
round(cor(step1.pca$x[,"PC1"],mBestProxies),2)

# CORRELA??O DAS PROXIES APÓS A DEFASAGEM
as.dist(round(cor(mBestProxies),2))

########## Sentiment - ACP Step 2

# Estimando Componente principal da terceira etapa
step2.pca <-prcomp(mBestProxies, scale=T)
# Verificando correlacao com o primeiro indice
cor(step1.pca$x[,"PC1"],step2.pca$x[,"PC1"])

# Percentual explicado da variancia
summary(step2.pca)
# summary(princomp(mProxies, scores=T, cor=T)) # Metodo alternativo (deu igual)

# Equacao do Indice de Sentimento Nao Ortogonalizado
step2.pca$rotation[,"PC1"]

########## Sentiment - ACP Step 3 # Otogonalizado

########## Carregando matriz de proxies
mMacroeconomics   <- read.table ("Input/mMacroeconomics.csv",   header = T, 
                                 sep=";", dec=",", na.strings="-", row.names=1)

# Filtrando a data em matriz mensal
mMacroeconomics <-  mMacroeconomics[(
        as.Date(rownames(mMacroeconomics), format="%d/%m/%Y") >= START 
        &
                as.Date(rownames(mMacroeconomics), format="%d/%m/%Y") <= as.Date("2013-12-01")
        #as.Date(rownames(mMacroeconomics), format="%d/%m/%Y") <= END
),]

END   <- as.Date("2014-07-01") # ????????????????????

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
step3.pca <-prcomp(mProxiesOrtog, scale=T)

# Estimando Componentes Principais da Terceira Etapa
step3.pca <-prcomp(mProxiesOrtog, scale=T)
# step3.pca <- princomp(mProxiesOrtog, scores=T, cor=T) # Metodo alternativo

# Verificando correlacao com o primeiro indice
cor(step2.pca$x[,"PC1"],step3.pca$x[,"PC1"])

# Percentual explicado da variancia
summary(step3.pca)
# summary(princomp(mProxiesOrtog, scores=T, cor=T)) # Metodo alternativo

# Scree plot of eigenvalues
# screeplot(step3.pca, type="line", main="Scree Plot Sentimento Ortogonalizado")

# Equacao do Indice de Sentimento Ortogonalizado
step3.pca$rotation[,"PC1"]

# Sent <- step3.pca$x[,"PC1"]

# RESULTADOS ------------------------------------------------------------------------------------------------------
as.dist(round(cor(mProxies),2))                       # Verificando correlação entre as proxies
round(cor(step1.pca$x[,"PC1"],mProxies),2)            # Correlação das Proxies com 1ª Componente da 1ª Etapa
round(cor(step1.pca$x[,"PC1"],mBestProxies),2)        # Correlação Proxies Escolhidas c/ 1ª Componente da 1ª Etapa
cor(step1.pca$x[,"PC1"],step2.pca$x[,"PC1"]) * (-1)   # Verificando correlacao com o primeiro indice
summary(step2.pca)                                    # Percentual explicado da variancia
step2.pca$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Nao Ortogonalizado
as.dist(round(cor(mBestProxies),2))                   # Correlação Proxies Escolhidas
round(cor(step2.pca$x[,"PC1"],mBestProxies),2) * (-1) # Correlação Proxies Escolhidas c/ 1ª Componente da 2ª Etapa
cor(step2.pca$x[,"PC1"],step3.pca$x[,"PC1"])          # Correlação do Indice da 3ª etapa com o da 2ª etapa
summary(step3.pca)                                    # Percentual explicado da variancia
step3.pca$rotation[,"PC1"] * (-1)                     # Equacao do Indice de Sentimento Ortogonalizado


# Scree plot of eigenvalues
screeplot(step3.pca, type="line", main="Scree Plot Sentimento Ortogonalizado")

# 1-investor_sentiment.R == ===================================================

# Depois colocar em um arquivo investor_sentiment.R

InvestorSentimentStep1 <- function (sentimentProxies, nLag) {
        # Estima Indice com todas as proxies
        # retorna (indice0)
}

ChooseBestProxies <- function (prx, nLag) {
        # prx:  Proxies para o Sentimento do Investidor
        # nLag: Quantidade de periodos da defasagem
        # Estima sentimento 1
        # IMPRIME: correlations
        # IMPRIME: correlations with 1st componente
        # IMPRIME: bestproxies
        # IMPRIME: correlations
        # RETORNA: Série de Melhores Proxies
}

InvestorSentimentStep2 <- function (bestProxies, invSent1) {
        # Estima
        # Apresenta correlação com o primeiro
        # Apresenta percentual de variancia explicadas
        # retorna (indice1)
}

InvestorSentimentStep3 <- function (bestProxies, macroVariables, invSent2) {
        # Estima
        # Apresenta correlação com o segundo
        # Apresenta percentual de variancia explicada
        # retorna (indice2)
}


test <- function (...) {
        print(x)
        print(sum(...))
}
test(1,1,2,3,4,5)

## PRICING MODEL ## ###########################################################
## 3. Fatores de Risco
# 3.1 Fator de Mercado
# 3.2 Construir Carteiras
# 3.3 Interagir Carteiras
# 3.4 Retorno das Carteiras Ponderado pelo Valor

## CONSTRUCT PORTFOLIOS ## ####################################################

# 2-portfolio_construction.R == ===============================================
## 2. Portfolios
# 2.1 Construir Carteiras
#       portfolioAssets cria_matriz_carteira - retorna dCriterio
# 2.2 Interação de Carteiras
#       portfolioAssetesInteracao = portfolioAssets1 x portfolioAssets2
# 2.3 Retorno das Carteiras
#       portfolioSerie - retorna ...

SelectStockBaskets <- function (strategy, nPortfolios, iPortfolio) {
        
}
constructPortfolio <- function (strategy, nPortfolios, iPortfolio) {
        
}
PortfolioSerie     <- function (SelectedStocks, Returns, MarketValues) {
        # Retorna Serie de determinado portfolio
}
LongShortSeries    <- function (strategy, nPortfolios, RET, MV) {
        cat("rLong, mvLong, rShort, mvShort")
}

# ---

# Retorna os valores máximos e mínimos para formação de um portfolio
portfolioRange <- function(CRITERIO, nPortfolios=5, portfolio=1) {
        # Cria vetor de sequencia
        x <- c(0,seq(1:nPortfolios)/nPortfolios)
        # Salvando o valor maximo e minimo
        RANGE <- quantile(CRITERIO, x[portfolio:(portfolio+1)], na.rm=T)
        # Retorna a faixa de valor do portfolio escolhido
        return(RANGE)
}

# ---

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

# ---

portfolioAssets <- function(CRITERIO, nPortfolios=5, portfolio=1) {
        
        #-----------------------------------------
        # CRITERIO .... Vetor de criterio
        # nPortfolios . Número de portfolios
        # iPortfolio .. Portfolio desejado
        #-----------------------------------------
        
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

# ---

portfolioSerie <- function (RETURN, MV, PortfolioAssets) {
        
        # INPUT
        # ______________________________________________________________
        #
        # RETURN ...... Matriz de Retornos
        # MV .......... Matriz com os Valores de Mercado
        # PortfolioAssets ... Matriz de ativos pertecentes ao Portfolio
        # ______________________________________________________________
        
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

# Calculando matriz de retornos
mPrices     <- importaBaseCSV("Input/mPrices.csv", (START-31), END, formato="%d/%m/%Y")
mReturns    <- as.data.frame(diff(log(as.matrix(mPrices))))
mPrices     <- importaBaseCSV("Input/mPrices.csv", START, END, formato="%d/%m/%Y")

# Importando valores de valor de Mercado
mMarketValue     <- importaBaseCSV("Input/mMarketValue.csv",
                                   START,
                                   END,
                                   formato="%d/%m/%Y",
                                   pula_linha=1)

# Transformando matriz mensal em anual
yMarketValue <- mMarketValue[dateIndex$M==12,]

# Calcular BM
yBM <- yBookFirm / yMarketValue

##Teste Range
portfolioRange(yMarketValue[1,],2,1)
portfolioRange(yBM[1,],3,1) # Verificar o -Inf e o +Inf

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
summary(lm(seriePortBM1$rWV[(1+LAG):156]  ~ step3.pca$x[,"PC1"][1:(156-LAG)]))


length(seriePortBM1$rWV[13:156])
length(step3.pca$x[,"PC1"][1:144])


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

## INVESTOR SENTIMENT AND ANOMALIES ## #########################################
# Sentimento do Investidor e Anomalias
# 1. Análise das Médias após períodos de Sentimento Alto e Baixo
# 2. Modelos Econométricos
# 2.1 Extremos e sentimento defasado
# 2.2 Extremos, sentimeto defasado e fatores de risco
# 2.3 Extremos, dummys
#

# Análise de Médias == =========================================================

# Predictive Regressions == ====================================================

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