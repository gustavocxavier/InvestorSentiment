# My Functions
#
# 1 Data Functions
# 2 Sentiment Functions
# 3 Portfolio Functions 

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
    ## Filtrar apenas ações com patrimonio liquido positivo em n-1
    ##
    for ( i in 2:nrow(Sample)) {
        Sample[i,][( Book[(i-1),]<=0 | is.na(Book[(i-1),]) )] <- 0
    }
 
    return(Sample)
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
    
    V <- as.matrix(V)
    
    # Criando matriz Out igual a matriz V porem com todos valores FALSE
    Out <- apply(V, 2, function (x) x <- rep(FALSE, length(x)))
    rownames(Out)=rownames(V)
    Report <- data.frame(row.names=c("MIN", "MAX"))
        
    for ( i in 1:nrow(V)) {
        x <- c(0,seq(1:nPort)/nPort) # Sequencia de todos os quantis
        RANGE <- quantile(V[i,], x[iPort:(iPort+1)], na.rm=T) # Valor max e min
        Report <- (cbind(Report, RANGE)) # Adicionar o RANGE ao relatorio
        if ( !is.na(RANGE[1]) ) { # Verifica "RANGE = valores validos"
            # Valores no RANGE, atribuir TRUE. Fora do RANGE, atribui FALSE
            Out[i,] <- V[i,] %between% RANGE
            Out[i,][is.na(Out[i,])] <- FALSE # Valores NA, atribuir FALSE
        }
    }
    
    if ( report == T ) {
        colnames(Report) <- substr(rownames(V), 1, 4)[1:ncol(Report)]
        Report <- rbind(Report,QTD=rowSums(Out))
        cat(paste(iPort,"º portfolio dos ", nPort,".\n", sep=""))
        print(t(as.matrix(Report)))
    }
    
    return(as.data.frame(Out))
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

cleanData <- function(yData, Sample, LAG=0) {
    if ( LAG == 0 ) {
        # Atribui NA em todos os valores yData em n qnd Sample em n for FALSE
        yData[(Sample==F)] <- NA
        # Atribui NA em todos os valores yData no ultimo ano
        yData[nrow(yData),] <- NA
    } else if ( LAG == 1 ) {
        # Atribui NA em todos os valores yData em n-1 qnd Sample em n for FALSE
        yData[-nrow(yData),][(Sample[-1,]==F)] <- NA
    } else if ( LAG != 0 & 1 ) { print("Valores validos para LAG sao 1 ou 0")}
    return(yData)
}


# constructPortfolio <- function (strategy, nPortfolios, iPortfolio) {}
# rebalancedPortfolios <- function ()
# SelectStockBaskets <- function (strategy, nPort, iPort) {}