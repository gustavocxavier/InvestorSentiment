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

portfolioSerie  <- function (Returns, Values, Assets) {
    
    # INPUT
    # _________________________________________________________________
    #
    # Return .... Matriz de Retornos (Returns)
    # Values .... Matriz com os Valores de Mercado (MarketValues)
    # Assets .... Matriz de ativos pertecentes ao Portfolio (SelectedStocks)
    # _________________________________________________________________
    
    require(lubridate)
    
    M <- as.numeric(substr(rownames(Returns),6,7)) # meses
    # M <- month(as.Date(rownames(Returns))) # c/ lubridate (+ elegante)
    n <- (seq(M))[M==7][2]                         # segundo JULHO
    N <- sort(seq(M)[M==6], decreasing = T)[2]     # penultimo JUNHO
    
    rDate <- as.Date(rownames(Returns[n:(N-6),]))
    aDate <- as.Date(rownames(Assets))
    
    # Verificar se utiliza ano presente, ou dados do ano anterior
    if ( month(aDate)[1] < 7 ) { # utilizar n
        assetYears <- year(aDate) %between% (year(rDate)) 
    } else {                     # utilizar n-1
        assetYears <- year(aDate) %between% (year(rDate)-1) 
    }
    
    Returns <- Returns[n:N,] # Apenas meses de JULHO/ano1 a JUNHO/ano
    Values <- Values[n:N,] # Apenas meses de JULHO/ano1 a JUNHO/ano
    
    Assets <- Assets[assetYears, ] # Apenas os anos utilizados para o calculo
    
    # Deixando matrizes de ativos e de retornos do mesmo tamnho
    RepeatAssets <- nrow(Returns)/nrow(Assets)
    Assets <- Assets[rep(seq_len(nrow(Assets)), each=RepeatAssets),]
    
    # Transpondo matrizes e tansfromando em data.frame p/ usar as func. apply
    A <- as.data.frame(t(Assets))
    R <- as.data.frame(t(Returns))
    V <- as.data.frame(t(Values))
    
    rEW <- mapply( function(x,y) mean(x[y]), R, A )
    rVW <- mapply( function(x,y,z) { sum(x[z]*y[z]/sum(y[z])) }, R, V, A )
    MV  <- mapply( function(x,y) { sum(x[y]) }, V, A )
    nA  <- sapply( A, sum)
    
    as.data.frame(cbind(rEW, rVW, MV, nA))
        
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
}

allQuintiles <- function (Criterion, Return, Value) {
    
    ## ______________________________________________________________
    ##
    ## Imprime Média e Retorna a Série de todos os Portfolios
    ##
    ## ARGUMENTS:
    ## Criterion ... Variavel de Interesse(criterio/caracteristica).
    ## Return ...... Matriz de Retornos
    ## Value ....... Matriz com os Valores de Mercado
    ## ______________________________________________________________
    P1A <- portfolioSelectAssets(Criterion, 5, 1)
    P2A <- portfolioSelectAssets(Criterion, 5, 2) 
    P3A <- portfolioSelectAssets(Criterion, 5, 3)
    P4A <- portfolioSelectAssets(Criterion, 5, 4)
    P5A <- portfolioSelectAssets(Criterion, 5, 5)
    
    P1R <- portfolioSerie(Return, Value, P1A)
    P2R <- portfolioSerie(Return, Value, P2A)
    P3R <- portfolioSerie(Return, Value, P3A)
    P4R <- portfolioSerie(Return, Value, P4A)
    P5R <- portfolioSerie(Return, Value, P5A)
    
    P1=c(mean(P1R$rVW), sd(P1R$rVW), mean(P1R$rEW), sd(P1R$rEW))*100
    P2=c(mean(P2R$rVW), sd(P2R$rVW), mean(P2R$rEW), sd(P2R$rEW))*100
    P3=c(mean(P3R$rVW), sd(P3R$rVW), mean(P3R$rEW), sd(P3R$rEW))*100
    P4=c(mean(P4R$rVW), sd(P4R$rVW), mean(P4R$rEW), sd(P4R$rEW))*100
    P5=c(mean(P5R$rVW), sd(P5R$rVW), mean(P5R$rEW), sd(P5R$rEW))*100
    
    data.frame(cbind(P1,P2,P3,P4,P5),
               row.names=c("VW r", "VW sd", "EW r", "EW sd"))
}

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