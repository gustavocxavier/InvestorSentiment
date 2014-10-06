# My Functions
#
# 1 Data Functions
# 2 Sentiment Functions
# 3 Portfolio Functions 

#   1.2.1 Data Functions - ----------------------------------------------------

importaBaseCSV <- function(arquivo, periodo, formato="%d/%m/%Y", ignora=0) {
    
    # Descricao: Funcao para importar base Economatica apos salvo como CSV
    #
    # Argumentos:
    # arquivo ...... Nome do arquivo CSV a ser importado
    # periodo ...... Periodo escolhido. Ex.: "2000-06/2014-07"
    # formato ...... Formato da data, para "01/01/2000" utilize "%d/%m%Y"
    # ignora ....... Quantidade de linhas que serao ignoradas
    #
    
    require(xts)

    tabela <- read.table (arquivo, header = T, sep=";", dec=",", row.names=1,
                          skip=ignora, na.strings="-", stringsAsFactors=F)
    
    if ( formato=="%Y" ) {
        data_completa <- paste(rownames(tabela),"-12-01", sep="")
        row.names(tabela) <- as.Date(data_completa, "%Y-%m-%d")
    } else {
        row.names(tabela) <- as.Date(row.names(tabela), format=formato)
    }
    
    # Filtrando Periodo
    tempXTS <- as.xts(tabela)[PERIOD.XTS]
    tabela  <- data.frame(as.matrix(tempXTS))
    
    return(tabela)
}

initialSample <-  function (rawPrices) {
    
    ## Funçao que retorna matriz anual de amostra inicial a partir de dados
    ## brutos de precos.
    
    require(xts)
    Out <- as.matrix(rawPrices)
    Out[!is.na(rawPrices)] <- 1 # Atribuir valor 1 aos nao NA
    Out[is.na(rawPrices)   ] <- 0 # Atribuir valor 0 aos NA
    
    Out <- as.matrix(apply.yearly(as.xts(Out), mean)) # Calcular Media ou
    # percentual c/ cotacao no
    # ano.
    
    Out[Out>0]             <- 1 # Media anual(%) maior do que zero teve cotacao
    
    OutNames <- rownames(Out) # Salvar nome das linhas
    Out <- apply(Out, 2, as.logical) # Transformar matriz em logica
    rownames(Out) <- OutNames # Recuperar nomes de linhas salvos
    
    as.data.frame(Out) # Retornar matriz lógica como um data.frame
}

filterNoFinancial <- function(Sample, dbFile) {

    ## Exclui as empresas financeiras
    ##
    ## Sample ...... Matriz de Amostra
    ## dbFile ...... Arquivo onde tem a relacao de empresas

    db <- read.table(dbFile, dec=",", sep=";", header = T, na.strings="-",
                     stringsAsFactors=F)
    
    Out <- as.matrix(Sample) # Transforma em matriz por ser mais rapido
    Out[,(db$SetorEconomatica=="Financas e Seguros")] <- FALSE
    
    as.data.frame(Out)
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

filterNo24months <- function(monthlyPrices, InitialSample) {
    
    ## Filtra amostra deixando apenas as empresas que apresentarem precos
    ## durante 24 meses consecutivos, sendo considerado os 12 meses antes e 12
    ## depois do dia da formacao das carteiras, que ocorre em 30/jun.
    ##
    ## monthlyPrices ......... Matriz de precos mensais
    ## InitialSample ......... Matriz de Amostra Inicial
    ##
    ## Para calculo do Fator Momento: precos  de jun/(n-1):mai/(n  ) (12 meses)
    ##                                retorno de jul/(n-1):mai/(n  ) (11 meses)
    ##
    ## Para calculo do das Carteiras: precos  de jun/(n  ):jun/(n+1) (13 meses)
    ##                                retorno de jul/(n  ):jun/(n+1) (12 meses)
    ##
    
    require(xts)
    
    prices <- as.xts(monthlyPrices)
    n <- as.numeric(format(first(index(prices)),"%Y")) # First year
    N <- as.numeric(format( last(index(prices)),"%Y")) # Last year
    
    Out <- as.matrix(InitialSample)
    Out[(n - (n - 1)),] <- FALSE # First year equal zero
    for ( i in (n+1):(N-1) ) {
        p <- paste(i-1,"-06/",i+1,"-07", sep="") # Periodo de Interesse
        Out[(i+1 - n),] <- !apply(prices[p], 2, anyNA)
    }
    Out[(N+1 - n),] <- FALSE # Last year equal zero

    as.data.frame(Out) # Retornar matriz lógica como um data.frame
}

#   1.2.2 Sentiment Functions - -----------------------------------------------

coletaDEBnaCVM <- function (ano) {
    require(XML)
    # COLETANDO DEBENTURES
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/deb.asp?Ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo=pagina[[i]][j,3],
                                         valor=pagina[[i]][j,4],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             pagina[[i]][j,3],
                                             pagina[[i]][j,4]))
                }
            }
        }
    }
    # COLETANDO IPO SECUNDARIAS
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/nota.asp?Ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo="NP",
                                         valor=pagina[[i]][j,3],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             "NP",
                                             pagina[[i]][j,3]))
                }
            }
        }
    }
    if ( exists("tabela") ) {
        tabela <- tabela[ !is.na(tabela$valor) ,]
        tabela <- tabela[ !as.logical(sapply(tabela[,2], FUN=pmatch, x="TOTAL", nomatch=0)) ,]
        row.names(tabela) <- seq(1:nrow(tabela))
        return(tabela)
    } else { cat(paste("Ano de ",ano ," sem informação.\n")) }
}

coletaIPOnaCVM <- function (ano) {
    require(XML)
    # COLETANDO IPO PRIMARIAS
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/acoes.asp?ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo=pagina[[i]][j,3],
                                         valor=pagina[[i]][j,4],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             pagina[[i]][j,3],
                                             pagina[[i]][j,4]))
                }
            }
        }
    }
    # COLETANDO IPO SECUNDARIAS
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/secnd3.asp?grp_emis=1&ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo="SEC",
                                         valor=pagina[[i]][j,3],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             "SEC",
                                             pagina[[i]][j,3]))
                }
            }
        }
    }
    if ( exists("tabela") ) {
        tabela <- tabela[ !is.na(tabela$valor) ,]
        tabela <- tabela[ !as.logical(sapply(tabela[,2], FUN=pmatch, x="TOTAL", nomatch=0)) ,]
        row.names(tabela) <- seq(1:nrow(tabela))
        return(tabela)
    } else { cat(paste("Ano de ",ano ," sem informação.\n")) }
}

coletaSubsequentesnaCVM <- function (ano) {
    require(XML)
    # COLETANDO OFERTAS SUBSEQUENTES PRIMARIAS
    # http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/acoes3.asp?ano=2013
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/acoes3.asp?ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo=pagina[[i]][j,3],
                                         valor=pagina[[i]][j,4],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             pagina[[i]][j,3],
                                             pagina[[i]][j,4]))
                }
            }
        }
    }
    # COLETANDO OFERTAS SUBSEQUENTES SECUNDARIAS
    url <- "http://www.cvm.gov.br/asp/cvmwww/registro/ofertasreg2/secnd4.asp?grp_emis=1&ano="
    url <- paste(url, ano, sep="")
    pagina <- readHTMLTable(url, stringsAsFactors = F)
    numero_linhas <- length(attributes(pagina)[[1]])
    if ( numero_linhas > 1 ) {
        for (i in 2:numero_linhas) {
            sub_linhas <- length(pagina[[i]][,1])
            for ( j in 1:sub_linhas) {
                if ( !exists("tabela") ) {
                    # SE FOR A TABELA NAO EXISTE, CRIA
                    tabela <- data.frame(data=pagina[[i]][j,2],
                                         empresa=pagina[[i]][j,1],
                                         tipo="SEC",
                                         valor=pagina[[i]][j,3],
                                         stringsAsFactors=F)
                } else { # SE EXISTE, APENAS ADICIONAR LINHAS
                    tabela <- rbind(tabela,c(pagina[[i]][j,2],
                                             pagina[[i]][j,1],
                                             "SEC",
                                             pagina[[i]][j,3]))
                }
            }
        }
    }
    if ( exists("tabela") ) {
        tabela <- tabela[ !is.na(tabela$valor) ,]
        tabela <- tabela[ !as.logical(sapply(tabela[,2], FUN=pmatch, x="TOTAL", nomatch=0)) ,]
        row.names(tabela) <- seq(1:nrow(tabela))
        return(tabela)
    } else { cat(paste("Ano de ",ano ," sem informação.\n")) }
}

coletaVariosAnosCVM <- function(anos, funcao) {
    cat("Baixando dados direto do site da CVM.\n")
    variosAnos <- funcao(anos[1])
    cat(paste(anos[1], "OK\n"))
    for (n in anos[2:length(anos)]) {
        variosAnos <- rbind(variosAnos, funcao(n))
        cat(paste(n, "OK\n"))
        #Sys.sleep(3)
    }
    cat("FIM")
    return(variosAnos)
}

cleanString <- function(x){
    tmp <- iconv(x, from="UTF8", to ="ASCII//TRANSLIT")
    gsub("[^[:alpha:]]", " ", tmp)
}


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
    ## V     ... Variavel de Interesse(criterio/caracteristica/estrategia).
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
    
    P1=c(mean(P1R$rVW, na.rm=T), sd(P1R$rVW, na.rm=T))*100
    P2=c(mean(P2R$rVW, na.rm=T), sd(P2R$rVW, na.rm=T))*100
    P3=c(mean(P3R$rVW, na.rm=T), sd(P3R$rVW, na.rm=T))*100
    P4=c(mean(P4R$rVW, na.rm=T), sd(P4R$rVW, na.rm=T))*100
    P5=c(mean(P5R$rVW, na.rm=T), sd(P5R$rVW, na.rm=T))*100
    
    P1=c(P1,c(mean(P1R$rEW, na.rm=T), sd(P1R$rEW, na.rm=T))*100)
    P2=c(P2,c(mean(P2R$rEW, na.rm=T), sd(P2R$rEW, na.rm=T))*100)
    P3=c(P3,c(mean(P3R$rEW, na.rm=T), sd(P3R$rEW, na.rm=T))*100)
    P4=c(P4,c(mean(P4R$rEW, na.rm=T), sd(P4R$rEW, na.rm=T))*100)
    P5=c(P5,c(mean(P5R$rEW, na.rm=T), sd(P5R$rEW, na.rm=T))*100)
    
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
        # yData[nrow(yData),] <- NA
    } else if ( LAG == 1 ) {
        # Atribui NA em todos os valores yData em n-1 qnd Sample em n for FALSE
        yData[(Sample[-1,]==F)] <- NA
    } else if ( LAG != 0 & 1 ) { print("Valores validos para LAG sao 1 ou 0")}
    return(yData)
}

# constructPortfolio <- function (strategy, nPortfolios, iPortfolio) {}
# rebalancedPortfolios <- function ()
# SelectStockBaskets <- function (strategy, nPort, iPort) {}