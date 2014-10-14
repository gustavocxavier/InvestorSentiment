# My Functions
#
# 1 Data Functions
# 2 Sentiment Functions
# 3 Portfolio Functions 

#   1.2.1 Data Functions - ----------------------------------------------------

asLogical <- function (DF) {
    as.data.frame( lapply(DF, function (x) as.logical(x) ), row.names=rownames(DF))
}

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
    
    Out <- read.table (arquivo, header = T, sep=";", dec=",", row.names=1,
                       skip=ignora, na.strings="-", stringsAsFactors=F)
    
    if ( formato=="%Y" ) { # Series anuais é acrescida "-12-01" ao ano.
        data_completa <- paste(rownames(Out),"-12-01", sep="")
        row.names(Out) <- as.Date(data_completa, "%Y-%m-%d")
    } else { # Seires mensais é apenas transformado em data
        row.names(Out) <- as.Date(row.names(Out), format=formato)
    }
    
    # Transformar Out em xts, filtrar periodo e retornar como data.frame
    data.frame(as.matrix(as.xts(Out)[periodo]))
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

sampleReportAll <- function(...) {
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
    cbind(F1, F2, F3, F4, F5)
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

filterPortfReturns <- function(Returns, ano_inicial, ano_final) {
    ## Descrição: Retorna matriz de filtro com TRUE p/ ativos que tem retornos
    ## validos durante aquele ano.
    ##
    ## Para calculo do Fator Momento: precos  de jun/(n-1):mai/(n  ) (12 meses)
    ##                                retorno de jul/(n-1):mai/(n  ) (11 meses)
    ##
    
    require(xts)
    
    ## Carregando Matriz de Retorno como XTS apenas nas datas de interesse
    ## jul a jun a partir do segundo ano.
    n <- ano_inicial
    N <- ano_final
    R <- as.xts(Returns)[paste(n,"-07/",N,"-06", sep="")]
    
    ## Todo ano vai 
    ep  <- c(0,grep("-06-", index(R)))
    
    ## Aplica TRUE a todos os periodos em que a matriz de retorno nao tem NA
    Out <- period.apply(R, ep, function(x) !apply(x, 2, anyNA))
    years <- as.numeric(substr(index(Out),1,4)) - 1
    
    Out <- as.data.frame(Out, row.names=paste(years,"-06-01", sep=""))     
    
    # Adicionar o último ano
    Out <- rbind(Out, last_year = rep(FALSE, ncol(Out) ))
    rownames(Out)[nrow(Out)] <- paste(N,"-06-01", sep="")
    
    return(Out)
}

filterMonthMV <- function(MarketValues, ano_inicial, ano_final) {
    ## Descrição: Retorna matriz de filtro com TRUE p/ ativos que tem retornos
    ## validos durante aquele ano.
    ##
    ## Para calculo do Fator Momento: precos  de jun/(n-1):mai/(n  ) (12 meses)
    ##                                retorno de jul/(n-1):mai/(n  ) (11 meses)
    ##
    
    require(xts)
    
    ## Carregando Matriz de Retorno como XTS apenas nas datas de interesse
    ## jul a jun a partir do segundo ano.
    n <- ano_inicial
    N <- ano_final
    MV <- as.xts(MarketValues)[paste(n,"-07/",N,"-06", sep="")]
    
    ## Todo ano vai 
    ep  <- c(0,grep("-06-", index(MV)))
    
    ## Aplica TRUE a todos os periodos em que a matriz de retorno nao tem NA
    Out <- period.apply(MV, ep,
                        function(x) apply(x, 2, function (a) !any(a<0|is.na(a)))
                        )
                        
    years <- as.numeric(substr(index(Out),1,4)) - 1
    
    Out <- as.data.frame(Out, row.names=paste(years,"-06-01", sep=""))     
    
    # Adicionar o último ano
    Out <- rbind(Out, last_year = rep(FALSE, ncol(Out) ))
    rownames(Out)[nrow(Out)] <- paste(N,"-06-01", sep="")
    
    return(Out)
}

filterMOM <- function(Returns, InitialYear, FinalYear) {
    
    ## Descricao: TRUE p/ acoes comprecos disponiveis p/ calculo do momento
    ## Calcula o retorno da anomalia momento
    ## 
    ## Para calculo do Fator Momento: retorno de jul/(n-1):mai/(n  ) (11 meses)
    
    require(xts)
    
    R <- Returns
    n <- InitialYear # First year
    N <- FinalYear   # Last year
    
    ## Retira todo o junho
    row_names <- rownames(R)[grep("-06-", rownames(R) )]
    R <- R[-grep("-06-", rownames(R)),]
    ## Salvar todos os endpoints em Maio
    ep <- c(0,grep("-05-", rownames(R)))
    
    ## Verifica se tem algum NA de Jul/n-1 a Mai/n
    R <- as.xts(R)
    R <- period.apply(R, ep, function(x) !apply(x, 2, anyNA))
    Out <- as.data.frame(R)
    ## Criando uma linha NA referente ao primeiro ano p/ que a matriz
    ## possa ser utilizada em outras funcoes
    # ano_1 <- as.numeric(substr(rownames(Out)[1],1,4))-1
    Out <- rbind(first_year=rep(FALSE ,ncol(Out)), Out)
    rownames(Out) <- row_names

    ## Retorna a matriz de controle
    return(Out)
}

filterNA <- function(Variable, FirstYear, LastYear, Month) {
    
    ## Descricao: TRUE p/ acoes comprecos disponiveis p/ calculo do momento
    ## Calcula o retorno da anomalia momento
    ## 
    ## Para calculo do Fator Momento: retorno de jul/(n-1):mai/(n  ) (11 meses)
    
    require(xts)
    require(lubridate)
    
    V <- Variable  # Variavel de interesse
    n <- FirstYear # Primeiro ano da amostra
    N <- LastYear  # Segundo ano da amostra
    
    # Filtro de Tempo
    periodo <- paste(n,"/", N, sep="")
    V <- as.data.frame(as.xts(V)[periodo])
    
    row_names <- rownames(V)[month(rownames(V))==6]
    
    ## Retira todo o junho
    V <- V[months(as.Date(rownames(V)), abbreviate=T)==Month,]
    nMonth <- month(rownames(V))[1]
    
    
    Out <- as.data.frame( lapply(V, function(x) (!is.na(x)) ) )
    
    # Se o mes de referencia for apos junho adicionar uma linha
    if ( nMonth > 6 ) { Out <- rbind(firstY=rep(FALSE, ncol(Out)), Out) }
    
    # Nomear linhas com os meses de junho
    firstJun  <- as.Date(paste(n,"-06-01", sep=""))
    lastJun   <- as.Date(paste(N,"-06-01", sep="")) 
    rownames(Out) <- seq(firstJun, lastJun, by="year")
    
    ## Retorna a matriz de controle
    return(Out)
}

filterGreaterThan <- function(Variable, Value, FirstYear, LastYear, Month) {
    
    ## Descricao: TRUE p/ acoes comprecos disponiveis p/ calculo do momento
    ## Calcula o retorno da anomalia momento
    ## 
    ## Para calculo do Fator Momento: retorno de jul/(n-1):mai/(n  ) (11 meses)
    
    require(xts)
    require(lubridate)
    
    V <- Variable  # Variavel de interesse
    n <- FirstYear # Primeiro ano da amostra
    N <- LastYear  # Segundo ano da amostra
        
    # Filtro de Tempo
    periodo <- paste(n,"/", N, sep="")
    V <- as.data.frame(as.xts(V)[periodo])
    
    ## Retira todo o junho
    V <- V[months(as.Date(rownames(V)), abbreviate=T)==Month,]
    nMonth <- month(rownames(V))[1]
    
    # Verdadeiro para os valores que atendem a condição
    Out <- as.data.frame(lapply(V, function(x) (x >= Value)))
    
    # Falso para os valores NA
    Out[is.na(V)] <- FALSE
        
    # Se o mes de referencia for apos junho adicionar uma linha
    if ( nMonth > 6 ) { Out <- rbind(firstY=rep(FALSE, ncol(Out)), Out) }
    
    # Nomear linhas com os meses de junho
    firstJun  <- as.Date(paste(n,"-06-01", sep=""))
    lastJun   <- as.Date(paste(N,"-06-01", sep="")) 
    rownames(Out) <- seq(firstJun, lastJun, by="year")
    
    ## Retorna a matriz de controle
    return(Out)
}


computeMomentum <- function (monthlyReturns) {

    ## Calcula o retorno da anomalia momento
    ## 
    ## Para calculo do Fator Momento: retorno de jul/(n-1):mai/(n  ) (11 meses)
    
    require(xts)
    
    ## Retira todo o junho
    mMomentum <- monthlyReturns[-grep("-06-", rownames(monthlyReturns)),]
    ## Salvar todos os endpoints em Maio
    ep <- c(0,grep("-05-", rownames(mMomentum)))
    ## Somatorio de Jul/n-1 a Mai/n
    Out <- period.apply(mMomentum, ep, function(x) apply(x, 2, sum))
    ## Criando uma linha NA referente ao primeiro ano p/ que a matriz
    ## possa ser utilizada em outras funcoes
    ano_1 <- as.numeric(substr(rownames(Out)[1],1,4))-1
    Out <- rbind(first_may=rep(NA,ncol(Out)), Out)
    rownames(Out)[1] <- paste(ano_1,"-05-01", sep="")
    
    ## Retorna a matriz anual com os retornos acumulados
    return(Out)
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
    Out <- funcao(anos[1])
    cat(paste(anos[1], "OK\n"))
    for (n in anos[2:length(anos)]) {
        Out <- rbind(Out, funcao(n))
        cat(paste(n, "OK\n"))
        #Sys.sleep(3)
    }
    
    # Transformar dados de valor em decimeal (substitui virgula por ponto)
    Out$valor <- as.numeric(gsub(",", ".", gsub("\\.", "", Out$valor, fixed=F)))
    
    Out$data <- as.Date(Out$data, format="%d/%m/%Y") # Transformar Formato Datas
    Out <- Out[order(Out$data, Out$empresa),]        # Ordenar matriz
    rownames(Out) <- NULL                            # Ordem antiga, irrelevante
    
    Out$empresa0 <- Out$empresa              # Copia dos nomes das empresas coletada
    Out$empresa  <- cleanString(Out$empresa) # Tratar caracteris dos nomes das empresas
    
    cat("FIM")
    return(Out)
}

cleanString <- function(x){
    # Trata os dados brutos dos nomes das empresas
    
    x <- gsub("^\\** ", "", x)     # Retirar "** " no comeco
    x <- gsub("S\\.A\\.", "SA", x) # Substituir S.A. por SA
    x <- gsub("S.A", "SA", x)      # Substituir S.A e S/A por SA
    x <- gsub("\\-", "", x)        # Retirar todos os tracos
    x <- gsub("  ", " ", x)        # Retirar 2 espacos juntos
    
    # Substituir caracteris espec.
    tmp <- iconv(x, from="UTF8", to ="ASCII//TRANSLIT")
    x   <- gsub("[^[:alpha:]]", " ", tmp)
    
    x <- toupper(x)           # Colocar em letra maiuscula
    
    return(x)
}

calcularNIPO <- function(Dados) {
    
    ## Calcula a proxy NIPO a partir dos dados baixados do site da CVM
    ##
    ## INPUTS
    ## Dados ... Matriz de dados criada atraves do coletaVariosAnosCVM
    ##
    
    ## Criar Vetor de controle para desconsiderar a segunda transacao de uma
    ## mesma empresa.
    EMPRESA  <- substr(Dados$empresa,1,12) # Gerar STRING de comparacao
    REPETIDO <- duplicated(EMPRESA)        # Vetor de empresas que repetiram
    
    ## Verificando Quantidade de IPO por ano 
    ano       <- substr(Dados$data[!REPETIDO],1,4)
    mes       <- substr(Dados$data[!REPETIDO],6,7)
    Out      <- data.frame(table(ano, mes))
    rownames(Out) <- as.Date(paste(Out$ano, Out$mes, "01", sep="-"))
    Out$ano  <- as.numeric(Out$ano)
    Out$mes  <- as.numeric(Out$mes)
    Out      <- Out[order(Out$ano,Out$mes),]
    Out$ano <- NULL ; Out$mes <- NULL
    colnames(Out) <- "CVM"
    
    return(Out)
}

calcularS <- function(IPO, Subsequentes, Dividas) {
    CVM.S  <- IPO[c("data", "valor")]
    CVM.S2 <- Subsequentes[c("data", "valor")]
    CVM.S3 <- Dividas[c("data", "valor")]
    
    CVM.S$tipo  <- "ACOES" ; CVM.S2$tipo <- "ACOES" ; CVM.S3$tipo <- "DIVID"
    
    CVM.S  <- rbind(CVM.S, CVM.S2, CVM.S3)
    CVM.S$mes <- as.numeric(substr(CVM.S$data, 6,7))
    CVM.S$ano <- as.numeric(substr(CVM.S$data, 1,4))
    
    Out <- as.data.frame(xtabs(valor~ano+mes, CVM.S[(CVM.S$tipo=="ACOES"),]))
    tmp <- as.data.frame(xtabs(valor~ano+mes, CVM.S[(CVM.S$tipo=="DIVID"),]))$Freq
    Out <- cbind(Out,tmp)
    colnames(Out) <- c("Y","M","A","DEB")
    Out <- Out[order(Out$Y,Out$M),]
    rownames(Out) <- as.Date(paste(Out$Y, Out$M, "01", sep="-"))
    Out$Y <- NULL ; Out$M <- NULL
    Out$Issues <- Out$A / ( Out$A + Out$DEB)
    return(Out)
}

calcularTURN <- function(Negociab, QN, QT, Periodo, lagDetrend, Liq) {
    ## Calcula a proxy TURN
    ##
    ## INPUTS
    ##
    ## Negociab ... Matriz mensal de negociacao
    ## QN ......... Matriz mensal c/ quantidade de negocios no periodo
    ## QT ......... Matriz mensal c/ quantidade de titulos
    ## Periodo .... String com o periodo desejado de filtragem
    ## lagDetrend ... Numero de anos da MMA.
    ## Liq .......... Numero minimo de liquidez
    
    require(TTR)
    
    # PERIOD.PRX <- paste(PERIOD.n,"-01/", PERIOD.N, "-06", sep="")
    #                      JAN/n     a        JUN/N (Ex. 1999-01/2014-06)
    
    n <- as.numeric(substr(Periodo, 1, 4)) # Extraindo ano inicial
    
    # Periodo suficiente p/ calculo da MMA que destendenciara a serie
    periodo_turn <- sub(n, (n - lagDetrend), Periodo)
    
    mNegociab <- importaBaseCSV(Negociab, periodo_turn)
    mQN       <- importaBaseCSV(QN, periodo_turn, ignora=1)
    mQT       <- importaBaseCSV(QT, periodo_turn, ignora=1)
    
    dNegociab <- mNegociab
    dNegociab[!is.na(mNegociab)]  <- NA
    dNegociab <- as.data.frame(apply(dNegociab, 2, function(x) as.logical(x) ))
    rownames(dNegociab) <- rownames(mNegociab)
    dNegociab[is.na(dNegociab) ]  <- F
    dNegociab[mNegociab >= Liq]  <- T
    
    ## Total de QT e QN
    Out <- mapply(function(qt,dn) { sum(qt[dn], na.rm=T) },
                  as.data.frame(t(mQT)), as.data.frame(t(dNegociab)))
    Out <- as.data.frame(Out) ; colnames(Out) <- "QT"
    Out$QN <- mapply(function(qn,dn) { sum(qn[dn], na.rm=T) },
                     as.data.frame(t(mQN)), as.data.frame(t(dNegociab)))
    
    Out$QN <- Out$QN / 10000
    Out$QT <- Out$QT / 10000
    
    # Calcular TURNOVER
    Out$TURN <- Out$QN / Out$QT
    Out$dTURN <- (Out$TURN - SMA(Out$TURN, 12*lagDetrend)) # Turnover detrend
    as.data.frame(as.xts(Out)[Periodo])
}

calcularPVOL <- function () {

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
    N <- sort(seq(M)[M==6], decreasing = T)[1]     # penultimo JUNHO
    
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

portfolioSerie2  <- function (Returns, Values, Assets) {
    
    # Função p/ calcular retorno dos portfolios feito p/ matrizes simetricas
    #
    # INPUT
    # _________________________________________________________________
    #
    # Return .... Matriz de Retornos (Returns)
    # Values .... Matriz com os Valores de Mercado (MarketValues)
    # Assets .... Matriz de ativos pertecentes ao Portfolio (SelectedStocks)
    # _________________________________________________________________
    
#     require(lubridate)
    
    # nrow(mReturns[-1,])/nrow(f_MKT[-nrow(f_MKT),])
    
    #M <- as.numeric(substr(rownames(Returns),6,7)) # meses
    #     M <- month(as.Date(rownames(Returns))) # c/ lubridate (+ elegante)
    #     n <- (seq(M))[M==7][1]                         # primeiro JULHO
    #     N <- sort(seq(M)[M==6], decreasing = T)[1]     # ultimo   JUNHO

    #rDate <- as.Date(rownames(Returns[n:(N-6),]))
    #aDate <- as.Date(rownames(Assets))
    ## Apenas os meses de interesse
    # assetYears <- year(rDate) %between% (year(aDate)) 
    #     period <- ydm(rownames(mReturns)) %between% ydm(rownames(f_MKT))
    #     Returns <- Returns[period,] # Apenas meses de JULHO/ano1 a JUNHO/ano
    #     Values <- Values # Apenas meses de JULHO/ano1 a JUNHO/ano
    #     Assets <- Assets # Apenas os anos utilizados para o calculo
    #     
    # Deixando matrizes de ativos e de retornos do mesmo tamnho
    
    periodR <- paste(PERIOD.n,"-07/", PERIOD.N, "-06", sep="")
    periodA <- paste(PERIOD.n, "-06/", PERIOD.N-1, "-06", sep="")
    
    Returns <- as.data.frame(as.xts(mReturns)[periodR])
    Values  <- as.data.frame(as.xts(Values)[periodR])
    Assets  <- as.data.frame(as.xts(f_MKT)[periodA])
    
#     Returns <- Returns[-1,]
#     Values  <- Values[-1,]
#     Assets  <- Assets[-nrow(Assets),]
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

riskFreeRate <- function (ano_inicial, ano_final) {
    
    ## DESCRICAO: Baixa a Serie da SELIC e retorna o retorno logaritmo.
    ##
    
    require("Quandl")
    # require("xts")
    # Rf <- importaBaseCSV("Input/mMacroeconomics.csv")[-(1:12),]
    # tmp <- as.xts(Rf)
    # Rf  <- as.data.frame( diff(log(tmp), lag=1) ) ; rm(tmp)
    
    n <- as.Date(paste(ano_inicial+1,"-07-01", sep=""))
    N <- as.Date(paste(ano_final,"-06-30", sep=""))
    # https://www.quandl.com/BCB/4390
    SELIC <- Quandl("BCB/4390", type="ts", collapse="monthly", sort="asc",
                    # transformation="rdiff",
                    trim_start=n, trim_end=N)
    
    ## Retorna a taxa livre de risco logarítima
    
    log(1+SELIC/100)
}

computeLongShort <- function(Returns, MarketValue, Variable, nPortfolios,
                             Rf) {
    
    ## DESCRICAO: Calcula a série de LongShort conforme a anomalia.
    ##
    ## ARGUMENTOS:
    R  <- Returns     # Matriz de Retornos dos Ativos
    MV <- MarketValue # Valor de Mercado p/ calculo do peso
    V  <- Variable    # Variavel de Interesse
    nP <- nPortfolios # Numero de portfolios (5 p/ quintis, 10 p/ decis)
    
    ShortAssets  <- portfolioSelectAssets(V, nP, nP)
    ShortReturns <- portfolioSerie(R, MV, ShortAssets)
    
    LongAssets   <- portfolioSelectAssets(V, nP, nP/nP)
    LongReturns  <- portfolioSerie(R, MV, LongAssets)
    
    Out <- data.frame(LONG=ts(LongReturns$rVW, start=c(PERIOD.n+1,7), frequency=12),
                      SHORT=ts(ShortReturns$rVW, start=c(PERIOD.n+1,7), frequency=12),
                      row.names=rownames(Long))
    
    print(colMeans(Out)*100)
    
    return(Out)
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