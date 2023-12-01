
library("tidyverse")
library("dplyr")
library("readxl")


# Nível 1
# É passado como parametro um dataframe e retorna o mesmo dataframe
filtro_inicial <- function(df){
    colunas_zero <- apply(df == 0,2, all)# retorna um vetor lógico
    
    # Remove todas as colunas zeradas
    df <- subset(df, select = !colunas_zero)
    df <- df %>% 
        select(-contains('Perc')) %>% 
        select(-contains('TotalEsperado')) %>% 
        select(-contains('TotalValidos'))
    # Remove as colunas que contém "perc" e "total" 
    
    # Remove as 3 primeiras colunas : unidade, edicao e categorias
    df <- df[, -c(1:3)]
    
    return (df)
}

# Nível 2
# FORMATAÇÃO - renomear a coluna cursos
# É passado como parametro um dataframe e retorna  o mesmo dataframe
renomear_cursos <- function(df){
    if(all(c("FormularioApelido", "Curso") %in% colnames(df))){
        df <- subset(df, select = -FormularioApelido)
    }
    names(df)[names(df) == "FormularioApelido"] <- "Curso"
    if ("Curso" %in% colnames(df)) {
        if(df$Curso[1] == "Docentes Fatec"){
            df$Curso <- "Docente"
        }else if(df$Curso[1] == "Funcionário Fatec"){
            df$Curso <- "Funcionario"
        }else{
            df$Curso <- "Discente"
        }
    }
    return(df)
}

# Nível 2
# FORMAÇÃO - retirar a palavra "Dimensão" da coluna área, por exemplo: "Dimensão: Organização e Gestão da Instituição", isso causa redundância nos gráficos.
# É passado como parametro um dataframe e retorna o mesmo dataframe
formatacao_dimensao_area <- function(df){
    for(i in seq_along(df$Area)){
        texto_resposta <-(df$Area[i])
        texto_resposta <- substr(texto_resposta, start = 1, stop=5)
        if(texto_resposta == "Dimen"){
            texto <- as.character(df$Area[i])
            texto <- substr(texto, start = 11, stop = nchar(texto))
            df$Area[i] <- texto
        }
        next
    }
    return(df)
}


# Nível 3
# É passada como parametro um dataframe e retorna uma lista. 
quebrar_dataframe_em_lista <- function(df){
    dados_quebrados <- split(df, df$NumeroPergunta)
    return(dados_quebrados)
}


# Nível 4
# É passado como parametro uma lista e retorna a mesma lista
remover_dados_cadastrais <- function(lista){
    dados_juntos <- do.call("rbind", lista)
    df <- dados_juntos[dados_juntos$Indicador != "Cadastro",]
    df <- split(df, df$NumeroPergunta) #vira lista
    df <- as.array(df)
    rownames(df) <- 1:length(df)
    return(df)
}


# Nível 5
# É passado como parametro uma lista e retorna a mesma lista
remover_valores_duplicados <- function(lista){
    for (i in seq_along(lista)) {
        df <- lista[[i]]
        
        aux <- unique(df[, c(1:7)]) # para as colunas do tipo caractere
        aux2 <- t(as.data.frame(colSums(df[, 8:ncol(df)]))) # para as colunas do tipo numero
        rownames(aux2)<- NULL
        aux3 <- cbind(aux,aux2)
        
        
        df <- aux3 # atualizo o valores no dataframe da vez
        
        lista[[i]] <- df # atualizo o dataframe na lista
    }
    return(lista)
}