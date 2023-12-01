
library('tidyverse') #usar o pipe %>% 
library('dplyr') # usar função select, filter e rename
library('ggplot2')
library('ggrepel')


# Útil em alguns momentos
# Recebe como parametro uma lista e retorna um dataframe
lista_em_dataframe <- function(lista){
    dados_juntos <- do.call("rbind", lista)
    return(dados_juntos)
}

#um grafico para uma pergunta
filtrar_pergunta <- function(df, pergunta){
    df <- df %>% 
        filter(NumeroPergunta == pergunta)
    return(df)
}

#conjunto de perguntas
filtrar_perguntas <- function(df, pergunta_inicio, pergunta_fim ){
    df <- df %>% 
        filter(NumeroPergunta >= pergunta_inicio & NumeroPergunta <= pergunta_fim)
    return(df)
}

# Nível 1
# Recebe como parametro dois dataframes, ambos são usados durante a lógica da função, retorna um dataframe
renomear_colunas_grafico <- function(df, dff){
    for(j in seq_along(dff)){
        #primeira coluna
        texto_resposta <- df$Respostas
        texto_resposta <- substr(texto_resposta, start = 1, stop=7)
        if(texto_resposta == "A - Sim"){
            if(names(dff)[j] == "TotalA"){
                dff <- dff %>% 
                    rename("Sim" = "TotalA")
            }
            if(names(dff)[j] == "TotalB"){
                dff <- dff %>% 
                    rename("Não" = "TotalB")
            }
        }
        if(texto_resposta == "A - Exc"){
            if(names(dff)[j] == "TotalA"){
                dff <- dff %>% 
                    rename("Excelente" = "TotalA")
            }
            if(names(dff)[j] == "TotalB"){
                dff <- dff %>% 
                    rename("Muito bom" = "TotalB")
            }
            if(names(dff)[j] == "TotalC"){
                dff <- dff %>% 
                    rename("Regular" = "TotalC")
            }
            if(names(dff)[j] == "TotalD"){
                dff <- dff %>% 
                    rename("Insuficiente" = "TotalD")
            }
            if(names(dff)[j] == "TotalE"){
                dff <- dff %>% 
                    rename("Nao se aplica" = "TotalE")
            }
            if(names(dff)[j] == "TotalF"){
                dff <- dff %>% 
                    rename("Nao sei responder" = "TotalF")
            }
        }
        if(texto_resposta == "A - Sem"){
            if(names(dff)[j] == "TotalA"){
                dff <- dff %>% 
                    rename("Sempre" = "TotalA")
            }
            if(names(dff)[j] == "TotalB"){
                dff <- dff %>% 
                    rename("Quase sempre" = "TotalB")
            }
            if(names(dff)[j] == "TotalC"){
                dff <- dff %>% 
                    rename("Algumas vezes" = "TotalC")
            }
            if(names(dff)[j] == "TotalD"){
                dff <- dff %>% 
                    rename("Muito raramente" = "TotalD")
            }
        }
    }
    return(dff)
}


# Nível 2
# Recebe como parametro um dataframe e não retorna nada, ele criará apenas um gráfico salvo no diretório(pasta) por vez.
grafico_pizza <- function(df,caminho){
    #caminho de salvamento do arquivo
    titulo_grafico <-  gsub("[: ]", "",df$Curso)
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",df$Indicador))
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",df$Dimensao))
    
    if(df$Curso[1] == "Docente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Docente/", titulo_grafico)
    }else if(df$Curso[1] == "Discente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Discente/", titulo_grafico)
    }else{
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Funcionario/", titulo_grafico)
    }
    
    
    #faço uma tabela auxiliar com os valores das respostas
    df2 <- df %>% 
        select(grep("^T", colnames(df), value = TRUE))
    colunas_maior_que_zero <- colnames(df2)[colSums(df2 > 0) > 0]
    df2 <- df2[, colunas_maior_que_zero]
    
    df2 <- renomear_colunas_grafico(df, df2)
    #Titulo, subtítulo e legendas
    # texto_legenda <- c(unlist(strsplit(df$Respostas, ", ")))
    # texto_legenda <- substring(texto_legenda, first = 5)
    
    Titulo <- df$Area
    Titulo <- paste(df$Dimensao, sep = " - ", Titulo)
    
    Subtitulo <- df$Curso
    
    texto_legenda <- colnames(df2)
    
    texto <- as.character(df$Questao)
    texto <- substr(texto, start = 6, stop = nchar(texto))
    texto <- str_wrap(texto, width = 40)
    #adiciono quebras de linhas no texto 
    
    #criação da tabela para criar o gráfico
    valores<-unlist(df2[1,])
    #pego a linha do df2, porque eu só quero os valores. 
    df3 <- data.frame(rotulos = texto_legenda, frequencia = valores)
    df3 <- transform(df3, porcentagem = round(frequencia / sum(frequencia) * 100))
    #calcula a porcentagem
    rownames(df3) <- NULL
    #gráfico
    
    grafico <- df3 %>% 
        mutate(soma_acumulada = rev(cumsum(rev(frequencia))), 
               pos = frequencia/2 + lead(soma_acumulada, 1),
               pos = if_else(is.na(pos), frequencia/2, pos)) %>% 
        ggplot(aes(x = "", y = frequencia, fill = fct_inorder(rotulos))) +
        geom_col(width = 1, color = 1) +
        geom_label_repel(aes(y = pos,
                             label = glue::glue("{paste0(porcentagem,'%')}"), 
                             fill = rotulos),
                         size = 6,
                         fontface = "bold",
                         nudge_x = 0.8,
                         show.legend = FALSE) +
        labs(fill = "Rotulos") +
        coord_polar(theta = "y") +
        geom_bar(stat = "identity", width = 1, color = "grey40") + 
        theme_void() +
        theme(legend.title = element_blank(), legend.text = element_text(size = 20)) +
        labs(caption = texto,
             title = Titulo,
             subtitle = Subtitulo) + 
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 20),
              plot.tag = element_text(hjust = 0.5, size = 20, face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 20, face = "bold", vjust = 9), 
              plot.margin = margin(0, 0, 0, 0)) + 
        scale_fill_brewer(palette = "Pastel1") 
    print(titulo_grafico)
    #salvar
    ggsave(titulo_grafico, plot = grafico, width = 10, height = 8, dpi = 300)
    
}

# Nível 2
# Recebe como parametro um dataframe e não retorna nada, ele criará apenas um gráfico salvo no diretório(pasta) por vez.
grafico_barra <- function(df, caminho){
    #caminho de salvamento do arquivo
    
    titulo_grafico <-  gsub("[: ]", "",df$Curso)
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",df$Indicador))
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",df$Dimensao))
    
    if(df$Curso[1] == "Docente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Docente/", titulo_grafico)
    }else if(df$Curso[1] == "Discente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Discente/", titulo_grafico)
    }else{
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",df$NumeroPergunta, "-", df$NumeroPergunta[nrow(df)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0(caminho, "/Funcionario/", titulo_grafico)
    }
    
    
    #faço uma tabela auxiliar com os valores das respostas
    df2 <- df %>% 
        select(grep("^T", colnames(df), value = TRUE))
    colunas_maior_que_zero <- colnames(df2)[colSums(df2 > 0) > 0]
    df2 <- df2[, colunas_maior_que_zero]
    
    df2 <- renomear_colunas_grafico(df, df2)
    #Titulo, subtítulo e legendas
    # texto_legenda <- c(unlist(strsplit(df$Respostas, ", ")))
    # texto_legenda <- substring(texto_legenda, first = 5)
    
    Titulo <- df$Area
    Titulo <- paste(df$Dimensao, sep = " - ", Titulo)
    
    Subtitulo <- df$Curso
    
    texto_legenda <- colnames(df2)
    
    texto <- as.character(df$Questao)
    texto <- substr(texto, start = 6, stop = nchar(texto))
    texto <- str_wrap(texto, width = 40)
    #adiciono quebras de linhas no texto 
    
    #criação da tabela para criar o gráfico
    valores<-unlist(df2[1,])
    #pego a linha do df2, porque eu só quero os valores. 
    df3 <- data.frame(rotulos = texto_legenda, frequencia = valores)
    df3 <- transform(df3, porcentagem = round(frequencia / sum(frequencia) * 100))
    #calcula a porcentagem
    rownames(df3) <- NULL
    altura <- sum(df3$frequencia)
    #gráfico
    grafico <- ggplot(df3, aes(x = rotulos, y = frequencia, fill = fct_inorder(rotulos))) +
        
        geom_bar(stat = "identity", position = "dodge", width = 0.6) +
        
        labs(title = "Gráfico de Barras por Data Frame") +
        
        theme_bw() +  ylab("") + xlab("") +
        
        theme(legend.position = "bottom", 
              legend.direction = "horizontal", 
              legend.title = element_blank()) +
        
        # legend.text = element_text(size = 12)) +
        scale_fill_brewer(palette = "Dark2") + 
        
        geom_text(aes(label = paste0(frequencia," - ",paste0("(", porcentagem, ")"),"%")), position = position_dodge(width = 0.7), vjust = -0.5, color = "black", size = 5 ,  fontface = "bold") +
        
        theme(axis.text.y = element_text(size =14, face = "bold"), 
              axis.text.x = element_blank(), 
              legend.text = element_text(size = 14)) +
        scale_y_continuous(limits = c(0, altura), breaks = seq(0, altura, round(altura/4))) +
        labs(caption = texto,
             title = Titulo,
             subtitle = Subtitulo) + 
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 20),
              plot.tag = element_text(hjust = 0.5, size = 20, face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"), 
              plot.margin = margin(0, 0, 0, 0)) +
        theme(plot.margin = margin(1, 10, 1, 10, "cm"))
    ggsave(titulo_grafico, plot = grafico, width = 15, height = 8, dpi = 300)
}


#Nível 3
# Recebe como parametro uma lista e não retorna nada. Essa função está projetada para criar um gráfico usando a função grafico_barra para cada posição da lista. 
gerar_grafico_barra <- function(lista, caminho){
    for(i in seq_along(lista)){
        df <- lista[[i]] 
        #pego o dataframe da vez
        grafico_barra(df, caminho)
        
    }
}

#Nível 3 
# Recebe como parametro uma lista e não retorna nada. Essa função está projetada para criar um gráfico usando a função grafico_pizza para cada posição da lista.
gerar_grafico_pizza <- function(lista, caminho){
    for(i in seq_along(lista)){
        df <- lista[[i]] 
        #pego o dataframe da vez
        grafico_pizza(df, caminho)
    }
}


# Nível 1
# Recebe como parametro um único dataframe e retorna um gráfico. Essa função cria o gráfico do dataframe passado como parametro. 
grafico_barra_eixo <- function(df){
    #faço uma tabela auxiliar com os valores das respostas
    df2 <- df %>% 
        select(grep("^T", colnames(df), value = TRUE))
    colunas_maior_que_zero <- colnames(df2)[colSums(df2 > 0) > 0]
    df2 <- df2[, colunas_maior_que_zero]
    
    df2 <- renomear_colunas_grafico(df, df2)
    #Titulo, subtítulo e legendas
    # texto_legenda <- c(unlist(strsplit(df$Respostas, ", ")))
    # texto_legenda <- substring(texto_legenda, first = 5)
    
    Titulo <- df$Area
    Titulo <- paste(df$Dimensao, sep = " - ", Titulo)
    
    Subtitulo <- df$Curso
    
    texto_legenda <- colnames(df2)
    
    texto <- as.character(df$Questao)
    texto <- substr(texto, start = 6, stop = nchar(texto))
    texto <- str_wrap(texto, width = 40)
    #adiciono quebras de linhas no texto 
    
    #criação da tabela para criar o gráfico
    valores<-unlist(df2[1,])
    #pego a linha do df2, porque eu só quero os valores. 
    df3 <- data.frame(rotulos = texto_legenda, frequencia = valores)
    df3 <- transform(df3, porcentagem = round(frequencia / sum(frequencia) * 100))
    #calcula a porcentagem
    rownames(df3) <- NULL
    #gráfico
    altura <- sum(df3$frequencia)
    grafico <- ggplot(df3, aes(x = rotulos, y = frequencia, fill = fct_inorder(rotulos))) +
        
        geom_bar(stat = "identity", position = "dodge", width = 0.6) +
        
        labs(title = "Gráfico de Barras por Data Frame") +
        
        theme_bw() +  ylab("") + xlab("") +
        
        theme(legend.position = "bottom", 
              legend.direction = "horizontal", 
              legend.title = element_blank()) +
        
        # legend.text = element_text(size = 12)) +
        scale_fill_brewer(palette = "Dark2") + 
        
        geom_text(aes(label = paste0(frequencia," - ",paste0("(", porcentagem, ")"),"%")), position = position_dodge(width = 0.7), vjust = -0.5, color = "black", size = 5 ,  fontface = "bold") +
        
        theme(axis.text.y = element_text(size =14, face = "bold"), 
              axis.text.x = element_blank(), 
              legend.text = element_text(size = 14)) +
        
        scale_y_continuous(limits = c(0, altura), breaks = seq(0, altura, round(altura/4))) +
        
        labs(caption = texto,
             title = Titulo,
             subtitle = Subtitulo) + 
        
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 20),
              plot.tag = element_text(hjust = 0.5, size = 20, face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 20, face = "bold"), 
              plot.margin = margin(0, 0, 0, 0)) +
        
        theme(plot.margin = margin(1, 10, 1, 10, "cm"))
    return(grafico)
}

# Nível 2
# Recebe como parametro um único dataframe, não retorna nada. Essa função serve para gerar um gráfico com questões agrupadas, 
# se no dataframe tiver questões com respostas sim ou não na lista de perguntas realizadas pelo usuário, 
# então ele criará um gráfico a parte no mesmo diretório(pasta) na qual o gráfico com perguntas agrupadas será salvo.
gerar_grafico_barra_por_eixo <- function(dff){
    
    vetor_perguntas_sim <- vector("numeric", length = 0)
    
    for(i in seq_along(dff$Respostas)){
        texto_resposta <- dff$Respostas[i]
        texto_resposta <- substr(texto_resposta, start = 1, stop=7)
        titulo_grafico <-  gsub("[: ]", "",dff$Curso[1])
        titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",dff$Indicador[1]))
        titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",dff$Dimensao[1]))
        if(texto_resposta == "A - Sim"){
            vetor_perguntas_sim <- dff$NumeroPergunta[i]
            aux <- dff %>% 
                filter(NumeroPergunta == vetor_perguntas_sim)
            grafico <- grafico_barra_eixo(aux)
            if(dff$Curso[1] == "Docente"){
                titulo_grafico <- paste(titulo_grafico, "" , paste0("Questao",dff$NumeroPergunta[1]))
                titulo_grafico <- paste0(titulo_grafico,".jpeg")
                titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Docente/",titulo_grafico)
            }else if(dff$Curso[1] == "Discente"){
                titulo_grafico <- paste(titulo_grafico, "" , paste0("Questao", dff$NumeroPergunta[1]))
                titulo_grafico <- paste0(titulo_grafico,".jpeg")
                titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Discente/",titulo_grafico)
            }else{
                titulo_grafico <- paste(titulo_grafico, "" , paste0("Questao",dff$NumeroPergunta[1]))
                titulo_grafico <- paste0(titulo_grafico,".jpeg")
                titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Funcionario/",titulo_grafico)
            }
            ggsave(titulo_grafico, plot = grafico, width = 25, height = 15, dpi = 300)
            # Fazer com que plote no arquivo já
        }
    }
    
    #faço uma tabela auxiliar com os valores das respostas
    if(length(vetor_perguntas_sim) > 0){
        dff <- dff %>% 
            filter(NumeroPergunta != vetor_perguntas_sim)
    }
    df2 <- dff %>% 
        select(grep("^T", colnames(dff), value = TRUE))
    colunas_maior_que_zero <- colnames(df2)[colSums(df2 > 0) > 0]
    df2 <- df2[, colunas_maior_que_zero]
    
    #renomeio as colunas
    df2 <- renomear_colunas_grafico(dff[1,], df2)
    
    #renomeio as linhas
    rownames(df2)<-NULL
    
    #renomeio a coluna de questões do df
    for (i in seq_len(nrow(dff))) {
        texto <- as.character(dff$Questao[i])
        texto <- substr(texto, start = 6, stop = nchar(texto))
        texto <- str_wrap(texto, width = 15)
        dff$Questao[i] <- texto
    }
    
    
    df2 <- t(df2)
    df2 <- as.data.frame(df2)
    # pego a linha do df2, porque eu só quero os valores. 
    df3 <- data.frame(rotulos = rownames(df2), frequencia = df2)
    # calcula a porcentagem
    rownames(df3) <- NULL
    
    df3 <- df3 %>% gather(frequencia, valor, -rotulos)
    rotulos_X <- dff$Questao
    
    Titulo <- dff$Area[1]
    Titulo <- paste(dff$Dimensao[1], sep = " - ", Titulo)
    
    Subtitulo <- dff$Curso[1]
    
    #caminho de salvamento do arquivo
    
    titulo_grafico <-  gsub("[: ]", "",dff$Curso[1])
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",dff$Indicador[1]))
    titulo_grafico <- paste(titulo_grafico, gsub("[: ]", "",dff$Dimensao[1]))
    print(paste("A imagem foi salva com o nome:  ", titulo_grafico))
    if(dff$Curso[1] == "Docente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",dff$NumeroPergunta[1], "-",         dff$NumeroPergunta[nrow(dff)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Docente/",titulo_grafico)
    }else if(dff$Curso[1] == "Discente"){
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",dff$NumeroPergunta[1], "-",         dff$NumeroPergunta[nrow(dff)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Discente/",titulo_grafico)
    }else{
        titulo_grafico <- paste(titulo_grafico, "" , paste("Questoes",dff$NumeroPergunta[1], "-",         dff$NumeroPergunta[nrow(dff)]))
        titulo_grafico <- paste0(titulo_grafico,".jpeg")
        titulo_grafico <- paste0("./Imagens-grafico-de-varias-perguntas/Funcionario/",titulo_grafico)
    }
    
    
    # Criar o gráfico de barras
    altura <- sum(df2$V1)
    grafico <- ggplot(df3, aes(x = frequencia, y = valor, fill = fct_inorder(rotulos))) +
        geom_bar(stat = "identity", position = "dodge", width = 0.7) +
        labs(x = "Frequência", y = "Valor", fill = "Rótulos") +
        theme_bw() +
        ylab("") + xlab("") +
        theme(legend.position = "bottom",
              legend.direction = "horizontal",
              legend.title = element_blank()) +
        theme(axis.text.y = element_text(size = 16, face = "bold", color = "black"),
              axis.text.x = element_text(size = 16, hjust = 0.5, vjust = 1, color = "black"),  # Rotaciona os rótulos do eixo x
              legend.text = element_text(size = 18, face = "bold", color = "black"),
              plot.margin = margin(50, 50, 50, 50), 
              panel.grid.major = element_line(color = "grey80"),
              panel.border = element_blank()) +
        geom_text(aes(label = valor), position = position_dodge(width = 0.7), vjust = -0.6, color = "black", size = 5) +
        scale_x_discrete(labels = rotulos_X)+
        scale_y_continuous(limits = c(0, altura), breaks = seq(0, altura, round((altura/4)))) +
        labs(title = Titulo,
             subtitle = Subtitulo) + 
        theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
              plot.subtitle = element_text(hjust = 0.5, size = 20),
              plot.tag = element_text(hjust = 0.5, size = 20, face = "bold"),
              plot.caption = element_text(hjust = 0.5, size = 20, face = "bold")) +
        theme(plot.margin = margin(1, 10, 1, 10, "cm"))
    
    ggsave(titulo_grafico, plot = grafico, width = 25, height = 15, dpi = 300)
}

#server R
limpeza_de_dados <- function(df, planilha){
    #entrada e saida dataframe

    
    if(planilha == "Docente"){
        
        #limpeza dos dados
        #formato de dataframe
        Docente1 <- filtro_inicial(df)
        Docente1 <- renomear_cursos(Docente1)
        Docente1 <- formatacao_dimensao_area(Docente1)
        
        #formato de listas
        lista_Docente <- quebrar_dataframe_em_lista(Docente1)
        lista_Docente <- remover_dados_cadastrais(lista_Docente)
        
        #lógica específica para geração do gráfico
        df <- lista_em_dataframe(lista_Docente)
    }
    if(planilha == "Discente"){
        
        #limpeza dos dados
        #formato de dataframe
        Discente1 <- filtro_inicial(df)
        Discente2 <- renomear_cursos(Discente1)
        Discente2 <- formatacao_dimensao_area(Discente2)
        
        #formato de listas
        lista_Discente <- quebrar_dataframe_em_lista(Discente2)
        lista_Discente <- remover_valores_duplicados(lista_Discente) # exclusivo para Alunos
        lista_Discente <- remover_dados_cadastrais(lista_Discente)
        
        #lógica específica para geração do gráfico
        df <- lista_em_dataframe(lista_Discente)
    }
    if(planilha == "Funcionario"){
        
        #limpeza dos dados
        #formato de dataframe
        Funcionario1 <- filtro_inicial(df)
        Funcionario2 <- renomear_cursos(Funcionario1)
        Funcionario2 <- formatacao_dimensao_area(Funcionario2)
        
        #formado de listas
        lista_Funcionario <- quebrar_dataframe_em_lista(Funcionario2)
        lista_Funcionario <- remover_dados_cadastrais(lista_Funcionario)
        
        #lógica específica para geração do gráfico
        df <- lista_em_dataframe(lista_Funcionario)
    }
    return(df)
}