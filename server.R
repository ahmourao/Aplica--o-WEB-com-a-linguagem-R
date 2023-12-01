library(shiny)
source('funcoes-de-limpeza.R')
source('funcoes-globais.R')
library(zip)
library(shinyBS) #bootstrap
library(shinyjs) #javascript

# Lógica do servidor
function(input, output, session) {
    # input é uma lista de todas as variáveis dos id de entrada
    
    #REMOÇÃO
    file_path <- "diretorio_zipado.zip"
    # Verifique se o arquivo existe antes de removê-lo
    if (file.exists(file_path)) {
        unlink(file_path)
        print("O arquivo foi removido com sucesso.")
    }
    
    file_path2 <- "diretorio_zipado2.zip"
    # Verifique se o arquivo existe antes de removê-lo
    if (file.exists(file_path2)) {
        unlink(file_path2)
        print("O arquivo foi removido com sucesso.")
    }
    
    file_path3 <- "diretorio_zipado3.zip"
    # Verifique se o arquivo existe antes de removê-lo
    if (file.exists(file_path3)) {
        unlink(file_path3)
        print("O arquivo foi removido com sucesso.")
    }
    
    diretorio <- "Imagens-um-grafico-uma-pergunta"
    # Verifica se o diretório existe antes de removê-lo
    if (dir.exists(diretorio)) {
        # Remove o diretório
        unlink(diretorio, recursive = TRUE)
    }
    
    diretorio2 <- "um-grafico-para-cada-pergunta-todas-as-perguntas"
    # Verifica se o diretório existe antes de removê-lo
    if (dir.exists(diretorio2)) {
        # Remove o diretório
        unlink(diretorio2, recursive = TRUE)
    }
    
    diretorio3 <- "Imagens-grafico-de-varias-perguntas"
    # Verifica se o diretório existe antes de removê-lo
    if (dir.exists(diretorio3)) {
        # Remove o diretório
        unlink(diretorio3, recursive = TRUE)
    }
    
    
    
    
    # GRAFICO DE TODAS AS PERGUNTAS
    #variável que está armazenado o dataframe da planilha que o usuário mandou via upload
    data1 <- reactive({
        req(input$file1)
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)

        # Verifica se o arquivo é do tipo xlsx
        if (!grepl(".xlsx", inFile$name))
            return(NULL)

        # Lê o arquivo XLSX
        df <- read_xlsx(inFile$datapath)
        
        return(df)
    })
    
    
    #objeto dinâmico de armazenamento de variáveis 
    objetosAba1 <- reactiveValues(planilha = NULL)
    objetosAba1 <- reactiveValues(tipoDeGrafico = NULL)
    observeEvent(data1(),{
        
        df <- data1()
        if(df[1,3] == "Docentes Fatec"){
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo DOCENTE",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba1$planilha <- "Docente"
            
        }else if(df[1,3] == "Alunos Fatec") {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo ALUNO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba1$planilha <- "Discente"
            
        }else{
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo FUNCIONÁRIO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba1$planilha <- "Funcionario"
            
        }
    })
    
    
    observeEvent(input$tipoDeGraficoAba1, {
        objetosAba1$tipoDeGrafico <- input$tipoDeGraficoAba1
        #ele pega o valor de input do tipo de gráfico
    })
    
    # BOTÃO DE ENVIAR
    observeEvent(input$botaoEnviar, {
        
        # Crie o pop-up
        showModal(
            modalDialog(
                title = "PROCESSANDO...",
                "Aguarde enquanto fazemos os gráficos...",
                footer = NULL,
                size = "s"
            )
        )
        
        # cria os diretórios
        diretorio_principal <- "um-grafico-para-cada-pergunta-todas-as-perguntas"
        subpastas <- c("Discente", "Docente", "Funcionario")
        if (!dir.exists(diretorio_principal)) {
            dir.create(diretorio_principal)
            cat("Diretório principal criado com sucesso!", "", diretorio_principal)
        } else {
            print("Diretório já existe")
        }
        for (subpasta in subpastas) {
            caminho_subpasta <- file.path(diretorio_principal, subpasta)

            if (!file.exists(caminho_subpasta)) {
                dir.create(caminho_subpasta)
                print(paste("Subpasta", subpasta, "criada com sucesso!"))
            } else {
                print(paste("A subpasta", subpasta, "já existe."))
            }
        }
        caminho <- "um-grafico-para-cada-pergunta-todas-as-perguntas"
        
        #obtenção dos dados
        dados <- data1() #pega o dataframe do input de UPLOAD
        #limpeza
        df <- limpeza_de_dados(dados, objetosAba1$planilha)
        
        if(objetosAba1$planilha == "Docente"){
            lista <- quebrar_dataframe_em_lista(df)
            if(objetosAba1$tipoDeGrafico == "Pizza"){
                #gerar gráfico
                gerar_grafico_pizza(lista, caminho)
            }else if(objetosAba1$tipoDeGrafico == "Barras"){
                #gerar gráfico
                gerar_grafico_barra(lista, caminho)
            }else{
                
            }
            

        }else if(objetosAba1$planilha == "Discente"){
            lista <- quebrar_dataframe_em_lista(df)
            if(objetosAba1$tipoDeGrafico == "Pizza"){
                #gerar gráfico
                gerar_grafico_pizza(lista, caminho)
            }else if(objetosAba1$tipoDeGrafico == "Barras"){
                #gerar gráfico
                gerar_grafico_barra(lista, caminho)
            }else{
                
            }

        }else{
            
            lista <- quebrar_dataframe_em_lista(df)
            if(objetosAba1$tipoDeGrafico == "Pizza"){
                #gerar gráfico
                gerar_grafico_pizza(lista, caminho)
            }else if(objetosAba1$tipoDeGrafico == "Barras"){
                #gerar gráfico
                gerar_grafico_barra(lista, caminho)
            }else{
                
            }
        }
        
        # Diretório que será zipado
        dir_to_zip <- "um-grafico-para-cada-pergunta-todas-as-perguntas"
        # Nome do arquivo zip a ser criado
        zip_filename <- "diretorio_zipado.zip"
        # Cria o arquivo zip
        zip(zip_filename, dir_to_zip)
        
        shinyjs::enable("botaoDownload") 
        output$botaoDownload <- downloadHandler(
            filename = function() {
                "diretorio_zipado.zip"
            },
            content = function(file) {
                file.copy("diretorio_zipado.zip", file)
            }
        )
        
        
        # Fecha o pop-up de carregamento
        removeModal() 
        #informa ao usuário
        output$status <- renderText("Você já pode baixar os gráficos.")
    })#botão de enviar

    observeEvent(input$refresh_button1, {
        session$reload()
    })

    # GRÁFICO PARA UMA ÚNICA PERGUNTA
    data2 <- reactive({
        req(input$file2)
        inFile <- input$file2
        if (is.null(inFile))
            return(NULL)

        # Verifica se o arquivo é do tipo xlsx
        if (!grepl(".xlsx", inFile$name))
            return(NULL)

        # Lê o arquivo XLSX
        df <- read_xlsx(inFile$datapath)

        return(df)
    })
    
    # objeto dinâmico de armazenamento de variáveis 
    valores_reativos2 <- reactiveValues(planilha = NULL)
    valores_reativos2 <- reactiveValues(eixo = NULL)
    valores_reativos2 <- reactiveValues(df = NULL)
    valores_reativos2 <- reactiveValues(dimensao = NULL)
    valores_reativos2 <- reactiveValues(pergunta = NULL)
    valores_reativos2 <- reactiveValues(perguntaCpa = NULL)
    valores_reativos2 <- reactiveValues(tipoDeGrafico = NULL)
    
    
    #quando data 2 mudar é feito a análise do tipo de planilha
    observeEvent(data2(),{
        df <- data2()
        
        if(df[1,3] == "Docentes Fatec"){
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo DOCENTE",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            valores_reativos2$planilha <- "Docente"
            
        }else if(df[1,3] == "Alunos Fatec") {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo ALUNO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            valores_reativos2$planilha <- "Discente"
            
        }else{
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo FUNCIONÁRIO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            valores_reativos2$planilha <- "Funcionario"
            
        }
        shinyjs::disable("botaoEnviarAux")
        shinyjs::enable("eixoAba2")
        shinyjs::disable("botaoEnviar2")
        shinyjs::disable("perguntasAba2")
        shinyjs::disable("tipoDeGraficoAba2")
    })
    
    #valores reativos
    atualizarComponenteDimensao <- reactiveVal(FALSE)
    atualizarComponentePerguntaCpa <- reactiveVal(FALSE)
    atualizarComponenteTipoDeGrafico <- reactiveVal(FALSE)
    atualizarComponenteQuestoes <- reactiveVal(FALSE)
    controladorDeBotaoAux <- reactiveVal(FALSE)
    
    
    observeEvent(input$eixoAba2, {
        valores_reativos2$eixo <- input$eixoAba2
        #ele pega o valor de input da eixo
        
        dados <- data2() #pega o dataframe do input de UPLOAD
        shinyjs::disable("tipoDeGraficoAba2")
        
        #funções
        criadorCpaPerguntas <- function(df){
            
            df <- df %>% 
                filter(Indicador == "Cpa")
            rownames(df) <- NULL
            valores_possiveis <- df$NumeroPergunta
            
            
            shinyjs::disable("dimensaoAba2")
            shinyjs::disable("botaoEnviarAux")
            #cria os elementos de radiobutton
            
            if(atualizarComponentePerguntaCpa ()){
                updateRadioButtons(session, "perguntasCpaAba2", 
                                   "3. (CPA)Escolha uma questão:",
                                   choices = c(Nenhum = "default", 
                                               valores_possiveis),
                                   selected = "default")
            }else{
                insertUI(
                    selector = "#perguntasCpaAba2",
                    ui = radioButtons("perguntasCpaAba2", 
                                      "3. (CPA)Escolha uma questão:", 
                                      choices = c(Nenhum = "default", 
                                                  valores_possiveis), 
                                      selected = "default")
                )   
                atualizarComponentePerguntaCpa (TRUE)
            }
            if(atualizarComponenteTipoDeGrafico () != TRUE){
                insertUI(
                    selector = "#tipoDeGraficoAba2",
                    ui = radioButtons("tipoDeGraficoAba2", 
                                      "4. Selecione o tipo de gráfico que deseja:", 
                                      choices = c(Nenhum = "default",
                                                  Pizza = "Pizza",
                                                  Barras = "Barras"),
                                      selected = "default")
                )
                atualizarComponenteTipoDeGrafico(TRUE)
            }
            
        }
        criadorPerguntas <- function(df){
            eixo_resposta <- paste("Eixo:", valores_reativos2$eixo)
            #filtra o dataframe para apenas valores daquela dimensão
            df <- df %>% 
                filter(Indicador == eixo_resposta)
            # Valores válidos para a dimensão
            total_dimensoes <- unique(df$Dimensao)
            #FORMATAÇÃO
            for (i in seq_along(total_dimensoes)) {
                total_dimensoes[i] <- substr(total_dimensoes[i], start = 11, stop = 12)
            }
            shinyjs::disable("perguntasCpaAba2")
            if(atualizarComponenteDimensao()){
                updateRadioButtons(session, "dimensaoAba2", 
                                   "3. Selecione a dimensão:",
                                   choices = c(Nenhum = "default",total_dimensoes),
                                   selected = "default")
            }else{
                insertUI(
                    selector = "#dimensaoAba2", 
                    ui = radioButtons("dimensaoAba2", 
                                      "3. Selecione a dimensão:",
                                      choices = c(Nenhum = "default",total_dimensoes),
                                      selected = "default")
                )
                atualizarComponenteDimensao(TRUE)
            }
        }
        
        #lógica para o eixo
        
        df <- limpeza_de_dados(dados, valores_reativos2$planilha)
        if(valores_reativos2$eixo == "CPA"){
            criadorCpaPerguntas(df)
        }else{
            criadorPerguntas(df)
        }
        
    })#fim do evento do eixo
    
    observeEvent(input$dimensaoAba2, {
        valores_reativos2$dimensao <- input$dimensaoAba2
        if(valores_reativos2$dimensao != "default"){
            shinyjs::enable("botaoEnviarAux")
        }else{
            shinyjs::disable("botaoEnviarAux")
        }
    })#fim do evento da dimensão
    
    
    # BOTÃO DE ENVIAR
    observeEvent(input$botaoEnviarAux, {
        
        #controle
        shinyjs::disable("dimensaoAba2")
        shinyjs::disable("eixoAba2")
        shinyjs::enable("tipoDeGraficoAba2")
        shinyjs::enable("botaoEnviar2") 
        shinyjs::disable("perguntasCpaAba2")
        
        dados <- data2() #pega o dataframe do input de UPLOAD
        
        #limpeza
        df <- limpeza_de_dados(dados, valores_reativos2$planilha)
            #filtros
        
        eixoResposta <- paste("Eixo:", valores_reativos2$eixo)
                #filtra o dataframe para apenas valores daquele eixo
        df <- df %>% 
            filter(Indicador == eixoResposta)
        
        dimResposta <- paste("Dimensão:", valores_reativos2$dimensao)
                #filtra o dataframe para apenas valores daquela dimensão
        df <- df %>%
            filter(Dimensao == dimResposta)
        
        valoresPossiveis <- df$NumeroPergunta
        
        if(atualizarComponenteQuestoes()){
            updateRadioButtons(session, "perguntasAba2",
                               "4. Escolha uma questão:", 
                               choices = c(Nenhum = "default",
                                           valoresPossiveis), 
                               selected = "default")
        }else{
            insertUI(
                selector = "#perguntasAba2",
                ui = radioButtons("perguntasAba2", 
                                  "4. Escolha uma questão:", 
                                  choices = c(Nenhum = "default", valoresPossiveis),
                                  selected = "default")
            )
            atualizarComponenteQuestoes(TRUE)
        }
        
        if(atualizarComponenteTipoDeGrafico()){
            updateRadioButtons(session, "tipoDeGraficoAba2", 
                               "5. Selecione o tipo de gráfico que deseja:",
                               choices = c(Nenhum = "default",
                                           Pizza = "Pizza", 
                                           Barras = "Barras"),
                               selected = "default")
        }else{
            insertUI(
                selector = "#tipoDeGraficoAba2", 
                ui = radioButtons("tipoDeGraficoAba2", 
                                  "5. Selecione o tipo de gráfico que deseja:",
                                  choices = c(Nenhum = "default", 
                                              Pizza = "Pizza",
                                              Barras = "Barras"),
                                  selected = "default")
            )
            atualizarComponenteTipoDeGrafico(TRUE)
        }
        shinyjs::disable("botaoEnviarAux")
        controladorDeBotaoAux(TRUE)
    })
    
    observeEvent(input$perguntasAba2, {
        valores_reativos2$pergunta <- input$perguntasAba2
        #ele pega o valor de input da pergunta
    })
    
    observeEvent(input$perguntasCpaAba2, {
        valores_reativos2$perguntaCpa <- input$perguntasCpaAba2
        
        #ele pega o valor de input da pergunta
        if(valores_reativos2$perguntaCpa != "default"){
            shinyjs::enable("botaoEnviar2")
            shinyjs::enable("tipoDeGraficoAba2")
        }else{
            shinyjs::disable("botaoEnviar2")
        }
        controladorDeBotaoAux(FALSE)
    })
    
    observeEvent(input$tipoDeGraficoAba2, {
        valores_reativos2$tipoDeGrafico <- input$tipoDeGraficoAba2
        #ele pega o valor de input do tipo de gráfico
    })
    

    observeEvent(input$botaoEnviar2, {
        # Crie o pop-up de carregamento
        showModal(
            modalDialog(
                title = "Processando...",
                "Aguarde enquanto a tarefa é executada...",
                footer = NULL,
                size = "m"
            )
        )
        
        
        # cria as pastas
        diretorio_principal <- "Imagens-um-grafico-uma-pergunta"
        subpastas <- c("Discente", "Docente", "Funcionario")
        if (!dir.exists(diretorio_principal)) {
            dir.create(diretorio_principal)
            cat("Diretório principal criado com sucesso!","", diretorio_principal)
        }else{
            print("Diretório já existe")
        }
        for (subpasta in subpastas) {
            caminho_subpasta <- file.path(diretorio_principal, subpasta)
            
            if (!file.exists(caminho_subpasta)) {
                dir.create(caminho_subpasta)
                print(paste("Subpasta", subpasta, "criada com sucesso!"))
            } else {
                print(paste("A subpasta", subpasta, "já existe."))
            }
        }
        caminho <- "Imagens-um-grafico-uma-pergunta"
        
        if(controladorDeBotaoAux()){
            if(valores_reativos2$pergunta != "default" & valores_reativos2$tipoDeGrafico != "default"){
                dados <- data2() #pega o dataframe do input de UPLOAD
                
                #limpeza
                df <- limpeza_de_dados(dados, valores_reativos2$planilha)
                #filtros
                
                eixo_resposta <- paste("Eixo:", valores_reativos2$eixo)
                #filtra o dataframe para apenas valores daquele eixo
                df <- df %>%
                    filter(Indicador == eixo_resposta)
                
                dim_reposta <- paste("Dimensão:", valores_reativos2$dimensao)
                #filtra o dataframe para apenas valores daquela dimensão
                df <- df %>%
                    filter(Dimensao == dim_reposta)
                
                #gráfico
                dff <- filtrar_pergunta(df, valores_reativos2$pergunta)
                if(valores_reativos2$tipoDeGrafico == "Pizza"){
                    grafico_pizza(dff,caminho)
                }else{
                    grafico_barra(dff,caminho)
                }
                
            }else{
                output$status2 <- renderText("Você não selecionou uma pergunta ou um tipo de gráficos")
            }
        }else{
            if(valores_reativos2$perguntaCpa != "default"){
                
                dados <- data2() #pega o dataframe do input de UPLOAD
                #limpeza
                df <- limpeza_de_dados(dados, valores_reativos2$planilha)
                #filtros
                #filtra o dataframe para apenas valores daquela dimensão
                df <- df %>%
                    filter(Indicador == "Cpa")
                rownames(df) <- NULL
                
                dff <- filtrar_pergunta(df, valores_reativos2$perguntaCpa)
                
                if(valores_reativos2$tipoDeGrafico == "Pizza"){
                    grafico_pizza(dff,caminho)
                }
                if(valores_reativos2$tipoDeGrafico == "Barras"){
                    grafico_barra(dff,caminho)
                }
                
            }else{
                output$status2 <- renderText("Você não selecionou uma pergunta ou um tipo de gráficos")
            }   
        }
            
        # Feche o pop-up de carregamento
        removeModal()  # Feche o pop-up de carregamento

        #zipagem
        # Diretório que será zipado
        dir_to_zip <- "Imagens-um-grafico-uma-pergunta"
        # Nome do arquivo zip a ser criado
        zip_filename <- "diretorio_zipado2.zip"
        
        # Cria o arquivo zip
        zip(zip_filename, dir_to_zip)
        output$status2 <- renderText("Você já pode baixar os gráficos.")
        shinyjs::enable("botaoDownload2")
        output$botaoDownload2 <- downloadHandler(
            filename = function() {
                "diretorio_zipado2.zip"
            },
            content = function(file) {
                file.copy("diretorio_zipado2.zip", file)
            }
        )
    })
    
    observeEvent(input$refresh_button2, {
        session$reload()
    })



    # grafico para um conjunto de perguntas
    data3 <- reactive({
        req(input$file3)
        inFile <- input$file3
        if (is.null(inFile))
            return(NULL)
        # Verifica se o arquivo é do tipo xlsx
        if (!grepl(".xlsx", inFile$name))
            return(NULL)
        # Lê o arquivo XLSX
        df <- read_xlsx(inFile$datapath)

        return(df)
    })
    
    # objeto dinâmico de armazenamento de variáveis 
    objetosAba3 <- reactiveValues(planilha = NULL)
    objetosAba3 <- reactiveValues(eixo = NULL)
    objetosAba3 <- reactiveValues(df = NULL)
    objetosAba3 <- reactiveValues(dimensao = NULL)
    objetosAba3 <- reactiveValues(perguntas = NULL)
    objetosAba3 <- reactiveValues(perguntaCpa = NULL)
    objetosAba3 <- reactiveValues(tipoDeGrafico = NULL)
    
    
    #quando data 2 mudar é feito a análise do tipo de planilha
    observeEvent(data3(),{
        df <- data3()
        
        if(df[1,3] == "Docentes Fatec"){
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo DOCENTE",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba3$planilha <- "Docente"
            
        }else if(df[1,3] == "Alunos Fatec") {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo ALUNO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba3$planilha <- "Discente"
            
        }else{
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "AVISO",
                    "Você fez UPLOAD de um arquivo que é do tipo FUNCIONÁRIO",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            
            objetosAba3$planilha <- "Funcionario"
            
        }
        shinyjs::disable("botaoEnviar3")
        shinyjs::enable("eixoAba3")
        shinyjs::disable("botaoEnviar3")
        shinyjs::disable("perguntasAba3")
        
    })#evento de observação
    
    #valores reativos
    atualizarComponenteDimensao3 <- reactiveVal(FALSE)
    atualizarComponentePerguntaCpa3 <- reactiveVal(FALSE)
    atualizarComponenteQuestoes3 <- reactiveVal(FALSE)
    controladorDeBotaoAux3 <- reactiveVal(FALSE)
    
    
    observeEvent(input$eixoAba3, {
        objetosAba3$eixo <- input$eixoAba3
        #ele pega o valor de input da eixo
        
        dados <- data3() #pega o dataframe do input de UPLOAD
        shinyjs::disable("botaoEnviar3")
        criadorPerguntas <- function(df){
            eixo_resposta <- paste("Eixo:", objetosAba3$eixo)
            #filtra o dataframe para apenas valores daquela dimensão
            df <- df %>% 
                filter(Indicador == eixo_resposta)
            # Valores válidos para a dimensão
            total_dimensoes <- unique(df$Dimensao)
            #FORMATAÇÃO
            for (i in seq_along(total_dimensoes)) {
                total_dimensoes[i] <- substr(total_dimensoes[i], start = 11, stop = 12)
            }
            # shinyjs::disable("perguntasCpaAba3")
            if(atualizarComponenteDimensao3()){
                updateRadioButtons(session, "dimensaoAba3", 
                                   "3. Selecione a dimensão:",
                                   choices = c(Nenhum = "default",total_dimensoes),
                                   selected = "default")
            }else{
                insertUI(
                    selector = "#dimensaoAba3", 
                    ui = radioButtons("dimensaoAba3", 
                                      "3. Selecione a dimensão:",
                                      choices = c(Nenhum = "default",total_dimensoes),
                                      selected = "default")
                )
                atualizarComponenteDimensao3(TRUE)
            }
        }
        
        #lógica para o eixo
        
        df <- limpeza_de_dados(dados, objetosAba3$planilha)
        if(objetosAba3$eixo == "CPA"){
            # criadorCpaPerguntas(df)
            shinyjs::enable("botaoEnviar3")
            shinyjs::disable("botaoEnviarAux3")
            shinyjs::disable("dimensaoAba3")
        }else{
            criadorPerguntas(df)
        }
        
    })#fim do evento do eixo
    
    observeEvent(input$dimensaoAba3, {
        objetosAba3$dimensao <- input$dimensaoAba3
        if(objetosAba3$dimensao != "default"){
            shinyjs::enable("botaoEnviarAux3")
        }else{
            shinyjs::disable("botaoEnviarAux3")
        }
    })#fim do evento da dimensão
    
    observeEvent(input$faixa_valores, {
        objetosAba3$perguntas <- input$faixa_valores
        #ele pega o valor de input da pergunta
        pergunta_inicio <- objetosAba3$perguntas[1]
        pergunta_fim <- objetosAba3$perguntas[2]
        
        if (pergunta_inicio == pergunta_fim) {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "ERRO",
                    "Os valores não podem ser iguais",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            shinyjs::disable("botaoEnviar3")
        }else if (pergunta_inicio >= pergunta_fim) {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "ERRO",
                    "A primeria pergunta deve ser menor que a última perguinta.",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            shinyjs::disable("botaoEnviar3")
        } else if ((pergunta_fim - pergunta_inicio) < 1) {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "ERRO",
                    "Digite novamente um valor de pergunta inicial, depois a pergunta final.",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            shinyjs::disable("botaoEnviar3")
        } else if ((pergunta_fim - pergunta_inicio) > 7) {
            # Crie o pop-up de carregamento
            showModal(
                modalDialog(
                    title = "ERRO",
                    "A diferença entre as perguntas não pode ser maior que 8. Você só pode escolher até 8 perguntas.",
                    footer = NULL,
                    size = "s"
                )
            )
            Sys.sleep(3)
            removeModal()
            shinyjs::disable("botaoEnviar3")
        } else {
            shinyjs::enable("botaoEnviar3")
        }
    })
    
    # BOTÃO DE ENVIAR
    observeEvent(input$botaoEnviarAux3, {
        
        #controle
        shinyjs::disable("dimensaoAba3")
        shinyjs::disable("eixoAba3")
        shinyjs::enable("botaoEnviar3") 
        # shinyjs::disable("perguntasCpaAba3")
        
        dados <- data3() #pega o dataframe do input de UPLOAD
        
        #limpeza
        df <- limpeza_de_dados(dados, objetosAba3$planilha)
        #filtros
        
        eixoResposta <- paste("Eixo:", objetosAba3$eixo)
        #filtra o dataframe para apenas valores daquele eixo
        df <- df %>% 
            filter(Indicador == eixoResposta)
        
        dimResposta <- paste("Dimensão:", objetosAba3$dimensao)
        #filtra o dataframe para apenas valores daquela dimensão
        df <- df %>%
            filter(Dimensao == dimResposta)
        
        valoresPossiveis <- df$NumeroPergunta
        
        minimo <- head(valoresPossiveis, n=1)
        maximo <- tail(valoresPossiveis, n=1)
        
        if(atualizarComponenteQuestoes3()){
            updateSliderInput(session, 
                              "faixa_valores", 
                              "4. Selecione um intervalo de questões:",
                              min = minimo, max = maximo,
                              value = c(minimo, maximo))
        }else{
            insertUI(
                selector = "#faixa_valores", 
                ui = sliderInput("faixa_valores",
                                 "4. Selecione um intervalo de questões:",
                                 min = minimo, max = maximo,
                                 value = c(minimo, maximo))
            )
            atualizarComponenteQuestoes3(TRUE)
        }
        
        shinyjs::disable("botaoEnviarAux3")
        controladorDeBotaoAux3(TRUE)
    })
    
    observeEvent(input$botaoEnviar3, {
        # Crie o pop-up de carregamento

        showModal(
            modalDialog(
                title = "Processando...",
                "Aguarde enquanto a tarefa é executada...",
                footer = NULL,
                size = "m"
            )
        )
        
        # cria as pastas
        diretorio_principal <- "Imagens-grafico-de-varias-perguntas"
        subpastas <- c("Discente", "Docente", "Funcionario")
        if (!dir.exists(diretorio_principal)) {
            dir.create(diretorio_principal)
            cat("Diretório principal criado com sucesso!","", diretorio_principal)
        }else{
            print("Diretório já existe")
        }
        for (subpasta in subpastas) {
            caminho_subpasta <- file.path(diretorio_principal, subpasta)
            
            if (!file.exists(caminho_subpasta)) {
                dir.create(caminho_subpasta)
                print(paste("Subpasta", subpasta, "criada com sucesso!"))
            } else {
                print(paste("A subpasta", subpasta, "já existe."))
            }
        }
        caminho <- "Imagens-grafico-de-varias-perguntas"
        
        if(controladorDeBotaoAux3()){
            dados <- data3() #pega o dataframe do input de UPLOAD
            
            #limpeza
            df <- limpeza_de_dados(dados, objetosAba3$planilha)
            
            #filtros
            eixo_resposta <- paste("Eixo:", objetosAba3$eixo)
            #filtra o dataframe para apenas valores daquele eixo
            df <- df %>%
                filter(Indicador == eixo_resposta)
            
            dim_reposta <- paste("Dimensão:", objetosAba3$dimensao)
            #filtra o dataframe para apenas valores daquela dimensão
            df <- df %>%
                filter(Dimensao == dim_reposta)
            rownames(df) <- NULL
            
            #gráfico
            dff <- filtrar_perguntas(df, objetosAba3$perguntas[1], objetosAba3$perguntas[2])
            gerar_grafico_barra_por_eixo(dff)
            
        }else{
            dados <- data3() #pega o dataframe do input de UPLOAD
            
            #limpeza
            df <- limpeza_de_dados(dados, objetosAba3$planilha)
            #filtros
            df <- df %>% 
                filter(Indicador == "Cpa")
            gerar_grafico_barra_por_eixo(df)
        }
        
        # Feche o pop-up de carregamento
        removeModal()  # Feche o pop-up de carregamento
        
        #zipagem
        # Diretório que será zipado
        dir_to_zip <- "Imagens-grafico-de-varias-perguntas"
        # Nome do arquivo zip a ser criado
        zip_filename <- "diretorio_zipado3.zip"
        
        # Cria o arquivo zip
        zip(zip_filename, dir_to_zip)
        output$status3 <- renderText("Você já pode baixar os gráficos.")
        shinyjs::enable("botaoDownload3")
        output$botaoDownload3 <- downloadHandler(
            filename = function() {
                "diretorio_zipado3.zip"
            },
            content = function(file) {
                file.copy("diretorio_zipado3.zip", file)
            }
        )
    })
    
    observeEvent(input$refresh_button3, {
        session$reload()
    })
    
}#fim do servidor
