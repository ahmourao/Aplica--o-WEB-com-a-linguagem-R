library(shiny)
library(shinyBS) #bootstrap
library(shinyjs) #javascript
fluidPage(
    tags$head(
        tags$link(
            rel = "stylesheet",
            href = "https://fonts.googleapis.com/css?family=Montserrat",
            type = "text/css"
        )
    ),
    
    tags$style(
        HTML(
            "
            *{
            font-family: 'Montserrat', sans-serif;'
            }
            
            h2{
            font-weight: bold;
            }
            
      .custom-header {
        background-color: #212529;
        color: white;
        text-align: center;
        padding: 20px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }

      .logo {
        max-width: 150px;
        max-height: 150px;
        margin-right: 30px;
      }
      
      .custom-logo{
        display: flex;
        align-items: center;
      }
      
      .container-maior{
        display: flex;
      }
      
      p{
        font-size: 16px;
      }
      
      .coluna-cinza { 
      background-color: #E7E7E7;
      padding: 25px;
      border-radius: 16px;
      }
      
      .navbar-static-top .navbar-brand {
        font-size: 24px; /* Tamanho da fonte para o título do navbarPanel */
        color: #B11116;
        }
    
    .nav-tabs > li > a {
      font-size: 20 px; /* Tamanho da fonte para cada tabPanel */
        font-weight: bold;
    }
    .negrito{
        font-weight: bold;
    }
      
      "
        )
    ),
    
    div(class = "custom-header",
        div(class = "custom-logo",
            img(class = "logo", src = "https://www.fateclins.edu.br/web/img/logo-fateclins-white.png"),
            img(class = "logo", src = "https://bkpsitecpsnew.blob.core.windows.net/uploadsitecps/sites/1/2022/10/centro-paula-souza-logo.svg")),
        titlePanel("Gerador de gráficos das respostas do WEBSAI"),
        div(class = "custom-autores",
            h5("Desenvolvido por Ana Cristina Moura,"),
            h5("sob orientação de Alexandre Teso"))
    ),
        navbarPage(
            "Modalidades:",
            tabPanel("Gráfico de todas as perguntas",
                    fluidRow(
                        column(4, class = "coluna-cinza",
                                #conteúdo
                                h2("Preencha os campos"),
                                fileInput("file1", "1. Envie um arquivo EXCEL (.xlsx)"),
                                radioButtons("tipoDeGraficoAba1", "2. Selecione o tipo de gráfico que deseja:",
                                             choices = c(Nenhum = "default",
                                                         Pizza = "Pizza",
                                                         Barras = "Barras"),
                                             selected = "default"),
                                actionButton("botaoEnviar", "Fazer gráficos"), 
                                verbatimTextOutput("status"),
                                shinyjs::useShinyjs(),
                                downloadButton("botaoDownload", "Baixar as imagens", disabled = TRUE),
                                h4("Aperte o seguinte botão para reiniciar o programa"),
                                actionButton("refresh_button1", "Atualizar o navegador")
                                ), 
                        column(8, 
                                #conteúdo
                                h2("Regras de uso"), 
                                p("Nessa seção será gerado todos os gráficos de uma só vez. 
                                  Poderá fazer fazer o donwload de uma única planilha ou mais, desde que não reinicie o programa e só faça o upload de outro arquivo.
                                  Se mudar a opção de gráfico, será sobrescrito caso já tenha feito os gráficos anteriormente. "),
                                p("1. Envie apenas um arquivo por vez. Será aceito apenas arquivos com formato de planilha eletrônica do Excel, cuja extensão é '.xlsx' "),
                               p("2. O tipo de gráfico pode ser:"), 
                               div(class = "container-maior",
                                    div(class = "container-menor",
                                        p("    -Formato de pizza:", class = "negrito"), 
                                        img(src = "https://live.staticflickr.com/65535/53279781884_f6d82a2e46_h.jpg", width = "100%")),
                                    div(class = "container-menor",
                                        p("    -Formato de barras verticais:", class = "negrito"), 
                                        img(src = "https://live.staticflickr.com/65535/53279714508_78700a68b5_h.jpg", width = "100%"))
                                    )
                                )
                     )
            ),
            tabPanel("Gráfico para uma pergunta",
                    fluidRow(
                        column(4, class = "coluna-cinza",
                                #conteudo
                                h2("Preencha os campos"),
                                fileInput("file2", "1. Envie um arquivo EXCEL (.xlsx)"),
                                radioButtons("eixoAba2", "2. Selecione o eixo:",
                                             choices = c(Nenhum = "default",
                                                         "1",
                                                         "2",
                                                         "3", 
                                                         "4", 
                                                         "5", 
                                                         "CPA"),
                                             selected = "default"), 
                                uiOutput("dimensaoAba2"),
                                h4("Clique no botão para poder selecionar as perguntas do eixo e dimensão anteriormente escolhidos"),
                                actionButton("botaoEnviarAux", "Verificar perguntas disponíveis"),
                                uiOutput("perguntasAba2"),  # Espaço para exibir dinamicamente o radioButton
                                uiOutput("perguntasCpaAba2"),  # Espaço para exibir dinamicamente o radioButton
                                uiOutput("tipoDeGraficoAba2"),  # Espaço para exibir dinamicamente o radioButton
                                shinyjs::useShinyjs(),
                                actionButton("botaoEnviar2", "Fazer gráfico", disabled = TRUE),
                                verbatimTextOutput("status2"),
                                downloadButton("botaoDownload2", "Baixar as imagens", disabled = TRUE),
                                h4("Aperte o seguinte botão para reiniciar o programa"),
                                actionButton("refresh_button2", "Atualizar o navegador")
                                ), 
                         column(8, 
                                #conteúdo
                                h2("Regras de uso"),
                                p("Nesse segmento é realizado um único gráfico para uma pergunta. É possível alterar a pergunta e fazer download de vários gráficos de uma única vez."),
                                p("1. Envie apenas um arquivo por vez. Será aceito apenas arquivos com formato de planilha eletrônica do Excel, cuja extensão é '.xlsx' "),
                                p("Para as questões 2 e 3, consulte a relação a seguir:", class = "negrito"),
                                img(src = "https://live.staticflickr.com/65535/53279945200_9584e51235_b.jpg", width = "40%"),
                                p("Para o Indicador CPA não é necessário selecionar eixo ou dimensão.", class = "negrito"),
                                p("4. / 3. Selecione alguma pergunta"),
                                p("5. O tipo de gráfico pode ser"),
                                div(class = "container-maior",
                                    div(class = "container-menor",
                                        p("    -Formato de pizza:", class = "negrito"), 
                                        img(src = "https://live.staticflickr.com/65535/53279781884_f6d82a2e46_h.jpg", width = "100%")),
                                    div(class = "container-menor",
                                        p("    -Formato de barras verticais:", class = "negrito"), 
                                        img(src = "https://live.staticflickr.com/65535/53279714508_78700a68b5_h.jpg", width = "100%"))
                                )
                         )
                     )#fim do fluidRow
            ), #fim do tabpanel
            tabPanel("Conjunto de perguntas",
                    fluidRow(
                            column(4, class = "coluna-cinza",
                                   #conteúdo
                                   h2("Preencha os campos"),
                                   fileInput("file3", "1. Envie um arquivo EXCEL (.xlsx)"),
                                   radioButtons("eixoAba3", "2. Selecione o eixo:",
                                                choices = c(Nenhum = "default",
                                                            "1",
                                                            "2",
                                                            "3", 
                                                            "4", 
                                                            "5", 
                                                            "CPA"),
                                                selected = "default"),
                                   uiOutput("dimensaoAba3"),
                                   h4("Clique no botão para poder selecionar as perguntas do eixo e dimensão anteriormente escolhidos"),
                                   actionButton("botaoEnviarAux3", "Verificar perguntas disponíveis"),
                                   uiOutput("faixa_valores"),  # Espaço para exibir dinamicamente o radioButton
                                   uiOutput("perguntasCpaAba3"),  # Espaço para exibir dinamicamente o radioButton
                                   uiOutput("tipoDeGraficoAba3"),  # Espaço para exibir dinamicamente o radioButton
                                   shinyjs::useShinyjs(),
                                   actionButton("botaoEnviar3", "Fazer gráfico", disabled = TRUE),
                                   verbatimTextOutput("status3"),
                                   downloadButton("botaoDownload3", "Baixar as imagens", disabled = TRUE),
                                   h4("Aperte o seguinte botão para reiniciar o programa"),
                                   actionButton("refresh_button3", "Atualizar o navegador")
                                   ),
                            column(8,
                                #conteúdo
                                h2("Regras de uso"),
                                p("Nessa seção será realizado um único gráfico para um conjunto de perguntas. O limite é de 8 perguntas em um único gráfico. Você pode mudar os valores das perguntas e fazer um novo gráfico, ao final você fará donwload de todas os gráficos. O tipo de gráfico dessa seção é:"),
                                img(src = "https://live.staticflickr.com/65535/53295475709_a21490982f_b.jpg", width="50%"),
                                p("1. Envie apenas um arquivo por vez. Será aceito apenas arquivos com formato de planilha eletrônica do Excel, cuja extensão é '.xlsx' "),
                                p("Para as questões 2 e 3, consulte a relação a seguir:", class = "negrito"),
                                img(src = "https://live.staticflickr.com/65535/53279945200_9584e51235_b.jpg", width = "40%"),
                                p("Para o Indicador CPA não é necessário selecionar eixo ou dimensão.", class = "negrito"),
                                p("4. Selecione uma faixa de perguntas disponíveis para o indicador e dimensão anteriormente selecionados. Não pode ser maior que 8 e menor que 2.")
                                )
                            )#fim do fluidRow
                     )#fim do tabpanel
        )#navbar
    
)
