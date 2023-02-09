library(shiny) # Application
library(pdftools) # read pdf
library(dplyr) # process
library(tidyr) # process
library(tm) # text process
library(tidytext) # text process
library(ggwordcloud) # plot

ui = fluidPage(
  titlePanel("Topics modeling tools"),
  br(),
  fluidRow(
    column(
      width = 3
      , align = "center"
      , style = "border: 1px solid #e6e9ef; padding: 20px; margin-left: 20px;"
      , fileInput(
        inputId = "file"
        , label = "Choisir un fichier PDF"
        , accept = ".pdf")
      , numericInput(
        inputId = "num_topics"
        , label = "Nombre de sujets"
        , value = 3
        , min = 1
        , max = 20)
      , numericInput(
        inputId = "num_words"
        , label = "Nombre de mots par nuages"
        , value = 20
        , min = 1
        , max = 100)
      , br()
      , actionButton(
        inputId = "submit"
        , label = "Soumettre"
        , icon = icon("cloud")
        , width = "250px"
        , style = "border-radius:15px; padding:5px; background-color: lightblue;")
      ),
    column(
      width = 8
      , align = "center"
      , h3("Bienvenue dans notre analyseur de textes !", align = "center")
      , br()
      , tags$p("Cet outil vous permet d'explorer et de visualiser les thèmes (topics) contenus dans un texte. 
               Il utilise des techniques avancées de traitement automatique de la langue naturelle pour nettoyer et analyser votre texte.
               Il est basé sur des packages tels que LDAvis pour générer des thèmes et faciliter votre analyse.
               N'hésitez pas à expérimenter avec différents textes et paramètres pour obtenir les résultats les plus pertinents pour vous."
               , align = "justify"
               , style = "font-size: 18px; line-height: 1.5; margin-left : 40px; margin-top : 40px;")
      )
    )
  , br()
  ,
  fluidRow(
    column(
      width = 12
      , div(
        plotOutput("wordclouds")
        , style = "border: 1px solid #e6e9ef; padding: 20px;")
      , tags$style(type = "text/css", "#wordclouds {overflow: auto;}")
      )
    )
  )

# Define the server function
server = function(input, output, session) {
    
  text_data = eventReactive(input$submit, {
    if (is.null(input$file)) return(NULL)
    pdf_text(input$file$datapath)
  })
  
  cleaned_text = eventReactive(input$submit, {
    # Clean the text by removing special characters, numbers and stopwords
    cleaned_text = gsub("[^[:alpha:]]", " ", text_data())
    cleaned_text = gsub("[0-9]", " ", cleaned_text)
    cleaned_text = tolower(cleaned_text)
    stopwords = stopwords("fr")
    cleaned_text = removeWords(cleaned_text,stopwords)
    stopwords = stopwords("en")
    cleaned_text = removeWords(cleaned_text,stopwords)
    cleaned_text
  })
  
  text_corpus = eventReactive(input$submit, {
    Corpus(VectorSource(cleaned_text()))
  })
  
  text_tdm = eventReactive(input$submit, {
    DocumentTermMatrix(text_corpus())
  })
  
  text_topic_model = eventReactive(input$submit, {
    if (is.null(text_tdm())) return(NULL)
    topicmodels::LDA(text_tdm(), k = input$num_topics)
  })
  
  text_topic = eventReactive(input$submit, {
    tidy(text_topic_model(), matrix = "beta")
  })
  
  text_plot = eventReactive(input$submit, {
    if (is.null(text_topic())) return(NULL)
    
    tps = text_topic() %>%
      group_by(term) %>% 
      arrange(desc(beta)) %>% 
      mutate(pos = row_number()) %>% 
      filter(pos == 1) %>% 
      ungroup() %>% 
      group_by(topic) %>%
      top_n(n = input$num_words, wt = beta) %>%
      ungroup() %>% 
      select("word" = term, "freq" = beta, topic)
    
    wc = lapply(unique(tps$topic), function(i) {
      tps %>% 
        filter(topic == i) %>% 
        mutate_at(vars(freq), ~ (.x - min(.x)) / (max(.x) - min(.x))) %>% 
        select(-topic) %>% 
        ggwordcloud2()
    })
    
    if(input$num_topics > 2) {
      pl = gridExtra::grid.arrange(grobs = wc, ncol = 3)
    } else {
      pl = gridExtra::grid.arrange(grobs = wc, ncol = input$num_topics)
    }
    
    return(pl)
  })
  

  output$wordclouds = renderPlot({
    if (is.null(text_plot())) return(NULL)
    text_plot()
    })
  
}

# Launch the Shiny app
shinyApp(ui = ui, server = server)
