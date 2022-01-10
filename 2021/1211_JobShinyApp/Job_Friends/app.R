
library(tidyverse)
library(tidytext)
library(shiny)
library(shinythemes)

df <- readr::read_csv(here::here("data/bible_kjv_new.csv")) %>% 
    filter(book == "Job") %>% 
    select(-c(book, citation, verse)) %>% 
    filter(chapter %in% 4:37) %>% 
    mutate(
        character = case_when(
            chapter %in% c("6", "7", "9", "10", "12", "13", "14", "16", "17", "19", "21", "23", "24", "26", "27", "28", "29", "30", "31") ~ "Job",
            chapter %in% c("4", "5", "15", "22") ~ "Eliphaz",
            chapter %in% c("8", "18", "25") ~"Bildad",
            chapter %in% c("11", "20") ~ "Zophar",
            chapter %in% c("32", "33", "34", "35", "36", "37") ~"Elihu",
            TRUE ~ as.character(chapter)
        )) 

job_stop_words <- c("thou", "thy", "thine", "hast", "art", "mine",
                    "shalt", "thee", "hath", "ye", "yea", "thereof", 
                    "job", "god", "day", "doth", "canst", "cometh", 
                    "wilt", "dost", "wherefore", "shouldes", "lo")



ui <- fluidPage(theme = shinytheme("flatly"),
                titlePanel("Job and His Friends"),
                
                sidebarLayout(
                    sidebarPanel(
                        
                        selectInput("char", 
                                    label = "Select a character",
                                    choices = c("All",
                                                "Job",
                                                "Eliphaz",
                                                "Bildad",
                                                "Zophar",
                                                "Elihu"
                                    ), 
                                    selected = 1),
                        br(),
                        numericInput("limit", 
                                     label = "Enter a minimum frequency:", 
                                     value = 35,
                                     min = 1),
                        helpText("Note: Plot won't be available if there are no world above your selected minimum"),
                        br(),
                        radioButtons("ngram",
                                     label = "Select one:",
                                     choices = c("Word" = 1,
                                                 "Bigram" = 2,
                                                 "Trigram" = 3),
                                     selected = 1),
                        submitButton("Pivot!", icon("refresh"))
                    ),
                    mainPanel(plotOutput("plot", height = "700px"))
                )
)



server <- function(input, output) {
    
    words <- reactive({
        if(input$char != "All") {
            ngrams <- df %>%
                filter(character == input$char) %>%
                unnest_tokens(word, text, token = "ngrams", 
                              n = as.integer(input$ngram)) %>%
                filter(!is.na(word)) %>% 
                filter(!word %in% job_stop_words)
            
        } else {
            ngrams <- df %>%
                unnest_tokens(word, text, token = "ngrams", n = as.integer(input$ngram)) %>%
                filter(!is.na(word)) %>% 
                filter(!word %in% job_stop_words)
        }
        
        if(as.integer(input$ngram) == 2) {
            ngrams <- ngrams %>%
                separate(word, c("word1", "word2"), sep = " ") %>% 
                filter(!word1 %in% stop_words$word) %>% 
                filter(!word2 %in% stop_words$word) %>% 
                unite(word, word1, word2, sep = " ")
        } else if(as.integer(input$ngram) == 3){
            ngrams <- ngrams %>%
                separate(word, c("word1", "word2", "word3"), 
                         sep = " ") %>%  
                filter(!word1 %in% stop_words$word) %>% 
                filter(!word2 %in% stop_words$word) %>% 
                filter(!word3 %in% stop_words$word) %>% 
                unite(word, word1, word2, word3, sep = " ") 
        } else {
            ngrams <- ngrams %>%
                anti_join(stop_words, by = "word") %>% 
                filter(!word %in% job_stop_words)
            
        }
        
        ngrams %>% 
            count(word, sort = TRUE) %>%
            filter(n > input$limit)
    })
    output$plot <- renderPlot({
        ggplot(words(),
               aes(x = n,
                   y = reorder(word, n))) +
            geom_col(fill = "#15404e",
                     color = "gray93") +
            theme_minimal() +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.text.y = element_text(size = 18),
                panel.grid.major.y = element_blank(),
                panel.grid.minor = element_blank()
            )
    })
}

shinyApp(ui, server)


