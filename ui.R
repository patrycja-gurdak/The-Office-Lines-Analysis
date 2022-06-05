ui = fluidPage(theme = shinytheme("flatly"),

              titlePanel(strong("The Office Lines Analysis")),

              tabsetPanel(
                tabPanel("General Info",
                              sliderInput(inputId = "season_slider", label = "Choose season",
                                          value = 1, min = 1, max = 9),
                              h1("Here we see a plot: "),
                              plotOutput("season_plot")
                         ),

                tabPanel("Character Stats", fluid = TRUE,
                        sidebarLayout(
                          sidebarPanel(selectInput("select_char", label = h3("Select character"), 
                                                   choices = reg_cast, 
                                                   selected = 1),
                                      
                                       h2(textOutput("name")),
                                       imageOutput("image", height = "200px"),
                                       br(),
                                       p("First occurence: ", textOutput("first_ep", inline=T)),
                                       p("Last occurence: ", textOutput("last_ep", inline=T)),
                                       p("Total number of lines: ", textOutput("total_lines", inline=T)),
                                       textOutput("text_more_lines")
                                       ),
                                  
                          
                          mainPanel(
                              h3(textOutput("title_word_cloud")),
                              fluidRow(
                                column(7, plotOutput("cloud",width = "100%",height="600px"))
                              )
                            )
                          )
                        ),


              tabPanel("Line Searcher",
                       sidebarPanel(
                         textInput("phrase",
                                   label = h3("Write phrase/line you want to search for:"),
                                   value = "",
                                   ),
                         checkboxInput("exact_match", label = "Exact match", value = FALSE),
                         
                         p("Phrases found: ", textOutput("phrase_number", inline=T))
                       ),        
                       mainPanel(
          
                        dataTableOutput("search_line")
                      )
                )
          )
    )