ui = navbarPage(theme = shinytheme("flatly"),

              title = strong("The Office Lines Analysis"),

              tabPanel("General Info",
                       fluidPage(
                         sidebarLayout(
                           sidebarPanel(
                           ),
                           
                           mainPanel(
                             selectInput(inputId = "plot_chars", label = "Choose characters",
                                         choices = c("All characters", reg_cast),
                                         selected = "All characters",
                                         multiple = TRUE,
                                         selectize = TRUE,
                                         width = "50%"
                             ),
                            plotOutput("season_plot")
                           )
                         )
                       )
                      ),

              tabPanel("Character Stats",
                       fluidPage(
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
                            tabsetPanel(
                              tabPanel("Word Cloud",
                                h4(textOutput("title_word_cloud")),
                                wordcloud2Output("cloud")
                                
                              ),
                              tabPanel("Plots",
                                       h3("plots")
                                       
                              )
                            )
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
