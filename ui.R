library("shiny")
library("shinythemes")
library("ggplot2")

reg_cast = c("Michael", "Pam", "Dwight", "Jim", "Ryan", "Stanley", "Kevin", "Meredith", "Angela", "Oscar",
             "Phyllis", "Roy", "Toby", "Jan", "Kelly", "Andy", "Creed", "Darryl", "Erin", "Gabe", "Holly",
             "Robert", "Nellie", "Clark", "Pete")

ui = fluidPage(theme = shinytheme("sandstone"),

              titlePanel(strong("The Office Lines Analysis")),

              # p("Who in the Office is the most talkative character? Following page is analysis of
              #        all lines spoken in our beloved TV series."),

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
                                       br(),
                                       h2(textOutput("name")),
                                       imageOutput("image", height = "200px"),
                                       br(),
                                       p("First occurence: ", textOutput("first_ep", inline=T)),
                                       p("Last occurence: ", textOutput("last_ep", inline=T)),
                                       p("Total number of lines: ", textOutput("total_lines", inline=T))
                                       ),
                          
                          mainPanel(h2(textOutput("title_word_cloud")),
                                    plotOutput("cloud")
                                    
                            )
                          )
                        ),


              tabPanel("Line Searcher",
                       mainPanel(
                        textInput("line_search",
                                  label = h3("Write phrase/line you want to search for:"),
                                  value = "Enter text...",
                                  width = "50%")
                )
              )
  )
)
    

#shinyApp(ui = ui, server = server)
#runApp("C:/code/The Office Lines")
