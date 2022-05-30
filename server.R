require("ggplot2")
require(stringr)
require(wordcloud2)
require(RColorBrewer)
require("SnowballC")
require(tm)

df = read.csv("C:/code/Datasets/TheOfficeLines/the-office_lines.csv", encoding="UTF-8")
reg_cast = c("Michael", "Pam", "Dwight", "Jim", "Ryan", "Stanley", "Kevin", "Meredith", "Angela", "Oscar",
             "Phyllis", "Roy", "Toby", "Jan", "Kelly", "Andy", "Creed", "Darryl", "Erin", "Gabe", "Holly",
             "Robert", "Nellie", "Clark", "Pete")

df$Line <- str_replace_all(df$Line, "’", "'")

count_lines <- function(char, season){
  uniques = unique(df$Character[startsWith(df$Character, substr(char, 1, 3))])
  uniques = uniques[!startsWith(uniques, paste(char, "’S", sep=""))]
  count = sum(str_count(subset(df, Season %in% season)$Character, pattern = paste(uniques, collapse='|')))
  return(count)
}

get_data <- function(season = seq(1,9)){
  n_lines = data.frame(name = reg_cast, number = sapply(reg_cast, count_lines, season))
  rownames(n_lines) <- NULL
  return (n_lines)
}

first_episode <- function(char){
  x = subset(df, Character == char)[,c(4,5)]
  first = x[order(x$Season, x$Episode_Number),][1,]
  return(first)
}

last_episode <- function(char){
  x = subset(df, Character == char)[,c(4,5)]
  last = x[order(-x$Season, -x$Episode_Number),][1,]
  return(last)
}

server = function(input, output, session) {
  
  season <- reactive({
    input$season_slider
  })
  
  output$season_plot <- renderPlot({
    
    ggplot(get_data(season()))+
      geom_bar(aes(x= reorder(name, -number), y=number, fill = number),stat="identity") +
      scale_fill_gradient(high='orange',low = "blue") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "None") +
      xlab("Character") +
      ylab("Number of lines") +
      coord_cartesian(ylim=c(0,2500))
  })
  
  output$name <- renderPrint({
    cat(input$select_char)
  })
  
  output$first_ep <- renderPrint({
    char = input$select_char
    cat("Season", first_episode(char)[[1]], "episode ",first_episode(char)[[2]])})
  
  output$last_ep <- renderPrint({
    char = input$select_char
    cat("Season", last_episode(char)[[1]], "episode ",last_episode(char)[[2]])})
  
  output$total_lines <- renderPrint({
    char = input$select_char
    data = get_data()
    cat(subset(data, name == char)$number)})
  
  output$image <- renderImage({
    char = input$select_char
    return(list(
        src = paste0("C:/code/The Office Lines/img/",char,".jpg"),
        contentType = "image/jpg",
        height = 200
      ))
    
  }, deleteFile = FALSE)
  
  output$title_word_cloud <- renderText({
    char = input$select_char
    return(paste0("Most common words spoken by ", char))}
  )
  
  output$cloud <- renderPlot({
    max_words = 30
    char = input$select_char
    data = subset(df, Character == char)$Line
    
    words <- Corpus(VectorSource(data))
    
    words <- tm_map(words, removeWords, stopwords("english"))
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, stripWhitespace)
    
    tdm <- as.matrix(TermDocumentMatrix(words))
    data <- sort(rowSums(tdm), decreasing = TRUE)[1:max_words]
    data <- data.frame(word = names(data), freq = as.numeric(data))
    
    wordcloud2(data, shape = 'circle', color = 'random-light')
  })
}
