require(ggplot2)
require(dplyr)
require(stringr)
require(wordcloud)
require(RColorBrewer)
require(tm)
require(DT)

df = read.csv("C:/code/The-Office-Lines-Analysis/lines_prepared.csv", encoding="UTF-8")
reg_cast = c("Michael", "Pam", "Dwight", "Jim", "Ryan", "Stanley", "Kevin", "Meredith", "Angela", "Oscar",
             "Phyllis", "Roy", "Toby", "Jan", "Kelly", "Andy", "Creed", "Darryl", "Erin", "Gabe", "Holly",
             "Robert", "Nellie", "Clark", "Pete")

count_lines <- function(char, season){
  uniques = unique(df$Character[startsWith(df$Character, substr(char, 1, 3))])
  uniques = uniques[!startsWith(uniques, paste(char, "'S", sep=""))]
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

lines_number = get_data()

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
    cat(subset(lines_number, name == char)$number)})
  
  output$text_more_lines <- renderPrint({
    char = input$select_char
    index = which(lines_number$name == char)
    more_lines = round(mean(lines_number$number[-index] < lines_number[index,]$number) * 100, 2)
    
    cat(paste0("Did you know that ", char, " has more lines than ",
      more_lines, "% of other main characters in The Office?"))
  })
  
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
  
  wordcloud_rep <- repeatable(wordcloud)
  
  output$cloud <- renderPlot({
    max_words = 100
    char = input$select_char
    data = subset(df, Character == char)$Line
    
    words <- Corpus(VectorSource(data))
    
    words <- tm_map(words, removeWords, stopwords("english"))
    words <- tm_map(words, removePunctuation, ucp = TRUE)
    words <- tm_map(words, stripWhitespace)
    
    wordcloud_rep(words, max.words = max_words, scale=c(5,1),
              colors=brewer.pal(8, "Dark2"),
              random.order = FALSE)
  })
  
  #Regex searching with highlights thanks to: 
  #https://codehunter.cc/a/r/highlight-word-in-dt-in-shiny-based-on-regex
  
  df_reactive <- reactive({
    phrase_not_exact = paste(c("(", input$phrase, ")"), collapse = "")
    phrase_exact = paste(c("\\b(", input$phrase, ")\\b"), collapse = "")
    
    if (input$exact_match == TRUE){
      df %>% 
        # Filter if input is anywhere, even in other words.
        filter_at(.vars = vars(Line),
                  any_vars(grepl(input$phrase, ., ignore.case= T, perl= T))) %>% 
  
        # Replace complete words with same in HTML.
        mutate_at(.vars = vars(Line),
                  ~ gsub(
                  phrase_exact,
                  "<span style='background-color:yellow;'>\\1</span>",
                  .,
                  ignore.case = TRUE,
                  perl = TRUE
                  )  
        )
    }
    else{
      df %>% 
        # Filter if input is anywhere, even in other words.
        filter_at(.vars = vars(Line),
                  any_vars(grepl(input$phrase, ., ignore.case= T, perl= T))) %>% 
        
        # Replace complete words with same in HTML.
        mutate_at(.vars = vars(Line),
                  ~ gsub(
                    phrase_not_exact,
                    "<span style='background-color:yellow;'>\\1</span>",
                    .,
                    ignore.case = TRUE,
                    perl = TRUE
                  )  
        )
    }

  })
  
  output$search_line <- renderDataTable({
    datatable(df_reactive(), escape = F, options = list(dom = '<"top" p>'))
  })
  
  output$phrase_number <- renderText(({
    if (input$phrase == ''){
      return("-")
    }else{
      if (input$exact_match == TRUE){
        return(sum(str_count(df$Line, regex(paste0("\\b", input$phrase, "\\b"), ignore_case=TRUE))))
      }
      else{
        return(sum(str_count(df$Line, fixed(input$phrase, ignore_case=TRUE))))
      }
    }
    
  }))
}