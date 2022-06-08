
server = function(input, output, session) {
  
  plot_chars <- reactive({
    names_set = input$plot_chars
    return(subset(data_long, name %in% names_set))
    
  })
  
  output$season_plot <- renderPlot({
    
      ggplot(plot_chars(), aes(x = season, y = lines)) +
        geom_bar(aes(fill = name), position = "dodge", stat="identity") +
        ylab("Number of lines") +
        scale_x_discrete(labels = seq(1,9), name ="Season") +
        labs(x = "", fill = "Character")
    
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
    cat(subset(lines_per_season, name == char)$sum)})
  
  output$text_more_lines <- renderPrint({
    char = input$select_char
    index = which(lines_per_season$name == char)
    more_lines = round(mean(lines_per_season$sum[-index] < lines_per_season[index,]$sum) * 100, 2)
    
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
  
  wordcloud_rep <- repeatable(wordcloud2)
  
  output$cloud <- renderWordcloud2({
    max_words = 75
    char = input$select_char
    data = subset(df, Character == char)$Line
    
    words <- Corpus(VectorSource(data))
    
    words <- tm_map(words, removeWords, stopwords("english"))
    words <- tm_map(words, removePunctuation, ucp = TRUE)
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, stripWhitespace)
    
    dtm <- TermDocumentMatrix(words)
    m <- as.matrix(dtm)
    v <- sort(rowSums(m),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)[1:max_words,]
    
    wordcloud_rep(data = d)
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
                  any_vars(grepl(phrase_exact, ., ignore.case= T, perl= T))) %>% 
  
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
                  any_vars(grepl(input$phrase, ., ignore.case= T, perl= T, fixed = T))) %>% 
        
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