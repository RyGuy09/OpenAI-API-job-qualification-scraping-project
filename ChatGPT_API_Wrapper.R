library(httr)
library(jsonlite)
library(tidyverse)
library(ggplot2)

# Set your OpenAI API key
api_key <- "*REDACTED*"

job_description_graph <- function(data_type, data_vector=NULL) {  
  
  if (is.null(data_vector)) {
    if (data_type == "education_level"){
      data_vector <- c("Bachelor's", "Master's", "PhD")
    }
    else if (data_type == "programming_languages"){
      data_vector <- c("Python", "SQL", "R", "C", "Java", "Javascript")
    }
    else if (data_type == "stat_model"){
      data_vector <- c("Machine Learning", "Deep Learning", "Regression", 
                       "Classification", "Neural Networks", 
                       "Natural Language Processing", "Clustering", 
                       "Unsupervised Learning")
    }
    else if (data_type == "years_of_experience"){
      data_vector <- c("0-2 years", "3-5 years", "5+ years")
    }
  }
  
  if (data_type == "AI") {
    data_vector <- c("Yes", "No")
    cumulative_df <- data.frame(
      AI = c("Yes", "No"), 
      Frequency = rep(0, 2), 
      stringsAsFactors = FALSE
    )
  }
  
  # Turn the vector of job qualifications into string for the prompt
  data_string <- paste(data_vector, collapse = ", ")
  i = 9
  # Loop through each file and accumulate the results
  for (i in 1:9) {
    print(i)
    file <- paste0("sampleJobDescriptions", i, ".txt")
    text <- readLines(file, warn = FALSE)
    job_posting <- paste(text, collapse = "\n")
    
    # Define the prompt
    if (data_type == "AI") {
      prompt <- paste("go through this text file containing job 
                    postings (partitioned by lines of dashes) and look for 
                    instances where the job posting outlines a requirement of 
                    experience using AI tools or understanding of AI concepts, 
                    and record them into a csv file (containing 
                    merely yes or no for the AI column and its frequency). 
                    In essence, the postings should be scraped for the 
                    mention of and AI tools a posting one is 
                    mentioned as a requirement threshold but another is 
                    preferred record both for the same posting:\n\n", 
                    job_posting,"\n\n (your response should only be exactly 
                    like this with different numbers and actual types of 
                    qualification instead of x,y,z:\n 
                    AI,Frequency\nYes, 6\nNo, 4 \n\n and include all different 
                    data occurnaces that you didn't find one for must be 
                    placed into the table just alongside a 0 and once again do 
                    not add anything else in your response you should just 
                    output what has been shown above (between the '\n' 
                    symbols repetitively without any other textual response, 
                    do not output anything else (i.e. do not include 
                    aesthetic dividers around the response either)!!!")
      
    } else if (data_type == "other_qualifications"){
      prompt <- paste("go through this text file containing job postings 
                      (partitioned by lines of dashes) and look for instances 
                      where the job posting outlines a requirement of ", 
                      data_type, "and record them into a csv file (containing 
                      merely ", data_type," and its frequency). In essence, 
                      the postings should be scraped for the occurrences of any
                      term that doesn't pertain to typical job parameters such 
                      as education level, programming languages and stat models;
                      more so things like software packages and platforms, 
                      libraries within programming languages, certain hardwares 
                      and only those. if in a posting one is 
                      mentioned as a requirement threshold but another is 
                      preferred record both (a maximum of one of each specific 
                      qualification should be recorded per posting):\n\n", 
                      job_posting,"\n\n (your response should only be exactly 
                      like this with different numbers and actual types of 
                      qualification instead of x,y,z:\n "
                      ,data_type,",Frequency\nx, 6\ny, 4\nz, 1 
                      \n
                      and include all different 
                      data occurnaces that you didn't find one for must be 
                      placed into the table just alongside a 0 and once again do 
                      not add anything else in your response you should just 
                      output what has been shown above (between the '\n' 
                      symbols repetitively without any other textual response, 
                      do not output anything else (i.e. do not include 
                      aesthetic dividers around the response either)!!!")
    } else if (is.null(data_vector)){
      prompt <- paste("go through this text file containing job postings 
                      (partitioned by lines of dashes) and look for instances 
                      where the job posting outlines a requirement of ", 
                      data_type, "and record them into a csv file (containing 
                      merely the ", data_type," and its frequency). In essence, 
                      the postings should be scraped for the occurrences of 
                      the categories that relate to ", data_type," and only 
                      those; if in a posting one is 
                      mentioned as a requirement threshold but another is 
                      preferred record both (a maximum of one of each specific 
                      qualification should be recorded per posting):\n\n", 
                      job_posting,"\n\n (your response should only be exactly 
                      like this with different numbers and actual types of 
                      qualification instead of x,y,z:\n "
                      ,data_type,",Frequency\nx, 6\ny, 4\nz, 1 
                      \n
                      and include all different 
                      data occurnaces that you didn't find one for must be 
                      placed into the table just alongside a 0 and once again do 
                      not add anything else in your response you should just 
                      output what has been shown above (between the '\n' 
                      symbols repetitively without any other textual response, 
                      do not output anything else (i.e. do not include 
                      aesthetic dividers around the response either)!!!")
    }else {
      prompt <- paste("go through this text file containing job postings 
                      (partitioned by lines of dashes) and look for instances 
                      where the job posting outlines a requirement of ", 
                      data_type, "and record them into a csv file (containing 
                      merely the ", data_type," and its frequency). In essence, 
                      the postings should be scraped for the occurrences of ", 
                      data_string," and only those; if in a posting one is 
                      mentioned as a requirement threshold but another is 
                      preferred record both (a maximum of one of each specific 
                      qualification should be recorded per posting):\n\n", 
                      job_posting,"\n\n (your response should only be exactly 
                      like this with different numbers and actual types of 
                      qualification instead of x,y,z:\n "
                      ,data_type,",Frequency\nx, 6\ny, 4\nz, 1 
                      \n
                      and include all different 
                      data occurnaces that you didn't find one for must be 
                      placed into the table just alongside a 0 and once again do 
                      not add anything else in your response you should just 
                      output what has been shown above (between the '\n' 
                      symbols repetitively without any other textual response, 
                      do not output anything else (i.e. do not include 
                      aesthetic dividers around the response either)!!!")
    }
    
    # Make the API request to ChatGPT
    chatGPT_response <- POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(Authorization = paste("Bearer", api_key)),
      content_type_json(),
      encode = "json",
      body = toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(list(role = "user", content = prompt))
      ), auto_unbox = TRUE)
    )
    
    # Extract and parse the response content
    response_content <- content(chatGPT_response, as = "parsed")
    answer <- response_content$choices[[1]]$message$content
    
    # Print the response in plain text
    cat(answer, "\n")
    answer <- as.character(answer)
    
    # Split the response by new lines first to get rows
    split_answer <- strsplit(answer, "\n")
    
    if (length(split_answer) > 0 && length(split_answer[[1]]) > 0) {
      rows <- split_answer[[1]]
    }
    # Split each row by commas to get columns
    data_list <- lapply(rows, function(row) strsplit(row, ",")[[1]])
    
    # Find the maximum number of columns
    max_cols <- max(sapply(data_list, length))
    
    data_list <- lapply(data_list, function(x) {
      length(x) <- max_cols
      return(x)
    })
    
    # Initialize an empty data frame to store cumulative results
    
    df <- data.frame(
      col1 = data_vector,
      Frequency = rep(0, length(data_vector)), 
      stringsAsFactors = FALSE
    )
    if (i == 1){
    cumulative_df <- data.frame(
      col1 = data_vector, 
      Frequency = rep(0, length(data_vector)),
      stringsAsFactors = FALSE
    )
    }
    cumulative_df <- na.omit(cumulative_df)
    df <- na.omit(df)
    colnames(df)[1] <- data_type
    
    # Convert the list to a data frame
    df <- do.call(rbind, lapply(data_list, function(x) 
      data.frame(t(x), stringsAsFactors = FALSE)))
    
    # Set second row as column names and remove the first two rows
    colnames(df) <- df[2, ]
    df <- df[-1, , drop = FALSE]
    colnames(df) <- c(data_type, "Frequency")
    
    # Save the response to a text file
    writeLines(answer, paste0("response_", i, ".txt"))
    
    # Save the parsed data to a CSV file
    write.csv(df, paste0("education_levels_", i, ".csv"), row.names = FALSE)
    
    # Convert the Frequency column to numeric
    df <- na.omit(df)
    cumulative_df <- na.omit(cumulative_df)
    df$Frequency <- as.numeric(df$Frequency)
    df <- na.omit(df)
    # Ensure all data types are present in df
    #for (level in cumulative_df[1]) {
      #if (!(level %in% df[1])) {
        #df <- rbind(df, data.frame(
                          #data_type = level, 
                          #Frequency = 0, 
                          #stringsAsFactors = FALSE
                          #)
                    #)
      #}
    #}
    df <- na.omit(df)
    cumulative_df <- na.omit(cumulative_df)
    
    for (level in df[,1]) {
      print(level)
      
      if (level %in% cumulative_df[,1]){
        cumulative_df$Frequency[cumulative_df[,1] == level] <- 
          cumulative_df$Frequency[cumulative_df[,1] == level] + 
          df$Frequency[df[,1] == level]
      } else {
        cumulative_df <- 
          rbind(cumulative_df, c(level,df$Frequency[df[,1] == level]) )
      }
    }
  }
  
  # special colour code
  color_code <- rgb(152, 41, 50, maxColorValue = 255)
  
  # Plot the bar graph using ggplot2
  bar_plot <- 
    ggplot(cumulative_df, aes_string(x = data_type, y = "Frequency")) +
    geom_bar(stat = "identity", fill = color_code) + 
    labs(title = paste("Frequency of", data_type, "in Job Postings"),
         x = data_type,
         y = "Frequency") +
    theme_minimal()
  
  # Calculate the total frequency sum
  freq_sum <- sum(cumulative_df$Frequency)
  
  # Calculate the proportions
  cumulative_df <- cumulative_df %>%
    mutate(Proportion = Frequency / freq_sum)
  
  # Plot the proportional bar graph using ggplot2
  proportion_plot <- 
    ggplot(cumulative_df, aes_string(x = data_type, y = "Proportion")) +
    geom_bar(stat = "identity", fill = color_code) + 
    labs(title = paste("Distribution of", data_type, "in Job Postings"),
         x = data_type,
         y = "Proportion") +
    theme_minimal()
  
  # Return both plots as a list
  return(list(bar_plot = bar_plot, proportion_plot = proportion_plot,
              cumulative_df = cumulative_df))
}

print(bar_plot)
print(proportion_plot)


# Example usage - Education Level
data_type <- "education_level"
data_vector <- c("Bachelor's", "Master's", "PhD")

educ_plots <- job_description_graph(data_type, data_vector)

print(educ_plots$bar_plot)
print(educ_plots$proportion_plot)

# Example usage - AI
data_type <- "AI"
data_vector <- c("Yes","No")

AI_plots <- job_description_graph(data_type, data_vector)

print(AI_plots$bar_plot)
print(AI_plots$proportion_plot)
