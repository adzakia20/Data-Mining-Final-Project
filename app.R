##VISUALIZATION

#setwd("C:/Users/Asus/OneDrive - Institut Teknologi Sepuluh Nopember/KULIAH/Semester 6/Data Mining dan Visualisasi/Final Project")
getwd()

library(plotly)
library(readr)
library(ggplot2)
library(wordcloud)
library(tm)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(stringr)

df = read.csv("emails.csv", header = TRUE, sep = ",")
View(df)

#===Word Cloud===
get_word_freq <- function(df) {
  df %>%
    select(-'Email.No.', -Prediction) %>%
    pivot_longer(everything(), names_to = 'word', values_to = 'count') %>%
    group_by(word) %>%
    summarize(frequency = sum(count)) %>%
    arrange(desc(frequency))
}

#Get word frequencies
word_freq <- get_word_freq(df)
View(word_freq)

#Filtered word
filtered_word <- word_freq[grepl("^.{3,}$", word_freq$word), ]
View(filtered_word)

wordcloud(words = filtered_word$word, freq = filtered_word$frequency, scale = c(6, 1),
          max.words = 100, random.order = FALSE, colors = c("#2D56B2","#78A8CE","#49525E"))

#===Distribution of email length===
email_lengths <- df %>%
  select(-'Email.No.') %>%
  mutate(across(everything(), as.character)) %>%
  rowwise() %>%
  mutate(email_length = sum(nchar(c_across(everything())))) %>%
  ungroup()

#Histogram
plot1 <- ggplot(email_lengths, aes(x = email_length, fill = as.factor(df$Prediction))) +
  geom_histogram(binwidth = 50, position = 'identity', alpha = 0.5) +
  labs(x = 'Email Length (Characters)',
       y = 'Frequency',
       fill = 'Prediction') +
  scale_fill_manual(values = c("#2D56B2", "#D9D9D9")) + 
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(email_lengths$email_length), by = 500))

#===Word frequency per spam email===
#Define stopwords in English
stop <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", "should", "now")

#Subset columns excluding stopwords and select rows where Prediction is 1
df_word = df[2:3001]
col_subset <- names(df_word)[!tolower(names(df_word)) %in% tolower(stop)]
spam_df <- df[df$Prediction == 1, col_subset]

#Function to binarize counts
binarize <- function(keyword_list) {
  count_list <- ifelse(keyword_list < 1, 0, 1)
  return(count_list)
}

#Apply binarize function to spam_df, excluding 'Email No.' and 'Prediction' columns
word_freq_df <- spam_df[, !(names(spam_df) %in% c("Email No.", "Prediction"))]
word_freq_df <- apply(word_freq_df, 2, binarize)
word_freq_df <- colSums(word_freq_df)
word_freq_df <- data.frame(word = names(word_freq_df), count = word_freq_df)
word_freq_df <- word_freq_df[order(word_freq_df$count, decreasing = TRUE), ]
filtered_wordcount <- word_freq_df[grepl("^.{3,}$", word_freq_df$word), ]

#Select top 50 words
filtered_wordcount <- head(filtered_wordcount, 50)
filtered_wordcount

#Histogram
plot2 <- ggplot(filtered_wordcount, aes(x = reorder(word, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#2D56B2", color = "#F3F8FF") +
  labs(x = 'Word',
       y = 'Count') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#===Percentage of Spam vs Non-spam===
spam_count = sum(df$Prediction==1)
non_spam_count = nrow(df) - spam_count
total_emails = nrow(df)

#Calculate percentages
spam_percent <- (spam_count / total_emails) * 100
non_spam_percent <- (non_spam_count / total_emails) * 100

#Data frame
data_pie <- data.frame(
  category = c("Spam", "Non-spam"),
  count = c(spam_count, non_spam_count),
  percent = c(spam_percent, non_spam_percent)
)

#Pie chart
plot4 <- ggplot(data_pie, aes(x = 2, y = percent, fill = category)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0) +
  scale_fill_manual(values = c("#D9D9D9", "#2D56B2")) +
  geom_text(aes(label = sprintf("%1.1f%%", percent)), position = position_stack(vjust = 0.5), color = "black") +
  theme_void() +
  xlim(0.5, 2.5) + 
  theme(plot.title = element_text(hjust = 0.5))

#=========================================================================================================================================================================
##SHINY DASHBOARD

#Library
library(shiny)
require(shinydashboard)
library(ggplot2)
library(moments)
library(dplyr)
library(DT)
library(rsconnect)

#Dashboard Header
header <- dashboardHeader(title = "Email Dashboard",
                          tags$li(class = "dropdown",
                                  tags$style(HTML('.main-header {background-color: #2D56B2;} 
                                           .main-header .logo {background-color: #2D56B2;} 
                                           .main-header .navbar {background-color: #2D56B2;}'))))  

#Sidebar Content of the Dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "Home", icon = icon("home")),
    menuItem("Descriptive Statistics", tabName = "Statdesc", icon = icon("square-poll-horizontal")),
    menuItem("Summary & Visualization", tabName = "Summary", icon = icon("chart-simple")), 
    menuItem("Database", tabName = "Database", icon = icon("database")),
    menuItem("Author", tabName = "Profile", icon = icon("user"))
  )
)

#Dashboard Body Content
frow11 <- fluidRow(
  column(width = 12,
         box(status = "info",
             solidHeader = TRUE,
             width = 12,
             align = "middle",
             imageOutput("myImage")
            )
  )
)

frow12 <- fluidRow(
  column(width = 12,
         box(title = "Background",
             status = "info",
             solidHeader = TRUE,
             width = 12,
             uiOutput("longText") #verbatimTextOutput
            )
  )
)

frow21 <- fluidRow(
  column(width = 12,
         fileInput("file1", "Choose CSV File",
                   accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
         selectInput("var", "Variable", choices = NULL),
         selectInput("plot_type", "Graph type",
                     choices = c("Histogram", "Boxplot")),
         checkboxGroupInput("summary_stats", "Summary Statistics",
                            choices = c("Count" = "count",
                                        "Mean" = "mean",
                                        "Median" = "median",
                                        "Standard deviation" = "sd",
                                        "Variance" = "var",
                                        "Min" = "min",
                                        "Max" = "max",
                                        "Quartiles" = "quantile",
                                        "Skewness" = "skew",
                                        "Kurtosis" = "kurt",
                                        "Show data table" = "data_table"))

         )
)

frow22 <- fluidRow(
  column(width = 12,
         tabsetPanel(
           tabPanel("Summary", verbatimTextOutput("summary")),
           tabPanel("Graph", plotOutput("plots")),
           tabPanel("Data", tableOutput("tabledata"))
         )
  )
)


frow31 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

frow32 <- fluidRow(
  box(
    title = "Distribution of Email Lengths by Prediction Class"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("dist_email", height = "300px")
  )
  
  ,box(
    title = "Words Most Frequently Seen in Emails"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("word_cloud", height = "300px")
  ) 
  
)

frow33 <- fluidRow(
  box(
    title = "Word Frequency Per Spam Email"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("word_freq_spam", height = "300px")
  )
  
  ,box(
    title = "Percentage of Spam vs Non-spam"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("pie", height = "300px")
  ) 
  
)

frow41 <- fluidRow(
  box(title = "Database of Email Spam Classification",
      status = "info",
      solidHeader = TRUE,
      width = 12,
      DTOutput("table")
  )
)

frow51 <- fluidRow(
  box(
    width = 6,
    title = "Author 1",
    status = "info",
    solidHeader = TRUE,
    style = "padding: 20px;",
    imageOutput("author1"),
    verbatimTextOutput("author1Name"),
    div(style = "text-align: center;",
        a(href = "https://www.instagram.com/ima.az", target = "_blank",
          icon("instagram", lib = "font-awesome", class = "fa-3x", 
               style = "color: #1DA1F2; margin-right: 20px;")),
        a(href = "www.linkedin.com/in/dzakiafathimatul", target = "_blank",
          icon("linkedin", lib = "font-awesome", class = "fa-3x", 
               style = "color: #0077B5;"))
    )
  )
  
  ,box(
    width = 6,
    title = "Author 2",
    status = "info",
    solidHeader = TRUE,
    style = "padding: 20px;",
    imageOutput("author2"),
    verbatimTextOutput("author2Name"),
    div(style = "text-align: center;",
        a(href = "https://www.instagram.com/nabilahazharudin", target = "_blank",
          icon("instagram", lib = "font-awesome", class = "fa-3x", 
               style = "color: #1DA1F2; margin-right: 20px;")),
        a(href = "https://www.linkedin.com/in/nabilahazharudin?utm_source=share&utm_campaign=share_via&utm_content=profile&utm_medium=android_app", target = "_blank",
          icon("linkedin", lib = "font-awesome", class = "fa-3x", 
               style = "color: #0077B5;"))
    )
  )
)


#Dashboard Body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Home", h2("Welcome to Home Page"), frow11, frow12),
    tabItem(tabName = "Statdesc", h2("Desciptive Statistics"), frow21, frow22),
    tabItem(tabName = "Summary", frow31, frow32, frow33),
    tabItem(tabName = "Database", h2("Database"), frow41),
    tabItem(tabName = "Profile", h2("About the Author"), frow51)
  )
)

#UI Dashboard Page
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')

options(shiny.maxRequestSize = 30*1024^2)

#Server Functions for the Dashboard  
server <- function(input, output, session) { 
  
  #Tab Item 1
  output$myImage <- renderImage({
    list(src = "emails.jpeg", 
         contentType = "image/jpeg", 
         width = 795, 
         height = 400,
         align = "middle")
  }, deleteFile = FALSE)
  
  output$longText <- renderUI({
    HTML(
      '<div style="white-space: pre-wrap; overflow-y: auto; height: 200px;">
       Seiring kemajuan era digital, email telah menjadi salah satu sarana komunikasi yang paling umum digunakan. Penggunaan email secara umum dapat diklasifikasikan untuk keperluan pribadi maupun bisnis. Namun, seiring peningkatan dari penggunaan email, muncul pula salah satu masalah yang signifikan seperti spam. Spam adalah email yang tidak diinginkan dan seringkali dikirim dalam jumlah besar dengan tujuan promosi, phishing, atau penyebaran malware. Email spam ini dapat mengganggu produktivitas, membahayakan keamanan informasi, dan yang paling parah dapat juga mengakibatkan kerugian finansial.
       Untuk mengatasi masalah ini, sistem klasifikasi email yang efektif diperlukan untuk memisahkan email non-spam dari email spam. Salah satu pendekatan yang umum digunakan adalah dengan menerapkan teknik machine learning yang memanfaatkan data untuk melatih model dalam mengenali karakteristik spam. Dataset yang digunakan dalam studi ini berisi informasi terkait 5172 email yang dipilih secara acak dan label masing-masing untuk klasifikasi spam maupun non-spam.
       </div>'
    )
  })
  
  #Tab Item 2
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  observe({
    req(data())
    updateSelectInput(session, "var", choices = names(data()))
  })
  
  output$summary <- renderPrint({
    req(data(), input$var)
    df <- data()
    y <- df[[input$var]]
    stats <- list()
    if ("count" %in% input$summary_stats) stats$count <- sum(!is.na(y))
    if ("mean" %in% input$summary_stats) stats$mean <- mean(y, na.rm = TRUE)
    if ("median" %in% input$summary_stats) stats$median <- median(y, na.rm = TRUE)
    if ("sd" %in% input$summary_stats) stats$sd <- sd(y, na.rm = TRUE)
    if ("var" %in% input$summary_stats) stats$var <- var(y, na.rm = TRUE)
    if ("min" %in% input$summary_stats) stats$min <- min(y, na.rm = TRUE)
    if ("max" %in% input$summary_stats) stats$max <- max(y, na.rm = TRUE)
    if ("quantile" %in% input$summary_stats) stats$quantile <- quantile(y, na.rm = TRUE)
    if ("skewness" %in% input$summary_stats) stats$skew <- skewness(y, na.rm = TRUE)
    if ("kurtosis" %in% input$summary_stats) stats$kurt <- kurtosis(y, na.rm = TRUE)
    if (length(stats) > 0) print(stats)
  })
  
  output$plots <- renderPlot({
    req(data(), input$var, input$plot_type)
    df <- data()
    y <- df[[input$var]]
    
    if (input$plot_type == "Histogram") {
      hist(y, main = paste("Histogram of", input$var), xlab = input$var)
    } else if (input$plot_type == "Boxplot") {
      boxplot(y, main = paste("Boxplot of", input$var), ylab = input$var)
    } 
  })
  
  output$tabledata <- renderTable({
    req(data())
    if ("data_table" %in% input$summary_stats) {
      return(data())
    }
  })
  
  #Tab Item 3
  top_filtered_word <- filtered_word %>% filter(frequency==max(frequency))
  top_filtered_wordcount <- filtered_wordcount %>% filter(count==max(count))
  spam_count = sum(df$Prediction==1)
  non_spam_count = nrow(df) - spam_count
  total_emails = nrow(df)
  spam_percentage <- (spam_count / total_emails) * 100
  
  #Create the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(top_filtered_word$frequency)
      ,paste('Most Common Word:', top_filtered_word$word)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "blue")
  })
  
  output$value2 <- renderValueBox({
    valueBox(
      formatC(top_filtered_wordcount$count)
      ,paste('Top Word in Spam Email:', top_filtered_wordcount$word)
      ,icon = icon("sort-by-alphabet",lib='glyphicon')
      ,color = "blue")
    
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      paste0(formatC(spam_percentage, format = "f", digits = 2), "%")
      ,paste('Spam Email Percentage')
      ,icon = icon("equalizer",lib='glyphicon')
      ,color = "blue")
  })
  
  #Create the plotOutput content
  output$dist_email <- renderPlot({
    plot1
  })
  
  output$word_cloud <- renderPlot({
    wordcloud(words = filtered_word$word, freq = filtered_word$frequency, scale = c(4.5, 0.6),
              max.words = 100, random.order = FALSE, colors = c("#2D56B2","#78A8CE","#49525E"))
  })
  
  output$word_freq_spam <- renderPlot({
    plot2
  })
  
  output$pie <- renderPlot({
    plot4
  })
  
  #Tab Item 4
  output$table <- renderDataTable({
    df
  })
  
  #Tab Item 5
  output$author1 <- renderImage({
    list(src = "author1.png", 
         contentType = "image/png", 
         width = "90%", 
         height = "auto",
         align = "middle")
  }, deleteFile = FALSE
  )
  
  output$author1Name <- renderText({
    "Dzakia Fathimatul Azmi"
  })
  
  output$author2 <- renderImage({
    list(src = "author2.jpeg", 
         contentType = "image/jpeg", 
         width = "90%", 
         height = "auto",
         align = "middle")
  }, deleteFile = FALSE
  )
  
  output$author2Name <- renderText({
    "Nabilah Azharudin"
  })
}

shinyApp(ui, server)