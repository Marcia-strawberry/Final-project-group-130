library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(DT)
library(plotly)
library(tidyverse)
library(shinyWidgets)
require(maps)
require(viridis)
library(RColorBrewer)
library(patchwork)

#Read the dataste
data <- read_csv("data_drop_2cols.csv")
data_all <- read_csv("Cleaned Data.csv")
genres <- read_csv("genres_two_columns.csv")
countries <- read_csv("countries_two_columns.csv")
regioncode <- read_csv("regioncode.csv")

# Clean the data without 2 cols
data <- data %>% 
  drop_na(title)
data$description[is.na(data$description)] <-"Not Applicable"
data$imdb_score[is.na(data$imdb_score)] <-0
data$tmdb_score[is.na(data$tmdb_score)] <-0
data$imdb_votes[is.na(data$imdb_votes)] <-0
data$tmdb_popularity [is.na(data$tmdb_popularity )] <-0
data$title <- iconv(data$title, from = 'UTF-8', to = 'ASCII//TRANSLIT') 

# Clean countries
Count <-countries %>%
  group_by(countries) %>%
  summarise(obcount = n())
country_list<-Count %>%
  arrange(desc(obcount) )%>%
  filter(row_number() <=15) %>%
  left_join(regioncode, by = "countries") %>%
  subset(select=-c(obcount))

countries <-countries %>%
  full_join(country_list, by = "countries")

countries$name[is.na(countries$name)] <- "Other countries"

country_list_by_order = countries %>%
    distinct(name)%>%
    pull()

# Clean the data with all cols
data_all <- data_all %>% 
  drop_na(title)
data_all$description[is.na(data_all$description)] <-"Not Applicable"
data_all$imdb_score[is.na(data_all$imdb_score)] <-0
data_all$tmdb_score[is.na(data_all$tmdb_score)] <-0
data_all$imdb_votes[is.na(data_all$imdb_votes)] <-0
data_all$tmdb_popularity [is.na(data_all$tmdb_popularity )] <-0
data_all$title <- iconv(data_all$title, from = 'UTF-8', to = 'ASCII//TRANSLIT') 
data_all$genres <- gsub("[]['']","" ,data_all$genres,ignore.case = TRUE)
data_all$production_countries <- gsub("[]['']","" ,data_all$production_countries,ignore.case = TRUE)

## Clean the data of map
datamap <- read_csv("coutry_combined_clean.csv")
datamap1 <- datamap[,-c(1,4:12)]
count <- datamap1 %>% 
  group_by(countries) %>%
  summarise(n=n()) %>%
  arrange(desc(n))%>%
  head(10)
region <- read_csv("all2.csv")
region <- region[,-(3:11)]

map <- map_data("world")
map <- map[,-c(4,6)]

world_map <- map %>%
  left_join(region, by = c("region"="name"))

final_map <- world_map %>%
  left_join(count, by = c("alpha-2"="countries"))%>%
  filter(!is.na(n))

## Creat the dataset for analysis part
country_combine <- read_csv("coutry_combined_clean.csv")
genre_combine <- left_join(genres, data, by = "ID")
country_match <- read_csv("all2.csv")[,1:2]

#---------------- UI Part------------------------

ui <- dashboardPage(
  skin = "midnight",
  dashboardHeader(title = span("Platform Selection ", style = "color: white;  font-weight: bold")),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("book")),
      menuItem("Movies", tabName = "movie", icon = icon("film")),
      menuItem("Distributions", tabName = "distribution", icon = icon("globe")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-area")),
      menuItem("Data", tabName = "data", icon = icon("database"))
    )
  ),
  
  dashboardBody(

    tabItems(
    tabItem(tabName = "intro",
            fluidRow(
              column(
                width = 12,
                box(
                  title = tags$strong("Data Source"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tags$div(
                    "Our datasets are retrieved from",
                    tags$strong("Kaggle"),
                    "website and contain basic information of movies and shows from different platforms including.",
                    style = "font-size:15px"
                  ),
                  
                  a("HBO Max", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/hbo-max-tv-shows-and-movies", targets =
                      "_blank", "\t"),
                  a("Amazon_Prime", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/amazon-prime-tv-shows-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Netflix", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/netflix-tv-shows-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Disney+", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/disney-tv-shows-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Hulu", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/hulu-tv-shows-and-movies?select=titles.csv", targets =
                      "_blank"),
                  br(),
                  a("Rakuten Viki", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/rakuten-tv-dramas-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Crunchyroll", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/crunchyroll-animes-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Paramount", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/paramount-tv-shows-and-movies?select=titles.csv", targets =
                      "_blank"),
                  a("Dark_Matter", href =
                      "https://www.kaggle.com/datasets/victorsoeiro/dark-matter-tv-shows-and-movies", targets =
                      "_blank")
                ),
               
                 box(
                  title = tags$strong("Main Content"),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  tags$span(
                    tags$li(
                      tags$strong("Movies:"),
                      "Search for movies and use filters to find top-rated movies."
                    ),
                    tags$li(
                      tags$strong("Distributions:"),
                      "The plot shows the distribution and a total number of movies in top 10 countries."
                    ),
                    tags$li(
                      tags$strong("Analysis:"),
                      "The 4 plots show the analysis of different platforms that are important components of mainstream media in the film industry, by using factors including the IMDB votes, TMDB scores, countries, and genres. "
                    ),
                    tags$li(tags$strong("Data:"), "Raw datasets."),
                    style = "font-size:15px"
                  )
                ),
                box(
                  title = tags$strong("Team Members"),
                  status = "primary",
                  width = NULL,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  column(12,tags$div(
                    fluidRow( 
                      column(2,img(src ="zhangshiyu.jpg",height = 100, width = 100)),
                      column(10,style = "font-size:16px",
                             tags$strong("Shiyu Zhang"),
                             br(),
                             tags$li("szhan156@jh.edu"),
                             tags$li("Master of Science in Business Analytics and Risk Management from the Johns Hopkins University."),
                             tags$li("Passionate data analyst with abilities to solve complex problems successfully using solid computer skills, logical reasoning techniques, and strong attention to detail.
                                     Highly organized with excellent time management and multitasking abilities. Talented team leader with superb interpersonal and communication skills."),
                             tags$li("In this project, she is responsible for the design of the Movie Tab including both UI and Server codes.")))),br(),),
                  column(12,tags$div(
                    fluidRow( 
                      column(2,img(src ="shiqing.jpg",height = 100, width = 100)),
                      column(10,style = "font-size:16px",
                             tags$strong("Qing Shi"),
                             br(),
                             tags$li("qshi12@jh.edu"),
                             tags$li("Master of Science in Business Analytics and Risk Management from the Johns Hopkins University."),
                             tags$li("In this project, she is responsible for the design of distribution and introduction tabs including both UI and Server codes.")))),br(),),
                  column(12,tags$div(
                    fluidRow( 
                      column(2,img(src ="wunanxi.jpg",height = 100, width = 100)),
                      column(10,style = "font-size:16px",
                             tags$strong("Nanxi Wu"),
                             br(),
                             tags$li("nwu12@jh.edu"),
                             tags$li("Master of Science in Business Analytics and Risk Management from the Johns Hopkins University."),
                             tags$li("In this project, she is responsible for the data cleaning, and the design of Analysis Tab including both UI and Server codes.")))),br(),),
                  column(12,tags$div(
                    fluidRow( 
                      column(2,img(src ="rachellee.jpg",height = 100, width = 100)),
                      column(10,style = "font-size:16px",
                             tags$strong("Rachel Lee"),
                             br(),
                             tags$li("rlee134@jh.edu"),
                             tags$li("Master of Science in Business Analytics and Risk Management from the Johns Hopkins University."),
                             tags$li("In this project, she is responsible for the design of Data tab including both UI and Server codes.")))),br(),
                    
                    tags$div(
                      "Feel free to contact us via email if you have any concerns related to this App."
                    ),
                    style = "font-size:15px"
                  )
                )
              )
            ))
    ,

    
    tabItem(tabName = "movie",
          fluidRow(
              
              box(        
                title = strong("Select one platform",style="color:white"),
                width = 12,
                status = "primary",
                height = "200px",
                

                   radioGroupButtons(
                     inputId = "platformfilter",
                     label = "Find your favourite platform:",
                     choices = c("Amazon Prime", 
                                 "Netflix",
                                 "Disney+",
                                 "Hulu", 
                                 "Rakuten Viki",
                                 "Crunchyroll", 
                                 "Paramount", 
                                 "Dark Matter", 
                                 "HBO Max"),
                     status = "primary"
                   )
              )
            ),
            
        
          fluidRow(
              
              box(        
                title = strong("Search",style="color:white"),
                width = 12,
                status = "primary",
                height = "200px",
                
                searchInput(
                  inputId = "searchbytxt",
                  label = "Find what you like from top rated movies:", 
                  placeholder = "Type here",
                  btnSearch = icon("search"), 
                  btnReset = icon("remove"),
                  width = "100%"
                ),
                
                tableOutput("txttable")
                
              )
            ),
            
            
      fluidRow(
        
        box( 
          title = strong("Filter",style="color:white"),
          width = 12,
          status = "primary",
          height = 100,

        column(width = 12,
            
            sliderInput(
              inputId = "yearfilter",
              label = h4("Year",style="color:white"),
              min = 1900,
              max = 2022,
              value = c(1900, 2022))),
        
            column(width = 2,
            awesomeRadio(
                     inputId = "typefilter",
                     label =h4("Type",style="color:white"), 
                     choices = c("SHOW", "MOVIE"),
                     selected = "SHOW",
                     inline = TRUE, 
                     status = "success"
                   )
             ),
          
            column(width = 5, 
              tags$h4("Genre",style="color:white"),
              pickerInput(
                inputId = "genrefilter",
                label = "Select/deselect all genres:", 
                choices = c(
                  "action", "animation", "comedy", "crime",     
                  "documentation", "drama", "european", "family",    
                  "fantasy", "history", "horror", "music" ,       
                  "reality", "romance", "scifi", "sport",     
                  "thriller", "war", "western" ),
                options = list(
                  `actions-box` = TRUE), 
                multiple = TRUE
                  )
            ),
    
            column(width = 5,
                tags$h4("Country",style="color:white"),
                pickerInput(
                  inputId = "countryfilter",
                  label = "Select/deselect all countries:", 
                  choices = sort(country_list_by_order),
                  options = list(
                    `actions-box` = TRUE), 
                  multiple = TRUE
          
              )
                ),

      column(width = 12,
         column(
              width = 6,
              height = 50,
              sliderInput(
                "imdbfilter",
                label = h4("IMDB Score",style="color:white"),
                min = 0,
                max = 10,
                value = c(0, 10)
              )
            ),

        column(
          width = 6,
          height = 50,
          sliderInput(
            "tmdbfilter",
            label = h4("TMDB Score",style="color:white"),
            min = 0,
            max = 10,
            value = c(0, 10)
          )
        ),
       
         tags$div(
          "Tip: You need to apply all the filters."
        ),
        
)),



      box(title = strong("Results",style="color:white"),
        width = 12,
        status = "primary",

        column(width =12,
        numericInput("n_movie_input", "Select Number of Top Rated Movies (1-10)",
                     value = 10, min = 1, max = 10, step = 1),
        
        tags$div(
          "Tip: If you don't find any results, try adding more filters and tags."
        ),
        
        plotlyOutput('barplot')
        )
      )

      


)),


    tabItem(tabName = "distribution",
            plotlyOutput('myMap')            ),
    
    tabItem(tabName = "analysis",
            checkboxInput("do1", "Top 10 total amount of movies produced countries", value = F),
            checkboxInput("do2", "Total amount of movies in each genre", value = F),
            checkboxInput("do3", "Average IMDB Scores of each movies in each platfrom", value = F),
            checkboxInput("do4", "Average TMDB Scores and Popularity of each movies in each platform", value = F),
            plotOutput("plot1"),
            plotOutput("plot2"),
            plotOutput("plot3"),
            plotOutput("plot4"),
            ),
    
    tabItem(tabName = "data",
            #dataTableOutput("myTable")
            DT::DTOutput("myTable"))
  
)
)
)





# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # -------------------Movie page----------------------------  
  
  #search by txt
  
  output$txttable <- renderTable({
    input$searchbytxt
    str = data_all%>%
      filter(input$searchbytxt == title)%>%
      filter(platform == input$platformfilter) %>% 
      subset(select=-c(1))
  })
  #
  
  #filter search
  ##filter genrate
  filter_reactive<- reactive({

    genre_filter_id <- genres %>% 
      filter(genres %in% input$genrefilter) %>%
      distinct(ID) %>%
      pull()
     
    
    country_filter_id <- countries %>% 
        filter(name %in% input$countryfilter ) %>%
        distinct(ID) %>%
        pull()
    
  
    
    filter_df <-data %>% 
          
          filter(platform == input$platformfilter) %>% 
          filter(release_year >= input$yearfilter[1] &release_year <= input$yearfilter[2] ) %>% 
          filter(type == input$typefilter)%>%
          filter(imdb_score >= input$imdbfilter[1] &imdb_score <= input$imdbfilter[2] ) %>% 
          filter(tmdb_score >= input$tmdbfilter[1] &tmdb_score <= input$tmdbfilter[2] ) %>%  
          filter(ID %in% genre_filter_id & ID %in% country_filter_id) %>% 
          mutate(average_score = imdb_score+tmdb_score) %>%
          arrange(desc(average_score)) %>%
          filter(row_number() <= input$n_movie_input)

  }  )
     
    ## Generate barplot
   barplot <- reactive(
      ggplot(filter_reactive(
                                     ),
             aes(x = title, y = imdb_score+tmdb_score)
            )+
      geom_col(fill = "#FFDEAD")+
      
      coord_flip(ylim = axis_range())+
        theme_bw()+
        theme(axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major.y = element_blank())+
        labs(x = "Movie", y = "Total scores",
             title = paste("Top", input$n_movie_input, "Movies with Highest IMDB and TMDB Scores"))
      

  )
   
   axis_range <- reactive(
     c(floor=0, ceiling=20)
   )
   
   barplotly <- reactive(
       ggplotly(barplot(), height = 530, tooltip = 'text') %>% 
         add_annotations(x = rep(axis_range()[1],input$n_movie_input),
                         y = c(1:input$n_movie_input),
                         text = filter_reactive()$title,
                         xref = "x",
                         yref = "y",
                         showarrow = FALSE,
                         xanchor = 'left')
   )
   
   
   
   output$barplot <- renderPlotly(
     barplotly( )

     )
   #   

# -------------------Distribution page----------------------------

   ##Generate map
   output$myMap <- renderPlotly({

   mp <- final_map %>%
     ggplot(aes(x = long, y = lat, group = group)) +
     geom_polygon(aes(fill= n), colour = "white") +
     scale_x_continuous(breaks = seq(-180, 210, 45), labels = function(x){paste0(x, "°")}) +
     scale_y_continuous(breaks = seq(-60, 100, 30), labels = function(x){paste0(x, "°")}) +
     scale_fill_gradient(low = "lightblue", high="steel blue") +
     labs(title="Movie Distributions",
          y="Latitude", x="Longitude",
          subtitle = "The plot shows the total number of movies made by top 10 country and their distribution.\n"
     ) +
     guides(fill = guide_legend(title ="Amount of movies"))+
     theme_light() 
     })

# -------------------Analysis page----------------------------

   ## Plot prepared
   ## The total amount of movies in each countries(top 10)
     count1 <- country_combine %>%
       group_by(countries) %>%
       summarise(n=n())
     plot1_data <- left_join(count1, country_match, by = c("countries" = "alpha-2")) 
     
     plot1.cols <- 10
     mycolors1 <- colorRampPalette(brewer.pal(8, "Blues"))(plot1.cols)
     
     plot1 <- plot1_data[order(desc(plot1_data$n)),][1:10,] %>%
       ggplot(aes(x=name, y=n, fill = name) )+
       geom_bar(stat='identity')+
       geom_text(aes(label=n), position=position_dodge(width = 0.9), vjust=-0.40)+
       scale_fill_manual(values = mycolors1) +
       theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank())
     
     plot1 <- plot1+
       labs(
         title = "Top 10 total amount of movies produced countries ",
         caption = "Data source: Kaggle",
         x = "Country",
         y = "Total amount of movies"
       )
     plt1 <- reactive({
       input$do1
       if(input$do1){
         return(plot1)
       }
     })
     
     ## The total amount of movies in each genres
     count2 <- genre_combine %>%
       group_by(genres) %>%
       summarise(n=n())
     
     plot2.cols <- 19
     mycolors2 <- colorRampPalette(brewer.pal(8, "Blues"))(plot2.cols)
     
     plot2 <- count2[order(desc(count2$n)),] %>%
       ggplot(aes(x=genres, y=n, fill = genres) )+
       geom_bar(stat='identity')+
       geom_text(aes(label=n), position=position_dodge(width = 0.9), vjust=-0.40)+
       scale_fill_manual(values = mycolors2)+
       theme(axis.text.y=element_blank(), axis.ticks.y = element_blank(),
             panel.grid.major = element_blank(), panel.grid.minor = element_blank())
     
     plot2 <- plot2+
       labs(
         title = "Total amount of movies in each genre ",
         caption = "Data source: Kaggle",
         x = "Genre",
         y = "Total amount of movies"
       ) 
     
     plt2 <- reactive({
       input$do2
       if(input$do2){
         return(plot2)
       }
       })
     
     ## The platform with the average score and votes in IMDB
     imdb_score <- aggregate(data$imdb_score, by=list(platform = data$platform), mean, na.rm = TRUE)
     imdb_votes<- aggregate(data$imdb_votes, by=list(platform = data$platform), mean, na.rm = TRUE)
     imdb <- left_join(imdb_score,imdb_votes, by = "platform")
     tmdb_score <- aggregate(data$tmdb_score, by=list(platform = data$platform), mean, na.rm = TRUE)
     tmdb_pop <- aggregate(data$tmdb_popularity, by=list(platform = data$platform), mean, na.rm = TRUE)
     tmdb <- left_join(tmdb_score,tmdb_pop, by = "platform")
     
     plot3 <- imdb %>%
       ggplot(aes(x = platform)) +
       geom_line(aes(y = x.x), group = 1, col = "steelblue", lwd = 0.8)+
       labs(title = "Average IMDB Scores of movies in each platfrom",
            x = "Platform",
            y = "Scores")+
       theme_minimal()
     
     plot4 <- imdb %>%
       ggplot(aes(x = platform)) +
       geom_bar(aes(y = x.y), stat = "identity", col = "steelblue", fill ="steelblue")+
       labs(title = "Average IMDB Votes of movies in each platfrom",
            x = "Platform",
            y = "Votes")+
       theme_minimal()
     
     plot5 <- plot3/plot4 
     
     plt3 <- reactive({
       input$do3
       if(input$do3){
         return(plot5)
       }
     })
     
     ## The platform with the average score and Popularity in TMDB
     
     plot6 <- tmdb %>%
       ggplot(aes(x = platform)) +
       geom_line(aes(y = x.x, group = 1, color = "Scores"), lwd=0.8) +
       geom_point(aes(y = x.x))+
       geom_line(aes(y = x.y, group = 1, color = "Popularity"),lwd=0.8) +
       geom_point(aes(y = x.y))+
       scale_color_manual('Metric', values= c("darkgreen", "steelblue")) +
       labs(title = "Average TMDB Scores and Popularity of movies in each platfrom ",
            x = "Platform",
            y = "Scores/Popularity")+
       theme_minimal()
     
     plt4 <- reactive({
       input$do4
       if(input$do4){
         return(plot6)
       }
     })
     
     output$plot1 <- renderPlot({
       plt1()
     })
     output$plot2 <- renderPlot({
       plt2()
     })
     output$plot3 <- renderPlot({
       plt3()
     })
     output$plot4 <- renderPlot({
       plt4()
     })

#-------------Data Page     
     
     output$myTable <- DT::renderDT(
       expr = data_all,
       caption = "Raw data",
       style = "bootstrap4",
       rownames = TRUE,
       options = list(
         responsive = TRUE,
         scrollX = TRUE,
         "pageLength" = 5,
         columnDefs = list(
           list(className = 'dt-center',
                targets = 1:ncol(data_all)
                )
         )
       )
     ) 
     
          
}




# Run the application
shinyApp(ui = ui, server = server)
