library(shiny)
library(shinythemes)
library(archiveRetriever)
library(tidyverse)
library(lubridate)
library(imputeTS)
library(patchwork)
library(uuid)
library(fmsb)
library(GGally)

# Wrap entire app in function per https://mastering-shiny.org/scaling-packaging.html#converting-an-existing-app
runApp <- function(...){
  
# Define UI for application that draws a histogram ----
ui <- fluidPage(theme = shinytheme("darkly"),

    # Application title
    titlePanel("#Saddermetrics 0.2 ___ Coεmeta web app prototype"),

    # Sidebar with a slider input for number of bins -----
    sidebarLayout(
        sidebarPanel(width = 3, 
          textInput("handle", "Enter twitter handle (without '@')", 
                    value = "", width = NULL, placeholder = NULL),
          
          dateRangeInput("dates", label = "Date range",
                         start = "2021-01-01", end = today(),
                         min = "2013-01-01", max = today()),
          helpText(tags$i("longer ranges take longer to process.  
                   can't go earlier than 2013"), style="color:gray"),
          
          br(),
          actionButton("checkArchive", label = "Search for User data"),
          helpText(tags$i("(do this first. try bigger accounts)"), style="color:gray"),
          
          br(),
          actionButton("scrapeArchive", label = "Scrape Available User data"),
          helpText(tags$i("(this can take a while, 
                          the data will appear at the bottom of the 'Start here' tab when done,
                          then other tabs should be populated)"), style="color:gray"),
          br(),
          helpText(tags$i("note: this is a super fragile prototype & 
                          almost anything will blow it up. 
                          reload the page & try again as needed")),
          # helpText(paste("searching archive.org for @",
          #                output$handle))
          
          #verbatimTextOutput("handle")
        ),

        # Main panel -----
        mainPanel(width = 9,
          # Put tabset panel top of page
          tabsetPanel(type = "tabs",
                      
                      # Start tab ----
                      tabPanel("Start here",
                               br(),
                               em("Use the left sidebar to search for an account.
                               Medium-large accounts work best. Must have at least 3 
                                  results to work properly."),
                               br(),
                               br(),
                               p("This app currently searches Archive.org for past snapshots 
                                 of twitter profiles, then scrapes stats from them, using the archiveRetriever package."),
                               p("Might eventually switch to a paid service with more
                                 reliable results if there is enough interest."),
                               p("Data can be seen & downloaded via the tables & 
                                 buttons at the bottom of each tab."),
                               h2(textOutput("num_results")),
                               br(),
                               br(),
                               
                               plotOutput("overviewPlot"),
                               br(),
                               br(),
                               dataTableOutput('table'),
                               br(),
                               downloadButton("scrape_download", "Download data as CSV (after scraping)")
                               
                        ),
                      
                      # plots & curves tab ----
                      tabPanel("Stats & Curve Fit Plots",
                               br(),
                               p(em("This tab produces plots of the scraped data, & 
                                  performs nonlinear curve fitting on the trends 
                                  to derive scaling exponents as a measure of growth velocity.")),
                               p(em("An exponent of 1 means the trend is linear; >1 indicates 
                                    accelerating 'superlinear' growth (convex curve); <1 indicates 
                                    decelerating 'sublinear' growth (concave).")),
                               br(),
                               fluidRow(column(6, plotOutput("plot", height = "500px")),
                                        column(6, plotOutput("plot2",height = "500px"))),
                               br(),
                               br(),
                               p(em("Below is a scatterplot matrix or 'pairs plot' which depicts the 
                                 relationships & correlations between each metric in a series of rows & columns.")),
                               br(),
                               plotOutput("pairsPlot"),
                               br(),
                               br(),
                               dataTableOutput('curvesTable'),
                               br(),
                               downloadButton("curves_download", "Download data as CSV (after scraping)")
                      ),
                      
                      # radar tab ----
                      tabPanel("#Saddermetrics Radar Chart",
                               br(),
                               p(em("This tab calculates ratios & rate metrics, then plots 
                                    them in a radar chart, against fairly arbitrary benchmark 
                                    ranges.")),
                               p(em("Future iterations will enable users to compare several twitter 
                                    accounts in the same chart, to give a sense of relative ranges.")),
                               br(),
                               tableOutput('smsTable'),
                               br(),
                               plotOutput("smsPlot",
                                          height = "666"),
                               br(),
                               br(),
                               downloadButton("sms_download", "Download data as CSV (after scraping)")
                      ),
                      
                      # info tab  -----
                      tabPanel("...wtf is going on here",
                               br(),
                               p("This is a super early prototype experimenting with
                                 the idea of ",
                                 a("advanced analytics", href='https://en.wikipedia.org/wiki/Advanced_statistics_in_basketball'),
                                 " for social media."), 
                                p("Kinda like ", 
                                 a("Sabermetrics", href='https://en.wikipedia.org/wiki/Sabermetrics'),
                                  "for twitter. So obviously it's called ", tags$b("#saddermetrics.")),
                               p("Maybe it stands for", 
                                 tags$i(tags$b("Social Analytics Depicting Deeper Empirical Reality")),
                                 " or something."),
                               p("But probably not."),
                               br(),
                               p("Some background & motivation can be found in this ",
                                 a("video walkthru", href="https://www.youtube.com/watch?v=9hn5EhbTqyk"),
                                 ", this ", 
                                 a("blog post", href="https://dnlmc.medium.com/ingroup-colossus-visakanv-744c08f408f2?sk=79d5a0c6880c38dc6c5e82d05c3a8383"),
                                 " & ", 
                                 a("these", href="https://twitter.com/dnlmc/status/1482388817868988416"),
                                 a("twitter", href="https://twitter.com/dnlmc/status/1480220400491634689"),
                                 a("threads", href="https://twitter.com/dnlmc/status/1471526268860608520"),
                                 "."),
                               p(em(tags$b("TLDR:"), " I'm interested in different patterns of online community-building, & 
                               the behavioral & relational dynamics of different sorts of activity on social networks. This is a first pass at developing 
                                    & exploring richer analytics than I've been able to find elsewhere, to that end.")),
                               br(),
                               p("It is being actively developed by ",
                                 a("Daniel McNichol", href="https://twitter.com/dnlmc"),
                                 "& will be properly documented & open-sourced asap."),
                                 
                                p("Find out more about what me & my company ",
                                 a("Coεmeta", href="https://twitter.com/co3meta"),
                                 " are up to at ",
                                 a("coemeta.xyz", href="https://coemeta.xyz")
                                 )
                               
                      ),
            )
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  # initialize resettable tibble 
  # per: https://stackoverflow.com/questions/49344468/resetting-fileinput-in-shiny-app
  tib <- reactiveValues(data = NULL,
                        curves_list = list(),
                        #dates = c("2021-01-01", "2021-12-31"),
                        sms = NULL,
                        follower_factor = 1,
                        clear = FALSE)
  
  #output$dates <- renderPrint({ input$dates })
  
  selected_dates <- reactive({input$dates})
  
  # start_date <- selected_dates()[1] #"2021-01-01"
  # end_date <- selected_dates()[2] #"2021-12-31"
    
  
  
  output$handle <- renderText({ input$handle })
  
  # define scrape fucntion
  get_archive_stats <- function(handle, start_date, end_date){
    
    mementos <- retrieve_urls(homepage = paste0("https://www.twitter.com/", 
                                                handle),
                              startDate = start_date,
                              endDate = end_date)
    
    if(length(mementos) > 50){
      mementos <- sample(mementos,50)
    }
    
    stats_list <- list()
    
    for(i in 1:length(mementos) ){
      stats_temp <- scrape_urls(Urls = mementos[i], 
                                ignoreErrors = T, stopatempty = F,
                                Paths = c(tweets = "(//span[@data-count]/@data-count)[1]",
                                          following = "(//span[@data-count]/@data-count)[2]",
                                          followers = "(//span[@data-count]/@data-count)[3]",
                                          likes = "(//span[@data-count]/@data-count)[4]")) 
      
      stats_list[[i]] <- stats_temp
    }
    
    archive_stats <- bind_rows(stats_list)
    archive_stats$handle <- handle
    archive_stats$date <- ymd(str_extract(archive_stats$Urls, regex("\\d{8}")))
    
    archive_stats <- archive_stats %>% mutate(across(tweets:likes, as.integer)) %>% 
      arrange(date)
    
    #drop_na() 
    
    return(archive_stats)
    
  }
  
  # funs -------
  power_fit_fn <- function(input_df, y_var, x_var){
    # y_var = enquo(arg = y_var)
    # x_var = enquo(arg = x_var)
    # 
    # y = select(input_df, !!y_var) %>% as.data.frame() %>% as.vector()
    # x = select(input_df, !!x_var) %>% as.data.frame() %>% as.vector()
    
    #print(input_df[[y_var]])
    
    y <- input_df[[y_var]]
    x <- input_df[[x_var]]
    
    df <- data.frame(y=y, x=x)
    
    pf_model <- nls(y ~ I(a + k * x^n),
                    data = df,
                    start = list(a = min(y),
                                 k= 1, n=1),
                    trace = T,
                    control = list(warnOnly = T,
                                   minFactor = 1/8182,
                                   maxiter = 1000))
    
    return(pf_model)
    
    # pf_model <- nls(y ~ I(a + k * df$x^n),
    #                 data = df,
    #                 start = list(a = min(df$y),
    #                              k= 1, n=1),
    #                 trace = T, #lower = c(a=min(stats_expand$followers_kalman)-100,
    #                 #          k=1, n=1),
    #                 # algorithm = "port", 
    #                 control = list(warnOnly = T,
    #                                minFactor = 1/8182,
    #                                maxiter = 100))
  }
  
  fit_curves <- function(input_tibble){
    
    follower_scale_factor <- 1
    
    if(max(input_tibble$followers[!is.na(input_tibble$followers)]) > 100000) {
      follower_scale_factor <- 1000
      input_tibble$followers <- input_tibble$followers / follower_scale_factor
      #follower_factor <- 1000
    }
    
    # build expanded df with daily rows
    stats_expand <- tibble(date = seq.Date(min(input_tibble$date), 
                                           max(input_tibble$date), by = "day"))
    
    stats_expand <- stats_expand %>% left_join(input_tibble)
    
    
    # kalman filter interpolations
    stats_expand$followers_kalman <- na_kalman(stats_expand$followers,
                                               type="level")
    
    stats_expand$following_kalman <- na_kalman(stats_expand$following,
                                               type="level")
    
    stats_expand$tweets_kalman  <- na_kalman(stats_expand$tweets,
                                             type="level")
    
    stats_expand$likes_kalman <- na_kalman(stats_expand$likes, 
                                           type="level")
    
    # create date index column for nonlinear curve-fitting 
    stats_expand$date_index = 1:length(stats_expand$date)
    
    try({
      
      # fit power curves & add fitted values to df
      followers_power_fit <- power_fit_fn(stats_expand, "followers_kalman", 
                                          "date_index")
      
      stats_expand$followers_powerfit <- predict(followers_power_fit,
                                                 newdata = stats_expand )
      
      tweets_power_fit <- power_fit_fn(stats_expand, "tweets_kalman", 
                                       "date_index")
      
      stats_expand$tweets_powerfit <- predict(tweets_power_fit,
                                              newdata = stats_expand )
      
      likes_power_fit <- power_fit_fn(stats_expand, "likes_kalman", 
                                      "date_index")
      
      stats_expand$likes_powerfit <- predict(likes_power_fit,
                                             newdata = stats_expand )
      
      
      # fit exponential models via log-linear model
      followers_exp_fit <- lm(log(stats_expand$followers) ~ stats_expand$date_index)
      
      stats_expand$followers_expfit <- exp(followers_exp_fit$coefficients[1] + 
                                             followers_exp_fit$coefficients[2] * stats_expand$date_index)
      
      
      # write out df to global variable
      stats_curve_fits <- list(df = stats_expand,
                               followers_powerfit_model = followers_power_fit,
                               tweets_powerfit_model = tweets_power_fit,
                               likes_powerfit_model = likes_power_fit,
                               followers_expfit_model = followers_exp_fit
      )
      
      stats_curve_fits$df <- stats_curve_fits$df %>% mutate(across(starts_with("follower"), 
                                                     ~ .x * follower_scale_factor))
      
      #stats_curve_fits <<- stats_expand
      return(stats_curve_fits)
      
    })
    
    #print("i got here")
    
    if(!exists("stats_curve_fits")) { return(list(df = stats_expand)) }
    
    
  }
  
  plot_stats <- function(input_tibble){
    
    # plots
    p1 <- ggplot(input_tibble, aes(x=date, y=followers)) +
      geom_point()+
      geom_line(aes(x=date, y=followers_kalman), linetype = 3) +
      #geom_line(aes(x=date, y=followers_powerfit), linetype = 2) +
      xlab("") + expand_limits(y=0) + theme_minimal()
    
    p2 <- ggplot(input_tibble, aes(x=date, y=tweets)) +
      geom_point()+
      geom_line(aes(x=date, y=tweets_kalman), linetype = 3) +
      #geom_line(aes(x=date, y=followers_powerfit), linetype = 2) +
      xlab("") + expand_limits(y=0) + theme_minimal()
    
    p3 <- ggplot(input_tibble, aes(x=date, y=likes)) +
      geom_point()+
      geom_line(aes(x=date, y=likes_kalman), linetype = 3) +
      #geom_line(aes(x=date, y=followers_powerfit), linetype = 2) +
      xlab("") + expand_limits(y=0) + theme_minimal()
    
    p4 <- ggplot(input_tibble, aes(x=date, y=following)) +
      geom_point()+
      geom_line(aes(x=date, y=following_kalman), linetype = 3) +
      #geom_line(aes(x=date, y=followers_powerfit), linetype = 2) +
      xlab("") + expand_limits(y=0) + theme_minimal()
    
    
    patchwork <- p1 / p2 / p3 / p4
    patchwork + plot_annotation(
      title = 'Stats Retrieved via Archive.org',
      subtitle = '(with kalman smoothing imputation)',
      #caption = 'Disclaimer: None of these plots are insightful'
    )
    #patchwork
    
  }
  
  plot_curves <- function(input_tibble, input_list){
    
    followers_plot_df <- input_tibble %>% select(date, followers, 
                                                 followers_powerfit#,
                                                 #followers_expfit
    )  %>% 
      pivot_longer(c(followers_powerfit), #,followers_expfit ), 
                   names_to = "model")
    
    followers_exp <- round(input_list$followers_powerfit_model$m$getPars()[3],
                           3)
    
    f_date_75th <- input_tibble$date[round(length(input_tibble$date) * .83)]
    f_y_20th <- max(input_tibble$followers_powerfit) * .075
    
    p1 <- ggplot(followers_plot_df, aes(x=date, y=followers)) +
      geom_point() +
      geom_line(aes(x=date, y=value, color = model, linetype = model)) +
      scale_color_manual(values = c("black", "steel blue"),
                         labels = c("Power Curve", "Exponential")) +
      scale_linetype_manual(values = c(3,2),
                            labels = c("Power Curve", "Exponential")) +
      xlab("") + expand_limits(y=0) + theme_minimal() +
      theme(legend.position= "none") +
      annotate("text", x = f_date_75th, y = f_y_20th, #-Inf, Inf, hjust = .75, vjust = .25 , 
               label = paste("scaling exponent (β) =",
                             followers_exp),
               fontface ="italic", size = 3,
               color = "grey33")
    
    # p1 <- ggplot(followers_plot_df, aes(x=date, y=followers)) +
    #         geom_point() +
    #         geom_line(aes(x=date, y=value, color = model, linetype = model)) +
    #         scale_color_manual(values = c("black", "steel blue"),
    #                            labels = c("Power Curve", "Exponential")) +
    #         scale_linetype_manual(values = c(3,2),
    #                               labels = c("Power Curve", "Exponential")) +
    #         xlab("") + expand_limits(y=0) + theme_minimal() +
    #         theme(legend.position= c(.5, .90), #"top",  #
    #               #legend.justification = "right",
    #               legend.direction = "horizontal",
    #               legend.background=element_rect(fill = alpha("white", 0.7),
    #                                              linetype = 1),
    #               legend.title=element_blank())
    
    
    tweets_plot_df <- input_tibble %>% select(date, tweets, 
                                              tweets_powerfit)  %>% 
      pivot_longer(c(tweets_powerfit), 
                   names_to = "model")
    
    tweets_exp <- round(input_list$tweets_powerfit_model$m$getPars()[3],
                        3)
    
    t_date_75th <- input_tibble$date[round(length(input_tibble$date) * .83)]
    t_y_20th <- max(input_tibble$tweets_powerfit) * .075
    
    p2 <- ggplot(tweets_plot_df, aes(x=date, y=tweets)) +
      geom_point() +
      geom_line(aes(x=date, y=value, color = model, linetype = model)) +
      scale_color_manual(values = c("black", "steel blue"),
                         labels = c("Power Curve", "Exponential")) +
      scale_linetype_manual(values = c(3,2),
                            labels = c("Power Curve", "Exponential")) +
      xlab("") + expand_limits(y=0) + theme_minimal() +
      theme(legend.position= "none") + #, #"top",  #
      #legend.justification = "right",
      # legend.direction = "horizontal",
      # legend.background=element_rect(fill = alpha("white", 0.7),
      #                                linetype = 1),
      # legend.title=element_blank())
      annotate("text", x = t_date_75th, y = t_y_20th, #-Inf, Inf, hjust = .75, vjust = .25 , 
               label = paste("scaling exponent (β) =",
                             tweets_exp),
               fontface ="italic", size = 3,
               color = "grey33")
    
    
    likes_plot_df <- input_tibble %>% select(date, likes, 
                                             likes_powerfit)  %>% 
      pivot_longer(c(likes_powerfit), 
                   names_to = "model")
    
    likes_exp <- round(input_list$likes_powerfit_model$m$getPars()[3],
                       3)
    
    l_date_75th <- input_tibble$date[round(length(input_tibble$date) * .83)]
    l_y_20th <- max(input_tibble$likes_powerfit) * .075
    
    p3 <- ggplot(likes_plot_df, aes(x=date, y=likes)) +
      geom_point() +
      geom_line(aes(x=date, y=value, color = model, linetype = model)) +
      scale_color_manual(values = c("black", "steel blue"),
                         labels = c("Power Curve", "Exponential")) +
      scale_linetype_manual(values = c(3,2),
                            labels = c("Power Curve", "Exponential")) +
      xlab("") + expand_limits(y=0) + theme_minimal() +
      theme(legend.position= "none") + #, #"top",  #
      #legend.justification = "right",
      # legend.direction = "horizontal",
      # legend.background=element_rect(fill = alpha("white", 0.7),
      #                                linetype = 1),
      # legend.title=element_blank())
      annotate("text", x = l_date_75th, y = l_y_20th, #-Inf, Inf, hjust = .75, vjust = .25 ,
               label = paste("scaling exponent (β) =",
                             likes_exp),
               fontface ="italic", size = 3,
               color = "grey33")
    
    patchwork <- p1 / p2 / p3
    patchwork + plot_annotation(
      title = 'Curve fitting: Power Functions',
      subtitle = expression(paste('( functional form: y = ',
                                  a + kx^beta, ' )'))
      #caption = 'Disclaimer: None of these plots are insightful'
    )
    
  }
  
  calc_saddermetrics <- function(curve_list){
    monthly <- curve_list$df %>% mutate(month = month(date), year = year(date)) %>% 
      group_by(year, month) %>% mutate(across(everything(), max )) %>% ungroup() %>% 
      unique() %>% 
      mutate(across(where(is.numeric), ~ .x - lag(.x))) %>%  
      select(ends_with(c("powerfit", "following_kalman"))) %>% tail(3) %>% colMeans(na.rm = T) %>% t() %>% as_tibble()
    
    colnames(monthly) <- monthly %>% colnames() %>% str_replace("_kalman|_powerfit", "")
    
    monthly$`growth velocity` <- round(curve_list$followers_powerfit_model$m$getPars()[3],3)
    
    
    monthly <- monthly %>% 
      mutate(`followers vs following` = followers / following,
             `followers per tweets+likes` = followers / (tweets + likes)) %>% 
      rename(`monthly followers` = followers, 
             `monthly tweets`= tweets,
             `monthly liked tweets` = likes,
             `new following per month` = following)
    
    
    return(monthly)
  }
  
  ## Transparent colors Mark Gardener 2015 www.dataanalytics.org.uk
  t_col <- function(color, percent = 50, name = NULL) {
    #      color = color name
    #    percent = % transparency
    #       name = an optional name for the color
    
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
                 max = 255,
                 alpha = (100 - percent) * 255 / 100,
                 names = name)
    
    ## Save the color
    invisible(t.col)
  }
  
  plot_saddermetrics <- function(sm_df){
    
    if(sm_df$`new following per month` <= 0) {
      sm_df$`new following per month` <- 0
      sm_df$`followers vs following` <- 50}
    
    
    # make max & min rows; followers, tweets, likes, following, growth, ratio, followers / tweet+likes
    sm_df <- rbind(sm_df, c(1000,1000,3000,50,3,50,2), c(0,0,0,0,0,0,0))
    
    sm_df$row <- c("avg", "max", "min")
    sm_df$order <- c(3,1,2)
    
    sm_df <- sm_df %>% arrange(order) %>% select(-order) %>% 
      column_to_rownames(var = "row")
    
    sm_df <- sm_df[,c(1,2,3,4,6,7,5)]
    
    
    p <- radarchart(sm_df,
                    axistype=0 , 
                    #custom polygon
                    pcol= c('slateblue3'),#, 'lightpink3', 
                    #'lightseagreen', 'antiquewhite4'), #colors_border , 
                    #fcol=colors_in , 
                    pfcol = c(t_col("steel blue", 70, 'su')),
                    plwd=5 , plty=1,
                    pty = 32,
                    #custom the grid
                    cglcol="grey", cglty=3, 
                    axislabcol="grey", 
                    #caxislabels=seq(0,20,5), 
                    cglwd=1,
                    #custom labels
                    vlcex=0.9,
                    centerzero = T,
                    #pdensity = c(5,5,5,5),
                    seg = 3,
                    title = "(stats vs arbitrary benchmark ranges)")#,
    # vlabels = c('followers per month',
    #             'tweets per month',
    #             'likes\nper month',
    #             'new following\nper month',
    #             'followers vs following',
    #             'followers\n per tweet',
    #             'followers per like')
    #)
    
    #return(p)
  }
  
  
  # stuff starts -------
  at_handle <- reactive({
    # Change when the "update" button is pressed...
    input$checkArchive
    input$handle
  })
  
  # selected_dates <- reactive({
  #   # Change when the "update" button is pressed...
  #   input$dates
  # })
    

  
  # observeEvent(input$dates,{  #require file or stop
  #   tib$dates <- input$dates })
  
  # make reactive tibble with either uploaded file or demo data
  observeEvent(input$checkArchive,{  #require file or stop
      req(input$handle)
      req(input$dates)
      #req(!tib$clear)
           
      try({
      overview <- archive_overview(homepage = paste0("https://www.twitter.com/",at_handle()),
                                   startDate = as.character(selected_dates()[1]),#start_date,
                                   endDate = as.character(selected_dates()[2]))
      
      mems <<- retrieve_urls(homepage = paste0("https://www.twitter.com/",at_handle()),
                            startDate = as.character(selected_dates()[1]),#start_date,
                            endDate = as.character(selected_dates()[2]))
      
      output$num_results <- renderText( paste("There are", 
                                              length(mems),
                                              #sum(!is.na(overview$data$availableDates)),
                                              "results to scrape."))
                                         
      output$overviewPlot <- renderPlot({plot(overview)})
      
      })
                                                                      
      #if (length(unique(dfDates$year)) > 1) {
    
  })
  
  
  observeEvent(input$scrapeArchive,{  #require file or stop
    req(input$handle)
    req(input$dates)
    
    if(length(mems) > 0){
    
    try({
    tib$data <- get_archive_stats(at_handle(), 
                                  start_date = as.character(selected_dates()[1]),#start_date,
                                  end_date = as.character(selected_dates()[2]))

    output$table <- renderDataTable(tib$data,
                                    options = list(scrollY = 200,
                                                   scrollX = T,
                                                   scrollCollapse = T))
    
    # Make data downloadable
    output$scrape_download <- downloadHandler(
      filename = 'saddermetrics_scraped_data.csv',
      content = function(file) {
        write.csv(tib$data, file)
      })

    })
      
    try({
    tib$curves_list <- fit_curves(tib$data)

    output$plot <- renderPlot({plot_stats(tib$curves_list$df)})

    output$plot2 <- renderPlot({plot_curves(tib$curves_list$df,
                                            tib$curves_list)})
    
    output$pairsPlot <- renderPlot({tib$curves_list$df %>% select(tweets, followers, following, likes) %>%
      #mutate(across(everything(), as.numeric)) %>% 
      drop_na() %>% ggpairs() + theme_linedraw()})
    
    output$curvesTable <- renderDataTable(tib$curves_list$df,
                                          options = list(scrollY = 200,
                                                         scrollX = T,
                                                         scrollCollapse = T))
    
    # Make data downloadable
    output$curves_download <- downloadHandler(
      filename = 'saddermetrics_curve_fit_data.csv',
      content = function(file) {
        write.csv(tib$curves_list$df, file)
      })
    })
    
    try({
    tib$sms <- calc_saddermetrics(tib$curves_list)
    
    output$smsPlot <- renderPlot({plot_saddermetrics(tib$sms)})
    
    output$smsTable <- renderTable(tib$sms)#,
    #options = list(scroll = 200,
    #                scrollCollapse = T))
    
    # Make data downloadable
    output$sms_download <- downloadHandler(
      filename = 'saddermetrics_data.csv',
      content = function(file) {
        write.csv(tib$sms, file)
      })
    })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

}
