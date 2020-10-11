library(shiny)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(janitor)
library(kableExtra)
library(gt)
library(gtsummary)
library(ggthemes)
library(ggpubr)
library(gridExtra)
library(shinythemes)

# CANNOT RUN TEST GOODNESS OF FIT TEST ON ALL VARIABLES _ ONLY NUMERIC
#importing and cleaning data#########################################################################
raw = read_csv("DATA2X02 class survey 2020 (Responses) - Form responses 1.csv")
# raw  = readr::read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTf8eDSN_2QbMTyVvO5bdYKZSEEF2bufDdYnLnL-TsR8LM-6x-xu1cxmDMohlbLrkMJn9DE7EG7pg5P/pub?gid=1724783278&single=true&output=csv")
# raw = read_csv("survey_data.csv")
x = raw %>% clean_names()

colnames(x) = stringr::str_replace(string = colnames(x),
                                   pattern = "what_is_your_",
                                   replacement = "")
colnames(x) = stringr::str_replace(string = colnames(x),
                                   pattern = "on_average_how_many_hours_per_week_did_you_",
                                   replacement = "")

colnames(x)[2] = "covid_test"
colnames(x)[4] = "postcode"
colnames(x)[5] = "dentist"
colnames(x)[6] = "university_work"
colnames(x)[7] = "social_media"
colnames(x)[8] = "dog_or_cat"
colnames(x)[9] = "live_with_parents"
colnames(x)[10] = "exercising"
colnames(x)[12] = "asthma"
colnames(x)[13] = "paid_work"
colnames(x)[14] = "fav_season"
colnames(x)[16] = "height"
colnames(x)[17] = "floss_frequency"
colnames(x)[18] = "glasses"
colnames(x)[20] = "steak_preference"
colnames(x)[21] = "stress_level"


x = x %>% mutate(
    postcode = as.character(postcode),
    timestamp = lubridate::dmy_hms(timestamp)
)





x = x %>% 
    filter(!is.na(social_media)) %>%
    mutate(
        social_media = case_when(
            tolower(substr(social_media, start = 1, stop = 3)) == "fac" ~ "Facebook",
            tolower(substr(social_media, start = 1, stop = 3)) == "ins" ~ "Instagram",
            tolower(substr(social_media, start = 1, stop = 3)) == "wec" ~ "WeChat",
            tolower(substr(social_media, start = 1, stop = 3)) == "twi" ~ "Twitter",
            tolower(substr(social_media, start = 1, stop = 3)) == "wec" ~ "WeChat",
            tolower(substr(social_media, start = 1, stop = 3)) == "mes" ~ "Messenger",
            tolower(substr(social_media, start = 1, stop = 3)) == "wec" ~ "WeChat",
            tolower(substr(social_media, start = 1, stop = 3)) == "red" ~ "Reddit",
            tolower(substr(social_media, start = 1, stop = 3)) == "tik" ~ "Tiktok",
            tolower(substr(social_media, start = 1, stop = 3)) == "sna" ~ "Snapchat",
            tolower(substr(social_media, start = 1, stop = 3)) == "bil" ~ "Bilibili", 
            TRUE ~ "Other"
        )
    ) %>%
    mutate(
      height = case_when(
        height < 10 ~ height*100, 
        TRUE ~ height
      )
    )

write.csv(x, "survey_data.csv")

# glimpse(x)

#########################################################################################################################
# cat_vars = x %>% select_if(!(is.numeric)) %>% colnames()
num_vars = x %>% select_if(is.numeric) %>% colnames()
# num_vars
# cat_vars
# 
# y_var_choices = c("Apparent temperature (°C)" = "apparent_t",
#                   "Dew point" = "dewpt",
#                   "Delta t" = "delta_t",
#                   "Air temperature" = "air_temp",
#                   "Pressure" = "press",
#                   "Wind speed (km/h)" = "wind_spd_kmh")
# x$social_media
# unique(x$social_media)

# cat_var_uniq = c(
#     "social_media" = unique(x$social_media), 
#     "postcode" = unique(x$postcode),
#     "glasses" = unique(x$glasses),
#     "fav_season" = unique(x$fav_season), 
#     "dentist" = unique(x$dentist),
#     "live_with_parents" = unique(x$live_with_parents),
#     "asthma" = unique(x$floss_frequency),
#     "floss_frequency" = unique(x$floss_frequency),
#     "steak_preference" = unique(x$steak_preference), 
#     "dominant_hand" = unique(x$dominant_hand)
# )


theor_dists = c("pois")

# distinct(x$social_media)

num_vars_choices = c(
                    "No. of COVID Tests" = "covid_test",
                    "Hours spent on university work" = "university_work",
                    "Hours spent exercising" = "exercising",  
                    "Hours of paid work" = "paid_work",
                    # "Shoe Size" = "shoe_size", 
                    "Height (cm)" = "height", 
                    "Stress level during the past week" = "stress_level"
                )

alt_hyp_choices = c(
                    "Greater than" = "greater", 
                    "Less than" = "less", 
                    "Not equal to" = "two.sided"
        )

x_vars_choices = c("No. of COVID Tests" = "covid_test",
                   "Hours spent on university work" = "university_work", 
                   "Favourite social media platform" = "social_media")

ind_test_choices = c(
                    # "No. of COVID Tests" = "covid_test",
                    # "Hours spent on university work" = "university_work", 
                    "Favourite social media platform" = "social_media", 
                    "Postcode" = "postcode",
                    "Wears glasses or contacts" = "glasses",
                    "Favouite season" = "fav_season", 
                    "Last visit to dentist" = "dentist",
                    "Lives with parents" = "live_with_parents",
                    "Has asthma" = "asthma",
                    "Flossing frequency" = "floss_frequency",
                    "Steak preference"  = "steak_preference", 
                    # "Eye colour" = "eye_colour", 
                    "Dominant hand" = "dominant_hand"
                    # "Stress level during the past week" = "stress_level"
                    ) 


p_val1 = 0
p_val2 = 0
p_val3 = 0


# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("readable"),
    # themeSelector(),
    # Application title
    titlePanel("DATA2x02 Survey Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            # conditionalPanel("input.tabs == 'Goodness of fit test'",
            #                  selectizeInput(inputId = "x_variable",
            #                                 "Select x variable:",
            #                                 choices = x_vars_choices)),
            #                  
            # conditionalPanel("input.tabs == 'Goodness of fit test'",
            #                   selectizeInput(inputId = "theor_dist",
            #                                  "Select theoritical distribution:",
            #                                  choices = theor_dists)), 
            # 
            
            #### Independence panels
            conditionalPanel("input.tabs == 'Test for independence'",
                             selectizeInput(inputId = "var1",
                                            "Select 1st variable:",
                                            choices = ind_test_choices), 
                             checkboxInput(inputId = "show_var1_barplot",
                                           label = "Show barplot?",
                                           value = FALSE)),
            
            conditionalPanel("input.tabs == 'Test for independence'",
                             selectizeInput(inputId = "var2",
                                            "Select 2nd variable:",
                                            choices = rev(ind_test_choices)), 
                             checkboxInput(inputId = "show_var2_barplot",
                                           label = "Show barplot?",
                                           value = FALSE)),
            
            conditionalPanel("input.tabs == 'Test for independence'",
                             numericInput(inputId = "alpha",
                                          label = "Set alpha:",
                                          value = 0.05, 
                                          min = 0.0,
                                          max = 1.0, 
                                          step = 0.05)),
            conditionalPanel("input.tabs == 'Test for independence'",
                             numericInput(inputId = "B",
                                          label = "Set number of bootstrap samples
                                                    for Monte-Carlo simulation:",
                                          value = 2000, 
                                          min = 0.0)),
            
            
            
            
            ##### one t-test panels
            conditionalPanel("input.tabs == 'One-sample t-test'",
                             selectizeInput(inputId = "one_t_var",
                                            "Select variable:",
                                            choices = num_vars_choices),
                            checkboxInput(inputId = "show_one_t_qq", 
                                          "Show Normal Q-Q plot?", 
                                          value = FALSE)),
            conditionalPanel("input.tabs == 'One-sample t-test'",
                             selectizeInput(inputId = "alt_hyp",
                                            "Alternative hypothesis choices:",
                                            choices = alt_hyp_choices)),
            conditionalPanel("input.tabs == 'One-sample t-test'",
                             numericInput(inputId = "one_t_alpha",
                                          label = "Set alpha:",
                                          value = 0.05, 
                                          min = 0.0,
                                          max = 1.0, 
                                          step = 0.05)),
            conditionalPanel("input.tabs == 'One-sample t-test'",
                             numericInput(inputId = "pop_mean",
                                          label = "Set population mean:", 
                                          value = 0)),
            
            ####### two sample t-test panels
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             selectizeInput(inputId = "cont_var",
                                            "Select continous variable:",
                                            choices = num_vars_choices)), 
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             selectizeInput(inputId = "cat_var",
                            "Select a categorical variable:",
                            choices = ind_test_choices)), 
            
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             uiOutput("cat_var_uniq1"), 
                             checkboxInput(inputId = "show_slice_var1_qq", 
                             label= "Show Normal Q-Q plot?", 
                             value= FALSE)), 
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             uiOutput("cat_var_uniq2"), 
                             checkboxInput(inputId = "show_slice_var2_qq", 
                             label= "Show Normal Q-Q plot?", 
                             value= FALSE)),
            
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             selectizeInput(inputId = "alt_hyp_2",
                                            "Alternative hypothesis choices:",
                                            choices = alt_hyp_choices)),
            
                          
                            # 
                            # checkboxInput(inputId = "show_boxplot",
                            #               label = "Show boxplot?",
                            #               value = FALSE))),
            conditionalPanel("input.tabs == 'Two-sample t-test'",
                             numericInput(inputId = "one_t_alpha",
                                          label = "Set alpha:",
                                          value = 0.05, 
                                          min = 0.0,
                                          max = 1.0, 
                                          step = 0.05))
            # conditionalPanel("input.cat_var" == "social_media", 
            #                  selectizeInput((inputId = "cat1")), 
            #                  "Select", 
            #                  choices = )
                             
                             
            
                       
            
            
            
        ),
        
        
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id = "tabs", 
                        
                        # tabPanel(
                        #     title = "Goodness of fit test",
                        #     plotly::plotlyOutput("x_barplot")
                        # ),
                        tabPanel(
                            title = "Test for independence",
                            
                            # if (in)
                            
                            HTML("<br/>"),
                            # renderUI("ind_test_table"),
                            gt_output("ind_test_table"),
                            
                            conditionalPanel(
                                "input.show_var1_barplot", 
                                plotly::plotlyOutput("var1_barplot")
                            ),
                            HTML("<br/>"),
                            
                            conditionalPanel(
                                "input.show_var2_barplot", 
                                plotly::plotlyOutput("var2_barplot")    
                            ),
                            HTML("<br/>"),
                            
                            # plotly::plotlyOutput("var1_barplot"),
                            
                            
                            h4("Hypothesis"),
                            
                            uiOutput("ind_hyp"),
                            
                            h4("Assumptions"),
                            
                            HTML("<ul><li>All observations are independent.
                                 </li><li> All expected cell counts are greater than 5. </li></ul>"),
                            
                            
                            
                            
                            # textOutput("ind_test_hyp"),
                            h2("Chi-squared test for independence"),
                            h4("Assumptions"),
                            
                            HTML("<ul><li>All observations are independent.
                                 </li><li> All expected cell counts are greater than 5. </li></ul>"),
                            
                            verbatimTextOutput("ind_test"), 
                            h4("Conclusion"),
                            uiOutput("ind_test_concl"),
                            
                            # verbatimTextOutput("ind_test_concl"),
                            
                            h2("Chi-squared test for independence with continuity correction"),
                            
                            
                            # p(output$ind_test_p),
                            
                           
                            verbatimTextOutput("ind_cor_test"),
                            h4("Conclusion"),
                            uiOutput("ind_cor_concl"),
                            
                            h2("Monte-Carlo simuluation"),
                          
                            
                            verbatimTextOutput("ind_sim_test"),
                            h4("Conclusion"),
                            uiOutput("ind_sim_concl"),
                            HTML("<br/>"),
                            HTML("<br/>")
                            # verbatimTextOutput("ind_test_concl"),
                            
                        ),
                        
                        
                        
                        
                        
                        tabPanel(
                            title = "One-sample t-test", 
                            plotly::plotlyOutput("one_t_var_barplot"),
                            headerPanel("One-sample t-test"),
                            
                            h4("Hypothesis"),
                            uiOutput("one_t_hyp"),
                            
                            h4("Assumptions"),
                            HTML("<ul><li>All observations are independent
                                 </li><li>Observations are normally distributed </li></ul>"),
                            
                            conditionalPanel(
                                "input.show_one_t_qq", 
                                plotly::plotlyOutput("one_t_qq")),     
                                HTML("<br/>"),
                            
                            verbatimTextOutput("one_t_test"),
                            h4("Conclusion"),
                            uiOutput("one_t_concl"), 
                            HTML("<br/>"),
                            HTML("<br/>")
                            
                            # print(input$one_t_alpha)
                            
                        ), 
                        tabPanel(
                            title = "Two-sample t-test", 
                            HTML("<br/>"),
                            
                            plotly::plotlyOutput("two_t_boxplot"),
                            HTML("<br/>"),
                            
                            h4("Hypothesis"),
                            uiOutput("two_t_hyp"),
                            h4("Assumptions"),
                            HTML("<ul><li>All observations from both categories are iid normal, with different means and equal variances
                                 </li><li>Observations from one category are independent to the other</li></ul>"),
                            p(""),
                            
                            conditionalPanel(
                              "input.show_slice_var1_qq", 
                              plotly::plotlyOutput("two_t_var1_qq")),     
                            HTML("<br/>"),
                            
                            conditionalPanel(
                              "input.show_slice_var2_qq", 
                              plotly::plotlyOutput("two_t_var2_qq")),
                            
                            headerPanel("Two-sample t-test"),
                            verbatimTextOutput("two_t_test"),
                            h4("Conclusion"),
                            uiOutput("two_t_concl"),
                            HTML("<br/>"),
                            HTML("<br/>")
                            
                            
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    ##### TWO T_TEST STUFFF   
    output$cat_var_uniq1 = renderUI({
      
      
      uniq = x %>%
        filter(!is.na(input$cat_var)) %>%
        dplyr::select(input$cat_var) %>%
        unique()
      print(glimpse(uniq))
      
      selectizeInput(inputId = "slice_var1", 
                     "Select a first category to slice the continuous variable", 
                     choices = uniq)
    })
    
    output$cat_var_uniq2 = renderUI({


      uniq = x %>%
        filter(!is.na(input$cat_var)) %>%
        dplyr::select(input$cat_var) %>%
        unique()
      
      uniq = uniq[!(uniq %in% c(NA))]

      print(glimpse(uniq))

      selectizeInput(inputId = "slice_var2",
                     "Select a second category to slice the continuous variable",
                     choices = uniq)
    })
    
    
    output$two_t_boxplot = plotly::renderPlotly({
      b1 = x %>%
        filter(!is.na(input$cont_var)) %>%
        filter(!is.na(input$cat_var)) %>%
        filter(x[input$cat_var] ==  input$slice_var1 | x[input$cat_var] ==  input$slice_var2)
        # dplyr::select(input)
      # print(input$cat_var)
      # print(input$slice_var1)
      # 
      # 
        # filter((input$cat_var == input$slice_var1) | (input$cat_var == input$slice_var2)) %>%
      b1 = b1 %>% ggplot() +
        aes_string(x = input$cat_var, y = input$cont_var) +
        geom_boxplot() +
        geom_jitter(width=0.15, size = 1.5, colour = "blue") + 
        labs(x = names(which(ind_test_choices == input$cat_var)), 
             y = names(which(num_vars_choices == input$cont_var)), 
             title = paste("Boxplot of", names(which(ind_test_choices == input$cat_var)),
                           "and", names(which(num_vars_choices == input$cont_var)), sep = " ")) + 
        theme_economist()
      b1
      
    })
    
    output$two_t_test = renderPrint({
      first = x %>%
          filter(!is.na(get(input$cont_var))) %>%
          filter(!is.na(get(input$cat_var))) %>%
          filter(get(input$cat_var) ==  input$slice_var1) %>%
          dplyr::select(input$cont_var)
        
        second = x %>%
          filter(!is.na(get(input$cont_var))) %>%
          filter(!is.na(get(input$cat_var))) %>%
          filter(get(input$cat_var) ==  input$slice_var2) %>%
          dplyr::select(input$cont_var)
        
    
        res = t.test(first, second, alternative = input$alt_hyp_2)
        res
        
        
    })


    output$two_t_concl = renderUI({
      first = x %>%
        filter(!is.na(get(input$cont_var))) %>%
        filter(!is.na(get(input$cat_var))) %>%
        filter(get(input$cat_var) ==  input$slice_var1) %>%
        dplyr::select(input$cont_var)
      
      second = x %>%
        filter(!is.na(get(input$cont_var))) %>%
        filter(!is.na(get(input$cat_var))) %>%
        filter(get(input$cat_var) ==  input$slice_var2) %>%
        dplyr::select(input$cont_var)
      
      
      res = t.test(first, second, alternative = input$alt_hyp_2)
      
      str1 = ""
      
      if (res$p.value < input$one_t_alpha) {
        p(paste("The p-value is less than the alpha.",
                "Therefore it suggests that there is evidence that the mean of",
                tolower(names(which(num_vars_choices == input$cont_var))), " are",
                tolower(names(which(input$alt_hyp_2 == alt_hyp_choices))),
                "the mean of",
                tolower(names(which(ind_test_choices == input$cat_var))),
                "with respect to the categories", 
                "(", input$slice_var1, ",", 
                input$slice_var2, ")",
                # "are inconsistent with the null hypothesis \n - DATA2X02 student's population mean of",
                # tolower(names(which(num_vars_choices == input$one_t_var))),
                # "IS NOT suggested to be", input$pop_mean, "\n\n",
                sep = " "))
      }
      else
      {
        p(paste("The p-value is greater than the alpha.",
                "Therefore it suggests that there is evidence that the mean of",
                tolower(names(which(num_vars_choices == input$cont_var))), "are equal the mean of", 
                tolower(names(which(ind_test_choices == input$cat_var))), 
                "with respect to the categories", 
                "(", input$slice_var1, ",", 
                input$slice_var2, ")",
                # "are consistent with the null hypothesis \n - DATA2X02 student's population mean of",
                # tolower(names(which(num_vars_choices == input$one_t_var))),
                # "IS NOT suggested to be", input$pop_mean, "\n\n",
                sep = " "))
      }
      
      
      
      
    })
    
    output$two_t_var1_qq <- plotly::renderPlotly({

      if (input$show_slice_var1_qq){
        q1 = x %>%
          filter(!is.na(get(input$cont_var))) %>%
          filter(!is.na(get(input$cat_var))) %>%
          filter(get(input$cat_var) ==  input$slice_var1) %>%
          dplyr::pull(input$cont_var) %>%
          ggqqplot() +
          labs(x = "Theoritical quantiles from a normal distribution",
               y = "Quantiles from sample",
               title = paste(input$slice_var1, "sample", sep = " ")) +
          theme_economist()

        ggplotly(q1)
      }


    })

    output$two_t_var2_qq <- plotly::renderPlotly({
      if (input$show_slice_var2_qq)
      {
        q2 = x %>%
          filter(!is.na(get(input$cont_var))) %>%
          filter(!is.na(get(input$cat_var))) %>%
          filter(get(input$cat_var) ==  input$slice_var2) %>%
          dplyr::pull(input$cont_var) %>%
          ggqqplot() +
          labs(x = "Theoritical quantiles from a normal distribution",
               y = "Quantiles from sample",
               title = paste(input$slice_var2, "sample", sep = " ")) +
          theme_economist()

        ggplotly(q2)
      }


    })
    
    
    
    output$two_t_hyp = renderUI({
      str1 = paste(strong("Null hypothesis:"), input$slice_var1, "category has mean equal to",
                   input$slice_var2, sep = " ")
      str2 = paste(strong("Alternative hypothesis:"), input$slice_var1, "category has mean", 
                   tolower(names(which(input$alt_hyp_2 == alt_hyp_choices))),
                   input$slice_var2, sep = " ")
      
      
  
      HTML(paste(str1, str2, sep="<br/>"))
      
    })
    
   
    ###### ONE T_TEST STUFF
    
    output$one_t_hyp = renderUI({
      str1 = paste(strong("Null hypothesis:"), names(which(num_vars_choices == input$one_t_var)),
                   "has a mean equal to",
                   input$pop_mean, sep = " ")
      str2 = paste(strong("Alternative hypothesis:"), names(which(num_vars_choices == input$one_t_var)),
                   "has a mean", tolower(names(which(input$alt_hyp == alt_hyp_choices))), 
                   input$pop_mean, sep = " ")
      HTML(paste(str1, str2, sep="<br/>"))
      
    })
    
    output$one_t_qq = plotly::renderPlotly({
      if (input$show_one_t_qq)
      {
        q2 = x %>%
          filter(!is.na(x[input$one_t_var])) %>%
          dplyr::pull(input$one_t_var) %>%
          ggqqplot() +
          labs(x = "Theoritical quantiles from a normal distribution",
               y = "Quantiles from sample",
               caption = paste("For", names(which(num_vars_choices == input$one_t_var)), "sample", sep = " ")) +
          theme_economist()
        
        ggplotly(q2)
      }
      
      
    })
    output$one_t_var_barplot <- plotly::renderPlotly({
        
        
            p1 = x %>% 
                filter(!is.na(input$one_t_var)) %>%
                ggplot() + 
                aes_string(x = input$one_t_var, fill = input$one_t_var ) + 
                labs(y = "Counts", x = names(which(num_vars_choices == input$one_t_var)), 
                     title = paste("Histogram of", names(which(num_vars_choices == input$one_t_var)), sep = " ", collapse = NULL)) + 
                theme_economist()
            
            
                p1 = p1 + geom_histogram() 
                

            
            plotly::ggplotly(p1)
        
        
        
    })
    
    output$one_t_test = renderPrint({
        x2 = x %>%
            dplyr::select(input$one_t_var) %>%
            t.test(mu = input$pop_mean, alternative = input$alt_hyp)
        
        
        x2
            
    })
    
    
    output$one_t_concl = renderUI({
        x2 = x %>%
            dplyr::select(input$one_t_var) %>%
            t.test(mu = input$pop_mean, alternative = input$alt_hyp)
        p(cat("worked"))
        if (x2$p.value < input$one_t_alpha) {
            p(paste("The p-value is less than the alpha.",
                        "Therefore the",
                        tolower(names(which(num_vars_choices == input$one_t_var))),
                        "is inconsistent with the null hypothesis \n - DATA2X02 student's population mean of",
                        tolower(names(which(num_vars_choices == input$one_t_var))),
                        "is NOT suggested to be", input$pop_mean, "\n\n",
                        sep = " "))
        }
        else {
            p(paste("p-value < alpha\n",
                        ". \n Therefore the",
                        tolower(names(which(num_vars_choices == input$one_t_var))),
                        "is consistent with the null hypothesis - \n DATA2X02 student's population mean of",
                        tolower(names(which(num_vars_choices == input$one_t_var))),
                        "is suggested to be", input$pop_mean, "\n\n",
                        sep = " "))
        }
    })  
    
     
    
    
  
    
    output$var1_barplot <- plotly::renderPlotly({
        
        if (input$show_var1_barplot) {
          
            # v = x %>%
            #   filter(!is.na(x[input$var1])) %>%
            #   dplyr::select(input$var1)
            # 
            # reorder(v, v, function(x)-length(x))
            # print(length(v))
            p1 = x %>% 
              filter(!is.na(x[input$var1])) %>%
              ggplot() + 
              aes_string(x = input$var1, fill = input$var1) + 
              # guides(fill=FALSE) + 
              labs(y = "Counts", x = names(which(ind_test_choices == input$var1)), 
                   title = paste("Barplot of", names(which(ind_test_choices == input$var1)), sep = " ", collapse = NULL)) +
              theme(legend.position="none") + 
              theme_economist() +
              geom_bar()
            
            
                # p1 = p1 + geom_histogram()
        
        
            
            plotly::ggplotly(p1)
        }
        
        
    })
    
    
    output$var2_barplot <- plotly::renderPlotly({
        
        if (input$show_var2_barplot) {
            p1 = x %>% 
                filter(!is.na(x[input$var2])) %>%
                ggplot() + 
                aes_string(x = input$var2, fill = input$var2) + 
                # guides(fill=FALSE) + 
                labs(y = "Counts", x = names(which(ind_test_choices == input$var2)), 
                     title = paste("Barplot of", names(which(ind_test_choices == input$var2)), sep = " ", collapse = NULL)) +
                theme(legend.position="none") + 
                theme_economist() +
              geom_bar()
            
            
                       # p1 = weather_data() %>% ggplot() +
            #     aes_string(x = "local_date_time_full", y = input$y_variable) +
            #     geom_line() +
            #     labs(y =  names(which(y_var_choices == input$y_variable)),
            #          x = "",
            #          title = input$station) +
            #     theme_bw()
            
            plotly::ggplotly(p1)
            # p1
        }
        
        
    })
    
    
    output$ind_test_table = render_gt({
           tb = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table()
          
          
          print(rownames(tb))
          gt_tb = gt(as.data.frame.matrix(
            x %>%
              dplyr::select(input$var1, input$var2) %>%
              na.omit() %>%
              table()
            
          ), rownames_to_stub = TRUE) %>%
            tab_header(title = paste("Contingency table of",  tolower(names(which(ind_test_choices == input$var1))),
                                     "and", tolower(names(which(ind_test_choices == input$var2))), sep = " "), 
                       subtitle = names(which(ind_test_choices == input$var2))) %>%
            tab_stubhead(label = names(which(ind_test_choices == input$var1)))
          
          
          
          print(gt_tb)
          gt_tb
          
          
        })  
      
      
  
    
    
    output$ind_test = renderPrint({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(correct = FALSE)
        # print('hererere')
        # print(glimpse(x2))
        # print(sum(is.na(x2)))
        p_val = x2$p.value
        
  
        cat(paste("Test statistic:", x2$statistic, "\n", sep=" "))
        x2
        
        
        
    })
    
    output$ind_test_concl = renderUI({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(correct = FALSE)
    
        
        str1 = ""
        str2 = ""
        if (x2$p.value < input$alpha) {
            
            str1 = paste("p-value of", round(x2$p.value, 4), "is less than the alpha ", input$alpha, 
                            "Therefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE NOT independent", sep = " ")
        }
        else {
            str1 = paste("p-value of", round(x2$p.value, 4), "is greater than the alpha ", input$alpha,   
                            "\nTherefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE independent \n\n", sep = " ")
        }
        
        if (TRUE %in% (x2$expected < 5)) {
            str2 = strong("WARNING: Some expected values for the cells were less than 5. \n Hence the chi-squared approximation may not be correct")
        }
        
        HTML(paste(str1, str2, sep="<br/>"))
        
    }) 
    
    
    output$ind_cor_test = renderPrint({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(correct = TRUE)
    
    
        cat(paste("Test statistic:", x2$statistic, "\n", sep=" "))    
        x2
        
        # if ()
    })
    
    output$ind_cor_concl = renderUI({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(correct = TRUE)
        
        str1 = ""
        str2 = ""
        
        if (x2$p.value < input$alpha) {
            str1 = paste("p-value of", round(x2$p.value, 4), "is less than the alpha ", input$alpha, 
                            "\nTherefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE NOT independent", sep = " ")
        }
        else {
            str1 = paste("p-value of", x2$p.value, "is greater than the alpha ", input$alpha, 
                            "\nTherefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE independent \n\n", sep = " ")
        }
        
        if (TRUE %in% (x2$expected < 5)) {
            str2  = strong("WARNING: Some expected values for the cells were less than 5. \n Hence the chi-squared approximation may not be correct")
        }
        
        
        HTML(paste(str1, str2, sep="<br/>"))
        
    })
    
    output$ind_sim_test = renderPrint({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(simulate.p.value = TRUE, B = input$B)
  
        cat(paste("Test statistic:", x2$statistic, "\n", sep=" "))
        x2
    })
    
    output$ind_sim_concl = renderUI({
        x2 = x %>%
            dplyr::select(input$var1, input$var2) %>%
            na.omit() %>%
            table() %>%
            chisq.test(simulate.p.value = TRUE, B = input$B)
        # print('hererere')
        # print(glimpse(x2))
        # print(sum(is.na(x2)))
        str1 = ""
        str2 = ""
        
        if (x2$p.value < input$alpha) {
            str1 = paste("p-value of", round(x2$p.value, 4), "is less than the alpha ", input$alpha, 
                            "\n.Therefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE NOT independent", sep = " ")
        }
        else {
           str1 = paste("p-value of", x2$p.value, "is greater than the alpha ", input$alpha, 
                            "\nTherefore the", 
                            tolower(names(which(ind_test_choices == input$var1))), 
                            "and the",
                            tolower(names(which(ind_test_choices == input$var2))),
                            "ARE independent \n\n", sep = " ")
        }
        
        p(paste(str1, sep="\n"))
        
    })


    
    output$ind_hyp = renderUI({
      str1 = paste(strong("Null hypothesis:"), names(which(ind_test_choices == input$var1)),
                   "and the",
                   names(which(ind_test_choices == input$var2)), "are independent", sep = " ")
        
      str2 = paste(strong("Alternative hypothesis:"), names(which(ind_test_choices == input$var1)),
                  "and the",
                  names(which(ind_test_choices == input$var2)), "are not independent",
                  sep = "\n")
      
      HTML( paste(str1, str2, sep = "<br/><br/>"))
      # print(final)
      # p(str1)
      # p(str2)
    })
    
    
    
 
    
    output$err_same_var = renderText({
        "Please pick 2 different variables"
    }) 
}

# Run the application
shinyApp(ui = ui, server = server)
