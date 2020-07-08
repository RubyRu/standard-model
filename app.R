
library(shiny)
library(tidyverse)
library(quantreg)
library(DT)
library(shinyWidgets)
library(shinythemes)

#source("model-nonsampling.R")
source("model-childesfreqs.R")

theme_set(theme_classic())
# Define UI
ui <- fluidPage(
    theme = shinytheme("spacelab"),
    titlePanel("Standard Model of Early Word Learning"),

    sidebarLayout(
        sidebarPanel(
            selectInput("distro", "Word frequency distribution:",
                        list("log(Zipfian)" = "logzipf",
                             "Uniform" = "uniform",
                             "Zipfian" = "zipf")),
            chooseSliderSkin("Modern"),
            setSliderColor(c("Black", # start_age
                             "DarkSlateGrey", "DarkSlateGrey", # Input
                             "DeepSkyBlue", "DeepSkyBlue", # Word Threshold
                             "Green", "Green", # axes
                             "DarkPink", "DarkPink", # Proc Speed Adult Asymptote
                             "DarkRed", "DarkRed", # Proc Speed Rate of Development
                             "Green", "Green",
                             "Green", "Green",
                             "Green", "Green"), 
                           c(1, 2,3, 4,5, 6,7, 8,9, 10,11, 12,13, 14,15, 16,17)), # color code param means and SDs
            sliderInput("start_age", "Age (mos) when words start accumulating (e.g., age to segmentation):", 
                        min=1, max=8, value=1, step=1), 
            # best-fitting parms for start_age=1 logzipf: c(204, 2436, 6937, 2127, 0.56, 0.03, 0.72, 0.21)
            sliderInput("input_rate", "Input rate mean (tokens/hour):", 
                        min=0, max=2500, value=1000, step=50), 
            helpText("e.g., Hart & Risley low SES: 616/hr; high SES: 2153/hr; we assume 12 waking hours/day"),
            sliderInput("input_rate_sd", "Input rate standard deviation (SD):", 
                        min=0, max=3000, value=900, step=100), 
            helpText("Gilkerson et al. 2017 daily SD range: 4,100-8,200"),
            
            sliderInput("threshold", "Threshold mean (occurrences needed to learn a word):", 
                        min=100, max=7000, value=4000, step=100),
            sliderInput("threshold_sd", "Threshold standard deviation:", 
                        min=0, max=4000, value=1000, step=100),
            helpText("McMurray (2007) used a mean of 4000 and a large SD."),
            #sliderInput("learning_rate", "Mean learning rate (scales value of occurrence; truncated at .1):", 
            #            min = .5, max = 10, value = 1, step=.5),
            checkboxInput("proc_facilitates", "Processing facilitates acquisition", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs", id="tabs", 
                tabPanel("Vocabulary Growth", 
                         plotOutput("ageVocab"), 
                         br(),
                         textOutput("acceleration"),
                         # flexible axes
                         fluidRow(
                             column(6,
                                    sliderInput("x_range", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                        column(6,
                               sliderInput("y_range", "Vocabulary Size",
                                           min=0, max = 8000, value = c(0,8000), step = 1000))
                             ),
                        br()
                    ),
                tabPanel("Processing Speed", 
                         fluidRow(
                            column(3,
                                  sliderInput("proc_speed_asymp", 
                                 div(HTML("Adult processing speed asymptote mean (<em>a</em>; scales value of occurrence; truncated at .01):")), 
                                 min = .01, max = 1, value = .56, step=.01)
                         ), 
                         column(3,
                                sliderInput("proc_speed_asymp_sd", div(HTML("Adult processing speed <em>a</em> SD:")), 
                                            min = 0, max = 1, value = .1, step=.01)
                         ), 
                         column(3,
                                sliderInput("proc_speed_dev", "Processing speed rate of development mean (c):", 
                                            min = 0, max = 1, value = 0.72, step= 0.02)
                         ), 
                         column(3,
                                sliderInput("proc_speed_dev_sd", 
                                            min = 0, max = 1, value = .1, step=.01, "Processing speed rate SD:")
                         )
               ),
                         plotOutput("ageRT"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range1", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                             column(6,
                                    sliderInput("y_range1", "Response time",
                                                min=0, max = 1.5, value = c(0,1.5), step = 0.1))
                         ),
                         br()
                    ),
                tabPanel("Vocabulary Growth Table", 
                         textOutput("summary"), 
                         br(),
                         downloadButton("download_table", "Download Table",
                                        class = "btn-default btn-xs"),
                         br(),
                         br(),
                         DT::dataTableOutput("mytable"),
                         br()
                    ),
                tabPanel("Growth per Word",
                         selectInput("selectword", "Select a word",
                                     cdi_list,
                                     multiple = TRUE),
                         plotOutput("ageWord"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range3", "Age of children (months)",
                                                min=0, max = 50, value = c(0,50), step = 10)),
                             column(6,
                                    sliderInput("y_range3", "Proportion of learners knowing words",
                                                min=0, max = 1, value = c(0,1), step = 0.1))
                         ),
                         br(),
                         br()), # add selector(s) to show particular words
               tabPanel("PoS",
                       #plotOutput("agePos"),
                       plotOutput("propPos"),
                       br()),
               tabPanel("Part of Speech",
                        plotOutput("avgPoS"),
                        br(),
                        ),
                tabPanel("CDI vs. Full Vocab",
                         plotOutput("cdi_vs_full"),
                         fluidRow(
                             column(6,
                                    sliderInput("x_range2", "CDI words known",
                                                min=0, max = 700, value = c(0,700), step = 200)),
                             column(6,
                                    sliderInput("y_range2", "Total vocabulary",
                                                min=0, max = 8000, value = c(0,8000), step = 2000))
                         ),
                         br(),
                         br() # also show correlation of CDI and full, and mean words outside of CDI known (per age?)
                         )
            )
        )
    )
)

# server logic
server <- function(input, output) {
    print(input)
    sim_data <- reactive({
        # fix: vocab_size=10000, n_learners=100, max_age=48
        parms = list(distro=input$distro,
                     input_rate = input$input_rate,
                     input_rate_sd = input$input_rate_sd,
                     threshold = input$threshold,
                     threshold_sd = input$threshold_sd,
                     mean_learning_rate = input$proc_speed_asymp, # ToDo: re-name param in model-nonsampling.R
                     learning_rate_sd = input$proc_speed_asymp_sd, # ToDo: re-name param in model-nonsampling.R
                     proc_facilitates = input$proc_facilitates,
                     proc_speed_dev = input$proc_speed_dev, 
                     proc_speed_dev_sd = input$proc_speed_dev_sd,
                     start_age = input$start_age
                )
        print(parms)
        simulate(parms)
        #simulate(vocab_size=vocab_size, distro=input$distro, input_rate=input$input_rate, input_rate_sd=input$input_rate_sd,
        #    n_learners=n_learners, threshold=input$threshold, max_age=max_age, 
        #    mean_learning_rate=input$learning_rate, learning_rate_sd=input$learning_rate_sd, threshold_sd=input$threshold_sd, 
        #    proc_faciliates=input$proc_facilitates, proc_speed_dev=input$proc_speed_dev, proc_speed_dev_sd=input$proc_speed_dev_sd)
    })
    
    output$ageVocab <- renderPlot({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$known_words, aes(x=month, y=words)) + 
            geom_line(aes(group = id), alpha = .1) + 
            geom_smooth(aes(x=month, y=words), color="black") + 
            # geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            # labs(colour="Quantile") + 
            xlab("Age (months)") + 
            ylab("Vocabulary Size") + #ylim(0, vocab_size) +
            xlim(input$x_range[1], input$x_range[2]) +
            ylim(input$y_range[1], input$y_range[2]) +
            geom_line(aes(x = month, y = cdi_words, group = id), alpha=.1, color = "red") +
            geom_smooth(aes(x = month, y = cdi_words), color="red")
            # geom_point(alpha=.1) +
            # geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) 
    })
    
    output$avgPoS <- renderPlot({
        ggplot(sim_data()$known_PoS, aes(x=Category, y=words, fill = PoS)) + 
            geom_bar(stat="identity") + 
            xlab("Age (months)") + 
            ylab("Mean Vocabulary Size") 
    })
    
    output$propPos <- renderPlot({
        dat2 <- sim_data()$known_pos
        ggplot(dat2, aes(x=Proportion, y=words/twords)) + 
            xlab("Vocabulary Size") + ylab("Proportion of category") +
            facet_wrap(~ PoS) +
            geom_point(aes(group=id), alpha=.1) + geom_smooth() 
    })
    
    output$agePos <- renderPlot({
        colors <- c("Known Verb" = "blue", "Known Noun" = "red", "Known Other" = "orange", "Known Adj" = "green")
        ggplot(mapping = aes(x=month, y=words)) + 
            geom_smooth(data = sim_data()$known_verb, aes(color = "Known Verb"))+ 
            geom_line(data = sim_data()$known_verb, aes(group = id, color = "Known Verb"), alpha = .1) + 
            geom_smooth(data = sim_data()$known_other, aes(color = "Known Other"))+
            geom_line(data = sim_data()$known_other, aes(group = id, color = "Known Other"), alpha = .1) + 
            geom_smooth(data = sim_data()$known_noun, aes(color = "Known Noun"))+
            geom_line(data = sim_data()$known_noun, aes(group = id, color = "Known Noun"), alpha = .1) + 
            geom_smooth(data = sim_data()$known_adj, aes(color = "Known Adj"))+
            geom_line(data = sim_data()$known_adj, aes(group = id, color = "Known Adj"), alpha = .1) + 
            xlab("Age (months)") + 
            ylab("Vocabulary Size") + #ylim(0, vocab_size) +
            ylim(0, 2500) +
            scale_color_manual(values = colors) +
            labs(colour = "Part of Speech")
    })
    
    # show proportion of learners knowing each word over time
    # (could also create a table of mean AoA per word)
    output$ageWord <- renderPlot({
        # maybe make this dataframe in model code..
        dw <- sim_data()$prop_knowing_word # need to make this long
        dl = data.frame(dw)
        names(dl) = 1:max_age
        dl$word = rownames(dw)
        dl = gather(dl, "month", "prop_know", 1:max_age) 
        dl$month = as.numeric(as.character(dl$month))
        dl$on_cdi = wf$on_cdi
        
        # select a small number of words..use selectizeInput ?
       # plot_words = c("you", "the", "have", "wanna", "mommy", 
                       #"daddy", "book", "dog", "boy", "baby")
        plot_word = c(input$selectword)
        ggplot(subset(dl, is.element(word, plot_word)), 
               aes(x=month, y=prop_know)) + 
            geom_line(aes(group = word, color=word), alpha = .8) + 
            #geom_smooth(aes(x=month, y=words), color="black") + 
            xlab("Age (months)") + 
            ylab("Proportion of Learners Knowing Word") +
            xlim(input$x_range3[1], input$x_range3[2]) +
            ylim(input$y_range3[1], input$y_range3[2]) 
           # xlim(input$x_range[1], input$x_range[2]) + ylim(0,1)
    })
    
    output$cdi_vs_full <- renderPlot({
        dat <- sim_data()$known_words
        dat$age_group = cut_interval(dat$month, 16)
        ggplot(dat, aes(x=cdi_words, y=words)) + 
            xlab("CDI Words Known") + ylab("Total Vocabulary") +
            #facet_wrap(~ age_group, nrow=4) +
            geom_line(aes(group=id), alpha=.1) + geom_smooth() +
            xlim(input$x_range2[1], input$x_range2[2]) +
            ylim(input$y_range2[1], input$y_range2[2]) # , color=age_group
    })
    
    output$ageRT <- renderPlot({
        qs <- c(0.10,0.25,0.50,0.75,0.90)
        ggplot(sim_data()$proc_speed, aes(x=month, y=words)) + geom_line(aes(group=id), alpha=.1) +  
            #geom_quantile(quantiles=qs, formula=y ~ poly(x, 2), aes(colour = as.factor(..quantile..))) + 
            labs(colour="Quantile") + geom_smooth() + 
            #geom_abline(intercept=0, slope=input$vocab_size/input$max_age, linetype="dashed", color="grey", size=1) + 
            xlab("Age (months)") + ylab("Response Time (seconds)") + 
            xlim(input$x_range1[1], input$x_range1[2]) +
            ylim(input$y_range1[1], input$y_range1[2]) 
            #+ ylim(0,1.7)
        #print(sim_data()$proc_speed)
    })
    
    output$summary <- renderText({ 
        paste("Mean of cumulative words known per month.") # input$distro
    })
    
    # fit linear and quadratic models to mean words known per month for all months where vocab is not at ceiling
    # NOT DONE - do we just want to check the r.squared of a model with a quadratic term? or 
    output$acceleration <- renderText({ 
        accel = acceleration_test(sim_data()) 
        paste("Average acceleration in vocabulary growth during the second year: ", round(accel, 2)) # input$distro
    })
    
    # vocabulary growth table
    output$mytable = DT::renderDataTable({
        sim_data()$known_words %>% group_by(month) %>% 
            summarise(mean=mean(words), sd=sd(words)) %>% 
            mutate(cumulative_tokens=input$input_rate*waking_hours_per_day*30.42*month) %>%
            datatable(options = list(lengthMenu = c(12, 24, 36), pageLength=49)) %>% 
            formatRound(columns=c("mean","sd"), digits=0)
    })
    
    # download button
    output$download_table <- downloadHandler(
        filename = function() paste0("standard_model_sim", ".csv"), # maybe a version number?
        content = function(file) {
            voc_mo <- sim_data()$known_words %>% group_by(month) %>% 
                summarise(mean=mean(words), sd=sd(words)) %>% 
                mutate(cumulative_tokens=input$input_rate*waking_hours_per_day*30.42*month)
            write.csv(voc_mo, file, row.names = FALSE)
        })
}

# Run the app
shinyApp(ui = ui, server = server)
