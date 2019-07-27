# Create a Shiny app showing 2018-19 KnowYourStuffNZ pill testing results

# Load required libraries

library(shiny)
library(ggplot2)
library(dplyr)


# Load data

Amphetamine <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Amphetamine_actual_content_2018_19.csv")
Cocaine <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Cocaine_actual_content_2018_19.csv")
Dissociative <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Dissociative_actual_content_2018_19.csv")
Indole <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Indole_actual_content_2018_19.csv")
MD <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_MD_actual_content_2018_19.csv")
Mixture <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Mixture_actual_content_2018_19.csv")
Other <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Other_actual_content_2018_19.csv")
PEA <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_PEA_actual_content_2018_19.csv")
PharmHerbal<- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_PharmHerbal_actual_content_2018_19.csv")
Unknown <- read.csv("https://knowyourstuff.nz/wp-content/uploads/2019/06/pres_Unknown_actual_content_2018_19.csv")


# Create a column in each table stating the presumed substance

Amphetamine <- Amphetamine %>%
    mutate(pres_substance = "Amphetamine")

Cocaine <- Cocaine %>%
    mutate(pres_substance = "Cocaine")

Dissociative <- Dissociative %>%
    mutate(pres_substance = "Dissociative")

Indole <- Indole %>%
    mutate(pres_substance = "Indole")

MD <- MD %>%
    mutate(pres_substance = "MD")

Mixture <- Mixture %>%
    mutate(pres_substance = "Mixture")

Other <- Other %>%
    mutate(pres_substance = "Other")

PEA <- PEA %>%
    mutate(pres_substance = "Phenethylamine (research chemicals)")

PharmHerbal<- PharmHerbal %>%
    mutate(pres_substance = "Psychoactive pharmaceutical/herbal")

Unknown <- Unknown %>%
    mutate(pres_substance = "Unknown")


# Merge tables into one

fulltable <- rbind(Amphetamine, 
                   Cocaine, 
                   Dissociative, 
                   Indole,
                   MD,
                   Mixture,
                   Other,
                   PEA,
                   PharmHerbal,
                   Unknown
                   )


# The following code creates a Shiny application that presents a chart and table of 
# substances (determined through testing) for each presumed substance (what  
# KnowYourStuffNZ clients thought they had before testing). App users can use 
# checkboxes to select which presumed substance/s they want to see results for.

##Define UI 

ui <- fluidPage(
        tabsetPanel(
            
            tabPanel("Pill testing results",
                # Application title
                titlePanel("KnowYourStuffNZ's pill testing results, 2018-19"),

                # Sidebar with a checkbox input for presumed substances 
                sidebarLayout(
                    sidebarPanel(
                        helpText("KnowYourStuffNZ tested 805 pills at 13 events in 2018-19. 
                                 Below you can select the substances that people thought they had. 
                                 On the right you will see what those pills actually contained."),
                        checkboxGroupInput(inputId = "presumed",
                            label = "What pills were thought to contain (before testing)",
                            choices = unique(fulltable$pres_substance)
                            )
                ),

                # Show a plot of the actual substances
                mainPanel(
                    h4("What pills actually contained (results grouped by drug family)"),
                    plotOutput("plot1"),
                    h4("What pills actually contained (detail on specific drugs)"),
                    tableOutput("table1")
                    )
                )
            ),
            
            tabPanel("Instructions",
                mainPanel(
                    h3("Context"),
                    p(a("KnowYourStuffNZ", href = "https://knowyourstuff.nz"),
                      " is a community organisation of volunteers operating in collaboration with the ",
                      a("New Zealand Drug Foundation", href = "https://www.drugfoundation.org.nz"), 
                      " to provide drug related harm reduction services at events around New Zealand. This application 
                      shows KnowYourStuffNZ's pill testing results for the events they attended between 1 April 2018 and 
                      31 March 2019."),
                    h3("How to use this application "),
                    p("Click on the 'Pill testing results' tab above."),
                    h4("Select which presumed substance/s you want to look at"),
                    p("In the left hand panel, select at least one substance from the list. These substances are 
                    what clients thought they had before their pills had been tested."),
                    p("Once you have selected at least one substance, you will see a chart and a table on the right. 
                    These show the testing results: what the pills (that clients thought contained the substance 
                    you selected) actually turned out to contain. For example, if you select 'Amphetamine', 
                    you will see the testing results for all of the pills that were thought by clients to contain 
                      Amphetamine."),
                    h4("Results chart"),
                    p("The chart shows testing results grouped into drug families. These families are groupings of 
                      drugs that have similar structures and psychoactive effects."),
                    h4("Results table"),
                    p("The table shows the full list of drugs that were found ('Drug' column), the family that 
                    each drug is grouped into ('Drug family' column), and the number of pills that contained 
                    that drug ('Number of samples' column)."),
                    h4("Selecting more than one presumed substance"),
                    p("If you select more than one presumed substance, the results chart and table will add up  
                      the testing results for the pills that were presumed to contain any of the substances you selected."),
                    h3("Further information"),
                    p("For more further results from KnowYourStuffNZ's 2018-19 pill testing, visit ", 
                      a("https://knowyourstuff.nz/2018-19-results", href = "https://knowyourstuff.nz/2018-19-results/"))
                )
            )
        )
    )



# Define server logic required to draw the plot and the table

server <- function(input, output) {

        output$plot1 <- renderPlot({
        
        # Check at least one presumed substance is selected and provide message if not
            validate(
                need(input$presumed, 'Please use the checkboxes on the left to select at least one presumed substance')
            )
                
        # select rows for the appropriate presumed substance based on input$presumed from ui.R
            plot1dat <- fulltable %>%
                filter(pres_substance %in% input$presumed) %>%
                select(Overall_result_drug_family_c, samples) %>%
                group_by(Overall_result_drug_family_c) %>%
                mutate(samples = sum(samples)) %>%
                distinct() %>%
                ungroup()
    
        # plot the chart of actual substances (by drug family) for the selected presumed subtance/s
            ggplot(plot1dat, aes(x = Overall_result_drug_family_c, 
                           y = samples)) +
                geom_bar(stat = "identity", 
                    position = "dodge", 
                    fill = "lightsalmon", 
                    alpha = 0.8) +
                labs(y = "\nNumber of samples") +
                scale_y_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + # makes y axis values integers
                coord_flip(clip = "off") +
                theme(axis.title = element_text(colour = "chocolate4", size = rel(1.2)),
                      axis.title.y = element_blank(),
                      axis.text = element_text(colour = "chocolate4", size = rel(1.1)),
                      panel.grid.major.x = element_line(colour = "bisque2"),
                      panel.grid.major.y = element_blank(),
                      panel.background = element_blank(),
                      axis.ticks = element_blank()
                    )

        },
        width = 600)
        
        output$table1 <- renderTable({
            
            # Check at least one presumed substance is selected and provide message if not
            validate(
                need(input$presumed, 'Please use the checkboxes on the left to select at least one presumed substance')
            )

            # select rows for the appropriate presumed substance based on input$presumed from ui.R
            table1dat <- fulltable %>%
                filter(pres_substance %in% input$presumed) %>%
                select(Overall_result_drug_family_c, Overall_result_drug_c, samples) %>%
                group_by(Overall_result_drug_c) %>%
                mutate(samples = sum(samples)) %>%
                distinct() %>%
                ungroup()

            # Rename the columns to be understandable
            table1 <- table1dat %>%
                      rename("Drug" = Overall_result_drug_c,
                             "Drug family" = Overall_result_drug_family_c,
                             "Number of samples" = samples)                   # rename the columns for the report

            # Arrange rows in descending order of the number of samples found
            table1$Drug <- factor(table1$Drug,
                                  levels = table1$Drug[order(table1$`Number of samples`, decreasing = TRUE)],
                                  ordered = TRUE)  # Makes Drug variable a factor, ordered from most to least samples

            table1 <- arrange(table1, Drug) # puts rows in order of the Drug levels

        },
        # Format table with striped rows, smaller row heights, and hover-over row highlighting
        striped = TRUE,
        hover = TRUE,
        spacing = "xs")
}

# Run the application 
shinyApp(ui = ui, server = server)
