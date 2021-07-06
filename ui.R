library(shiny)

options(spinner.color="#69A728", spinner.color.background="#ffffff", spinner.size=2)

shinyUI <- dashboardPage(
    dashboardHeader(
        ### changing logo
        title = shinyDashboardLogo(
            theme = "grey_light",
            boldText = "Shiny",
            mainText = "App",
            badgeText = "v2021.7.7"),
        dropdownMenuOutput("messageMenu")
    ),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
    ### changing theme
            shinyDashboardThemes(
            theme = "flat_red"
            ),
            tags$head(
                tags$head(tags$style(HTML('.main-header .logo {font-family: "Georgia", Times, "Times New Roman", serif;
                                                               font-weight: bold;font-size: 24px;}
                                          ')))
                ),
    navbarPage(
        title = 'Global IT Consulting Utilization Dashboard',theme = shinytheme("flatly"), collapsible = TRUE,
        tabPanel('Main Dashboard', icon = icon("th"),
                 tags$h2("KEY INDICATORS"),
                 tags$h5("You may see what this firm has achieved so far:"),
                 fluidRow(withSpinner(infoBoxOutput("1B"),type=1),
                          infoBoxOutput("2B"),
                          infoBoxOutput("3B"),
                          infoBoxOutput("4B"),
                          infoBoxOutput("5B"),
                          infoBoxOutput("6B")
                 ),
                 tags$h2("GLOBAL ENGAGEMENT PRESENCE"),
                 tags$h5("Here is a map view about the active projects, client cities, and labeled project funding scale:"),
                 column(withSpinner(leafletOutput("mymap")),width=12)
        ),
        tabPanel('Exploratory Data', icon = icon("dashboard"), column(width=12,
                                     titlePanel("EXPLORATORY DATA - TREE MAP"),
                                     fluidRow(
                                         column(12,
                                                includeText("User.txt"),
                                                br(),
                                                pre(includeText("treemap.txt"))
                                         )
                                     ),
                                     withSpinner(plotOutput("treemap"), type = 1),
                                     hr(),
                                     withSpinner(plotOutput("treemap2"), type = 1)
                                     )
        ),
        navbarMenu("IT Consulting Engagement",icon = icon("file-code-o"), 
                   tabPanel('List of Consultant',tags$h3("1. IT Consultant Search"), DT::dataTableOutput('ex1')),
                   tabPanel('Area of Interest',tags$h3("2. Areas of Interests"), DT::dataTableOutput('ex2')),
                   tabPanel('Language',tags$h3("3. Bilingual/Language"), DT::dataTableOutput('ex3')),
                   tabPanel('Project Experience',tags$h3("4. Project Experince by type and year"), DT::dataTableOutput('ex4')),
                   tabPanel('Skill Level',tags$h3("5. Project Skill Level"), DT::dataTableOutput('ex5'))
        ),
        tabPanel('Global IT Project Database',icon = icon("file-powerpoint",lib = "font-awesome"),
                 sidebarLayout(
                     sidebarPanel(
                         tags$h2("Global IT Project Database"),
                         tags$h4("Please select from below drop-down list:"),
                         pickerInput("p1", "-1-  Client:",   
                                     choices = unique(Client_Project2$'Client'), 
                                     options = list(`actions-box` = TRUE),
                                     selected = unique(Client_Project2$'Client'),
                                     multiple = TRUE),
                         pickerInput("p2", "-2-  Project Name:",   
                                     choices = unique(Client_Project2$'Project Name'),
                                     options = list(`actions-box` = TRUE),
                                     selected = unique(Client_Project2$'Project Name'),
                                     multiple = TRUE),
                         pickerInput("p3", "-3-  City/Region:",   
                                     choices = unique(Client_Project2$'City'),
                                     options = list(`actions-box` = TRUE),
                                     selected = unique(Client_Project2$'City'),
                                     multiple = TRUE),
                         pickerInput("p4", "-4-  Required Domain Skillset:",   
                                     choices = unique(Client_Project2$'Demanding Domain Skill'),
                                     options = list(`actions-box` = TRUE),
                                     selected = unique(Client_Project2$'Demanding Domain Skill'),
                                     multiple = TRUE),
                         pickerInput("p5", "-5-  Highlight the fileds in the view:",   
                                     choices = colnames(Client_Project2),
                                     options = list(`actions-box` = TRUE),
                                     selected = colnames(Client_Project2),
                                     multiple = TRUE),
                         submitButton("Update filters"),
                     ),
                     mainPanel(DT::dataTableOutput("tb1"))
                     )
                     ),
        tabPanel('Consultant Profile',icon = icon("address-card",lib = "font-awesome"),
                 fluidRow(
                     selectInput("c1", "Consultant Name:", 
                                 choices=Skill_Level$`Consultant Name`),
                     submitButton("Update filters"),
                     hr(),
                     helpText("Data from IT Consultancy Database.")
                 ),
                 fluidRow(box(title = "Basic Profile Information", 
                              width = 12,collapsible = TRUE,
                              tableOutput("tb2"))),
                 fluidRow(
                     box(title = "Expertise - ScatterPolar View", 
                                  width = 4,collapsible = TRUE,
                                  plotlyOutput("SkillPlot2", height = 350)
                         ),
                     box(title = "Interest Area / Expertise", solidHeader = TRUE,width = 4,
                         "Yellow Category represents the areas that the consultant feel interested in - ", br(),
                         " ******************************************************************************************** ",
                         collapsible = TRUE,
                         plotOutput("SkillPlot", height = 350)
                     ),
                     box(title = "Interest Area / Expertise", solidHeader = TRUE,width = 4,
                         "Yellow Category represents the areas that the consultant feel interested in - ", br(),
                         " ******************************************************************************************** ",
                         collapsible = TRUE,
                         plotOutput("EPlot", height = 350)
                    )
                )
        

       ),
    tabPanel('Expertise Comparison', icon = icon("chart-area",lib = "font-awesome"),
             tags$h2("EXPERTISE COMPARISON"),
             tags$h5("To visualize the graph of the consultant expertise, select the name from the drop-dowm menu. It is worth noting that graphics will be overlapped.:"),
             sidebarPanel(
                 selectInput("c2", "Please choose consultants from below:", multiple = T, selected = Skill_Level$`Consultant Name`[1],
                             choices=Skill_Level$`Consultant Name`),
                 submitButton("Update filters"),
                 hr(),
                 helpText("Data from IT Consultancy Database.")
             ),
             fluidRow(box(width = 6,plotlyOutput("SkillPlot3", width = 1200, height = 600)
                      )
                      
             )
             
             
    ),
    tabPanel(
        
        "About this App",icon = icon("globe",lib = "font-awesome"),
        
        # Various tabs.
        tabsetPanel(
            
            # General info.
            tabPanel(
                "Overview",
                titlePanel("How to Use this Tool - Goal"),
                fluidRow(
                    column(12,
                           includeText("Brief.txt"),
                           br(),
                           pre(includeText("Background.txt"))
                    )
                ),
                tags$p(HTML("The aim is to complement these resources with several interactive features using this app.")),
                hr(),
                tags$h1("Scope"),
                tags$p(HTML("This collection of visualizations addresses the question - ")),
                pre(includeText("Questions.txt")),
                tags$p(HTML("The aim is to complement these resources with several interactive features using this app.")),
                hr(),
                tags$h1("Approach"),
                tags$p(HTML("Data are processed using R:")),
                tags$ul(
                    tags$li(HTML("Data Collection: Collected through firmwide surveys and reports on bi-weekly basis from the pespective of resources (IT Consultants) and client projects.")),
                    tags$li(HTML("Data Manipulation: Through dyply, tidyr, etc. packages.")),
                    tags$li(HTML("Data Visualization: Using embedded ggplot2 and shiny functions."))
                ),
                hr(),
                tags$h1("GitHub"),
                tags$p(HTML("Source code is available at <a href=\"https://github.com/kaplanas/Shiny-Lego\">https://github.com/kaplanas/Shiny-Lego</a>."))
            ),
            
            # Acknowledgement/Credits
            tabPanel(
                "Acknowledgement",
                tags$h1("Datasets"),
                tags$p(HTML("All Shiny data comes from the files made available by <a href=\"https://www.kaggle.com/granjithkumar/it-employees-data-for-project-allocation\">Kaggle</a>. Ideally, The simulated app checks for new data from the firm's database on bi-weekly basis.")),
                hr(),
                tags$h1("R Packages"),
                tags$p(HTML("<a href=\"http://shiny.rstudio.com/\">Shiny</a> and the <a href=\"https://www.tidyverse.org/\">tidyverse</a>, are the foundamental.")),
                tags$p(HTML("The dashboard layouts are produced using <a href=\"https://https://rstudio.github.io/shinydashboard\">shinydashboard</a>.")),
                tags$p(HTML("<a href=\"https://leafletjs.com\">Leaflet</a> is used to model the data and produce an interactive map")),
                tags$p(HTML("Treemaps, polar charts, and bar charts are rendered with <a href=\"https://www.highcharts.com/\">Highcharts</a>, via <a href=\"http://jkunst.com/highcharter/\">Highcharter</a>.")),
                tags$p(HTML("Tables are rendered with <a href=\"https://datatables.net/\">DataTables</a>, using the <a href=\"https://rstudio.github.io/DT/\">DT</a> package.")),
            ),
            tabPanel(
                "Upload a file to the App",
                # Input: Select a file ----
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Checkbox if file has header ----
                checkboxInput("header", "Header", TRUE),
                
                # Input: Select separator ----
                radioButtons("sep", "Separator",
                             choices = c(Comma = ",",
                                         Semicolon = ";",
                                         Tab = "\t"),
                             selected = ","),
                
                # Input: Select quotes ----
                radioButtons("quote", "Quote",
                             choices = c(None = "",
                                         "Double Quote" = '"',
                                         "Single Quote" = "'"),
                             selected = '"'),
                
                # Horizontal line ----
                tags$hr(),
                
                # Input: Select number of rows to display ----
                radioButtons("disp", "Display",
                             choices = c(Head = "head",
                                         All = "all"),
                             selected = "head"),
                tableOutput("contents")
            ),
            tabPanel(
                "Download a file from the App",
                # Input: Choose dataset ----
                selectInput("dataset", "Choose a dataset:",
                            choices = c("Resource Master", "Project Master")),
                
                # Button
                downloadButton("downloadData", "Download")
            )            
            
        )
        
    )
    
    
   )
))

