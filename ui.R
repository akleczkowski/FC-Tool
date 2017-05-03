if (!"deSolve" %in% installed.packages()) install.packages("deSolve")
library(deSolve)
if (!"shiny" %in% installed.packages()) install.packages("shiny")
library(shiny)
if (!"shinythemes" %in% installed.packages()) install.packages("shinythemes")
library(shinythemes)

navbarPage("FDDST",id="select.tab",
  theme=shinytheme("superhero"),
#           shinythemes::themeSelector(),
           tabPanel("Welcome",
                    helpText(h4("Forest disease decision support tool")),
                    helpText("Created by Morag Macpherson and Adam Kleczkowski"),
                    helpText("Project led by Glyn Jones, FERA"),
                    helpText("Collaboration with Julia Touza and Piran White, York, and Stephen Parnell, Salford"),
                    tags$hr(),
                    helpText(h5("Warning: This is a development version. Only limited checking of variables and parameters is performed, hence the tool can break down or produce unreliable results if parameters are entered outside the realistic range.")),
                    tags$hr(),
                    helpText(h5("Version history:")),
                    helpText("Version 0.1: single zone; eradication control; control cost for whole area; value proportional to healthy area."),
                    helpText("Version 0.2: improved GUI and additional inputs/outputs; linked to PHRR"),
                    helpText("Version 0.3: changes to menu; system of equations rather than a single one; discounted profit")
           ),
           tabPanel("Dashboard",
                  sidebarLayout(
                      sidebarPanel(
                        fluidPage(
                        helpText(h4("Choose the parameters:")),
                        fluidRow(
                         sliderInput("beta.scale",
                                      h5("Select infection rate mutliplier (how fast will the disease spread?):"),
                                      min = 0,
                                      max = 2,
                                      step=0.05,
                                      value = 1.0)
                       ),
                        fluidRow(
                         sliderInput("beta.var",
                                      h5("Select infection rate variability (how uncertain are we about future disease spread?):"),
                                      min = 0,
                                      max = 5,
                                      step=0.05,
                                      value = 1.0)
                        ),
                        fluidRow(
                          sliderInput("y0.var",
                                      h5("Select initial state variability (how uncertain are we about how much disease is now?):"),
                                      min = 0,
                                      max = 1.5,
                                      step=0.05,
                                      value = 0)
                        ),
                        fluidRow(
                          sliderInput("effort",
                                      h5("Select control effort (number of man-weeks to implement control measures?):"),
                                      min = 0,
                                      max = 500,
                                      step=10,
                                      value = 0.0)
                        ),
                        submitButton("Update")
                      ) # fluid page
                    ), # sidebar panel
                      # Show a plot of the generated distribution
                    mainPanel(
                      fluidPage(
                        fluidRow(
#                          helpText(h4("Predictions:")),
                          column(6,
                                 h5("Proportion of healthy and infected trees 
                                    and total profit over a year:"),
                                 h4(tableOutput("table"))
                                 ),
                          column(6,
                                 h5("Distribution of combined profit/costs:\nred=with control, blue=do nothing"),
                                 plotOutput("distPlot",width="275px",height="200px")
                          )
                        ),
                        fluidRow(
                          column(6,
                            helpText(h4("Simulation trace:")),
                #            plotOutput("tracePlot",width="525px",height="350px")
                            plotOutput("tracePlot",width="350px",height="350px"),
                            helpText(h5("(green=healthy, red=infected; 10 replicates are shown)"))
                          ),
                          column(6,
                                 helpText(h4("Parameter distributions")),
                                 plotOutput("parsPlot",width="275px",height="350px")
                          )
                      ) # fluid row
                  ) # fluid page
                ) # main panel
                  )
           ), # Dashboard
navbarMenu("Settings",
          tabPanel("General parameters",
                    helpText(h4("Set general parameters")),
              fluidPage(
                fluidRow(
                  column(6,
                    numericInput("Nsize","Area (ha):",100,min=50,max=1000,step=50),
                    numericInput("Cost","Cost per man-week:",25,min=0,max=100,step=0.1),
                    numericInput("d.rate","Discount rate (per year):",0.05,min=0,max=0.5,step=0.01),
                    helpText(h4("Run-time parameters")),
                    helpText(h5("(set with caution, as large number means slow runs)")),
                    numericInput("Nrep","Number of replicates:",100,min=50,max=1000,step=50)
                  ),
                  column(6,
                    selectInput("control.prop", 
                      label = h5("Select model for control costs:"), 
                      choices = c("All area"=0,"Infected area only"=1),
                      selected = 0),
                    selectInput("control.varmodel", 
                      label = h5("Select model for control variability:"), 
                      choices = c("No variability"=0,"Exponential distribution"=1),
                      selected = 1)
                  )
                ) # fluidRow
              ), # fluidPage
              tags$hr(),
                   submitButton("Update"),
                   tags$hr(),
                    h4("Current list of parameters:"),
                    h3(tableOutput("parametersTable"))
           ),
            tabPanel("Values at risk",
                     helpText(h4("Set values at risk")),
                     helpText(h5("Currently implemented via scenarios")),
                     tags$hr(),
                     submitButton("Update")
            ),
            tabPanel("Initial prevalence when found",
                     helpText(h4("Set initial prevalence using rule of thumb")),
                     helpText(h5("Currently implemented via scenarios")),
                     tags$hr(),
                     submitButton("Update")
            ),
            tabPanel("Scenarios",
                    helpText(h4("Setting scenarios")),
                    uiOutput("selectScenario"),
                    tags$hr(),
                    h5("Available scenarios:"),
                    h4(tableOutput("scenarioTable.scenarios")),
#                    h4(textOutput("test.scenarios"))
                    tags$hr(),
                    submitButton("Update")
           )
),
navbarMenu("Input and output",
          tabPanel("Upload scenarios",
                    fileInput("uploadScoresFile", "Choose CSV File",
                      accept = c(
                        "text/csv",
                        "text/comma-separated-values,text/plain",
                        ".csv")
                    ),
                    tags$hr(),
                    h5("Available scenarios:"),
                   h4(tableOutput("scenarioTable.upload"))
          ),
          tabPanel("Download results",
                   selectInput("dataset", "Choose a dataset:", 
                               choices = c("Healthy", "Infected", "Profit")),
                   downloadButton('downloadData', 'Download')
          ),
          tabPanel("Generate report",
                   fluidPage(
                     h3("Simulation report"),
                     tags$hr(),
                     fluidRow(
                       helpText(h4("Predictions:")),
                                column(6,
                                       h5("Proportion of healthy and infected trees 
                                          and total profit over a year:"),
                                       h4(tableOutput("table.report"))
                                       ),
                                column(6,
                                       h5("Distribution of combined profit/costs:\nred=with control, blue=do nothing"),
                                       plotOutput("distPlot.report",width="275px",height="200px")
                                )
                       ),
                     tags$hr(),
                        h4("Current list of parameters:"),
                        h3(tableOutput("parametersTable.report"))
#                        tags$hr(),
#                        submitButton("Update")
                     ) # fluid page
          )
),
           tabPanel("Help",
                    fluidPage(
                      withMathJax(helpText(h2("Forest simulator ."))),
                      hr(),
                      h5("This code simulates disease progress in a forest. 
                         For details, see ",a(href="macpherson+16.pdf","Macpherson et al. (2017).",target="_blank")),
                      p("The model:"),         
                      hr(),
                      p("$$\\frac{dS}{dt}=-\\beta I\\left(1- \\frac{I}{N}\\right)$$"),
                      p("$$\\frac{dI}{dt}=\\beta I\\left(1- \\frac{I}{N}\\right) - C I$$"),
                      hr(),
                      p("where \\(S\\) represents healthy forest area, \\(I\\) represents infected area, and \\(N=S+I\\)."),
                      p("\\(\\beta\\) is the rate of spread and \\(C\\) represents control efforts."),
                      p("Time is measured in weeks and the horizon is assumed to be a year. Because of a short period, no discounting is applied."),
                      p("These equations are solved numerically twice, once with and once without control effort."),
                      p("Control effort is a function of investment:"),
                      hr(),
                      p("$$C=c_r e $$"),
                      hr(),
                      p("where \\(c_r\\) is a rate at which control is effective and \\(e\\) is the effort (represented here as the number of man-week)"),
                      p("The total profit as a function of time is given by:"),
                      hr(),
                      p("$$\\Pi(t)=v S(t) - \\int_0^t w_e e \\, dt$$"),
                      p("or"),
                      p("$$\\Pi(t)=v S(t) - \\int_0^t w_e e\\, \\frac{I(t)}{N}\\, dt$$"),
                      hr(),
                      p("where \\(e\\) is the effort (as above) and \\(w_e\\) is the cost of each man-week. \\(v\\) is the value of healthy forest and \\(S(t)\\) is the current number of healthy trees."),
                      p("Additionally, the state is summarised at the end of the year and final share of healthy and infected areas, the final profit and the cumulative cost are all shown."),
                      hr(),
                      h3("Usage:"),
                      p("A generic scenario is loaded at start; click on Dashboard to see the results"),
                      p("To upload an Excel (csv) file with Plant Health Risk Register data, click on Upload tab and select the file;"),
                      p("then click on the Scenario tab and select which scenario to load"),
                      p("The PHRR scores are translated into parameters and a new simulation is run."),
                      p("Basic parameters like the system size and number of simulations per run can be selected on the Parameters tab.")
                    )
           )
)
           