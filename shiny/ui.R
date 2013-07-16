shinyUI(pageWithSidebar(
    
  headerPanel("Custom Enrollment Patterns Tool Prototype - version 1.5"),
  
  sidebarPanel(
    tags$head(tags$style(type="text/css",
                         "label.radio { display: inline-block; margin: 3px}",
                         ".radio input[type=\"radio\"] { float: none; }"
    )),  
    strong("Please select from the following options and subgroups:"), br(),
  helpText("Use this box to switch between attachment patterns and intensity patterns."),
  checkboxInput("ftpt", "Toggle Attachment/Intensity", value=FALSE), br(),
#   sliderInput(inputId = "amount",
#               label = "Amount of something",
#               min = 1, max = 100, value = 20, step = 1),
# 
  helpText("Use these boxes to look at certain student groups."),
  radioButtons(inputId="collxfer", label="Transfer Status", choices=c("Transferred  "="collxfer", "Not transferred  "="nocollxfer", "All"="allxfer")),
  radioButtons(inputId="collxferprog", label="Programs", choices=c("Transfer (AA/AS/AFA)  "="collxferprog", "AAS  "="aas", "All"="allprog")),
  br(),
  h4("Demographics"),
  checkboxInput("male", "Male", value=TRUE), 
  checkboxInput("female", "Female", value=TRUE), 
  br(),
  checkboxInput("white", "White", value=TRUE), 
  checkboxInput("black", "Black", value=TRUE), 
  checkboxInput("amerind", "Native American", value=TRUE), 
  checkboxInput("hispanic", "Hispanic", value=TRUE), 
  checkboxInput("asian", "Asian", value=TRUE), 
  checkboxInput("otherrace", "Other race", value=TRUE), 
  br(),
  radioButtons(inputId="deved", label="Dev Ed", choices=c("Any Dev  "="anydev", "No Dev  "="nodev", "All"="alldev")), 
  selectInput("firstcollege", "Select college:",
              c("All"="All",collvec), selected="All"),
  
  br(),
  h4("Outcome Overlays"),
  checkboxInput(inputId = "showtran",
                label = "Show transfers up",
                value = FALSE),
  radioButtons(inputId="showcred", 
               label="Show Credentials", 
               choices=c("None", sort(unique(credset$credential)[1:7]))),
  checkboxInput(inputId = "showba",
                label = "Show 4-year credentials",
                value = FALSE),
  br(), 
  wellPanel(p("Images represent enrollment patterns for FTIC students who began in Fall 2003. In Attachment image, orange indicates credit was attempted. In Intensity image, orange indicates full-time and blue indicates part-time. Intensity in four-year schools is estimated from NSC data. Terms are numbered such that 1 is Fall, 2 is Spring, 3 is Summer, etc."))
  #conditionalPanel(condition="$('div#baseline').text()=='' || $('html').hasClass('shiny-busy')", img(src="loading.gif")),
  #conditionalPanel(condition="!($('div#baseline').text()=='' || $('html').hasClass('shiny-busy'))", br()),
  ),
  
  mainPanel(
  
#     h2("Output"), 
    plotOutput(outputId = "graphsoutput", height = "800px"),
    textOutput(outputId="enequal"),
    textOutput(outputId="oequal"),
    textOutput(outputId="xequal"),
    textOutput(outputId="fourequal")) 
  ))
