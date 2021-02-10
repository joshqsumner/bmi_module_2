library(shiny)
library(shinydashboard)
library(DT)
library(shinymanager)
library(plotly)
library(stringr)
library(RColorBrewer)
library(scales)
library(lubridate)
library(grDevices)
library(tidyverse)
library(shinyjs)
# Read in data



df<-read_csv("biomed2project-cleaning1.csv")


# Define UI 
ui <- dashboardPage(
  #useShinyjs(),
  dashboardHeader(title = "Five Guys"),
  dashboardSidebar(sidebarMenu(
    menuItem("Home",
             tabName = "home",
             icon = icon("readme")),
    menuItem("Example Cases",
             tabName = "ex_tab",
             icon=icon("bar-chart-o")))),
  dashboardBody(
    tabItems(
      tabItem("home", height="1000px", width="1000px",
              p("This shiny app will show a basic walkthrough of our logic model for treatment of VAP 
                and act as a wireframe for what a real UI could look like", br(),
                "There will be 2-3 example cases and the option to select a random simulated subject from our MDClone
                query results to show the difficulties in using MDClone data even once you extract it.",br(),"
                To get started, go to the next tab down on the left sidebar.")
              ), #CloseHome Tab
      tabItem("ex_tab",  height="1000px", width="1000px",
              tabBox(title="", height="1000px", width="1000px",
                                      tabPanel(title="Example Flow",
                                               column(width=8,
                                                      
                                                      conditionalPanel(condition = "input.slide === '1'",
                                                      box(title = "Pick a Test Case", width = NULL, height="400px", 
                                                          p("Either select an example case from the provided list or a random one from MDClone"),
                                                          br(),
                                                          radioButtons("subjectChosen", "Select Subject:", 
                                                                       choices=c("Test 1", "Test 2", "Test 3", "Random"),
                                                                       selected="Test 1"),
                                                          br(),
                                                          htmlOutput("firstSlide"),
                                                          br(),
                                                          p("Please review the patient records to ensure the patient has been 
                                                            diagnosed with pnemonia more than 48 hours post-intubation. If that is the case,
                                                            please move on to the next criteria.")
                                                          )),
                                                      conditionalPanel(condition = "input.slide === '2'",
                                                                       box(title = "MDR Risk Factors", width = NULL, height="400px", 
                                                                           p(HTML("<b> Are MDR Risk Factors Present? </b>"),
                                                                             br(),
                                                                             "One or more of:", br()),
                                                                           checkboxGroupInput(inputId = "mdr_risks", label="MDR Risks",
                                                                                              choiceNames=c(
                                                                                                "IV Antibiotic use withing the previous 90 days",
                                                                                                "Septic Shock at the time of VAP",
                                                                                                "ARDS preceding VAP",
                                                                                                "Greater than 5 days hospitalization prior to occurance of VAP",
                                                                                                "Acute renal replacement therapy prior to VAP onset"
                                                                                              ),
                                                                                              choiceValues=c(1,2,3,4,5))
                                                                           ),
                                                                       conditionalPanel(
                                                                    condition="input.mdr_risks.includes('1') || input.mdr_risks.includes('2') || input.mdr_risks.includes('3') || input.mdr_risks.includes('4') || input.mdr_risks.includes('5')",
                                                                                        box(width=NULL, status="primary", solidHeader = T,
                                                                                            title="Please Select Options from Prescriptions Lists",
                                                                                            radioButtons("mdr_drugs", "Select One of:",
                                                                                                               choices=c("Zosyn", "Cefepime", "Ceftazidime",
                                                                                                                         "Imipenem", "Meropenem", "Aztreonam")),
                                                                                            radioButtons("mdr_drugs_2", "Also Select One of:",
                                                                                                         choices=c("Amikacin", "Gentamicin", "Tobramycin",
                                                                                                                   "Levofloxacin", "Ciprofloxacin", "Aztreonam")),
                                                                                            radioButtons("mdr_drugs_3", "Also Select One of:",
                                                                                                         choices=c("Vancomycin", "Linezolid")),
                                                                                            actionButton("mdr_submit", "Done")
                                                                                            
                                                                                            ))
                                                                    
                                                                       ),
                                                      
                                                      
                                                      #end conditional panels for first column
                                                      box(
                                                        width = NULL, background = "black",
                                                        "Note: Some relevant data for Test Cases is not available from MDClone."
                                                      )
                                               ),
                                               column(width=4,
                                                      
                                                      conditionalPanel(condition = "input.slide === '1'",
                                                      box(title = "Patient Data", width = NULL, height="400px", 
                                                          p("Patient Information would be available to physician here", br(),br(),
                                                            tags$ol(
                                                              tags$li("Patient Anthropometrics"),
                                                              tags$li("Known Comorbidities"),
                                                              tags$li("Known Prescription Medications"),
                                                              tags$li("Known Other Drug Use")
                                                              ), br(), 
                                                            HTML("- <em>Notes Field</em>"), br(),
                                                            HTML("- <em>Notes Cont.</em>"), br(),
                                                            HTML("- <em>Notes Cont.</em>")," - ", code("Notes Timestamp"))
                                                          )#close box
                                                      ), 
                                                      
                                                      conditionalPanel(condition = "input.slide === '2'",
                                                                       box(title = "MDR Risk Notes", width = NULL, height="400px", 
                                                                           p("Risk Factors added to the patient record if they are not already present", br(),br(),
                                                                             textOutput("MDR_txt"), br(), 
                                                                             HTML("- <em>More Notes Space</em>")
                                                                             )
                                                                           )#close box
                                                                       ), 
                                                      
                                                      
                                                      #end conditional panels for second column
                                                      box(title = "Ready to Move On?", width = NULL, height="500px", 
                                                          p("Review main panel and move on when ready."),
                                                          selectInput("slide",
                                                                      NULL, 
                                                                      choices=c("1", "2","3","4"),
                                                                      selected = "1"))
                                                      
                                                      
                                                      ) #close column 2
                     )#close tabPanel
                     
                     )#close tabBox
              )#Close ex tab
                     ) #close tabitems
                     )#close dash body
              ) #close UI






# Define server logic required to draw a histogram
server <- function(input, output, session) {

  
  subject<-reactive({
    set.seed(as.integer(Sys.time()))
    if (input$subjectChosen=="Test 1"){
      sub<-df%>%
        filter(id=138)
    } else if (input$subjectChosen=="Test 2"){
      sub<-df%>%
        filter(id=2909)
    } else if (input$subjectChosen=="Test 3"){
      sub<-df%>%
        filter(id=22273)
    } else if (input$subjectChosen=="Random"){
      sub<-df%>%
        sample_n(1)
    }
    sub

  })
  
  # First Page Outputs
  output$firstSlide <- renderText({
    set.seed(678)
    if (input$subjectChosen=="Test 1"){
      text<-paste0("This subject is a 32 year old white male whose 
                   first ventilation was when they were 21 years old and 
                   who underwent a 'Continuous invasive mechanical ventilation for 96 consecutive hours or more'.")
    } else if (input$subjectChosen=="Test 2"){
      text<-paste0("This subject is an 88 year old black female whose 
                   first ventilation was when they were 69 years old and 
                   who underwent a 'Continuous invasive mechanical ventilation for 96 consecutive hours or more'.")
    } else if (input$subjectChosen=="Test 3"){
      text<-paste0("This subject is a 55 year old asian male whose 
                   first ventilation was when they were 47 years old and 
                   who underwent a 'Continuous invasive mechanical ventilation for 96 consecutive hours or more'.")
    } else if (input$subjectChosen=="Random"){
      if (is.na(sub$race[1]) & is.na(sub$sex[1])){
        RACE<-"person whose race and sex were not recorded"
        SEX<-""
      } else if (is.na(sub$sex[1]) & !is.na(sub$race[1])){
        SEX<-"person whose sex was not recorded"
        RACE<-paste0(str_to_lower(sub$race[1]), " person ")
      } else if (!is.na(sub$sex[1]) & is.na(sub$race[1])){

        RACE<-paste0(str_to_lower(sub$sex[1]), " whose race was not recorded")
        SEX<-""
        
      } else if (!is.na(sub$sex[1]) & !is.na(sub$race[1])){
        RACE<-paste0(str_to_lower(sub$race[1]))
        SEX<-paste0(str_to_lower(sub$sex[1]))
        
        }
      
      
      text<-paste0("This subject is a ", sub$age[1], " year old ", RACE , " ", SEX, ".")
      if(!is.na(sub$ref.procedure[1])){
        text<-paste0(text, " Who underwent '", str_to_lower(sub$ref.procedure[1]), "'.") 
      } else {
        text<-paste0(text, " Procedural details were not available via MD Clone.") 
      }
    }
    text<-paste0("<b>", text, "</b>")
    text
  })
  
  

  
  # Second Page Outputs
  
  Patient_Header<- renderText({
    set.seed(678)
    if (input$subjectChosen=="Test 1"){
      text<-paste0("32 year old white male")
    } else if (input$subjectChosen=="Test 2"){
      text<-paste0("88 year old black female")
    } else if (input$subjectChosen=="Test 3"){
      text<-paste0("55 year old asian male")
    } else if (input$subjectChosen=="Random"){
      if (is.na(sub$race[1]) & is.na(sub$sex[1])){
        RACE<-"person whose race and sex were not recorded"
        SEX<-""
      } else if (is.na(sub$sex[1]) & !is.na(sub$race[1])){
        SEX<-"person whose sex was not recorded"
        RACE<-paste0(str_to_lower(sub$race[1]), " person ")
      } else if (!is.na(sub$sex[1]) & is.na(sub$race[1])){
        RACE<-paste0(str_to_lower(sub$sex[1]), " whose race was not recorded")
        SEX<-""
      } else if (!is.na(sub$sex[1]) & !is.na(sub$race[1])){
        RACE<-paste0(str_to_lower(sub$race[1]))
        SEX<-paste0(str_to_lower(sub$sex[1]))  
      }
      text<-paste0(sub$age[1], " year old ", RACE , " ", SEX, ".")
    }
    text<-paste0("<b>", text, "</b>")
    text
  })
  
  output$patientHeader <- renderText({
    Patient_Header()
  })
  
  output$MDR_txt <- renderText({
    riskFactors <- paste(input$mdr_risks, collapse = ", ")
    paste("Added today:", riskFactors)
  })
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  observeEvent(input$Back, {
    session$sendCustomMessage(type = 'resetInputValue', message = "nextButton")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

