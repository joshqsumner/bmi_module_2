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
             icon=icon("bar-chart-o")),
    menuItem("Logic Model",
              tabName="img_tab",
              icon=icon("clipboard-list")))),
  dashboardBody(
    tabItems(
      tabItem("home", height="1000px", width="1000px",
              h1("Five Guys"),
              br(), h3("Shiny Wireframe"),
              p("This shiny app will show a basic walkthrough of our logic model for treatment of VAP 
                and act as a wireframe for what a real UI could look like. This is obviously not how an end product would look.", br(),
                "There will be 2-3 example cases and the option to select a random simulated subject from our MDClone
                query results.",br(),"
                To get started, go to the second tab on the left sidebar or review our logic model in the last tab..")
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
                                                          p("Please review the patient records (would be available in sidebar) to ensure the patient has been 
                                                            diagnosed with pnemonia more than 48 hours post-intubation. If that is the case,
                                                            please move on to the next criteria.")
                                                          )),
                                                      conditionalPanel(condition = "input.slide === '2'",
                                                                       box(title = "MDR Risk Factors", width = NULL, height=NULL, collapsible=T, collapsed=F, 
                                                                           p(HTML("<b> Are MDR Risk Factors Present? </b>"),
                                                                             br()),
                                                                           checkboxGroupInput(inputId = "mdr_risks", label="MDR Risks",
                                                                                              choiceNames=c(
                                                                                                "IV Antibiotic use withing the previous 90 days",
                                                                                                "Septic Shock at the time of VAP",
                                                                                                "ARDS preceding VAP",
                                                                                                "Greater than 5 days hospitalization prior to occurance of VAP",
                                                                                                "Acute renal replacement therapy prior to VAP onset"
                                                                                              ),
                                                                                              choiceValues=c(
                                                                                                "IV Antibiotic use withing the previous 90 days",
                                                                                                "Septic Shock at the time of VAP",
                                                                                                "ARDS preceding VAP",
                                                                                                "Greater than 5 days hospitalization prior to occurance of VAP",
                                                                                                "Acute renal replacement therapy prior to VAP onset"
                                                                                              )),
                                                                           p(br(),HTML("<b> If none of the above are present, select next page.</b>"))
                                                                           )),
                                                                       conditionalPanel(
                                                                    condition="input.mdr_risks.includes('IV Antibiotic use withing the previous 90 days') || 
                                                                    input.mdr_risks.includes('Septic Shock at the time of VAP') || 
                                                                    input.mdr_risks.includes('ARDS preceding VAP') ||
                                                                    input.mdr_risks.includes('Greater than 5 days hospitalization prior to occurance of VAP') ||
                                                                    input.mdr_risks.includes('Acute renal replacement therapy prior to VAP onset')",
                                                                                        box(width=NULL, status="primary", solidHeader = T,
                                                                                            title="Please Consider Options from Prescriptions Lists",
                                                                                            radioButtons("mdr_drugs", "Select One of:",
                                                                                                               choices=c("Zosyn", "Cefepime", "Ceftazidime",
                                                                                                                         "Imipenem", "Meropenem", "Aztreonam")),
                                                                                            radioButtons("mdr_drugs_2", "Also Select One of:",
                                                                                                         choices=c("Amikacin", "Gentamicin", "Tobramycin",
                                                                                                                   "Levofloxacin", "Ciprofloxacin", "Aztreonam")),
                                                                                            radioButtons("mdr_drugs_3", "Also Select One of:",
                                                                                                         choices=c("Vancomycin", "Linezolid")),
                                                                                            actionButton("slide_2_submit", "Order")
                                                                                            
                                                                                            )),
                                                      
                                                      conditionalPanel(condition = "input.slide === '3'",
                                                                       box(title = "Gram Negative Bacilli Risk Factors", width = NULL, height=NULL, 
                                                                           p(HTML("<b> Are GNB Risk Factors Present? </b>"),
                                                                             br(),
                                                                             "One or more of:", br()),
                                                                           checkboxGroupInput(inputId = "gnb_risks", label="Gram Negative Bacilli Risks",
                                                                                              choiceNames=c(
                                                                                                "Treatment in a unit in which > 10 % GNB are resistant to an agent considered for monotherapy",
                                                                                                "Treatment in a unit in which the prevalence of resistance among GNB is unknown"
                                                                                              ),
                                                                                              choiceValues=c(
                                                                                                "Treatment in a unit in which > 10 % GNB are resistant to an agent considered for monotherapy",
                                                                                                "Treatment in a unit in which the prevalence of resistance among GNB is unknown"
                                                                                              )),
                                                                           p(br(),HTML("<b> If none of the above are present, select next page.</b>")),
                                                                           collapsible = T, collapsed = F
                                                                       ),
                                                                       conditionalPanel(
                                                                         condition="input.gnb_risks.includes('Treatment in a unit in which > 10 % GNB are resistant to an agent considered for monotherapy') || 
                                                                    input.gnb_risks.includes('Treatment in a unit in which the prevalence of resistance among GNB is unknown')",
                                                                         box(title = "MRSA Risk Factors", width = NULL, height=NULL, 
                                                                             p(HTML("<b> Are One or More MRSA Risk Factors Present? </b>"),
                                                                               br(),
                                                                               br()),
                                                                             checkboxGroupInput(inputId = "mrsa_risks", label="MRSA Risk Factors:",
                                                                                                choiceNames=c(
                                                                                                  "Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant",
                                                                                                  "Treatment in a unit in which the prevalence of MRSA is not known",
                                                                                                  "No MRSA Risk Factors Present"
                                                                                                ),
                                                                                                choiceValues=c(
                                                                                                  "Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant",
                                                                                                  "Treatment in a unit in which the prevalence of MRSA is not known",
                                                                                                  "No MRSA Risk Factors Present"
                                                                                                )),
                                                                             collapsible = T, collapsed = F
                                                                         )),
                                                                         conditionalPanel(
                                                                           condition="input.mrsa_risks.includes('Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant')||
                                                                           input.mrsa_risks.includes('Treatment in a unit in which the prevalence of MRSA is not known')",
                                                                           box(width=NULL, status="primary", solidHeader = T, collapsible = T, collapsed = F,
                                                                               title="Please Consider Options from Prescriptions Lists",
                                                                               radioButtons("mdr_drugs", "Select One of:",
                                                                                            choices=c("Zosyn", "Cefepime", "Ceftazidime",
                                                                                                      "Imipenem", "Meropenem", "Aztreonam")),
                                                                               radioButtons("mdr_drugs_2", "Also Select One of:",
                                                                                            choices=c("Amikacin", "Gentamicin", "Tobramycin",
                                                                                                      "Levofloxacin", "Ciprofloxacin", "Aztreonam")),
                                                                               radioButtons("mdr_drugs_3", "Also Select One of:",
                                                                                            choices=c("Vancomycin", "Linezolid")),
                                                                               actionButton("slide_3_submit_1", "Order")
                                                                               
                                                                           )
                                                                         ),
                                                                         conditionalPanel(
                                                                           condition="input.mrsa_risks.includes('No MRSA Risk Factors Present')",
                                                                           box(width=NULL, status="primary", solidHeader = T,
                                                                               title="Please consider options from these prescription lists",
                                                                               radioButtons("no_mrsa_drugs", "Select One of:",
                                                                                            choices=c("Zosyn", "Cefepime", "Ceftazidime",
                                                                                                      "Imipenem", "Meropenem")),
                                                                               radioButtons("no_mrsa_drugs_2", "Also Select One of:",
                                                                                            choices=c("Amikacin", "Gentamicin", "Tobramycin",
                                                                                                      "Levofloxacin", "Ciprofloxacin", "Aztreonam")),
                                                                               actionButton("slide_3_submit_1", "Order")
                                                                               
                                                                           )
                                                                         )
                                                                       
                                                                       ),
                                                      conditionalPanel(condition = "input.slide === '4'",
                                                                       box(title = "MRSA Risk Factors", width = NULL, height=NULL, collapsible = T, collapsed = F, 
                                                                           p(HTML("<b> Are MRSA Risk Factors Present? </b>"),
                                                                             br(),
                                                                             "One or more of:", br()),
                                                                           checkboxGroupInput(inputId = "mrsa_risks_2", label="MRSA Risk Factors:",
                                                                                              choiceNames=c(
                                                                                                "Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant",
                                                                                                "Treatment in a unit in which the prevalence of MRSA is not known",
                                                                                                "No MRSA Risk Factors Present"
                                                                                              ),
                                                                                              choiceValues=c(
                                                                                                "Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant",
                                                                                                "Treatment in a unit in which the prevalence of MRSA is not known",
                                                                                                "No MRSA Risk Factors Present"
                                                                                              ))
                                                                       ),
                                                                       conditionalPanel(
                                                                         condition="input.mrsa_risks_2.includes('Treatment in a unit in which > 10 to 20% of S. aureus isolates are methicillin-resistant')||
                                                                         input.mrsa_risks_2.includes('Treatment in a unit in which the prevalence of MRSA is not known')",
                                                                         box(width=NULL, status="primary", solidHeader = T,
                                                                             title="Please Consider Options from Prescriptions Lists",
                                                                             radioButtons("mdr_drugs_yes", "Select One of:",
                                                                                          choices=c("Zosyn", "Cefepime", "Ceftazidime",
                                                                                                    "Levofloxacin", "Ciprofloxacin", "Aztreonam")),
                                                                             radioButtons("mdr_drugs_yes_2", "Also Select One of:",
                                                                                          choices=c("Vancomycin", "Linezolid")),
                                                                             actionButton("slide_4_submit_1", "Done")
                                                                             
                                                                         )
                                                                       ),
                                                                       conditionalPanel(
                                                                         condition="input.mrsa_risks_2.includes('No MRSA Risk Factors Present')",
                                                                         box(width=NULL, status="primary", solidHeader = T,
                                                                             title="Please Consider these Treatment Options",
                                                                             radioButtons("no_mrsa_drugs_2_1", "Select One of:",
                                                                                          choices=c("Zosyn", "Cefepime", "Levofloxacin")),
                                                                             actionButton("slide_4_submit_2", "Order")
                                                                         )
                                                                       )
                                                                       ),
                                                      
                                                      
                                                      #end conditional panels for first column
                                                      box(
                                                        width = NULL, background = "black",
                                                        "Note: Most relevant data for Test Cases is not available from MDClone."
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
                                                                           p("(Risk Factors added to the patient record if they are not already present)", br(),br(),
                                                                             textOutput("MDR_txt"), br(), 
                                                                             HTML("- <em>More Notes Space</em>")
                                                                             )
                                                                           )#close box
                                                                       ),
                                                      conditionalPanel(condition = "input.slide === '3'",
                                                                       box(title = "GNB Risk Notes", width = NULL, height="400px", 
                                                                           p("(Risk Factors added to the patient record if they are not already present)", br(),br(),
                                                                             textOutput("GNB_txt"), br(), 
                                                                             textOutput("MRSA_txt_1"), br(), 
                                                                             HTML("- <em>More Notes Space</em>")
                                                                           )
                                                                       )#close box
                                                      ),
                                                      conditionalPanel(condition = "input.slide === '4'",
                                                                       box(title = "MDR Risk Notes", width = NULL, height="400px", 
                                                                           p("Risk Factors added to the patient record if they are not already present", br(),br(),
                                                                             textOutput("mrsa_risks_2"), br(), 
                                                                             HTML("- <em>More Notes Space</em>")
                                                                           )
                                                                       )#close box
                                                      ),
                                                      
                                                      
                                                      #end conditional panels for second column
                                                      box(title = textOutput("Bottom_Panel_Title"), width = NULL, height="500px", 
                                                          br(),
                                                          textOutput("Other_Bottom_Panel_Text"),
                                                          selectInput("slide",
                                                                      NULL, 
                                                                      choices=c("1", "2","3","4"),
                                                                      selected = "1"))
                                                      
                                                      
                                                      ) #close column 2
                     )#close tabPanel
                     
                     )#close tabBox
              ),#Close ex tab
      tabItem("img_tab",  height="1000px", width="1000px",
              img(src="logicModel.png", height="650px", width="1000px")
              )#close image tab
                     ) #close tabitems
                     )#close dash body
              ) #close UI






# Define server logic required to draw a histogram
server <- function(input, output, session) {

  # General outputs
  output$Bottom_Panel_Title<-renderText({
    if(input$slide==4){
      text<-"End of this subject's workflow"
    } else{
      text<-"Ready to move on?"
    }
    text
  })
  output$Other_Bottom_Panel_Text<-renderText({
    if(input$slide==4){
      text<-"Select new subject on page 1"
    } else{
      text<-"Review Main Panel and go to next page if needed"
    }
    text
  })
  
  
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
      if (is.na(subject()%>%pull(race)) & is.na(subject()%>%pull(sex))){
        RACE<-"person whose race and sex were not recorded"
        SEX<-""
      } else if (is.na(subject()%>%pull(sex)) & !is.na(subject()%>%pull(race))){
        SEX<-"person whose sex was not recorded"
        RACE<-paste0(str_to_lower(sub$race), " person ")
      } else if (!is.na(subject()%>%pull(sex)) & is.na(subject()%>%pull(race))){

        RACE<-paste0(str_to_lower(subject()%>%pull(sex)), " whose race was not recorded")
        SEX<-""
        
      } else if (!is.na(subject()%>%pull(sex)) & !is.na(subject()%>%pull(race))){
        RACE<-paste0(str_to_lower(subject()%>%pull(race)))
        SEX<-paste0(str_to_lower(subject()%>%pull(sex)))
        
        }
      
      
      text<-paste0("This subject is a ", subject()%>%pull(age), " year old ", RACE , " ", SEX, ".")
       if(!is.na(subject()%>%pull(ref.procedure))){
         text<-paste0(text, " Who underwent '", str_to_lower(subject()%>%pull(ref.procedure)), "'.") 
       } else {
         text<-paste0(text, " Procedural details were not available via MD Clone.") 
       }
    }
    text<-paste0("<b>", text, "</b>")
    text
  })
  
  # Second Page Outputs

  output$MDR_txt <- renderText({
    if (length(input$mdr_risks)>0){
    riskFactors <- paste(input$mdr_risks, collapse = ", ")
    paste("Added today:", riskFactors)}
  })
  
# Third Page Outputs
  output$GNB_txt <- renderText({
    if (length(input$gnb_risks)>0){
    riskFactors <- paste(input$gnb_risks, collapse = ", ")
    paste("GNB:", riskFactors)}
  })
  
  output$MRSA_txt_1 <- renderText({
    if (length(input$mrsa_risks)>0){
    riskFactors <- paste(input$mrsa_risks, collapse = ", ")
    paste("MRSA:", riskFactors)}
  })


# Fourth Page Output
output$MRSA_txt_1 <- renderText({
  if (length(input$mrsa_risks_2)>0){
    riskFactors <- paste(input$mrsa_risks_2, collapse = ", ")
    paste("MRSA:", riskFactors)}
})

# Various submission buttons

observeEvent(input$slide_2_submit, {
  showModal(modalDialog("Ordering drugs for this patient...", footer=NULL))
  Sys.sleep(2)
  removeModal()
  showModal(modalDialog("I don't know how long this normally takes so it'll done in another second...", footer=NULL))
  Sys.sleep(1)
  removeModal()
  showModal(modalDialog("Done!", footer=NULL))
  Sys.sleep(1)
  #Finish the function
  removeModal()
})

#slide_3_submit_1 - 2
#slide_4_submit_1 - 2

observeEvent(input$slide_3_submit_1, {
  showModal(modalDialog("Ordering drugs for this patient...", footer=NULL))
  Sys.sleep(2)
  removeModal()
  showModal(modalDialog("I don't know how long this normally takes so it'll done in another second...", footer=NULL))
  Sys.sleep(1)
  removeModal()
  showModal(modalDialog("Done!", footer=NULL))
  Sys.sleep(1)
  #Finish the function
  removeModal()
})

observeEvent(input$slide_3_submit_2, {
  showModal(modalDialog("Ordering drugs for this patient...", footer=NULL))
  Sys.sleep(2)
  removeModal()
  showModal(modalDialog("I don't know how long this normally takes so it'll done in another second...", footer=NULL))
  Sys.sleep(1)
  removeModal()
  showModal(modalDialog("Done!", footer=NULL))
  Sys.sleep(1)
  #Finish the function
  removeModal()
})

observeEvent(input$slide_4_submit_1, {
  showModal(modalDialog("Ordering drugs for this patient...", footer=NULL))
  Sys.sleep(2)
  removeModal()
  showModal(modalDialog("I don't know how long this normally takes so it'll done in another second...", footer=NULL))
  Sys.sleep(1)
  removeModal()
  showModal(modalDialog("Done!", footer=NULL))
  Sys.sleep(1)
  #Finish the function
  removeModal()
})

observeEvent(input$slide_4_submit_2, {
  showModal(modalDialog("Ordering drugs for this patient...", footer=NULL))
  Sys.sleep(2)
  removeModal()
  showModal(modalDialog("I don't know how long this normally takes so it'll done in another second...", footer=NULL))
  Sys.sleep(1)
  removeModal()
  showModal(modalDialog("Done!", footer=NULL))
  Sys.sleep(1)
  #Finish the function
  removeModal()
})

}

# Run 
shinyApp(ui = ui, server = server)

