library(shiny)
library(shinythemes)
#install.packages("readxl")
#install.packages("GGally")
#install.packages('caret')
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("readr")
#install.packages("lattice")
#install.packages("imbalance")
#install.packages("cvms")
#install.packages("tidyverse")
#install.packages("weights")
#install.packages("vip")

library(e1071)
library(ggplot2)
library(lattice)
library("dplyr")
library("imputeTS")
library(bslib)
library('stringr')
library('caret')
library(Hmisc)
library(UsingR)

library(shinyjs)

library(plyr)
library(readr)

library(rpart)
library(rpart.plot)
library(imbalance)
library(cvms) 
library(tidyverse)


# Define UI for data upload app ----
ui <- navbarPage("Our application",
                 theme=shinytheme("flatly"),
 tabPanel(strong("Data Cleaning & Features Engineering"),
 
  shinythemes::themeSelector(),
 

 
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
   
    position ='right',
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select a file ----
     
      
      fileInput(
        "file1",
        "Choose File",
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv",
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/vnd.ms-excel"
        )
      ),
      
      helpText("Note: Firstly, set parameters."),
      
      # Horizontal line ----
      tags$hr(),
      
      radioButtons(
        "type",
        "Type",
        choices = c(.txt = "text",
                    .xlsx = "xlsx",
                    .csv="csv"),
        selected = "csv"
      ),
      
      # Horizontal line ----
      tags$hr(),
      
      
      
      # Input: Select separator ----
      radioButtons(
        "sep",
        "Separator",
        choices = c(
          Comma = ",",
          Semicolon = ";",
          Tab = "\t",
          Space = " "
        ),
        selected = " "
      ),
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons(
        "disp",
        "Display",
        choices = c(Head = "head",
                    All = "all"),
        selected = "head"
      ),
      tags$hr(),
      selectInput("Dataprint","Select a choice",
                  choices=c("Full Data Set","Quantitative Data","Qualitative Data"),
                  selected = "Qualitative Data"),
      actionButton("OKay","Okay"),
      tags$hr(),
      
      uiOutput("ui"),
      selectInput("impu","Select the imputation method to fill missing values"
                  ,choices = c("Hot Deck Imputation" = "hdi",
                               "Basic Numeric Imputation" = "bni",
                               "Interpolation" = "intu"))
      ,
      conditionalPanel(
        condition = "input.impu == 'bni'",
        selectInput(
          "mmm", "Choose one ",
          c("Mean",
            "Median",
            "Mode")))
      
      , actionButton("Impute","Impute"),
      
      tags$hr(),
      uiOutput("uiout"),
      
      radioButtons("outlier",label = h4("Outliers"),choices = c( "Dont Remove Outliers"="r1","Remove Outliers"="r2"),selected = NULL),
      tags$hr(),
      selectInput("norm","Select the normalization method for feature scaling"
                ,choices = c("Min Max scaling" = "mms",
                             "Standard Scaling"= "ss"
                             
                )),
      actionButton("Normalize","Normalize"),
     tags$hr(),
     radioButtons("Dummification",label = h4("Dummification"),choices = c( "Dummify your data"="r1","Don't dummify your data"="r2"),selected = NULL)
     ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Statlog ( Heart )", align="center"),
      p(),
      HTML('<center><img src = "https://th.bing.com/th/id/OIP.OBYJlEY9aqNDDxBvk5IAHAHaDu?w=327&h=176&c=7&r=0&o=5&dpr=1.3&pid=1.7", height = 260, width = 460></center>'),
      h3('Preliminary Information', align="center"),
      p("This dataset is a heart disease database similar to a database already present in the repository UCI (Heart Disease databases) but in a slightly different form"),
      p("The data is in an ASCII file. Attributes are separated by a single space."),
      p("Each line of the data file starts with a digit which tells the age of patient."),
      h5(strong("Note")),
      p("The goal field refers to the presence of heart disease in the patient.  It is integer valued from 0 (no presence) to 1.
        Experiments with the Cleveland database have concentrated on simply attempting to distinguish presence from absence (value 0)."),
      tabsetPanel(
      type = "tabs",
      tabPanel(strong("Show original Table"), tableOutput("contents")),
      tabPanel(strong("Show columns by type"),  tableOutput("table1")),
      tabPanel(strong("Table after imputation"),  tableOutput("table2")),
      tabPanel(strong('Outliers'),  verbatimTextOutput("done")),
      tabPanel(strong('Normalized Data'),  tableOutput("table3")),
      tabPanel(strong('Dummify Data'),  tableOutput("table4"))
      
      
      
      
     )
    
  )
  )
 ), 
 navbarMenu(strong("Data Analysis"),
                
                 tabPanel("univariate analysis",
                          mainPanel(
                            tabsetPanel(
                     
                     tabPanel(strong("BARPLOTS"),
                              h2('BARPLOTS', align="center"),
                              p(),
                              p("A barplot (or barchart) is one of the most common types of graphic. It shows the relationship between a numeric and a categoric variable. 
                                Each entity of the categoric variable is represented as a bar. The size of the bar represents its numeric value."),
                              plotOutput(outputId = "barplot"),
                              fluidRow(
                              column(8, plotOutput("barplotUni"), p('Interpretation : On remarque que la majorité des observations faites dans ce cadre de traitement concerne le genre masculin')),
                              column(8, plotOutput("barplotUni1"), p("Interpretation : La majorité des patients souffre d'une douleur thoracique de type Asymptotic. Par contre, la minorité
                                                                     concerne les patients qui ont une douleur thoracique de type Typical Angina")),
                              column(8, plotOutput("barplotUni2"), p("Interpretation: Nombreux sont les patients qui ne souffrent pas d'une douleur induite par l'exercice"))
                              )),
                              
                      tabPanel(strong("BOXPLOTS"),
                              h2("BOXPLOTS", align="center"),
                              p(),
                              p("une boîte à moustaches représente une synthèse des données en cinq informations cruciales identifiables en un coup d’œil: 
                                la mesure de position, la dispersion, l’asymétrie et la longueur de la moustache.
                                La mesure de position se caractérise par la ligne de séparation sur la médiane (ainsi que par le milieu de la boîte). 
                                La dispersion se définit par la longueur de la boîte à moustaches (ainsi que par la distance entre les extrémités des moustaches et l’écart). 
                                L’asymétrie correspond à la déviation de la ligne médiane du centre de la boîte à moustaches par rapport à la longueur de la boîte (ainsi que par la longueur de la moustache supérieure par rapport à la longueur de 
                                la moustache inférieure, et par le nombre d’observations individuelles de chaque côté). La longueur de la moustache correspond à la distance entre les extrémités des moustaches 
                                par rapport à la longueur de la boîte à moustaches (et au nombre d’observations marquées spécifiquement)."),
                              fluidRow(
                            column(6,
                                   plotOutput(outputId = "boxplot1")),
                            column(6,
                                   plotOutput(outputId = "boxplot2")),
                            column(6,
                                   plotOutput(outputId = "boxplot3")),
                            column(6,
                                   plotOutput(outputId = "boxplot4")),
                            column(6,
                                   plotOutput(outputId = "boxplot5")),
                            column(6,
                                   plotOutput(outputId = "boxplot6"))
                          )),
                             
                     
                     tabPanel(strong("Histogram"), 
                              fluidRow(
                                column(8, 
                                       plotOutput(outputId = "Histogram1"),
                                       sliderInput("bins",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5), 
                                       p('Interpretation : la majorité des patients ont atteint une fréquence cardiaque maximale > 150beats/min et <175beats/min')
                                ),
                                column(8, 
                                       plotOutput(outputId = "Histogram2"),
                                       sliderInput("bins2",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5)
                                )
                              ),
                              fluidRow(
                                column(8, 
                                       plotOutput(outputId = "Histogram3"),
                                       sliderInput("bins3",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5)
                                ),
                                column(8, 
                                       plotOutput(outputId = "Histogram4"),
                                       sliderInput("bins4",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5)
                                ),
                                column(8, 
                                       plotOutput(outputId = "Histogram5"),
                                       sliderInput("bins3",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5)
                                ),
                                column(8, 
                                       plotOutput(outputId = "Histogram6"),
                                       sliderInput("bins4",
                                                   "Number of bins:",
                                                   min = 1,
                                                   max = 50,
                                                   value = 5)
                                ),
                              )
                              
                     ),
                     tabPanel(strong("Summary"), 
                              fluidRow(
                                column(4, 
                                       htmlOutput(outputId = "Mean")
                                ),
                                column(4, 
                                       htmlOutput(outputId = "Median")
                                ),
                                column(4, 
                                       htmlOutput(outputId = "first_Quartile")
                                )
                              ),
                              fluidRow(
                                column(4, 
                                       htmlOutput(outputId = "Max")
                                ),
                                column(4, 
                                       htmlOutput(outputId = "Min")
                                ),
                                column(4, 
                                       htmlOutput(outputId = "third_Quartile")
                                )
                              )
                              
                     ),
                     tabPanel(strong("Pie"),
                              h2("Pie Chart", align="center"),
                              p(),
                              p("A pie chart, sometimes called a circle chart, is a way of summarizing a set of nominal data or displaying the different values of a given variable ."),
                              fluidRow(
                                column(6, 
                                       plotOutput(outputId = "Pie")
                                ),
                                p("Interpretation: les observations indiquant un absense de cardiopathie sont un peu plus nombreux que celles indiquant une présence de cardiopathie. 
                                  Cette remarque nous permet de prendre en considération l'équilibrage des classes durant l'entrainement des modèles de prédiction.")
                              ),
                              fluidRow(
                                column(6, 
                                       plotOutput(outputId = "Pie1")
                                ),
                                p('Interpretation: la majorité des patients sont des hommes')
                              ),
                              fluidRow(
                                column(6, 
                                       plotOutput(outputId = "Pie2")
                                ),
                                p("Interpretation: le taux de sucre dans le sang chez la majorité des patients ne dépasse pas 120mg/dl avant les repas")
                              ),
                              fluidRow(
                                column(6, 
                                       plotOutput(outputId = "Pie3")
                                )
                              )
                              
                     )

                     
                    
            ))),  
                     
                     
  
            tabPanel("Bivariate analysis",
                     mainPanel(
                       tabsetPanel(
                     
                     tabPanel(strong("Nuage"), 
                              h2("Nuage de points", align="center"),
                              p(),
                              p(" un nuage de points est une représentation graphique de données généralement nombreuses, éventuellement interprétable par l'identification de relations, 
                                répartitions plus ou moins homogènes, pouvant correspondre à l'application d'une loi normale"),
                              
                              h3("NB"),
                              p(),
                              p("The data points are so much spread in the graph, so we can say that there's a low correlation between the two variables
                              as an example the age of the patients and their max heart rate.
                                We're talking here about a Low negative correlation"),
                              fluidRow(
                                column(6, 
                                       plotOutput(outputId = "Nuage")
                                       
                                ),
                                column(6, 
                                       plotOutput(outputId = "Nuage2")
                                       
                                ),
                                column(6, 
                                       plotOutput(outputId = "Nuage3")
                                       
                                ),
                                column(6, 
                                       plotOutput(outputId = "Nuage4")
                                       
                                ),
                                column(6, 
                                       plotOutput(outputId = "Nuage5")
                                       
                                ),
                                column(6, 
                                       plotOutput(outputId = "Nuage6")
                                       
                                )
                              )
                     ),
                     tabPanel(strong("Histogrammes dos à dos"),
                              h2("Back to back histogram", align="center"),
                              p(),
                             
                              fluidRow(
                                column(6,  plotOutput("histbackback1")
                                       ),
                                column(6,  plotOutput("histbackback2")
                                ),
                                column(6,  plotOutput("histbackback3")
                                ),
                                column(6,  plotOutput("histbackback4")
                                )
                                
                              )
                     ),
                     tabPanel(strong("Nuage & Histogrammes"), 
                              h2("Nuage & Histogramme", align="center"),
                              p(),
                              p("Ce type de graphique combine les caractéristiques d'un simple nuage de points X-Y avec les histogrammes des variables X et Y.
                                Les histogrammes sont représentés parallèlement aux axes x et y du graphique, l'histogramme X en haut et l'histogramme Y à droite."),
                              fluidRow(
                                column(6, plotOutput("nuagePointshist1")
                                ),
                                column(6, plotOutput("nuagePointshist2")
                                ),
                                column(6, plotOutput("nuagePointshist3")
                                ),
                                column(6, plotOutput("nuagePointshist4")
                                ),
                                column(6, plotOutput("nuagePointshist5")
                                ),
                                column(6, plotOutput("nuagePointshist6")
                                )
                                
                              )
                     ),
                    
                     tabPanel(strong("Diag. Barres"),
                              h2("Barplots", align="center"),
                              p(),
                              fluidRow(
                                column(6, plotOutput("barplotBi1")
                                       ),
                                column(6, plotOutput("barplotDodgeBi1")
                                       ),
                                column(6, plotOutput("barplotBi2")
                                ),
                                column(6, plotOutput("barplotDodgeBi2")
                                ),
                                column(6, plotOutput("barplotBi3")
                                ),
                                column(6, plotOutput("barplotDodgeBi3")
                                ),
                                column(6, plotOutput("barplotDodgeBi4")
                                ),
                                column(6, plotOutput("barplotDodgeBi5")
                                )
                              )
                     ),
                     tabPanel(strong("Diag. Profils"), 
                
                              fluidRow(
                                h3("Gender & Chest Pain", align='center'),
                                p(),
                                column(6, plotOutput("barplotProfils1")),
                                column(6, tableOutput("contingency1"))
                              ),
                              fluidRow(
                                h3("Slope of Peak Exercise & Chest Pain", align='center'),
                                p(),
                                column(6, plotOutput("barplotProfils2")),
                                column(6, tableOutput("contingency2"))
                              ),
                              fluidRow(
                                h3("Fasting Blood Sugar & Resting Electrocardiographic results", align='center'),
                                p(),
                                column(6, plotOutput("barplotProfils3")),
                                column(6, tableOutput("contingency3"))
                              )
                     ),
                     tabPanel(strong("Boxplots"), 
                              fluidRow(
                                h3("Age & Chest Pain", align='center'),
                                p(),
                                p("NB: The patients over 40 has the highest chest pain"),
                                p(),
                                column(8, offset = 2, plotOutput("force1"))
                              ),
                              fluidRow(
                                h3("Age & Slope of the peak exercise ST segment", align='center'),
                                p(),
                                column(8, offset = 2, plotOutput("force2"))
                              ),
                              fluidRow(
                                h3("Age & Blood pressure during rest", align='center'),
                                p(),
                                column(8, offset = 2, plotOutput("force3"))
                              )
                     ),
                       )
                     
                     )
                     
                     
                     
                     
                     
                     
                     )
            ),
 navbarMenu(strong('Models Performance'),
            tabPanel("Decision Tree Classifier",
                     mainPanel(
                      h2('Decision Tree Classifier', align="center"),
                      p(),
                      h4('Confusion Matrix', align="center"),
                      p(),
                      plotOutput("cf_matrix1"),
                      h4('Classification Report', align="center"),
                      p(),
                      tableOutput("classification_report1"),
                      h4('ROC Curve', align="center"),
                      p(),
                      p("Note : The more that the ROC curve hugs the top left corner of the plot, the better the model does at classifying the data into categories.
                        To quantify this, we can calculate the AUC (area under the curve) which tells us how much of the plot is located under the curve.
                        The closer AUC is to 1, the better the model is."),
                      plotOutput("ROC1"),
                      p(),
                      h4('Feature Importance', align="center"),
                      p(),
                      plotOutput("feature_imp1")
                     )
            ),
            tabPanel("SVM Classifier",
                     mainPanel(
                       h2('SVM Classifier', align="center"),
                       p(),
                       h4('Confusion Matrix', align="center"),
                       p(),
                       plotOutput("cf_matrix2"),
                       h4('Classification Report', align="center"),
                       p(),
                       tableOutput("classification_report2"),
                       h4('ROC Curve', align="center"),
                       p(),
                       p("Note : The more that the ROC curve hugs the top left corner of the plot, the better the model does at classifying the data into categories.
                        To quantify this, we can calculate the AUC (area under the curve) which tells us how much of the plot is located under the curve.
                        The closer AUC is to 1, the better the model is."),
                       plotOutput("ROC2"),
                       p(),
                       h4('Feature Importance', align="center"),
                       p(),
                       htmlOutput("feature_imp2")
                     )
            ),
            tabPanel("Naive Bayes Classifier",
                     mainPanel(
                       h2('Naive Bayes Classifier', align="center"),
                       p(),
                       h4('Confusion Matrix', align="center"),
                       p(),
                       plotOutput("cf_matrix3"),
                       h4('Classification Report', align="center"),
                       p(),
                       tableOutput("classification_report3"),
                       h4('ROC Curve', align="center"),
                       p(),
                       p("Note : The more that the ROC curve hugs the top left corner of the plot, the better the model does at classifying the data into categories.
                        To quantify this, we can calculate the AUC (area under the curve) which tells us how much of the plot is located under the curve.
                        The closer AUC is to 1, the better the model is."),
                       plotOutput("ROC3"),
                       h4('Features Importance', align="center"),
                      
                     )
            )
 ),
 tabPanel(strong("Prediction"),
          h2('Heart Disease Prediction', align="center"),
          p(),
          HTML('<center><img src = "https://familypracticeofcadillac.com/wp-content/uploads/2020/03/TELEVISIT.jpg", height = 330, width = 520></center>'),
          p(),
          p("We picked Naive Bayes pretrained model to make predictions on new data as it gave us the highest accuracy among the three supervised models
            that were tested. All you need to do is to fill the form below with your personal data then click on the button 'Prediction' to get the result
            concerning your health state"),
          column(4,
          numericInput(inputId = "age", 
                       label = "Enter your age",
                       value = 55
          ),
          p(),
          radioButtons(inputId = "sex", 
                       label = "Select your gender",choices = list("Male" = 1, "Female" = 0
                       ),selected = 1),
          p(),
          radioButtons(inputId = "cp", 
                       label = "Select the type of your chest pain",choices = list("Typical angina" = 1,"Atypical angina" = 2, 
                                                                                   "Non-anginal pain" = 3, "Asymptomatic" = 4),
                       selected = 1
          ),
          p(),
          numericInput(inputId = "trestbps", 
                       label = "Enter resting blood pressure (in mm Hg on admission to the hospital)",
                       value = 130
          ), 
          p(),
          numericInput(inputId = "chol", 
                       label = "Enter serum cholestoral in mg/dl",
                       value = 250
          ),
          p(),
          radioButtons(inputId = "fbs", 
                       label = "Select if fasting blood sugar > 120mg/dL",
                       choices = list("No" = 0,"Yes" = 1),
                       selected = 1
          ), align='left'),
          column(4,
     
          radioButtons(inputId = "restecg", 
                       label = "Enter your resting electrocardiographic results",
                       choices = list("Normal" = 0,"Having ST-T wave abnormality" = 1, 
                                      "Showing probable or definite left ventricular hypertrophy" = 2),
                       selected = 1
          ),
          p(),
          numericInput(inputId = "thalach", 
                       label = "What was your maximum heart rate achieved?",
                       value = 150
          ),
          p(),
          radioButtons(inputId = "exang", 
                       label = "Do you have exercise induced angina? ",
                       choices = list("No" = 0,"Yes" = 1),
                       selected = 1
          ),
          p(),
          numericInput(inputId = "oldpeak", 
                       label = "oldpeak",
                       value=1
          )),
          column(4,
          radioButtons(inputId = "slope", 
                       label = "What is the slope of your peak exercise ST segment?",
                       choices = list("Upsloping" = 1,"Flat" = 2, 
                                      "Downsloping" = 3),
                       selected = 1
          ), 
          p(),
          radioButtons(inputId = "ca", 
                       label = "What is the number of major vessels (0-3) colored by flouroscopy?",
                       choices = list("0" = 0,"1" = 1, "2" = 2, "3" = 3),
                       selected = 1
          ),
          p(),
          radioButtons(inputId = "thal", 
                       label = "What is your thalassemic status?",
                       choices = list("Normal" = 3,"Fixed defect" = 6,
                                      "Reversable defect" = 7),
                       selected = 3
          ), p()),
          
          column(8,
          actionButton(inputId = "go", 
                       label = "Get Prediction",  class="btn-lg",  style="color: #fff; background-color: #337ab7; border-color: #2e6da4"), align='center'),
          mainPanel(
          htmlOutput ("pred", style="color:green"))
 
          )
                           
            
            
            )
 

  




# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  #pageButtonServer("page", parentSession = session)
  
 
  df2 <- reactive({
    req(input$file1)
  
    library("readxl")
    
    if (input$type == "csv") {
      df <- read.csv(input$file1$datapath,
                     sep = input$sep)
      
      mess <- scan("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/heart/heart.doc", what="character", sep=".")
      mess.names <- mess[seq(7,31,2)]
      mess.names[14] = 'Heart Disease'    
      names(df) <- mess.names
      colnames(df) <- c('age','sex','chest pain','serum cholestoral','resting blood pressure','fasting blood sugar', 'resting elecrocardio. results', 'max heart rate', 'exercise induced angina','oldpeak','slope of the peak exercise ST segment','number of major vessels',"thal",'Heart disease')
      #names(df)<-str_replace_all(names(df), c(" " = "." , "," = "" ))
      
    } else{
      df <- read_excel(input$file1$datapath,
                       sheet =  input$sheet)
    }
    
    return(df)
    

  })
  
  ## ce code a pour but d'attribuer les modalités à chaque variable qualitative afin que les données soient lisible et comprehensible lors de l'affichage de la table
  df1 <- reactive({
    df <- df2()
    colnames(df) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
                   'thalach','exang','oldpeak','slope','ca','thal','heart disease')
    for (row in 1:nrow(df)) {
      if (df[row, "heart disease"]==1){
        df[row, "heart disease"] <- "Yes"
      }
      else{
        df[row, "heart disease"] <- "No"
      }
      
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "fbs"]>0){
        df[row, "fbs"] <- "> 120"
      }
      else{
        df[row, "fbs"] <- "< 120"
      }
      
    }
  
    for (row in 1:nrow(df)) {
      if (df[row, "sex"]>0){
        df[row, "sex"] <- "Male"
      }
      else{
        df[row, "sex"] <- "Female"
      }
      
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "exang"]>0){
        df[row, "exang"] <- "Yes"
      }
      else{
        df[row, "exang"] <- "No"
      }
      
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "slope"]==1){
        df[row, "slope"] <- "Upslope"
      }
      else if (df[row, "slope"]==2) {
        df[row, "slope"] <- "Flat"
      }
      else{
        df[row, "slope"] <- "Downslope"
      }
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "restecg"]==0){
        df[row, "restecg"] <- "Normal"
      }
      else if (df[row, "restecg"]==1){
        df[row, "restecg"] <- "ST-T wave abnormality"
      }
      else{
        df[row, "restecg"] <- "Probable or definite left ventricular hypertrophy"
      }
      
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "cp"]==1){
        df[row, "cp"] <- "typical angina"
      }
      else if (df[row, "cp"]==2){
        df[row, "cp"] <- "atypical angina"
      }
      else if (df[row, "cp"]==3){
        df[row, "cp"] <- "non-anginal pain"
      }
      else{
        df[row, "cp"] <- "asymptomatic"
      }
    }
    
    for (row in 1:nrow(df)) {
      if (df[row, "thal"]=="3.0"){
        df[row, "thal"] <- "Normal"
      }
      else if (df[row, "thal"]== "6.0"){
        df[row, "thal"] <- "Fixed Defect"
      }
      else{
        df[row, "thal"] <- "Reversible Defect"
      }
      
    }
    return(df)
    
  })
  
  ##dataset used during analysis part 
  df <- reactive({
    df <- df1()
    colnames(df) <- c('age','sex','chest pain','serum cholestoral','resting blood pressure','fasting blood sugar', 'resting elecrocardio. results', 'max heart rate', 'exercise induced angina','oldpeak','slope of the peak exercise ST segment','number of major vessels',"thal",'Heart disease')
    return(df)
  })
  
  #affichage de la table selon le choix de l'utilisateur : head() or la table entière ?
  output$contents <- renderTable({
   
    if (input$disp == "head") {
      return(head(df1()))
    }
    else {
      return(df1())
    }
    
  })
  
  # le code ci-dessous a été ajouté afin qu'on puisse distinguer les colonnes quantitative de celles qualitative
  num1 <- reactive({
    data <- df1()
    # d'abord on remplace les chaines de caractères vides par a NaN value, afin de permettre à la machine de distinguer correctement les quali des quanti
    data <- replace(data,data == "",values = NA)
    num = 0
    char = 0
    for (i in 1:length(names(data))) 
    { 
      a <- c(data[,i])
      a <- gsub(",", "", a)  
      for (j in 1:length(a))
      {if (is.na(as.numeric(a[j])) == "TRUE")
      { 
        char = char+1}
        else
        { num = num + 1} 
       
        if (num >= char) 
        {
          data[,i]<- as.numeric(a)
          num=0
        }
        else
        {data[,i] <- as.character(a)
        char = 0}
      } }
    isolate( write.csv(data,file="data2.csv",row.names = FALSE))
    
    return(data)
  })
  
  #display either quantitative / qualitative or full dataset using the piece of code above 
  output$table1 <- renderTable({
  

    if (input$Dataprint == "Full Data Set")
    {num1()}
    else if (input$Dataprint == "Quantitative Data")
    {b=select_if(num1(),is.numeric)
    b}
    else if (input$Dataprint == "Qualitative Data" )
    {c=select_if(num1(),is.character)
    c
    }
  })
  
  
  #-----------------------------------------------------Handling missing values------------------------------------------
  
  ## allow the user to select the column w the wanted imputation method to fill its missing values 
  output$ui <- renderUI( { 
    data1 <- read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    #t <- c(names(select_if(data1,is.numeric)))
    t <- c(names(data1))
    selectInput("Col","Select a Column with the wanted imputation method to fill its missing values if existed",choices = t)
  } )
  
  ##Hot deck methods impute missing values within a data matrix by using available values from the same matrix
  hotdeck <- reactive({
    q <-  read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    m <- q[,input$Col]
    final_result <- na_random(m) 
    q[,input$Col] <- final_result
    return(q)
  })
 
  ##Interpolation is a technique used to estimate unknown data points between two known data points
  interpol <- reactive({
    q <-  read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    linear1=na_interpolation(q[,input$Col])
    q[,input$Col] <- linear1
    return(q)
  })
  ## filling missing values w basic numeric imputation : mean / median / mode
  num <- reactive({
    q <-  read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    if (input$mmm == "Mean")
    { n = na_mean(q[,input$Col], option= "mean")
    q[,input$Col] <- n
    return(q)
    }
    else if (input$mmm == "Median")
    {
      n = na_mean(q[,input$Col], option= "median")
      q[,input$Col] <- n
      return(q)
    }
    else if (input$mmm == "Mode")
    {
      n <- na_mean(q[,input$Col], option= "mode")
      q[,input$Col] <- n
      return(q)
    }
  })
  # Apply the selected imputation method : "BASIC NUMERIC IMPUTATION","K-IMPUTATION","INTERPOLATION" 
  Dataset1 <- reactive({
    if(input$Impute == 0) return()
    if (input$impu == "hdi")
    { k <-hotdeck()
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
    }
    else if (input$impu =="bni")
    {k<- num()
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
    }
    
    else if( input$impu == "intu")  
    {k<- interpol()
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
    }
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
  })
  ## After handling the missing values, the new table is gonna be displayed
 
  output$table2 <-  renderTable({
   
    Dataset1()
  })
  
  #-----------------------------------------------Outliers------------------------------------------------------------------------------------
  
  # removing outliers for a selected numeric column
  output$uiout <- renderUI( { 
    data1 <- read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    t <- c(names(select_if(data1,is.numeric)))
    checkboxGroupInput("out","Select Columns to remove its outlier",choices = t)
  } )
  box <- reactive({
    m <-  read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    outlier <- boxplot(m[,input$out])
    m <- m[-which(m[,input$out] %in% outlier )]
    
    return(m)
  })
  
  #if the ouliers are successfully removed, it's going to be shown to the user by a simple text "Outliers removed"
  output$done <- renderText({
    if (input$outlier == "r1"){"Outlier Not removed"}
    else if(input$outlier == "r2"){
      box()
      "Outlier Removed"}
  })
  
  #-------------------------------------------------------Normalization---------------------------------------------------------------------
  ## 2 methods were implemented : min-max, standard scaling
  minmax <- reactive({
    dat<- read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    t <- c(names(select_if(dat,is.numeric)))
    preproc2 <- preProcess(dat[,t], method=c("range"))
    
    norm2 <- predict(preproc2, dat[,t])
    return(norm2)
    
    
  }
  )
  
  standard <- reactive({
    dat<- read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    t <- c(names(select_if(dat,is.numeric)))
    preproc1 <- preProcess(dat[,t], method=c("center", "scale"))
    
    norm1 <- predict(preproc1, dat[,t])
    
    return(norm1)
    
  }
  )
  Dataset3 <- reactive({
    #if(input$Normalize == 0) return()
    if (input$norm== "mms")
    { k <-minmax()
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
    }
  
    else (input$norm == "ss")
    { k <- standard()
      (write.csv(k,file="data2.csv",row.names = FALSE))
      return(k)
    }
    (write.csv(k,file="data2.csv",row.names = FALSE))
    return(k)
  })
  output$table3 <-  renderTable({
   
    Dataset3()
  })
  
  #-----------------------------------------------------------Dummification---------------------------------- ----------------------------------------------
  output$table4 <- renderTable({
    library(weights)
    data <- read.csv(file="data2.csv",header = TRUE,stringsAsFactors = FALSE)
    t <- c(names(select_if(data,is.character)))
    #dummify(data)
    df2()
  })
  
  #-------------------------------------------PART 2 : ANALYSIS ----------------------------------------------------------------------------------------------
  
  #BOXPLOT
  output$boxplot1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(data$age,
            at = c(1),
            names = c("Age"),
            col = c("orange"),
            main = "PATIENT'S AGE BOXPLOT",
            xlab = "AGE",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  output$boxplot2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(data$resting.blood.pressure,
            at = c(5),
            names = c("Blood Pressure"),
            col = c("red"),
            main = "PATIENT'S BLOOD PRESSURE BOXPLOT",
            xlab = "BLOOD PRESSURE",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  output$boxplot3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(data$serum.cholestoral,
            at = c(4),
            names = c( "Serum Cholestoral"),
            col = c("blue"),
            main = "SERUM CHOLESTORAL BOXPLOT",
            xlab = "SERUM CHOLESTORAL IN MG/DL",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  output$boxplot4 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(data$max.heart.rate,
            at = c(8),
            names = c("Max Heart Rate"),
            col = c("red"),
            main = "Heart RATE BOXPLOT",
            xlab = "MAX HEART RATE",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  output$boxplot5 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot( data$number.of.major.vessels,
            at = c(10),
            names = c("Number of major vessels"),
            col = c("red"),
            main = "Major Vessels BOXPLOT",
            xlab = "Number of major vessels",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  output$boxplot6 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot( data$oldpeak,
            at = c(12),
            names = c("Oldpeak"),
            col = c("brown"),
            main = "OLDPEAK BOXPLOT",
            xlab = "OLDPEAK",las = 1,
            border = "brown",
            notch = TRUE,
            horizontal = TRUE
    )
  })
  
  #BARPLOT
  
  output$barplot <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    barplot(height=as.matrix(data), cex.axis=0.8, cex.names=0.7, 
            xlab=' Factors ', ylab='Count')
    
  })
  
  
  #HISTOGRAM
  
  output$Histogram1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    pw = data$max.heart.rate
    bins <- seq(min(pw), max(pw), length.out = input$bins + 1)
    hist(pw,
         breaks = bins,
         col = "orange",
         main = "Number of patients by the maximum of their heart rate achieved",
         xlab = "Maximum Heart Rate Achieved ",
         ylab = "Number of Patients")
    
  })
  
  output$Histogram2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    sw = data$age
    bins <- seq(min(sw), max(sw), length.out = input$bins2 + 1)
    hist(sw,
         breaks = bins,
         col = "red",
         main = "Number of concerned patients by their age",
         xlab = "Age",
         ylab = "Number of Patients")
    
  })
  
  output$Histogram3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    pl=data$oldpeak
    bins <- seq(min(pl), max(pl), length.out = input$bins3 + 1)
    
    hist(pl,             
         breaks = bins,
         col = "blue",
         main = "Number of Patients by their Old Peak",
         xlab = "Old Peak",
         ylab = "Number of Patients")
    
  })
  
  output$Histogram4 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    sl = data$number.of.major.vessels
    bins <- seq(min(sl), max(sl), length.out = input$bins4 + 1)
    
    hist(sl,
         breaks = bins,
         col = "orange",
         main = "Number of patients by their number of major vessels",
         xlab = "CA",
         ylab = "Number of Patients")
    
  })
  
  output$Histogram5 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    sl = data$resting.blood.pressure
    bins <- seq(min(sl), max(sl), length.out = input$bins4 + 1)
    
    hist(sl,
         breaks = bins,
         col = "orange",
         main = "Number of patients by their blood pressure",
         xlab = "Blood Pressure",
         ylab = "Number of Patients")
    
  })
  
  output$Histogram6 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    sl = data$serum.cholestoral
    bins <- seq(min(sl), max(sl), length.out = input$bins4 + 1)
    
    hist(sl,
         breaks = bins,
         col = "orange",
         main = "Number of patients by their cholesterol",
         xlab = "Serum Cholesterol",
         ylab = "Number of Patients")
    
  })
  
  #-------------------------------Summary----------------------------
  
  
  
  output$Mean <- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    result.mean1 <- mean(data$age)
    result.mean2 <- mean(data$number.of.major.vessels)
    result.mean3 <- mean(data$resting.blood.pressure)
    result.mean4 <- mean(data$max.heart.rate)
    result.mean5 <- mean(data$oldpeak)
    result.mean6 <- mean(data$serum.cholestoral)
    HTML(
      paste("<h2>MEAN</h2>"),
      paste("<h4>Patienr's age :",result.mean1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.mean2,"</h4>"),
      paste("<h4>Patient's resting blood pressure:",result.mean3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved:",result.mean4,"</h4>"),
      paste("<h4>Serum Cholestoral:",result.mean5,"</h4>"),
      paste("<h4> Oldpeak  :",result.mean6,"</h4>")
    )
   
  })
  output$Median <- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    result.median1 <- median(data$age)
    result.median2 <- median(data$number.of.major.vessels)
    result.median3 <- median(data$resting.blood.pressure)
    result.median4 <- median(data$max.heart.rate)
    result.median5 <- median(data$oldpeak)
    result.median6 <- median(data$serum.cholestoral)
    HTML(
      paste("<h2>MEDIAN</h2>"),
      paste("<h4>Patienr's age :",result.median1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.median2,"</h4>"),
      paste("<h4>Patient's resting blood pressure:",result.median3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved:",result.median4,"</h4>"),
      paste("<h4>Serum Cholestoral:",result.median5,"</h4>"),
      paste("<h4> Oldpeak  :",result.median6,"</h4>")
    )
  })
  output$Max<- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    result.max1 <- max(data$age)
    result.max2 <- max(data$number.of.major.vessels)
    result.max3 <- max(data$resting.blood.pressure)
    result.max4 <- max(data$max.heart.rate)
    result.max5 <- max(data$oldpeak)
    result.max6 <- max(data$serum.cholestoral)
    
    HTML(
      paste("<h2>MAX</h2>"),
      paste("<h4>Patienr's age :",result.max1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.max2,"</h4>"),
      paste("<h4>Patient's resting blood pressure:",result.max3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved:",result.max4,"</h4>"),
      paste("<h4>Serum Cholestoral:",result.max5,"</h4>"),
      paste("<h4> Oldpeak  :",result.max6,"</h4>")
    )
  })
  output$Min<- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    result.min1 <- min(data$age)
    result.min2 <- min(data$number.of.major.vessels)
    result.min3 <- min(data$resting.blood.pressure)
    result.min4 <- min(data$max.heart.rate)
    result.min5 <- min(data$oldpeak)
    result.min6 <- min(data$serum.cholestoral)
    HTML(
      paste("<h2>MIN</h2>"),
      paste("<h4>Patienr's age :",result.min1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.min2,"</h4>"),
      paste("<h4>Patient's resting blood pressure:",result.min3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved:",result.min4,"</h4>"),
      paste("<h4>Serum Cholestoral:",result.min6,"</h4>"),
      paste("<h4> Oldpeak  :",result.min5,"</h4>")
    )
  })
  output$first_Quartile<- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    
    k1 = quantile(data$age)
    k2 = quantile(data$number.of.major.vessels)
    k3 = quantile(data$resting.blood.pressure)
    k4 = quantile(data$max.heart.rate)
    k5 = quantile(data$oldpeak)
    k6 = quantile(data$serum.cholestoral)
    result.first_Quartile1 <- min(k1["25%"])
    result.first_Quartile2 <- min(k2["25%"])
    result.first_Quartile3 <- min(k3["25%"])
    result.first_Quartile4 <- min(k4["25%"])
    result.first_Quartile5 <- min(k5["25%"])
    result.first_Quartile6 <- min(k6["25%"])
    HTML(
      paste("<h2>First Quartile</h2>"),
      paste("<h4>Patienr's age :",result.first_Quartile1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.first_Quartile2,"</h4>"),
      paste("<h4>Patient's resting blood pressure:",result.first_Quartile3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved:",result.first_Quartile4,"</h4>"),
      paste("<h4>Serum Cholestoral:",result.first_Quartile6,"</h4>"),
      paste("<h4> Oldpeak  :",result.first_Quartile5,"</h4>")
    )
  })
  
  output$third_Quartile<- renderUI({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    
    k1 = quantile(data$age)
    k2 = quantile(data$number.of.major.vessels)
    k3 = quantile(data$resting.blood.pressure)
    k4 = quantile(data$max.heart.rate)
    k5 = quantile(data$oldpeak)
    k6 = quantile(data$serum.cholestoral)
    result.third_Quartile1 <- min(k1["75%"])
    result.third_Quartile2 <- min(k2["75%"])
    result.third_Quartile3 <- min(k3["75%"])
    result.third_Quartile4 <- min(k4["75%"])
    result.third_Quartile5 <- min(k5["75%"])
    result.third_Quartile6 <- min(k6["75%"])
    HTML(
      paste("<h2>THIRD QUARTILE</h2>"),
      paste("<h4>Patienr's age :",result.third_Quartile1,"</h4>" ),
      paste("<h4>Patient's number of vessels :",result.third_Quartile2,"</h4>"),
      paste("<h4>Patient's resting blood pressure :",result.third_Quartile3,"</h4>"),
      paste("<h4>Patient's max heart rate achieved :",result.third_Quartile4,"</h4>"),
      paste("<h4>Serum Cholestoral :",result.third_Quartile6,"</h4>"),
      paste("<h4>Oldpeak :",result.third_Quartile5,"</h4>")
    )
  })
  
  #--------------------------------- PIE --------------------------------------------------------
  
  output$Pie <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    pie(table(data$Heart.disease), labels = names(table(data$Heart.disease)), 
        main = "Heart Disease", col=c())    
  })
  
  output$Pie1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    pie(table(data$sex), labels = names(table(data$sex)), 
        main = "Gender", col=c())    
  })
  
  output$Pie2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    pie(table(data$fasting.blood.sugar), labels = names(table(data$fasting.blood.sugar)), 
        main = "Fasting Blood Sugar", col=c())    
  })
  
  output$Pie3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    pie(table(data$exercise.induced.angina), labels = names(table(data$exercise.induced.angina)), 
        main = "Exercise Induced Angina", col=c())    
  })
  
  #-----------------------NUAGE---------------------------------------
  library(ggplot2)
  
  output$Nuage <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x = resting.blood.pressure, y=age)) + geom_point()
    
  })
  output$Nuage2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x=serum.cholestoral, y=max.heart.rate)) + geom_point()
    
  })
  output$Nuage3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x=max.heart.rate, y=age)) + geom_point()
    
  })
  output$Nuage4 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x=resting.blood.pressure, y=serum.cholestoral)) + geom_point()
    
  })
  output$Nuage5 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x=resting.blood.pressure, y=oldpeak)) + geom_point()
    
  })
  output$Nuage6 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Basic scatter plot
    ggplot(data, aes(x=number.of.major.vessels, y=serum.cholestoral)) + geom_point()
    
  })

  
  # Histogrammes dos à dos
  # ----
  output$histbackback1 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "age"; y.var = "max.heart.rate";
    histbackback(data[, x.var], y = data[, y.var],
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  
  output$histbackback2 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "resting.blood.pressure"; y.var = "serum.cholestoral";
    histbackback(data[, x.var], y = data[, y.var],
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  
  output$histbackback3 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "resting.blood.pressure"; y.var = "age";
    histbackback(data[, x.var], y = data[, y.var],
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  
  output$histbackback4 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "serum.cholestoral"; y.var = "max.heart.rate";
    histbackback(data[, x.var], y = data[, y.var],
                 xlab = c(x.var, y.var), main = paste(x.var, "and", y.var), 
                 las = 2)
  })
  #----------------------------- Nuage & Histo-------------------------------------
  output$nuagePointshist1 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "age"; y.var = "max.heart.rate";
    AGE = data[, x.var]; 
    MHR = data[, y.var];
    scatter.with.hist( AGE, MHR)
  })
  output$nuagePointshist2 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "age"; y.var = "resting.blood.pressure";
    AGE = data[, x.var]; 
    Blood.Pressure = data[, y.var];
    scatter.with.hist( AGE, Blood.Pressure)
  })
  output$nuagePointshist3 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "resting.blood.pressure"; y.var = "max.heart.rate";
    Blood.Pressure = data[, x.var]; 
    MHR = data[, y.var];
    scatter.with.hist( Blood.Pressure, MHR)
  })
  output$nuagePointshist4 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "serum.cholestoral"; y.var = "max.heart.rate";
    Cholestoral = data[, x.var]; 
    MHR = data[, y.var];
    scatter.with.hist( Cholestoral, MHR)
  })
  output$nuagePointshist5 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "serum.cholestoral"; y.var = "resting.blood.pressure";
    Cholestoral = data[, x.var]; 
    Blood.Pressure= data[, y.var];
    scatter.with.hist( Cholestoral, Blood.Pressure)
  })
  output$nuagePointshist6 <- renderPlot({
    options(digits=1)
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    x.var = "serum.cholestoral"; y.var = "age";
    Cholestoral = data[, x.var]; 
    AGE = data[, y.var];
    scatter.with.hist( Cholestoral, AGE)
  })
  # Diagramme en barres
  # ----
  # Unidimensionnel
  output$barplotUni <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Diagramme en barres de la variable 'sex' avec ggplot
    ggplot(data, aes(x = sex)) + geom_bar()
  })
  output$barplotUni1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Diagramme en barres de la variable 'chest.pain' avec ggplot
    ggplot(data, aes(x = chest.pain)) + geom_bar()
  })
  output$barplotUni2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    # Diagramme en barres de la variable 'exercise.induced.angina' avec ggplot
    ggplot(data, aes(x = exercise.induced.angina)) + geom_bar()
  })
  

  
  # Bidimensionnel
  #--------------------------------------------------BARPLOTS--------------------------------------------------------------------------------
  output$barplotBi1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = age, y = serum.cholestoral)) + geom_bar(stat = "identity")
  })
  
  output$barplotBi2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    
    ggplot(data, aes(x = resting.blood.pressure, y = max.heart.rate)) + geom_bar(stat = "identity")
  })
  
  output$barplotBi3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = resting.blood.pressure, y = serum.cholestoral)) + geom_bar(stat = "identity")
  })
  
  

  output$barplotDodgeBi1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
 
    ggplot(data, aes(x = chest.pain, y = serum.cholestoral, fill=sex, colour = sex)) + geom_bar(stat = "identity", position = "dodge")
  })
  output$barplotDodgeBi2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
   
    ggplot(data, aes(x = resting.elecrocardio..results, y = max.heart.rate, fill=fasting.blood.sugar, colour = fasting.blood.sugar)) + geom_bar(stat = "identity", position = "dodge")
  })
  
  output$barplotDodgeBi3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = fasting.blood.sugar, y = resting.blood.pressure, fill=sex	, colour = sex	)) + geom_bar(stat = "identity", position = "dodge")
  })
  
  output$barplotDodgeBi4 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    ggplot(data, aes(x = slope.of.the.peak.exercise.ST.segment, y = max.heart.rate, fill=exercise.induced.angina, colour = exercise.induced.angina)) + geom_bar(stat = "identity", position = "dodge")
  })
  output$barplotDodgeBi5 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = chest.pain, y = number.of.major.vessels, fill=resting.elecrocardio..results, colour =resting.elecrocardio..results)) + geom_bar(stat = "identity", position = "dodge")
  })
  
  output$barplotProfils1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = chest.pain, fill = sex, colour = sex)) + geom_bar( position = "fill")
  })
  # Table de contingence entre 'Sex' et 'chest.pain'
  # ----
  output$contingency1 <- renderTable({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    tab = with(data, table(sex, chest.pain))
    round(tab/sum(tab), 3)
    tab
  })
  
  output$barplotProfils2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    
    ggplot(data, aes(x = chest.pain, fill = slope.of.the.peak.exercise.ST.segment)) + geom_bar(position = "fill")
  })
  # Table de contingence entre 'slope.of.the.peak.exercise.ST.segment' et 'chest.pain'
  # ----
  output$contingency2 <- renderTable({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    tab = with(data, table(slope.of.the.peak.exercise.ST.segment, chest.pain))
    round(tab/sum(tab), 3)
    tab
  })
  
  output$barplotProfils3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))

    ggplot(data, aes(x = fasting.blood.sugar	, fill = resting.elecrocardio..results, colour = resting.elecrocardio..results)) + geom_bar(position = "fill")
  })
  # Table de contingence entre 'fasting.blood.sugar' et 'resting.elecrocardio..results'
  # ----
  output$contingency3 <- renderTable({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    tab = with(data, table(fasting.blood.sugar	, resting.elecrocardio..results))
    round(tab/sum(tab), 3)
    tab
  })
  
  # BOXPLOTS 2 VARIABLES ( QUANT & QUAL)
  # ----
  output$force1 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" )) 
    boxplot(age ~ chest.pain, data=data, ylim=c(0, 50), 
            cex.axis=.6, ylab='Age')
    
  })
  output$force2 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(age ~ slope.of.the.peak.exercise.ST.segment, data=data, ylim=c(0, 50), 
            cex.axis=.6, ylab='Age')
    
  })
  output$force3 <- renderPlot({
    data <- df()
    names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
    boxplot(age ~ resting.elecrocardio..results, data=data, ylim=c(0, 50), 
            cex.axis=.6, ylab='Age')
    
  })


 
  
  
  
  
  #-------------------------------------------------SUPERVISED LEARNING MODELS-----------------------------------------------------
  
  #------------------------------ Model 1 : Decision Tree Classifier ------------------------------------------------------
  

DT_classifier <- function(index=NULL){
  data <- df2()
  names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
  trainRowNumbers <- createDataPartition(data$Heart.disease, p=0.7, list=FALSE)
  train <- data[trainRowNumbers,]
  test <- data[-trainRowNumbers,]
  #Model building with rpart
  tree_model = rpart(Heart.disease ~., data = train, 
                     method = "class", minsplit = 10, minbucket = 3)
  Prediction_train = predict(tree_model, data = train, type = "class")
  Prediction = predict(tree_model, newdata = test, type = "class")
  #cm & classificationreport
  cm=as.matrix(table(test$Heart.disease, Prediction))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  if (is.null(index)){
  C_mat_test=confusionMatrix(factor(Prediction),factor(test$Heart.disease))
  # Tibble for train data

  # Tibble for test data
  tab_fin_test=as_tibble(C_mat_test$table)
  colnames(tab_fin_test)=c("Target","Prediction","N")
  plot_confusion_matrix(tab_fin_test)
  } else if(index==1){
    ### print classification report
       data.frame(precision, recall, f1)
  } else if(index==2) {
    ### plot ROC, AUC CURVE
    library(pROC)
    sim_roc=roc(test$Heart.disease, factor(Prediction, ordered=TRUE))#AUC SCORE
    #plot(roc_score, main="ROC curve --- Decision Tree Classifier")
   

    ggroc(sim_roc, legacy.axes = TRUE) +
      labs(x = 'False-positive rate', y = 'True-positive rate', title = 'Simulated ROC curve') +
      annotate('text', x = .5, y = .5, label = paste0('AUC: ',round(auc(sim_roc), digits = 2)))
  } else {
    # Features Importance using the package vip
    library(vip)
    
    vi_tree <- tree_model$variable.importance
    print(vi_tree)
    p1 <- vip(tree_model) + ggtitle("Decision tree")
    # Display plots in a grid 
    grid.arrange(p1)
  } 
    
  }


output$cf_matrix1<- renderPlot({
    DT_classifier(index = NULL)
    
  })
output$classification_report1 <- renderTable({
  DT_classifier(index = 1)
})
output$ROC1 <- renderPlot({
  DT_classifier(index = 2)
})
output$feature_imp1 <- renderPlot({
  DT_classifier(index = 3)
})


#------------------------------ Model 2 : SVM Classifier ------------------------------------------------------


SVM_classifier <- function(index=NULL){
  data <- df2()
  names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
  trainRowNumbers <- createDataPartition(data$Heart.disease, p=0.7, list=FALSE)
  train <- data[trainRowNumbers,]
  test <- data[-trainRowNumbers,]
  #Model building with rpart
  svm_model = svm(Heart.disease ~., data = train, type = 'C-classification',kernel="linear" )
 
  #Model evaluation
  Prediction_train = predict(svm_model, data = train, type="class")
  Prediction = predict(svm_model, newdata = test, type="class")
  #cm & classificationreport
  cm=as.matrix(table(test$Heart.disease, Prediction))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  if (is.null(index)){
    C_mat_test=confusionMatrix(factor(Prediction),factor(test$Heart.disease))
    # Tibble for train data
    
    # Tibble for test data
    tab_fin_test=as_tibble(C_mat_test$table)
    colnames(tab_fin_test)=c("Target","Prediction","N")
    plot_confusion_matrix(tab_fin_test)
  } else if(index==1){
    ### print classification report
    data.frame(precision, recall, f1)
  } else if(index==2) {
    ### plot ROC, AUC CURVE
    library(pROC)
    sim_roc=roc(test$Heart.disease, factor(Prediction, ordered=TRUE))#AUC SCORE
    #plot(roc_score, main="ROC curve --- Decision Tree Classifier")
    
    ## Area under the curve
    
    ggroc(sim_roc, legacy.axes = TRUE) +
      labs(x = 'False-positive rate', y = 'True-positive rate', title = 'Simulated ROC curve') +
      annotate('text', x = .5, y = .5, label = paste0('AUC: ',round(auc(sim_roc), digits = 2)))
  } else {
   
    w <- t(svm_model$coefs) %*% svm_model$SV # weight vectors
  
    
    w <- apply(w, 2, function(v){sqrt(sum(v^2))})  # weight
    d=data.frame(names(train[,-1]), w)
    colnames(d) = c('features', "importance")
    w <- sort(w, decreasing = T)
    return(d)

    
    
  } 
  
}


output$cf_matrix2<- renderPlot({
  SVM_classifier(index = NULL)
  
})
output$classification_report2 <- renderTable({
  SVM_classifier(index = 1)
})
output$ROC2 <- renderPlot({
  SVM_classifier(index = 2)
})
output$feature_imp2 <- renderTable({
  SVM_classifier(index = 3)
})


#------------------------------ Model 3 : Naive Bayes Classifier ------------------------------------------------------


NB_classifier <- function(index=NULL){
  #library(naivebayes)
  data <- df2()
  #names(data)<-str_replace_all(names(data), c(" " = "." , "," = "" ))
  colnames(data)<- c('age','sex','cp','trestbps','chol','fbs','restecg',
                  'thalach','exang','oldpeak','slope','ca','thal','Heart.disease')
  trainRowNumbers <- createDataPartition(data$Heart.disease, p=0.7, list=FALSE)
  train <- data[trainRowNumbers,]
  test <- data[-trainRowNumbers,]
  #Model building 

  nb_model = naiveBayes(Heart.disease ~., data = train, type = 'C-classification' )
  
  #Model evaluation
  Prediction_train = predict(nb_model, newdata = train, type="class")
  Prediction = predict(nb_model, newdata = test, type="class")
  #cm & classificationreport
  cm=as.matrix(table(test$Heart.disease, Prediction))
  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class 
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  if(index==5){
    return(nb_model)
  }
  
  else if(index==0){
    C_mat_test=confusionMatrix(factor(Prediction),factor(test$Heart.disease))
    # Tibble for train data
    
    # Tibble for test data
    tab_fin_test=as_tibble(C_mat_test$table)
    colnames(tab_fin_test)=c("Target","Prediction","N")
    plot_confusion_matrix(tab_fin_test)
  } else if(index==1){
    ### print classification report
    data.frame(precision, recall, f1)
  } else {
    ### plot ROC, AUC CURVE
    library(pROC)
    sim_roc=roc(test$Heart.disease, factor(Prediction, ordered=TRUE))#AUC SCORE
    
    ggroc(sim_roc, legacy.axes = TRUE) +
      labs(x = 'False-positive rate', y = 'True-positive rate', title = 'Simulated ROC curve') +
      annotate('text', x = .5, y = .5, label = paste0('AUC: ',round(auc(sim_roc), digits = 2)))
  } 
   
  }
  



output$cf_matrix3<- renderPlot({
  NB_classifier(index = 0)
  
})
output$classification_report3 <- renderTable({
  NB_classifier(index = 1)
})
output$ROC3 <- renderPlot({
  NB_classifier(index = 2)
})

#--------------------------------------PREDICTION-------------------------------------------------------------------------------------


prediction <- eventReactive(input$go, {
  d<-df2()
  colnames(d)<- c('age','sex','cp','trestbps','chol','fbs','restecg',
                'thalach','exang','oldpeak','slope','ca','thal','Heart.disease')
  sample.obs <- cbind(input$age,input$sex,input$cp,input$trestbps,input$chol,
                      input$fbs,input$restecg,input$thalach,input$exang,
                      input$oldpeak,input$slope,input$ca,input$thal)
  colnames(sample.obs) <- c('age','sex','cp','trestbps','chol','fbs','restecg',
                            'thalach','exang','oldpeak','slope','ca','thal')
  cat.cols = c('sex','cp','fbs','restecg','exang','slope','ca','thal')
  sample.obs <- data.frame(sample.obs)
  sample.obs <- transform(
    sample.obs,
    age=as.integer(age),
    sex=factor(sex, levels = levels(d$sex)),
    cp=factor(cp, levels = levels(d$cp)),
    trestbps=as.integer(trestbps),
    chol=as.integer(chol),
    fbs=factor(fbs, levels = levels(d$fbs)),
    restecg=factor(restecg,levels = levels(d$restecg)),
    thalach=as.integer(thalach),
    exang=factor(exang, levels = levels(d$exang)),
    oldpeak=as.numeric(oldpeak),
    slope=factor(slope, levels = levels(d$slope)),
    ca=factor(ca, levels = levels(d$ca)),
    thal=factor(thal, levels = levels(d$thal))
  )
  predict(NB_classifier(index=5), newdata=sample.obs,type='class')
})



output$pred <- renderUI({
  if (prediction()==2){
    h3("Presence of heart disease : you have a high probability of having a heart disease.")
  } else{
  h3("Absence of heart disease : you have a low probability of having a heart disease.")
  }
})

  
}
  
  
 
 
  

  
 
  
  


# Create Shiny app ----
shinyApp(ui, server)