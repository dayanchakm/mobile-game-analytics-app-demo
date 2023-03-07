
ui <- fluidPage(
  navbarPage(
    theme = bslib::bs_theme(bootswatch = "flatly"),
    title = div(img(src = "logo.png"), "Arrhythmetrics"),
    tabPanel(title = "About",
             fluidRow(
               column(
                 6,
                 h4("About Cardiac Arrhythmia and Electrocardiogram (ECG)"),
                 p(
                   "An arrhythmia is an irregular heartbeat or rhythm. It can be classified into different classes.
                   Some cases of cardiac arrhythmias can be serious, and a quick response can reduce the risk of complications.
                   Diagnosing arrhythmias involves manipulating large amounts of electrocardiographic data, which carries the risk
                   of human error in interpreting the  data."
                 ),
                 p(
                   "Electrocardiography (ECG) is an established method in cardiology to analyze a patient's heart condition.
                   In its basic definition, an ECG is an electrical representation of the contractile activity of the heart
                   and can be recorded fairly easily  using surface electrodes placed on the patient's limbs or chest. ECG
                   is one of the most widely recognized and used biosignals in the medical field. Heart rates per minute (bpm)
                   can be easily calculated by counting the R-wave peaks of the ECG  during 1 minute of recording. More importantly,
                   the rhythm and shape of the ECG waveform is altered in conditions such as cardiovascular disease and cardiac arrhythmias."
                 ),
                 br(),
                 HTML(
                   '<iframe width="85%" height="84.4%" src="https://www.youtube.com/embed/nq7B08suZJw" title="YouTube video player"
                    frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture"
                    allowfullscreen></iframe>'
                 ),
               ),
               column(
                 6,
                 h4("Data Set Information"),
                 p(
                   "Dataset contains 452 observations with 10 attributes, 1 of which are nominal and the rest are linear."
                 ),
                 p(
                   'Regarding the study by H. Altay Guvenir: "The goal is to distinguish
                   the presence or absence of cardiac arrhythmias and assign them to  one of  16 groups.
                   Class 01 is the" normal "ECG Classes 02 to 15 are the variety of arrhythmias. Class 16 refers to the rest of the uncategorized.'
                 ),
                 h5("Attribute Information:"),
                 tags$ul(
                   tags$li(
                     "label: 0 refers to 'normal' ECG classes 1 refers to classes of arrhythmia."
                   ),
                   tags$li("Heart rate: Number of heart beats per minute ,linear"),
                   tags$li("Age: Age in years , linear"),
                   tags$li("Sex: Sex (Male, Female) , nominal"),
                   tags$li("Height: Height in centimeters (cm) , linear"),
                   tags$li("Weight: Weight in kilograms (kg) , linear"),
                   tags$li("QRS duration: Average of QRS duration in msec., linear"),
                   tags$li(
                     "P-R interval: Average duration between onset of P and Q waves in msec., linear"
                   ),
                   tags$li(
                     "Q-T interval: Average duration between onset of Q and offset of T waves in msec., linear"
                   ),
                   tags$li("T interval: Average duration of T wave in msec., linear"),
                   tags$li("P interval: Average duration of P wave in msec., linear")
                 ),
                 helpText(
                   "Note: 2 observations were removed due to incorrect input of data,
                          and data imputation was performed for weight attribute of one of the observations."
                 )
               )
             )),
    ######################## DATA PANEL #############################
    tabPanel(title = "Data",
             tabsetPanel(
               tabPanel(title = "View",
                        DT::DTOutput("data")),
               tabPanel(
                 title = "Summary",
                 fluidRow(verbatimTextOutput("summary")),
                 tags$ul(
                   tags$li(
                     "Among males proportion of patients who have different or unclassified type of Arrhythmia  ECG is 0.58"
                   ),
                   tags$li(
                     "Among females proportion of patients who have different or unclassified type of Arrhythmia is 0.35"
                   ),
                   tags$li("Among males proportion of patients who have normal ECG 0.42"),
                   tags$li("Among females proportion of patients who have normal ECG is 0.65"),
                   br(),
                   tags$li(
                     "Odds of a male having different and unclassified type of arrhythmia is 1.39."
                   ),
                   tags$li(
                     "Odds of a female having different and unclassified type of arrhythmia is 0.55."
                   ),
                   tags$li(
                     "Odds of having different and unclassified type of arrhythmia is 2.5 times higher for males than females with 95% confidence interval of (0.27,0.58)."
                   )
                 )
               ),
               tabPanel(title = "Structure",
                        verbatimTextOutput("str"))
             )),
    ########################## VISUALIZATION PANEL ###########################
    tabPanel(title = "Visualization",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput(
                   inputId = "var1",
                   label = "x:",
                   choices = names(Arrhythmia)
                 ),
                 selectInput(
                   inputId = "var2",
                   label = "y:",
                   choices = names(Arrhythmia),
                   selected = names(Arrhythmia)[2]
                 ),
                 selectInput(
                   inputId = "dist",
                   label = "Distribution:",
                   choices = names(Arrhythmia[-1])
                 )
               ),
               mainPanel(
                 fluidRow(column(6, plotlyOutput("plot")),
                          column(6,
                                 plotlyOutput("hist"),)),
                 br(),
                 fluidRow(column(6,
                                 plotlyOutput("hist_normal")),
                          column(6,
                                 plotlyOutput("hist_arr"))),
                 plotOutput("corrplot"),
                 p("It can be seen from correlation plot that:"),
                 tags$ul(
                   tags$li(
                     "There is a negative correlation between Q-T interval and Heart rate."
                   ),
                   tags$li(
                     "There is a positive correlation between QRS Duration and T interval."
                   ),
                   tags$li(
                     "There is a positive correlation between P-R interval and P interval, which is expected,
                         since P interval is included in P-R interval."
                   ),
                   tags$li("There is a positive correlation between Height and Weight."),
                   tags$li(
                     "There is a positive correlation between Weight and BMI, which is expected,
                         since BMI is calculated from Weight and Height."
                   )
                 )
               )
             )),
    ###################### MODEL PANEL ############################
    tabPanel(
      title = "Modeling",
      strong(helpText("Warning: For professional use only.")),
      fluidRow(
        column(
          4,
          h4("Model form"),
          selectInput(
            inputId = "mvars",
            label = "Select independent variable(s):",
            choices = c(1, names(Arrhythmia[,-1])),
            multiple = TRUE
          ),
          checkboxInput(inputId = "interaction",
                        label = "Include interactions"),
          fluidRow(column(
            6,
            radioButtons(
              inputId = "step",
              label = "Stepwise Model Selection:",
              choices = c("Forward", "Backward", "Both")
            )
          ),
          column(
            4,
            br(),
            br(),
            actionButton(inputId = "stepgo",
                         label = "Start")
          ))
        ),
        column(
          4,
          h4("Likelihood Ratio Test (LRT)"),
          br(),
          actionButton(inputId = "mod1", label = "Insert Model 1"),
          textOutput("model1"),
          br(),
          actionButton(inputId = "mod2", label = "Insert Model 2"),
          textOutput("model2"),
          br(),
          withMathJax(),
          helpText(
            tags$i(
              "\\(H_0:\\) Smaller model provides as good a fit for the data as the larger model"
            )
          ),
          helpText(
            tags$i(
              "\\(H_1:\\) Smaller model does not provide as good a fit for the data as the larger model"
            )
          ),
        ),
        column(
          4,
          h4("Miscellaneous Options"),
          
          checkboxInput(inputId = "vif",
                        label = "Variance Inflation Factor (VIF)"),
          checkboxInput(inputId = "contingency",
                        label = "Contingency Table: Observed vs Predicted"),
          checkboxInput(inputId = "odds",
                        label = "Odds Interval"),
          selectInput(
            inputId = "mcomp",
            label = "Pairwise comparison:",
            choices = c("BMIGroups", "AgeGroups")
          ),
          actionButton(inputId = "comp_button", label = "Compare")
        )
      ),
      fluidRow(
        column(
          4,
          h5(textOutput("step_text")),
          verbatimTextOutput("step"),
          h5(textOutput("model_text")),
          verbatimTextOutput("model")
        ),
        column(4,
               verbatimTextOutput("anova")),
        column(
          4,
          h5(textOutput("vif_text")),
          verbatimTextOutput("vif"),
          h5(textOutput("contingency_text")),
          verbatimTextOutput("contingency"),
          textOutput("accuracy"),
          textOutput("sensitivity"),
          textOutput("specificity"),
          h5(textOutput("odds_text")),
          verbatimTextOutput("odds"),
          verbatimTextOutput("mcomps")
        )
      )
    ),
    ######################### PREDICT PANEL ###############################
    tabPanel(
      title = "Predict",
      strong(helpText("Warning: For professional use only.")),
      fluidRow(column(
        12,
        h5(
          "In this section, the probability of having arrhythmia and related statistics
      can be calculated for one or two different people with the model obtained by
      Purposeful Selection method. Model used for predictions is as follows:"
        ),
        h6(
          "$$log(\\frac{\\hat{p}}{1-\\hat{p}})= -6.24 + 0.41*Age + 0.55*SexMale - 0.27*Weight + 0.06*QRSDuration -
          0.01*P.interval + 0.01*T.interval + 0.54*(Age*Weight) + 0.26*(Age*HeartRate)$$"
        ),
        tags$i(
          helpText(
            "Note: Age, Weight and Heart Rate variables were scaled to avoid multicollinearity problem."
          )
        )
      )),
      fluidRow(
        column(3,
               h5("95% CI for Log Odds:"),
               verbatimTextOutput("logit_ci")),
        column(3,
               h5("95% CI for Odds:"),
               verbatimTextOutput("odds_ci"))
      ),
      
      fluidRow(column(
        6,
        h5("Predict for Person 1"),
        fluidRow(
          column(
            4,
            selectInput(
              inputId = "sex1",
              label = "Sex:",
              choices = c("Male", "Female")
            ),
            numericInput(
              inputId = "age1",
              label = "Age:",
              value = 20,
              min = 0,
              max = 100,
              step = 1
            ),
            numericInput(
              inputId = "height1",
              label = "Height (cm):",
              value = 180,
              min = 100,
              max = 300,
              step = 1
            ),
            numericInput(
              inputId = "weight1",
              label = "Weight (kg):",
              value = 70,
              min = 20,
              max = 200,
              step = 1
            ),
            actionButton(inputId = "predict1",
                         label = "Predict")
          ),
          column(
            4,
            numericInput(
              inputId = "heartrate1",
              label = "Heart rate (bpm):",
              value = 80,
              min = 0,
              max = 200,
              step = 1
            ),
            numericInput(
              inputId = "qrs1",
              label = "QRS Duration (msec):",
              value = 85,
              min = 0,
              max = 200,
              step = 1
            ),
            numericInput(
              inputId = "t_interval1",
              label = "T Interval (msec):",
              value = 150,
              min = 0,
              max = 400,
              step = 1
            ),
            numericInput(
              inputId = "p_interval1",
              label = "P Interval (msec):",
              value = 90,
              min = 100,
              max = 250,
              step = 1
            )
          )
        )
      ),
      column(
        6,
        h5("Predict for Person 2"),
        fluidRow(
          column(
            4,
            selectInput(
              inputId = "sex2",
              label = "Sex:",
              choices = c("Male", "Female")
            ),
            numericInput(
              inputId = "age2",
              label = "Age:",
              value = 20,
              min = 0,
              max = 100,
              step = 1
            ),
            numericInput(
              inputId = "height2",
              label = "Height (cm):",
              value = 180,
              min = 100,
              max = 300,
              step = 1
            ),
            numericInput(
              inputId = "weight2",
              label = "Weight (kg):",
              value = 70,
              min = 20,
              max = 200,
              step = 1
            ),
            actionButton(inputId = "predict2",
                         label = "Predict")
          ),
          column(
            4,
            numericInput(
              inputId = "heartrate2",
              label = "Heart rate (bpm):",
              value = 80,
              min = 0,
              max = 200,
              step = 1
            ),
            numericInput(
              inputId = "qrs2",
              label = "QRS Duration (msec):",
              value = 85,
              min = 0,
              max = 200,
              step = 1
            ),
            numericInput(
              inputId = "t_interval2",
              label = "T Interval (msec):",
              value = 150,
              min = 0,
              max = 400,
              step = 1
            ),
            numericInput(
              inputId = "p_interval2",
              label = "P Interval (msec):",
              value = 90,
              min = 100,
              max = 250,
              step = 1
            )
          )
        )
      )),
      fluidRow(
        column(6,
               br(),
               textOutput("person1"),
               br(),
               br()),
        column(6,
               br(),
               textOutput("person2"),
               br(),
               br())
      )
    )
  )
)
