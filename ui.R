#ShinyUI
library(shiny)# to load shiny package

#defining  UI(User Interface) for shiny application

#fluidPage is to automatically adjust the layout according to the browser in which app is used

shinyUI(fluidPage(
  titlePanel(title=("Shiny App")),
  sidebarLayout(
    sidebarPanel(
      selectInput("task","Select Task to perform",choices = c("Data Import" = 1,"Descriptive Analytics" = 2,"Discrete Models" = 3,"Machine Learing" = 4),selected = 1),
      
      #######       For Task1=Data Import Sidebar   ########
      conditionalPanel(
        condition = "input.task ==1",
        selectInput("datasettype","Select Dataset to Import",choices = c("mtcars" =1,"Salary_Data" =2))
      ),
      
      #########     For task 2=Discriptive Analytics    #####################
      
      conditionalPanel(
        condition = "input.task == 2",
        selectInput("var","Select the variablrs from mtcars dataset",choices = c("mtcars.mpg"=1,"mtcars.cyl"=2,"mtcars.disp"=3,"mtcars.hp"=4,"mtcars.drat"=5,
                                                                               "mtcars.wt"=6,"mtcars.qsec"=7,"mtcars.vs"=8,"mtcars.am"=9,"mtcars.gear"=10,"mtcars.carb"=11)),
        
        numericInput("headlimit","No of rows for head",value = 10,step = 1)
      ),
      
      #######       For Task 3=Descrete Models
      
      conditionalPanel(
        condition = "input.task == 3",
        selectInput("dismodel","Select Discrete model",choices = c("Binomial"=1,"Poission"=2,"Geometric"=3),selected = NULL),

        #####   Poission Sidebar   ######      
        
        conditionalPanel(
          condition = "input.dismodel == 2",
          numericInput("j", "random value" , value = 1),
          numericInput("lam", "parameter lambda in Poisson" , value = 1),
          numericInput("max", "upper limit for x" , value = 5),  
          sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10)
        ),
        
#####    Binomial Sidebar   #####

        conditionalPanel(
          condition = "input.dismodel == 1",
          numericInput("n", "n in Binomial" , value = 10),
          numericInput("p", "p in Binomial" ,step = 0.1, value = 0.1)

        ),

#####    Geometric slidebar    ##############

        conditionalPanel(
          condition = "input.dismodel == 3",
          numericInput("maxgeo", "upper limit for x" , value = 5),
          sliderInput("sgeo", "number of simulated data" ,min=1, max=1000, value = 10),
          numericInput("jgeo", "j for geometric" , value = 1),
          numericInput("pgeo", "parameter p in Geometric" ,step = 0.1 ,value = 0.5)
          )
      ),
     
#######       For task 4=Machine Learining        ##########################
      
      conditionalPanel(
        condition = "input.task == 4",
        selectInput("mltype","Select Machine Learning Technique",choices = c("Support Vector Regression"=1,"Simple Linear Regression"=2,"Decision Tree Regression"=3),selected = 1)
      )

     
       
      ),

    mainPanel(
      
     ### For Task 1 === Selecting and importing Data    #####
     
      conditionalPanel(
        condition = ("input.task == 1"),
        conditionalPanel(
          condition ="input.datasettype ==1",
          dataTableOutput("mytable"),
        ),
        conditionalPanel(
          condition = "input.datasettype ==2",
          dataTableOutput("salary")
        )
      ),
     
     ####  For Task 2 === Analyticals Techniques    ######
     
      conditionalPanel(
        condition = ("input.task == 2"),
        tabsetPanel(type = "tab",
                    tabPanel("Data",tableOutput("data")),
                    tabPanel("Plot",plotOutput("plot1", click = "plot_click"),
                             verbatimTextOutput("info")),
                    tabPanel("Summary",verbatimTextOutput("summary")),
                    tabPanel("Head", verbatimTextOutput("mthead")),
                    tabPanel("Structure",verbatimTextOutput("structure")),
                    tabPanel("Dimensions",verbatimTextOutput("dimensions"))
                    )
      ),
     
      ### For Task 3 === Descrete Probability model ####
     
      conditionalPanel(
        condition = ("input.task==3"),
      
          ##### Main Panel for Binomial   #########
      
        conditionalPanel(
          condition = ("input.dismodel == 1"),
          tabsetPanel(type = "tab",
                      tabPanel("Binomial",tableOutput("binomialTable")),
                      tabPanel("Binomial Plot",plotOutput("binomialPlot"))
                      )
      ),

            #####  Main Panel for Poission  #####
      
      conditionalPanel(
        condition = ("input.dismodel == 2"),
        tabsetPanel(type = "tab",
                    tabPanel("Poission",tableOutput("poissionTable")),
                    tabPanel("Poission Plot",plotOutput("poissionPlot"))
        )
      ),
      
      ###### Main Panel for Geometric   ######
      
      conditionalPanel(
        condition = ("input.dismodel == 3"),
        tabsetPanel(type = "tab",
                    tabPanel("Geometric",tableOutput("geometricTable")),
                    tabPanel("Geometric Plot",plotOutput("geometricPlot"))
        )
      ),
        
      ),
     
     ####   For task 4 = Machine Learining Technique
     
     conditionalPanel(
       condition = ("input.task == 4"),
       
       ####   Support Vector Regression Plot  ####
       
       conditionalPanel(
         condition = ("input.mltype == 1"),
         tabsetPanel(type = "tab",
                     tabPanel("Plot",plotOutput("plotsvr")),
                     tabPanel("Dataset",verbatimTextOutput("svsa"))
         )
       ),
       
       #####      Decision Tree Regression Plot    ####
       
       conditionalPanel(
         condition = ("input.mltype == 3"),
         tabsetPanel(type ="tab",
                     tabPanel("Plot",plotOutput("plotdtr")),
                     tabPanel("Dataset",verbatimTextOutput("dtsa"))
         )
       ),
       
       ##### Simple Linear Regression
       
       conditionalPanel(
         condition = ("input.mltype == 2"),
         tabsetPanel(type ="tab",
                     tabPanel("Train Set Plot",plotOutput("trainplotslr")),
                     tabPanel("Test Set Plot",plotOutput("testplotslr")),
                     tabPanel("Dataset",dataTableOutput("slsa"))
         )
       )
     )
  )
)
)
)

