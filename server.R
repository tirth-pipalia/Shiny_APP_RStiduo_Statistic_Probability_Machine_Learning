#ShinyServer
library(shiny)# to load shiny package

#defing server page for shiny app
shinyServer(

    #declaring function for getting input form UI page and providing Output for Server page
  
  function(input,output){

    #####     Poission Plot   #######    
    
    output$poissionPlot<-renderPlot({
      
        par(mfrow=c(1,2))
        D=rpois(input$s, input$lam)
        tab=table(D)
        barplot(tab,col='blue')
        x1=0:input$max
        y1=dpois(x1,input$lam)
        plot(x1,y1,type='b')
      
    })
    
####    Binomial Plot   #######  
    
    output$binomialPlot<-renderPlot({
      par(mfrow=c(1,2))
      d<-density(rbinom(1000,input$n,input$p))
      plot(d,main="Kernel Density of generated data")
      polygon(d, col="red", border="blue")
      x=0:input$n
      plot(x,dbinom(x,input$n,input$p))
      
    })
  
#####         Geometric Plot    ###########
    
    output$geometricPlot <- renderPlot({
      par(mfrow=c(1,2))
      D=rgeom(input$sgeo,input$pgeo)
      tab=table(D)
      barplot(tab,col='blue')
      x2=0:input$maxgeo
      y2=dgeom(x2,input$pgeo)
      plot(x2,y2,type='b')
    })
    
#####    Support Vector Regression Plot ############
    
    output$plotsvr <- renderPlot({
      
      # SVR (Support Vector Regression) 
      
      # Importing the dataset
      dataset = read.csv('Position_Salaries.csv')#position salary
      dataset = dataset[2:3]
      
      # Fitting SVR to the dataset
      # install.packages('e1071')
      
      library(e1071)
      regressor = svm(formula = Salary ~ .,
                      data = dataset,
                      type = 'eps-regression',
                      kernel = 'radial')
      
      # Predicting a new result
      
      y_pred = predict(regressor, data.frame(Level = 6.5))
      
      # Visualising the SVR results
      # install.packages('ggplot2')
      
      dataset = read.csv('Position_Salaries.csv')#position salary
      dataset = dataset[2:3]
      library(ggplot2)
      ggplot() +
        geom_point(aes(x = dataset$Level, y = dataset$Salary),
                   colour = 'red') +
        geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
                  colour = 'blue') +
        ggtitle('Truth or Bluff (SVR)') +
        xlab('Level') +
        ylab('Salary')
    })
    
    
#####     Decision Tree Regression Plot     ######
    
    output$plotdtr<-renderPlot({
      
      # Decision Tree Regression
      # Importing the dataset
      
      dataset = read.csv('Position_Salaries.csv')#position salary
      dataset = dataset[2:3]
      
      
      library(rpart)
      regressor = rpart(formula = Salary ~ .,
                        data = dataset,
                        control = rpart.control(minsplit = 1))
      
      # Predicting a new result with Decision Tree Regression
      
      y_pred = predict(regressor, data.frame(Level = 6.5))
      
      # Visualising the Decision Tree Regression results (higher resolution)
      # install.packages('ggplot2')
      
      library(ggplot2)
      x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
      ggplot() +
        geom_point(aes(x = dataset$Level, y = dataset$Salary),
                   colour = 'red') +
        geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
                  colour = 'blue') +
        ggtitle('Truth or Bluff (Decision Tree Regression)') +
        xlab('Level') +
        ylab('Salary')
      
      # Plotting the tree
      
      plot(regressor)
      text(regressor)
    })
    
    ######       Simple Linear Train Dataset Plot #####
    
    output$trainplotslr<-renderPlot({
      
      # Simple Linear Regression
      # Importing the dataset
      
      dataset = read.csv('Salary_Data.csv')
      
      # Splitting the dataset into the Training set and Test set
      # install.packages('caTools')
      
      library(caTools)
      set.seed(123)
      split = sample.split(dataset$Salary, SplitRatio = 2/3)
      training_set = subset(dataset, split == TRUE)
      test_set = subset(dataset, split == FALSE)
      
      # Feature Scaling
      # training_set = scale(training_set)
      # test_set = scale(test_set)
      # Fitting Simple Linear Regression to the Training set
      
      regressor = lm(formula = Salary ~ YearsExperience,
                     data = training_set)
      
      # Predicting the Test set results
      #y_pred = predict(regressor, newdata = test_set)
      # Visualising the Training set results
      
      library(ggplot2)
      ggplot() +
        geom_point(aes(x = training_set$YearsExperience, y = training_set$Salary),
                   colour = 'red') +
        geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
                  colour = 'blue') +
        ggtitle('Salary vs Experience (Training set)') +
        xlab('Years of experience') +
        ylab('Salary')
    })
    
    ######       Simple Linear Test Dataset Plot #####
    
    output$testplotslr<-renderPlot({
      
      # Importing the dataset
      
      dataset = read.csv('Salary_Data.csv')
      
      # Splitting the dataset into the Training set and Test set
      # install.packages('caTools')
      
      library(caTools)
      set.seed(123)
      split = sample.split(dataset$Salary, SplitRatio = 2/3)
      training_set = subset(dataset, split == TRUE)
      test_set = subset(dataset, split == FALSE)
      
      # Feature Scaling
      # training_set = scale(training_set)
      # test_set = scale(test_set)
      # Fitting Simple Linear Regression to the Training set
      
      regressor = lm(formula = Salary ~ YearsExperience,
                     data = training_set)
      
      # Visualising the Test set results
      
      library(ggplot2)
      ggplot() +
        geom_point(aes(x = test_set$YearsExperience, y = test_set$Salary),
                   colour = 'red') +
        geom_line(aes(x = training_set$YearsExperience, y = predict(regressor, newdata = training_set)),
                  colour = 'blue') +
        ggtitle('Salary vs Experience (Test set)') +
        xlab('Years of experience') +
        ylab('Salary')
    })
    #### poission model ####
    output$poissionTable <- renderTable({
    c("dpois:-",dpois(input$j,input$lam))#,"dbinom:-",dbinom(input$j,input$n, input$p))
      
    })
    #### Binomial Model   ####
    output$binomialTable<-renderTable({
    c("dbinom:-",dbinom(input$j, input$n,input$p))  
    })
    #### geometric model    ####
    output$geometricTable<-renderTable({
      c("dgeom:-",dgeom(input$jgeo,input$pgeo))
    })
    #### To show Dimensions of "mtcars" dataset    ####
    output$dimensions<-renderPrint({
      dim(mtcars)
    })
    #### To show inteactive column option for "mtcars" dataset using some interactive features    ####
    output$data<-renderTable({
      colm <- as.numeric(input$var)
      mtcars[colm]
    })
    ####    To show structure of "mtcars" dataset   ####
    output$structure<-renderPrint({
      str(mtcars)
    })
    ####    To show summary of "mtcars" dataset   ####
    output$summary <- renderPrint({
      summary(mtcars)
    })
    #### To show head of "mtcars" dataset using some interactive feature    ####
    output$mthead<-renderPrint({
      head(mtcars,input$headlimit)
    })
    ####  To display "mtcars" dataset   ####
    output$mytable<-renderDataTable({
      mtcars
    })
    #### To show Salary_Data  dataset   ####
    output$slsa<-renderDataTable({
      sp<-read.csv('Salary_Data.csv')
      sp
    })
    #### To show Position_Salaries dataset    ####
    output$dtsa<-renderPrint({
      dt<-read.csv('Position_Salaries.csv')
      dt
    })

    output$svsa<-renderPrint({
      sv<-read.csv('Position_Salaries.csv')
      sv
    })
    ####    To display "Salary_Data" dataset    ####
    output$salary<-renderDataTable({
      sal<-read.csv('Salary_Data.csv')
      sal
    })
    ####    To display plot for "mtcars" dataset    ####
    output$plot1<-renderPlot({
      plot(mtcars$wt,mtcars$mpg)
    })
    
    output$info<-renderPrint({
      nearPoints(mtcars, input$plot_click, xvar="wt", yvar = "mpg")
    })
    
  }
)