library(shiny)
library(shinythemes)
library(rsconnect)
library(viridis)
library(corrplot)


#Collecting the data
library(readr)
insurance <- read_csv("insurance.csv")

#Creating a copy
#insurance_1 <- read_csv("DS 501/Final case study - app/insurance.csv")

#Exploratory data analysis
summary(insurance)

#Understanding the data entries for male and female
library(ggplot2)
ggplot(insurance, aes(x = sex, fill = sex)) +
  geom_bar() +
  labs(title = "Gender Graph")

print(paste("The data rows for male and female seems almost equal, so it seems balanced in terms of the gender."))

smoker_counts <- table(insurance$smoker)
gender_counts = table(insurance$sex)
# Creating a data frame for the pie chart
pie <- data.frame(smoker = names(smoker_counts),
                  counts = as.numeric(smoker_counts))

pie_gender <- data.frame(sex = names(gender_counts),
                         counts = as.numeric(gender_counts))

#Smoker and non-smoker
pie_chart = ggplot(pie, aes(x = "", y = counts, fill = smoker)) +
  geom_bar(stat = "identity", width = 1,color="white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(counts/sum(counts) * 100, 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Pie chart of Smoker or Non-Smoker") +
  theme_void() +
  theme(legend.position = "bottom")
pie_chart + scale_fill_manual(values = c("lightblue", "orange"))

#Region
ggplot(insurance, aes(x = region, fill = region)) +
  geom_bar() +
  labs(title = "Region Graph") +
  theme_minimal()

#Children
ggplot(insurance, aes(x = factor(children))) +
  geom_bar(fill = viridis_pal()(length(unique(insurance$children)))) +  # Assigning distinct colors
  labs(
    title = "Count of Children",
    x = "Number of Children",  # Renaming x-axis label
    y = "Count"            # Renaming y-axis label
  ) +
  scale_fill_viridis(discrete = TRUE, option = "magma") +  # Using viridis color palette
  theme_minimal()

#Age
ggplot(insurance, aes(x = age)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(
    title = "Histogram with KDE",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()

#BMI
ggplot(insurance, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "lightpink", color = "black", alpha = 0.7) +
  geom_density(color = "red") +
  labs(
    title = "Histogram with KDE for BMI",
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal()

#Insurance Charges
ggplot(insurance, aes(x = charges)) +
  geom_histogram(bins = 10, fill = "lightgreen", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram for Insurance Charges",
    x = "Charges",
    y = "Count"
  ) +
  theme_minimal()

#Age of each genders
ggplot(insurance, aes(x = sex, y = age, fill = sex)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.4) +
  labs(
    title = "Mean Age by Gender",
    x = "Gender",
    y = "Mean Age" ) +
  theme_minimal()

#BMI of each gender
ggplot(insurance, aes(x = sex, y = bmi, fill = sex)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.4) +
  labs(
    title = "Average BMI by Gender",
    x = "Gender",
    y = "Average BMI" ) +
  theme_minimal()

#Charges variation with number of children
ggplot(insurance, aes(x = factor(children), y = charges, fill = factor(children))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  labs(
    title = "Average charges for number of children",
    x = "Number of Children",
    y = "Charges" ) +
  theme_minimal()

#Charges variation with region
ggplot(insurance, aes(x = region, y = charges, fill = region)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  labs(
    title = "Average charges for each region",
    x = "Region",
    y = "Charges" ) +
  theme_minimal()

#Charges variation with Smoking
ggplot(insurance, aes(x = smoker, y = charges, fill = smoker)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  labs(
    title = "Average charges for smokers vs non-smokers",
    y = "Charges" ) +
  theme_minimal()

#Charges variation with respect to gender
ggplot(insurance, aes(x = sex, y = charges, fill = sex)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
  labs(
    title = "Average charges for each gender",
    x= "Gender",
    y = "Charges" ) +
  theme_minimal()

#Charges variation as a function of Age
ggplot(insurance, aes(x = age, y = charges)) + 
  geom_point(size = 1) + 
  labs(x = "Age", 
       y = "Charges", 
       title = "Variation of charges with age")

#Charges variation as a function of BMI
ggplot(insurance, aes(x = bmi, y = charges)) + 
  geom_point(size = 1) + 
  labs(x = "Age", 
       y = "Charges", 
       title = "Variation of charges with BMI")


#Checking for any missing values
numberOfNA = length(which(is.na(insurance)==T))
if(numberOfNA > 0) {
  cat('Number of missing values found: ', numberOfNA)
  cat('\nRemoving missing values...')
  insrance = insurance[complete.cases(insurance), ]
}


#Converting the categorical data to numerical data
insurance$sex <- ifelse(insurance$sex == 'male', 1, 0)
insurance$smoker <- ifelse(insurance$smoker == 'yes', 1, 0)
insurance$region <- ifelse(insurance$region == 'southwest', 0,
                         ifelse(insurance$region == 'southeast', 1,
                                ifelse(insurance$region == 'northwest', 2, 3)))


#Separating the data set into training (80%) and test set (20%)
smp_size <- floor(0.80 * nrow(insurance))
set.seed(2)
splitset <- sample(seq_len(nrow(insurance)), size = smp_size)
train_set <- insurance[splitset, ]
test_set <- insurance[-splitset, ]


#Training the model
#Linear Regression
first_model=lm(formula= charges ~ age + sex + bmi + children + smoker + region, data=train_set)
summary(first_model)
model_residuals=first_model$residuals
hist(model_residuals)

#Evaluation of the model
train_pred <- predict(first_model,train_set)
r2_train <- cor(train_set$charges, train_pred)^2
correlation_matrix=cor(train_set)
corrplot(correlation_matrix, method = "color", 
         addCoef.col = "black", number.cex = 1, tl.cex = 1)

#Testing the model
test_pred <- predict(first_model,test_set)
r2_test <- cor(test_set$charges, test_pred)^2
mean_error <- mean(test_pred - test_set$charges)

#Saving the training data to csv
#write.csv(train_set,"train_set.csv")


ui <- fluidPage(theme = shinytheme("united"),
      navbarPage(
      "Medical Insurance",
      tabPanel("About the App",
      "Medical Insurance app will give you an estimate of your yearly medical insurance charges.", 
       "It takes into account some of your health rated information such as Gender, Age, BMI, if you're a smoker, and other personal information such as which region you live in, how many children you have.",
      "To understand the variance of your health insurnace charges, with respect to your health factor, you can select a variable below and check out the plots.",
        br(),
        br(),
       fluidRow(
            column(4,offset=1,
            helpText("Let's check how the insurance charges vary with respect to factors below-"),
            br(),
            selectInput("option", ("Select a factor"), 
                    choices = list("Age" = 1, "Gender" = 2,
                                  "BMI" = 3, "Number of Children" = 4,
                                  "Smoker" = 5,"Region" = 6), selected = 3),
                        ),
            column(5, offset=2,
            helpText("We can get some more insight by looking into distribution of the variables in our data."),
            br(),
            selectInput("variable", ("Select a variable to check distribution"), 
                    choices = list("Age" = 1, "Gender" = 2,
                                  "BMI" = 3, "Number of Children" = 4,
                                  "Smoker" = 5), selected = 5)
                    )),
            br(),
            br(),
            fluidRow(
            splitLayout(cellWidths = c("50%", "50%"), plotOutput("factor"), plotOutput("variable1"))
                   )
                  ),
                  
            tabPanel("About the method",
                      "This app is designed on the basis of a Linear Regression Model which involved training data from about 1000 individuals.",
                      "Linear Regression is one of the methods of regression analysis where a dataset is analyzed in a highly-detailed fashion.",
                      "A target variable is chosed, such as medical insurance charges in this case, and its dependence on other variables such as Age, BMI, etc. is analyzed to understand their trends.",
                      "In the previous tab, we saw how the medical insurance charges vary with these individual factors. Here, we see other analyses such as the correlation matrix, that tells us how each factor affects every other factor.",
                      br(),
                      br(),
                      plotOutput("correlation_matrix")
                           #dataTableOutput("data_corr")
                           ),
                  
            tabPanel("Insurance prediction",
                      h4("Please enter your details to get an estimate of your medical insurance."),
                      br(),
                      fluidRow(
                      column(3,
                      numericInput("age",("Enter Your Age"), min=18,max=64,value=18), 
                      br(),
                      radioButtons("sex",("Select your gender"),choices = list("Male" = 1, "Female" = 0)),),
                      column(3,offset=1,
                      sliderInput("bmi",("Select Your BMI"),min = 15, max = 50, value = 15),
                      br(),
                      radioButtons("smoker",("Do you smoke?"),choices = list("Yes" = 1,"No" = 0)),),
                      column(4,offset=1,
                      sliderInput("children",("How many children do you have?"), 
                                                   min = 0, max = 5, value = 0),
                      br(),
                      radioButtons("region",("Which region do you live in?"),
                              choices = list("Southwest" = 0,"Southeast" = 1,
                                                              "Northwest" = 2,
                                                          "Northeast" = 3)), )),
                      br(),
                      br(),
                      br(),
                      verbatimTextOutput("charges"),
                    ),
                ),
)


server <- function(input, output){
 
   output$factor=renderPlot({
  if (input$option==1){
    #Charges variation as a function of Age
      ggplot(train_set, aes(x = age, y = charges)) + 
        geom_point(size = 1) + 
        labs(x = "Age", 
             y = "Charges", 
             title = "Variation of charges with age")+
             theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
                   axis.title.x = element_text(size=16,vjust=-0.5), 
                   axis.title.y = element_text(size=16,vjust=1.5),
                   axis.text = element_text(color = "darkblue",
                                            size = 14))
  }
   else  if (input$option==2){
       #Charges variation as a function of Gender
       ggplot(train_set, aes(x = sex, y = charges, fill = sex)) +
         geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
         labs(
           title = "Average charges for each gender",
           x= "Gender",
           y = "Charges" ) +
       theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
             axis.title.x = element_text(size=16,vjust=-0.5), 
             axis.title.y = element_text(size=16,vjust=1.5),
             axis.text = element_text(color = "darkblue",
                                      size = 14))
     }
    else if (input$option==3){
       #Charges variation as a function of BMI
       ggplot(train_set, aes(x = bmi, y = charges)) + 
         geom_point(size = 1) + 
         labs(x = "Age", 
              y = "Charges", 
              title = "Variation of charges with BMI")+
        theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
              axis.title.x = element_text(size=16,vjust=-0.5), 
              axis.title.y = element_text(size=16,vjust=1.5),
              axis.text = element_text(color = "darkblue",
                                       size = 14))
     }
    else if (input$option==4){
       #Charges variation as a function of Number of Children
       ggplot(train_set, aes(x = factor(children), y = charges, fill = factor(children))) +
         geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
         labs(
           title = "Average charges for number of children",
           x = "Number of Children",
           y = "Charges" ) +
         theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
                                 axis.title.x = element_text(size=16,vjust=-0.5), 
                                 axis.title.y = element_text(size=16,vjust=1.5),
                                 axis.text = element_text(color = "darkblue",
                                                          size = 14))
     }
     else if(input$option==5){
       #Charges variation with Smoking
       ggplot(train_set, aes(x = smoker, y = charges, fill = smoker)) +
         geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
         labs(
           title = "Average charges for smokers vs non-smokers",
           y = "Charges" ) +
         theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
               axis.title.x = element_text(size=16,vjust=-0.5), 
               axis.title.y = element_text(size=16,vjust=1.5),
               axis.text = element_text(color = "darkblue",
                                        size = 14))
     }
     else{
       #Charges variation with region
       ggplot(train_set, aes(x = region, y = charges, fill = region)) +
         geom_bar(stat = "summary", fun = "mean", position = "dodge", width = 0.6) +
         labs(
           title = "Average charges for each region",
           x = "Region",
           y = "Charges" ) +
         theme(plot.title = element_text(hjust=0.5,size=16,face="bold"),
               axis.title.x = element_text(size=16,vjust=-0.5), 
               axis.title.y = element_text(size=16,vjust=1.5),
               axis.text = element_text(color = "darkblue",
                                        size = 14))
     }
     },height=300,width=400)
  
 #output$data_factor=renderTable({insurance})
  
   output$variable1=renderPlot({
     if (input$variable==1){
       #Distribution of Age
       ggplot(insurance, aes(x = age)) +
         geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black", alpha = 0.7) +
         geom_density(color = "red") +
         labs(
           title = "Histogram with KDE",
           x = "Age",
           y = "Density"
         ) +
         theme_minimal()
     }
     else  if (input$variable==2){
       #Distribution of Gender
       pie_chart = ggplot(pie_gender, aes(x = "", y = counts, fill = sex)) +
         geom_bar(stat = "identity", width = 1,color="white") +
         coord_polar("y", start = 0) +
         geom_text(aes(label = paste0(round(counts/sum(counts) * 100, 1), "%")),
                   position = position_stack(vjust = 0.5)) +
         theme_void() +
         theme(legend.position = "top")
       pie_chart + scale_fill_manual(values = c("pink", "lightblue"))

     }
     else if (input$variable==3){
       #Distribution of BMI
       ggplot(insurance, aes(x = bmi)) +
         geom_histogram(aes(y = ..density..), bins = 10, fill = "lightpink", color = "black", alpha = 0.7) +
         geom_density(color = "red") +
         labs(
           title = "Histogram with KDE for BMI",
           x = "BMI",
           y = "Density"
         ) +
         theme_minimal()
     }
     else if (input$variable==4){
       #Distribution of Children
       ggplot(insurance, aes(x = factor(children))) +
         geom_bar(fill = viridis_pal()(length(unique(insurance$children)))) +  # Assigning distinct colors
         labs(
           title = "Count of Children",
           x = "Number of Children",
           y = "Count") +
         scale_fill_viridis(discrete = TRUE, option = "magma") +  # Using viridis color palette
         theme_minimal()
     }
     else if(input$variable==5){
       #Smoking
       pie_chart = ggplot(pie, aes(x = "", y = counts, fill = smoker)) +
         geom_bar(stat = "identity", width = 1,color="white") +
         coord_polar("y", start = 0) +
         geom_text(aes(label = paste0(round(counts/sum(counts) * 100, 1), "%")),
                   position = position_stack(vjust = 0.5)) +
         theme_void() +
         theme(legend.position = "top")
       pie_chart + scale_fill_manual(values = c("lightblue", "orange"))
       
     }
     #else{
       #region
       #ggplot(insurance, aes(x = region, fill = region)) +
        # geom_bar() +
         #labs(title = "Region Graph") +
         #theme_minimal()
     #}
   },height=350,width=350)
   
  output$correlation_matrix=renderPlot({
    correlation_matrix=cor(train_set)
    corrplot(correlation_matrix, method = "color", 
             addCoef.col = "black", number.cex = 0.8, tl.cex = 0.8)
  })
  
  #output$data_corr=renderTable(train_set)
  
  output$charges= renderText({
    input_data <- data.frame(age = input$age,
                             sex = as.numeric(input$sex),
                             bmi = input$bmi,
                             children = input$children,
                             smoker = as.numeric(input$smoker),
                             region = as.numeric(input$region))
    
    prediction <- predict(first_model, newdata = input_data)
  
    paste("Your estimated medical insurance for the upcoming year would be $", prediction)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)