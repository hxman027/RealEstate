# Mallory - STAT 550 2020###
# This is the server file of the shiny app

# fix main title when PIC is checked but empty 
# load rdas for each so that estimates can be made
# fix select input to choose only the top 52 categories and other


# in case modified data needs to be accessed
source("helpers.R")

# load models - RF for mill rate predictions and ??? for assessment value predictions
load("RegularizedLogisticRegression.rda")


# server:
server <- function(input, output, session) {
  
  filtered <- reactive({
    
    # Update when following inputs are changed
    input$updateButton
    
    newdata <- dat
    d <- NULL
    #print(dim(dat))
    
    #if(input$csvfile == NULL){
    # Filter data based on the user inputs
    isolate({
        #print("The type is not Select, it is:")
          #print(input$typeInput)
          
          # If using PIC:
          if(input$picInput && input$identInput!="")
            {
            #print(paste("PIC input is", input$picInput, "and is:", sep = " "))
            #print(input$identInput)
            
            d <- newdata  %>% 
              filter(PIC == input$identInput)
          }
          
          
          # If not using PIC, filter by municipality and tax class:
          if(!input$picInput){
            #print(paste("PIC input is:", input$picInput, sep = " "))
            d <- newdata %>% 
              filter(tax.class == input$taxclassInput,
                     municipality == input$municipalityInput)
          }
      
      })
    
    # return filtered data
    if(dim(d)[1]==0){
      d <- NULL
    }
    d
    #print(dim(d))
    #print(head(d))
    })
  
   ########## PLOTTING TAB ###################
   # Add plots of either mill rate or assessment value over time to plot tab
  munic <- reactive(input$municipalityInput)
  
  millrateplot <- reactive({
    input$updateButton
    
    data <- filtered()
    #print("plotting mill rate plot")
    #print(head(data))
    
          
    isolate({
    if(is.null(data)){
      p <- paste("No corresponding data.")
    }

    # plot mill rates over time for municipality chosen for mill rate predicitons
    if (input$typeInput == 'Mill Rate'){
      p <- ggplot(data, aes(x = year, y = mill.rate)) +
        geom_line(color="#FF3333") +
        geom_point(color="#FF3333") +
        theme_minimal() +
        xlab("Year") +
        ylab("Mill Rate") +
        ggtitle("Municipal Mill Rate Over Time")
    }
    })

    return(p)

  })

  assessplot <- reactive({
    input$updateButton
    
    data <- filtered()
    #print("plotting assessplot")
    #print(head(data))

    isolate({
      if(is.null(data)){
        p <- paste("No corresponding data.")
        }
      
    # plot assessment values over time 
      #print("trying to plot")
      if(input$typeInput == 'Assessment Value'){
          #print("ggplotting assessment values")
          p <- ggplot(data, aes(x = year, y = total.assessment)) +
            geom_line(color="#56B4E9") +
            geom_point(color="#56B4E9") +
            theme_minimal() +
            xlab("Year") +
            ylab("Assessment Value") +
            ggtitle("Assessment Values Over Time")
      }
      })
    
    return(p)
    
  })
  
   output$coolplot <- renderPlot({
     if (input$typeInput != 'Select'){
       if(input$typeInput == 'Assessment Value'){
         if(input$picInput && input$identInput!=""){
           assessplot()
         }
         else{
           return()
         }
       }
       
       else{
         millrateplot()
       }
     }
     
     else{
       return()
     }
     })

  
  ###### ESTIMATE TAB ####################
  # reactive text  - working
  estimates <- reactive({
    input$updateButton
    
    data <- filtered()
    
    isolate({
      if(is.null(data)){
        return(NULL)
      }
      
     # If using PIC: 
      if(input$picInput){
        if(input$identInput != ""){
          
          # If doing Mill Rate prediciton, use random forest
          if(input$typeInput == 'Mill Rate'){
            
            # extract latest mill rate
            past20 <- filtered() %>% 
              filter(year == 2020)
            
            past19 <- filtered() %>% 
              filter(year == 2019)
            
            # if 2020 column is nonempty of mill rates for this community:
            if(sum(!is.na(past20$mill.rate>0))){
              meanmillrate <- mean(past20$mill.rate)
            }
            
            # otherwise use 2019 mill rate data
            else{
              meanmillrate <- mean(past19$mill.rate)
            }
            
            # put data together in the way rfmill expects as input call it inputdata
            # columns include tax.class, municipality, total.assessment, past.mill
            meanassess <- mean(past20$total.assessment)
            
            pred.data <- cbind(filtered()$tax.class[1], 
                               filtered()$municipality[1],
                               meanassess,
                               meanmillrate)
            pred.data <- as.data.frame(pred.data)
            pred.data$municipality <- as.factor(pred.data$municipality)
                               
            colnames(pred.data) <- c('tax.class', 'municipality', 'total.assessment',
                                  'past.mill')
                               
            # predict next mill rate using random forest
            predict(rf.mill, newdata = pred.data)
          }
          
          # If doing Assessment Value prediction, use something else...
          if(input$typeInput == 'Assessment Value'){
            #...
          }
        }
      }
      
      # If not using PIC:
      else{
        # If doing Mill Rate prediciton, use random forest
        if(input$typeInput == 'Mill Rate'){
          #...
        }
        
        # If doing Assessment Value prediction, use something else...
        if(input$typeInput == 'Assessment Value'){
          #...
        }
      }
    })
  })
   
  estimatestext <- reactive({
    input$updateButton
    
   #print("estimates...")
      
   # If using PIC:
   if(input$picInput){
     if(input$identInput!=""){
       if(input$typeInput == 'Mill Rate'){   #need to be changed to extract values
         #print("estimates for mill rate")
         return(paste("Mill rate prediction for class", filtered()$tax.class[1],
                      "in", filtered()$municipality[1], "is...",
                      estimates(), sep = " ")) # RETURN PREDICTION
         }
          
       if(input$typeInput == 'Assessment Value'){
         return(paste("prediction for", input$identInput,
                      "is...", estimate(), sep = " "))  # RETURN PREDICTION
         }
          
       if(input$typeInput == 'Select'){
         return("Enter prediction type.")
       }
       }
     
     else{
       return("Please enter PIC.")
     }
     }
      
   # If not using PIC:
   else{
     if(input$typeInput == 'Mill Rate'){
       #print("estimates for mill rate")
       return(paste("Mill rate prediction for class", input$taxclassInput,
                    "in", input$municipalityInput, "is...", 
                    estimate(), sep = " ")) # RETURN PREDICTION
       }
        
     if(input$typeInput == 'Assessment Value'){
       return(paste("Predicted next assessment value is...", 
                    estimates(), sep = " "))  # RETURN PREDICTION
       }
        
     if(input$typeInput == 'Select'){
       return("Enter prediction type.")
     }
   }
   })
   
  # output the estimates text in the main panel
  output$results <- renderText({
    estimatestext()
  })
  
  
  # Titles text for main panel title - describes prediction type or PIC 
  # number if applicable
  titles <- reactive({
    input$updateButton
    
    data <- filtered()
    
    if(!input$picInput && input$typeInput == 'Select' && input$municipalityInput == '-' &&
       input$taxclassInput == '-'){
      return(paste(""))
    }
    
    if(is.null(data)){
      return(paste("Could not find matching data."))
    }
    
    else{
      if(input$picInput){
        if(input$identInput!=""){
          
          if(input$typeInput == 'Assessment Value'){
            return(paste("Assessment value for", input$identInput, sep = " ")) #ADD PREDICTION HERE
          }
          
          if(input$typeInput == 'Mill Rate'){
            return(paste("Class", filtered()$tax.class[1], 
                         "mill rate for", filtered()$municipality[1], sep = " ")) #ADD PREDICTION HERE
          }
          
          if(input$typeInput == 'Select'){
            return(paste("Select prediction type."))
          }
        }
      }
      
      else{
          #print("trying to post title")
          if(input$typeInput == 'Mill Rate'){
            return(paste("Class", input$taxclassInput, 
                         "mill rate for", input$municipalityInput, sep = " ")) #ADD PREDICTION HERE
          }
          else{
            return(paste("Could not find matching data."))
          }
        }
      }
  })
      
  
  output$resultsText <- renderText({     
    titles()
  })
}