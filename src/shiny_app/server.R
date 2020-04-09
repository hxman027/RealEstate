# Mallory - STAT 550 2020###
# This is the server file of the shiny app

# in case modified data needs to be accessed
source("helpers.R")

server <- function(input, output, session) {
  
  filtered <- reactive({
    
    # Update when following inputs are changed
    input$updateButton
    
    newdata <- dat
    d <- NULL
    #print(dim(dat))
    
    # Filter data based on the user inputs
    isolate({
      #if (input$typeInput != 'Select'){
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
    
   #print("estimates...")
      
   # If using PIC:
   if(input$picInput){
     if(input$identInput!=""){
       if(input$typeInput == 'Mill Rate'){   #need to be changed to extract values
         #print("estimates for mill rate")
         return(paste("Mill rate prediction for class", input$taxclassInput,
                      "in", input$municipalityInput, "is...", sep = " ")) # RETURN PREDICTION
         }
          
       if(input$typeInput == 'Assessment Value'){
         return(paste("prediction for", input$identInput,
                      "is...", sep = " "))  # RETURN PREDICTION
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
                    "in", input$municipalityInput, "is...", sep = " ")) # RETURN PREDICTION
       }
        
     if(input$typeInput == 'Assessment Value'){
       return(paste("prediction for", input$identInput,
                    "is...", sep = " "))  # RETURN PREDICTION
       }
        
     if(input$typeInput == 'Select'){
       return("Enter prediction type.")
     }
   }
   })
   
  output$results <- renderText({
    estimates()
  })
  
  
  # Titles text - mostly working...
  
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
            return(paste("Class", input$taxclassInput, 
                         "mill rate for", input$municipalityInput, sep = " ")) #ADD PREDICTION HERE
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