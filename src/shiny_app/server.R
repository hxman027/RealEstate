# Mallory - STAT 550 2020###
# This is the server file of the shiny app

# fix main title when PIC is checked but empty 
# load rdas for each so that estimates can be made
# fix select input to choose only the top 52 categories and other


# in case modified data needs to be accessed
source("helpers.R")

# load models - RF for mill rate predictions and for assessment value predictions
load("rf.mill.rda")
load("rf.as.rda")


# server:
server <- function(input, output, session) {
  
  filtered <- reactive({
    
    # Update when following inputs are changed
    input$updateButton
    
    newdata <- datshort
    d <- NULL
    #print(dim(dat))
    
    # Filter data based on the user inputs
    isolate({
          # If using PIC:
          if(input$picInput && input$identInput!=""){
            d <- newdata  %>% 
              filter(PIC == input$identInput)
          }
          
          
          # If not using PIC, filter by municipality and tax class:
          if(!input$picInput){
            
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
    
    })
  
   ########## PLOTTING TAB ###################
   # Add plots of either mill rate or assessment value over time to plot tab
  
  # create mill rate plot that reacts to inputs
  millrateplot <- reactive({
    input$updateButton
    
    data <- filtered()
    
          
    isolate({
    if(is.null(data)){
      p <- paste("No corresponding data to plot.")
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

    p

  })

  # create assessment plots that react to user inputs
  assessplot <- reactive({
    input$updateButton
    
    data <- filtered()

    isolate({
      if(is.null(data)){
        p <- paste("No corresponding data to plot.")
        }
      
    # plot assessment values over time 
      if(input$picInput && input$identInput!=""){
        if(input$typeInput == 'Assessment Value') {
          #print("ggplotting assessment values")
          p <- ggplot(data, aes(x = year, y = total.assessment)) +
            geom_line(color = "#56B4E9") +
            geom_point(color = "#56B4E9") +
            theme_minimal() +
            xlab("Year") +
            ylab("Assessment Value") +
            ggtitle("Assessment Values Over Time")
        }
      }
      else{
        p <- paste("No corresponding data to plot.")
      }
      })
    
    p
    
  })
  
  # output one of the above plots onto UI
   output$coolplot <- renderPlot({
     if (input$typeInput != 'Select'){
       if(input$typeInput == 'Assessment Value'){
         assessplot()
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
  # give predictions given user inputs for mill rate or assessment value
  estimates <- reactive({
    input$updateButton
    
    isolate({
      if(is.null(filtered())){
        pred <- paste("No data.")
      }

      else{
        # If using PIC: 
        
        # If doing Mill Rate prediciton:
        if(input$typeInput == 'Mill Rate'){
          #print("doing mill rate prediction")
          
          # extract latest mill rate
          past20 <- filtered() %>% 
            filter(year == 2020)
            
          # if 2020 column is empty, it will break by condition on mean mill rate=0
          meanmillrate <- mean(na.omit(past20$mill.rate))
          print(meanmillrate)
           
          # put data together in the way rfmill expects as input call it inputdata
          # columns include tax.class, municipality, total.assessment, past.mill
          meanassess <- mean(na.omit(past20$total.assessment))
          print(meanassess)
         
          pred.data <- cbind(filtered()$tax.class[1], 
                             filtered()$municipality[1],
                             meanassess,
                             meanmillrate)
          pred.data <- as.data.frame(pred.data, stringsAsFactors = FALSE)
          colnames(pred.data) <- c('tax.class', 'municipality',
                                   'avg_assessment','past.mill')
            
        
          pred.data$past.mill <- as.numeric(pred.data$past.mill)
          pred.data$avg_assessment <- as.numeric(pred.data$avg_assessment)
          pred.data$municipality <- factor(pred.data$municipality,
                                           levels = levels(rfdat$municipality))
          pred.data$tax.class <- factor(pred.data$tax.class, 
                                        levels = levels(rfdat$tax.class))
          print(pred.data)
        
          # predict next mill rate using random forest
          pred <- round(predict(rf.mill, newdata = pred.data), 2)
        
          if(meanmillrate==0 || is.na(pred)){
            pred <- paste("No previous mill rate found in data.")
          }
          
          pred
          
          }
          
        
        # If doing Assessment Value prediction:
        if(input$typeInput == 'Assessment Value'){
        
          print("doing assessment value prediction")
        
          # extract latest assessment value
          past20 <- filtered() %>% 
            filter(year == 2020)
           
          # if using PIC and 2020 column is NA for this property's
          # assessment value, it will break 
          if(input$picInput){
            print("using PIC")
            if(input$identInput != ""){
              if(!is.na(past20$total.assessment)){
                last.assess <- past20$total.assessment
                }
              else{
                last.assess <- 0
              }
              }
            print(last.assess)
            }
        
          # if not using PIC, assessment value must come from user input
          else{
            print("not using PIC")
            if(input$assessmentInput != ""){
              last.assess <- input$assessmentInput
              }
            else{
              last.assess <- 0
              }
            print(last.assess)
            }
        
        
          # put data together in the way rfmill expects as input call it inputdata
          # columns include tax.class, municipality, total.assessment, and mill.rate
          print(head(filtered()))
          pred.data <- cbind(filtered()$tax.class[1], 
                             filtered()$municipality[1],
                             last.assess,
                             past20$mill.rate[1])
          
          pred.data <- as.data.frame(pred.data, stringsAsFactors = FALSE)
          colnames(pred.data) <- c('tax.class', 'municipality',
                                   'total.assessment', 'mill.rate')
            
          
          pred.data$mill.rate <- as.numeric(pred.data$mill.rate)
          pred.data$total.assessment <- as.numeric(pred.data$total.assessment)
          pred.data$municipality <- factor(pred.data$municipality,
                                           levels = levels(asdat$municipality))
          pred.data$tax.class <- factor(pred.data$tax.class,
                                        levels = levels(asdat$tax.class))
          print(pred.data)
          
          # predict next assessment value using random forest
          pred <- round(predict(rf.as, newdata = pred.data),2)
          
          if(last.assess==0 || is.na(pred)){
            pred <- paste("Missing required data.")
          }
        }
      }
      })
    
    
    pred
    print(pred)
    
    })
   
   # create estimates as text for output
   estimatestext <- reactive({
     input$updateButton
    
      
     # If using PIC:
     if(input$picInput){
       if(input$identInput!=""){
         if(input$typeInput == 'Mill Rate'){   #need to be changed to extract values
           return(paste("Mill rate prediction for class", 
                         filtered()$tax.class[1],
                         "in", filtered()$municipality[1], "- \n", 
                         estimates(), sep = " "))  #  RETURN PREDICTION
           }
       
         if(input$typeInput == 'Assessment Value'){
           return(paste("Predicted next assessment value of property \n", input$identInput,
                        "-", estimates(), sep = " "))  # RETURN PREDICTION
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
         return(paste("Mill rate prediction for class", input$taxclassInput,
                      "in", input$municipalityInput, "- \n", 
                      estimates(), sep = " ")) # RETURN PREDICTION
         }
      
       if(input$typeInput == 'Assessment Value'){
         
         if(!is.null(filtered())){
           md <- asdat %>%
             filter(municipality == input$municipalityInput)

           minm <- min(na.omit(md$total.assessment))
           print(minm)

           maxm <- max(na.omit(md$total.assessment))
           print(maxm)
         }
         
         if(input$municipalityInput == '-' || 
            input$taxclassInput == '-'){
           return(paste("Complete user inputs."))
         }

         return(paste("Predicted next assessment value - \n",
                      estimates(), "\n Valid prediction range for this municipality is",
                      minm, "-", maxm, sep = " "))  # RETURN PREDICTION
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
        
        else{
          return(paste(""))
        }
      }
      
      else{
          if(input$typeInput == 'Mill Rate'){
            return(paste("Class", input$taxclassInput, 
                         "mill rate for", 
                         input$municipalityInput, sep = " ")) 
          }
        
          else{
            if(input$typeInput == 'Select'){
              return(paste("Select prediction type."))
            }
            else{
              return(paste("Assessment Value prediction for class", 
                           input$taxclassInput, "property \n in",
                           input$municipalityInput, sep = " "))
            }
          }
        }
      }
  })
      
  
  output$resultsText <- renderText({     
    titles()
  })
}