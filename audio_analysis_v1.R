library(shiny)
library(tuneR)
library(seewave)
library(xts)

#Main template: 1) UI, SERVER, SHINYAPP objects
#Wrapper to reference HTML, build bin, 
#knit server/ui components together etc....
#"Use this template until you have a reason not to.."

#Type ?func to get help in R

#Think in terms of inputs and outputs
#Inputs: users use to change things
#Outputs: reactions on the page

ui <- fluidPage(
  #*Input() functions,
  #*Output() functions
  
  #INPUT
  #Tiny uses functions to reference html objects, 
  #and the html objects reference css like normal
  #sliderInput() for example
  # sliderInput(
  #   inputId = "example",
  #   label = "Example Slider",
  #   value = 5,
  #   min = 1,
  #   max = 5
  # ),
  
  plotOutput("hist")
  
  #OUTPUT
  #Dedicated functions to plot images, 
  #html, data tables, ui, text, etc...
)

server <- function(input, output) {
  #3 RULES to writing server functions
  #  1) Save output object to output$element
  #     Notice the render function below
  
  output$hist <- renderPlot({
    
    #Assign object of class Wave
    audioData <- readWave('laser_jet_sounds.wav')
    str(audioData)
    
    #My options for wave parameters to work with and plot
    stLeft <- audioData@left
    stRight <- audioData@right
    sampleRate <- audioData@samp.rate
    bitDepth <- audioData@bit
    pulseCodeMod <- audioData
    
    #Refer Vector length
    vLength = length(stLeft)
    # cat(vLength)
    
    #Refer duration of file
    duration <- vLength / sampleRate
    cat("Audio file duration:", vLength, "\n")
    
    #Create an array of time points
    timeArray <- (0:(vLength-1)) / audioData@samp.rate
    
    #readWave() reads wav files as integer types
    #range -2^15 -> 2^15
    #convert to floating point values range -1 -> 1
    stLeft <- stLeft / 2^(audioData@bit -1)
    
    #Scale it to ms
    timeArray <- timeArray * 1000
    # cat(timeArray)
    
    # data(stLeft)
    #White palette in the background
    w <- colorRampPalette(c("white","white"))
    
    #Check Array Values
    cat("FIRST element of timeArray: ")
    cat(first(timeArray))
    cat("\nLAST element of timeArray: ")
    cat(last(timeArray), "\n")
    
    # selection of input audioData var and params 
    stLeftMod <- cutw(stLeft, 
                      f=22050, 
                      from=0.5,
                      to=0.9)
    
    # spectro with contour related arguments
    spectro(stLeftMod, 
            f=22050, 
            ovlp=95, 
            palette=w, 
            cont=TRUE, 
            colcont=temp.colors(8), 
            contlevels=seq(-40,0,5), 
            scale=FALSE)
    title(main="A contour plot (iso-5dB levels)")
  })
}

shinyApp(ui = ui, server = server)