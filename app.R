

library(shiny)
source("/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/ShinySpotify.R")





# Define UI for application that draws a histogram
ui <- fluidPage(
            includeCSS("/Users/kevinhutchins/Desktop/Stat433/ProjectFinal/app.css"),
           
            h1(id="title", "Spotify Song Generator"),
            mainPanel(
                conditionalPanel(condition = "output.renderId!=0", 
                    verbatimTextOutput(invisible("renderId"), placeholder = FALSE),
                    p("Input a playlist name, and the username of the creator of the playlist and this application will generate a song that matches that playlist. Generation is based on statistical information from the 1,000,000 playlists dataset."),
                    textInput(inputId = "inputUserName", "User Name", placeholder = NULL),
                    textInput(inputId = "inputPlaylistName", "Playlist Name", placeholder = NULL),
                    actionButton(inputId = "inputInfo", "Generate Song"),
                    textOutput("text")
                ),
                conditionalPanel(condition = "output.renderId==0", 
                         
                )
            )
)
#https://shiny.rstudio.com/tutorial/written-tutorial/lesson4/       --handle Output\removing text
#https://shiny.rstudio.com/articles/action-buttons.html             --handle buttons 
# Define server logic required to draw a histogram
server <- function(input, output) {
    v <- reactiveValues(data = NULL)
    observeEvent(input$inputInfo, {
        print("ppppp")
        #check to see if valid playlist
        print(input$inputUserName)
        print(input$inputPlaylistName)
        temp = retreivePlaylist(input$inputUserName, input$inputPlaylistName)
        print("AHWIO")
        valid = temp[1]
        if(valid){
            output$text <- renderText({ "Analyzing playlist, please wait" })
            print("ppppp BRUh")
            num = temp[2]
            #run playlist Function
            vector = convertPlaylistToVector(num)
            songs = predictSong(vector)
    
            output$renderId<-reactive({0})
            
            #throw songs 
            #output$songName<-reactive({songs????})
            #output$ArtistName<-reactive({songs????})
            
        }else{
            output$badPlaylist<-reactive({0})
            output$text <- renderText({ "Sorry, but we can't seem to find that playlist. Please try again." })
        }
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
