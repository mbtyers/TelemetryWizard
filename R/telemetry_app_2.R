# if(!require(shiny)) install.packages("shiny", dependencies=T)
# if(!require(leaflet)) install.packages("leaflet", dependencies=T)

library(shiny)
library(leaflet)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Magical telemetry data wizard"),
  
  # Sidebar layout with input and output definitions ----
  tabsetPanel(
    tabPanel("Data Input",
             h6("For an example dataset, copy-paste the following link when using the file selection window:"),
             h6("https://raw.githubusercontent.com/mbtyers/TelemetryWizard/main/ExampleData/7-17-22%20WING.TXT"),
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Select a file ----
                 fileInput("file1", "Choose .txt or .csv File",
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 radioButtons("delim","Data is delimited by",choices=c("whitespace","comma","semicolon"), selected="whitespace"),
                 textInput("firstline", "First line # of data (not including header)", value=NA),
                 textInput("lastline", "Last line # of data (if file contains extra lines)", value=NA),
                 textInput("headline", "Line # of header row (if present)", value=NA),
                 textInput("newhead", "Edit header row if needed, with column names separated by spaces", value=NA),
                 hr(),
                 textInput("latcol", "Column # for Latitude", value=NA),
                 textInput("loncol", "Column # for Longitude", value=NA),
                 textInput("sigcol", "Column # for Signal Strength", value=NA),
                 textInput("idcols", "Combination of column(s) giving a unique fish ID, separated by commas if needed: e.g. 6,7,8", value=NA),
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 # plotOutput("theplot", height="600px"),
                 # leafletOutput("theleaf", height="600px"),
                 uiOutput("theleafui"),
                 h4("First rows of data table:"),
                 tableOutput("astable"),
                 # dataTableOutput("astable"),
                 h4("..."),
                 h4("Last rows of data table:"),
                 tableOutput("astabletail"),
                 h4("Input as raw text:"),
                 verbatimTextOutput("astext")
               )
             )
    ),
    
    tabPanel("Point Selection",
             sidebarLayout(
               sidebarPanel(
                 h4("Navigate between fish"),
                 actionButton("prevfish","<= Prev fish"),
                 actionButton("nextfish","Next fish =>"),
                 checkboxInput("reviewed","Fish Reviewed?",value=F),
                 hr(),
                 radioButtons("zoomlevel","Zoom level",choices=c("All","Individual"),selected="All"),
                 # checkboxInput("crop","Crop plot", value=F),
                 # sliderInput("zoomslide","Zoom?",min=1,max=20,value=1,step=.1),
                 # sliderInput("leftright", "<= left --- right =>",min=-1,max=1,value=0,step=.02),
                 # sliderInput("downup", "<= down --- up =>",min=-1,max=1,value=0,step=.02),
                 hr(),
                 h4("Select row"),
                 actionButton("yes_bestsignal","Select strongest signal"),   
                 actionButton("yes_middle","Select middlemost location"), 
                 hr(),
                 actionButton("prevrow","Select [^] previous row"),       
                 actionButton("nextrow","Select [v] next row"),    
                 hr(),
                 actionButton("clearselection","Clear Selection for this fish"),   
                 # numericInput("whichrow", "Or select row manually", value=NA), 
                 # actionButton("storefish","Store Assigment..."), 
                 hr(),
                 textInput("comment", "Comment for selected row",value=NA),
                 actionButton("storecomment","Store Comment"),
                 hr(),
                 h4("Download data"),
                 textInput("fileID", "File ID (optional)",value=""),
                 downloadButton("allDL","Download All records..."),
                 downloadButton("selDL","Download Selected records...")
               ),
               mainPanel(
                 # plotOutput("fishplot", height="600px", click="theclick", hover="thehover"),
                 leafletOutput("fishleaf", height="600px"),
                 textOutput("saywhichfish"),
                 # textOutput("whichrow"),
                 # textOutput("isreviewed"),
                 tableOutput("fishtable")
               )
             )
    )
  )
)

# Define server logic to read selected file ----
server <- function(input, output, session) {
  
  # library(ggmap)
  # library(tmaptools)
  
  ### Data Input
  options(max.print=1000000)
  
  output$theleafui <- renderUI({
    if(!is.na(as.numeric(input$latcol)) & !is.na(as.numeric(input$loncol))) leafletOutput("theleaf", height="600px")
  })
  
  observeEvent(input$headline, {
    req(input$firstline)
    # theheader <- readLines(input$file1$datapath)[as.numeric(input$headline)]
    delim <- input$delim
    if(delim=="whitespace") sep <- ""
    if(delim=="comma") sep <- ","
    if(delim=="semicolon") sep <- ";"
    theheader1 <- read.table(input$file1$datapath,
                            skip=as.numeric(input$headline)-1, 
                            nrows=1,
                            header=F,
                            fill=T,
                            sep=sep)
    theheader <- paste(theheader1, collapse=" ")
    updateTextInput(inputId="newhead", value=theheader)
  })
  
  tableinput <- reactive({
    req(input$firstline)
    thenrows <- ifelse(is.na(as.numeric(input$lastline)), -1, 1+as.numeric(input$lastline)-as.numeric(input$firstline))
    delim <- input$delim
    if(delim=="whitespace") sep <- ""
    if(delim=="comma") sep <- ","
    if(delim=="semicolon") sep <- ";"
    thetable <- read.table(input$file1$datapath, 
                           skip=as.numeric(input$firstline)-1, 
                           nrows=thenrows,
                           header=F,
                           fill=T,
                           sep=sep)
    if(!is.na(as.numeric(input$headline)) & input$newhead=="") {
      theheader <- read.table(input$file1$datapath,
                              skip=as.numeric(input$headline)-1, 
                              nrows=1,
                              header=F,
                              fill=T,
                              sep=sep)
      colnames(thetable) <- theheader[1,]
    }
    if(input$newhead!="") {
      # if(!is.na(as.numeric(input$newhead))) {
      thenewhead <- strsplit(input$newhead, split=" +")
      # thenewhead <- scan(input$newhead, what="")
      colnames(thetable) <- thenewhead[[1]]
    }
    return(thetable)
  })
  # output$astable <- renderDataTable({
  #   req(input$file1)
  #   xx <- tableinput() #head(tableinput())
  #   colnames(xx) <- paste(1:ncol(xx), colnames(xx))
  #   return(xx)
  # })
  output$astable <- renderTable({
    req(input$file1)
    xx <- head(tableinput())
    colnames(xx) <- paste(1:ncol(xx), colnames(xx))
    return(xx)
  })
  output$astabletail <- renderTable({
    req(input$file1)
    xx <- tail(tableinput())
    colnames(xx) <- paste(1:ncol(xx), colnames(xx))
    return(xx)
  })
  
  output$astext <- renderPrint({
    req(input$file1)
    textinput <- readLines(input$file1$datapath)
    return(textinput)
  })
  
  lat <- reactive(unname(tableinput()[,as.numeric(input$latcol)]))
  lon <- reactive(unname(tableinput()[,as.numeric(input$loncol)]))
  ID <- reactive(apply(as.matrix(tableinput()[,as.numeric(strsplit(input$idcols,split=",")[[1]])]), 
                       1, paste, collapse="_"))
  sig <- reactive({
    if(!is.na(as.numeric(input$sigcol))) xx <- unname(tableinput()[,as.numeric(input$sigcol)])
    if(is.na(as.numeric(input$sigcol))) xx <- 1
    return(xx)
  })
  
  
  
  
  sel <- reactive({
    thevec <- rep(F, length(lat()))
    thevec[whichrowvec()] <- T
    return(thevec)
  })
  
  datamat <- reactive(data.frame(row=seq_along(lat()),sel=sel(),sig=sig(),
                                 lat=lat(),lon=lon(),ID=ID()))
  dataleaf <- reactive(data.frame(row=seq_along(lat()),sig=sig(),
                                 lat=lat(),lon=lon(),ID=ID()))
  
  # bbox1 <- reactive({
  #   # midlat <- (max(lat())+min(lat()))/2
  #   # midlon <- (max(lon())+min(lon()))/2
  #   # minlat <- midlat + (min(lat())-midlat)/input$zoomslide
  #   # maxlat <- midlat + (max(lat())-midlat)/input$zoomslide
  #   # minlon <- midlon + (min(lon())-midlon)/input$zoomslide
  #   # maxlon <- midlon + (max(lon())-midlon)/input$zoomslide
  #   c(min(lon()), min(lat()), max(lon()), max(lat()))
  #   # return(c(minlon, minlat, maxlon, maxlat))
  # })
  # bbox2 <- reactive({
  #   c(min(fishmat()$lon), min(fishmat()$lat), max(fishmat()$lon), max(fishmat()$lat))
  # })
  # map1 <- reactive({
  #   if(input$zoomlevel=="All") themap <- get_stamenmap(bbox=bbox1(), crop=F)
  #   if(input$zoomlevel=="Individual") themap <- get_stamenmap(bbox=bbox2(), crop=input$crop)
  #   return(themap)
  # })
  # 
  # output$theplot <- renderPlot({
  #   req(as.numeric(input$latcol) & as.numeric(input$loncol))
  #   ggmap(map1()) + geom_point(data=datamat(), aes(x=lon, y=lat, color=ID, alpha=sig))
  # })
  thecols <- reactive(adjustcolor(rainbow(length(unique(ID()))), red.f=.7, green.f=.7, blue.f=.7))
  output$theleaf <- renderLeaflet({
    req(as.numeric(input$latcol) & as.numeric(input$loncol))
    leaflet() %>% 
      addTiles() %>%
      # addProviderTiles(providers$Thunderforest.Landscape) %>%
      # setView(lng=mean(fishmatleaf()$lon), lat=mean(fishmatleaf()$lat), zoom=8) %>%
      # if(input$zoomlevel=="All") fitBounds(lng1=min(dataleaf()$lon), lng2=max(dataleaf()$lon), lat1=min(dataleaf()$lat), lat2=max(dataleaf()$lat)) %>%
      # if(input$zoomlevel=="Individual") fitBounds(lng1=min(fishmatleaf()$lon), lng2=max(fishmatleaf()$lon), lat1=min(fishmatleaf()$lat), lat2=max(fishmatleaf()$lat)) %>%
      # fitBounds(lng1=min(fishmatleaf()$lon), lng2=max(fishmatleaf()$lon), lat1=min(fishmatleaf()$lat), lat2=max(fishmatleaf()$lat)) %>%
      fitBounds(lng1=lng1(), lng2=lng2(), lat1=lat1(), lat2=lat2()) %>%
      # setMaxBounds(lng1=min(aa$Longitude), lng2=max(aa$Longitude), lat1=min(aa$Latitiude), lat2=max(aa$Latitiude))
      addCircles(lng=lon(), lat=lat(), color=thecols()[as.numeric(as.factor(ID()))], 
                 fillOpacity=sig()/max(sig()), 
                 opacity=sig()/max(sig()),
                 label=ID()) %>%
      addLegend(position="topright", colors=thecols(), labels=levels(as.factor(ID())), 
                title=paste(length(unique(ID())),"fish")) %>%
      # addMarkers(lng=fishmat()$lon[fishmat()$sel], lat=fishmat()$lat[fishmat()$sel]) %>% # , label=fishmat()$ID[fishmat()$sel]
      addScaleBar(position="bottomright")
  })
  
  
  ### Point selection
  whichfish <- reactiveVal(value=1)
  observeEvent(input$nextfish, {
    whichfish1 <- whichfish()
    if(whichfish1<length(unique(ID()))) whichfish1 <- whichfish1+1
    whichfish(whichfish1)
  })
  observeEvent(input$prevfish, {
    whichfish1 <- whichfish()
    if(whichfish1>1) whichfish1 <- whichfish1-1
    whichfish(whichfish1)
  })
  
  whichrowvec <- reactiveVal(value=NA)
  # observeEvent(input$storefish, {
  #   whichrowvec1 <- whichrowvec()
  #   whichrowvec1[whichfish()] <- input$whichrow
  #   whichrowvec(whichrowvec1)
  # })
  
  reviewedvec <- reactiveVal(value=F)
  observeEvent(input$reviewed, {
    reviewedvec1 <- reviewedvec()
    if(length(reviewedvec1) < length(unique(ID()))) {
      reviewedvec1 <- c(reviewedvec1, rep(F, length(unique(ID()))-length(reviewedvec1)))
    }
    reviewedvec1[whichfish()] <- input$reviewed
    reviewedvec(reviewedvec1)
  })
  observeEvent(whichfish(), updateCheckboxInput(session, "reviewed", value=reviewedvec()[whichfish()]))
  # updateCheckboxInput(session, "reviewed", value=reviewedvec()[whichfish()])
  
  observeEvent(input$yes_bestsignal, {
    whichrowvec1 <- whichrowvec()
    whichrowvec1[whichfish()] <- maxsig()
    whichrowvec(whichrowvec1)
  })
  observeEvent(input$yes_middle, {
    whichrowvec1 <- whichrowvec()
    whichrowvec1[whichfish()] <- mostmiddle()
    whichrowvec(whichrowvec1)
  })
  observeEvent(input$nextrow, {
    whichrowvec1 <- whichrowvec()
    fishrows <- fishmat()$row
    if(is.na(whichrowvec1[whichfish()])) {
      whichrowvec1[whichfish()] <- fishrows[1]
    } else {
      thisrow <- which(fishrows==whichrowvec1[whichfish()])
      if(thisrow<length(fishrows)) {
        whichrowvec1[whichfish()] <- fishrows[thisrow+1]
      } else {
        whichrowvec1[whichfish()] <- fishrows[1]
      }
    }
    whichrowvec(whichrowvec1)
  })
  observeEvent(input$prevrow, {
    whichrowvec1 <- whichrowvec()
    fishrows <- fishmat()$row
    if(is.na(whichrowvec1[whichfish()])) {
      whichrowvec1[whichfish()] <- fishrows[length(fishrows)]
    } else {
      thisrow <- which(fishrows==whichrowvec1[whichfish()])
      if(thisrow>1) {
        whichrowvec1[whichfish()] <- fishrows[thisrow-1]
      } else {
        whichrowvec1[whichfish()] <- fishrows[length(fishrows)]
      }
    }
    whichrowvec(whichrowvec1)
  })
  observeEvent(input$clearselection, {
    whichrowvec1 <- whichrowvec()
    whichrowvec1[whichfish()] <- NA
    whichrowvec(whichrowvec1)
  })
  # comment <- reactiveVal(value=rep(NA, length(lat())))
  comment <- reactiveVal(value="")
  observeEvent(input$storecomment, {
    comment1 <- comment()
    if(length(comment1)<length(lat())) {
      comment1 <- c(comment1, rep("",length(lat())-length(comment1)))
    }
    comment1[ID()==allID()[whichfish()] & sel()==T] <- input$comment  ########## figure this line out  - also prevent assigning if none is selected
    comment(comment1)
  })
  
  output$whichrow <- renderPrint(whichrowvec())
  output$isreviewed <- renderPrint(reviewedvec())
  
  ## whichrowvec will be a vector of length ID, and will take a single value for each fish
  ## comment will be the same???
  
  
  allID <- reactive(sort(unique(ID())))
  
  fishmat <- reactive(datamat()[datamat()$ID==allID()[whichfish()],])
  fishmatleaf <- reactive(dataleaf()[dataleaf()$ID==allID()[whichfish()],])
  maxsig <- reactive(fishmat()$row[which.max(fishmat()$sig)])
  mostmiddle <- reactive({
    fishmat()$row[which.min((4*(fishmat()$lon-mean(fishmat()$lon))^2) + # reasonable at ~60N
                              ((fishmat()$lat-mean(fishmat()$lat))^2))]
  })
  fishmat_tbl <- reactive({
    cbind(SELECTED=ifelse(sel()[datamat()$ID==allID()[whichfish()]],"***********",""),
          DATA_ROW=fishmat()$row, 
          SEL=sel()[datamat()$ID==allID()[whichfish()]],   ### could commment this out
          ID=ID()[datamat()$ID==allID()[whichfish()]], 
          tableinput()[datamat()$ID==allID()[whichfish()],],
          COMMENT=comment()[datamat()$ID==allID()[whichfish()]],  
          SELECTED=ifelse(sel()[datamat()$ID==allID()[whichfish()]],"***********",""))
  })
  
  # xlims <- reactive({
  #   midx <- (min(lon())+max(lon()))/2
  #   dx <- max(lon())-midx
  #   cx <- midx+input$leftright*dx
  #   minx <- cx-2*dx/input$zoomslide
  #   maxx <- cx+2*dx/input$zoomslide
  #   return(c(minx,maxx))
  # })
  # ylims <- reactive({
  #   midy <- (min(lat())+max(lat()))/2
  #   dy <- max(lat())-midy
  #   cy <- midy+input$downup*dy
  #   miny <- cy-2*dy/input$zoomslide
  #   maxy <- cy+2*dy/input$zoomslide
  #   return(c(miny,maxy))
  # })
  
  
  # output$fishplot <- renderPlot({
  #   ggmap(map1(), darken=c(.4,"white")) + 
  #     geom_point(data=fishmat(), aes(x=lon, y=lat, color=sig)) +
  #     scale_color_gradient(low = "grey60", high = "black") +
  #     ggtitle(paste("ID:",allID()[whichfish()], "\n fish", whichfish(), "of", length(allID()))) +
  #     geom_point(data=datamat()[maxsig(),], aes(x=lon, y=lat), shape=4, colour="darkgreen", size=5) +
  #     geom_point(data=datamat()[mostmiddle(),], aes(x=lon, y=lat), shape=4, colour="blue", size=5) +
  #     geom_point(data=fishmat()[fishmat()$sel,], aes(x=lon, y=lat), shape=9, colour="black", size=10) +
  #     scale_x_continuous(limits=xlims()) +#, expand=c(0,0)
  #     scale_y_continuous(limits=ylims())
  #     # coord_cartesian(xlim=xlims(), ylim=ylims())#, clip="off"
  # })
  output$saywhichfish <- renderText(paste("ID:",allID()[whichfish()], "   ---   ", "fish", whichfish(), "of", length(allID()), "\n"))
    
  lng1 <- reactive({
    if(input$zoomlevel=="All") x <- min(dataleaf()$lon)
    if(input$zoomlevel=="Individual") x <- ifelse(nrow(fishmatleaf())==1, min(mean(dataleaf()$lon), fishmatleaf()$lon), min(fishmatleaf()$lon))
    return(x)
  })
  lng2 <- reactive({
    if(input$zoomlevel=="All") x <- max(dataleaf()$lon)
    if(input$zoomlevel=="Individual") x <- ifelse(nrow(fishmatleaf())==1, max(mean(dataleaf()$lon), fishmatleaf()$lon), max(fishmatleaf()$lon))
    return(x)
  })
  lat1 <- reactive({
    if(input$zoomlevel=="All") x <- min(dataleaf()$lat)
    if(input$zoomlevel=="Individual") x <- ifelse(nrow(fishmatleaf())==1, min(mean(dataleaf()$lat), fishmatleaf()$lat), min(fishmatleaf()$lat))
    return(x)
  })
  lat2 <- reactive({
    if(input$zoomlevel=="All") x <- max(dataleaf()$lat)
    if(input$zoomlevel=="Individual") x <- ifelse(nrow(fishmatleaf())==1, max(mean(dataleaf()$lat), fishmatleaf()$lat), max(fishmatleaf()$lat))
    return(x)
  })
  output$fishleaf <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      # addProviderTiles(providers$Thunderforest.Landscape) %>%
      # setView(lng=mean(fishmatleaf()$lon), lat=mean(fishmatleaf()$lat), zoom=8) %>%
      # if(input$zoomlevel=="All") fitBounds(lng1=min(dataleaf()$lon), lng2=max(dataleaf()$lon), lat1=min(dataleaf()$lat), lat2=max(dataleaf()$lat)) %>%
      # if(input$zoomlevel=="Individual") fitBounds(lng1=min(fishmatleaf()$lon), lng2=max(fishmatleaf()$lon), lat1=min(fishmatleaf()$lat), lat2=max(fishmatleaf()$lat)) %>%
      # fitBounds(lng1=min(fishmatleaf()$lon), lng2=max(fishmatleaf()$lon), lat1=min(fishmatleaf()$lat), lat2=max(fishmatleaf()$lat)) %>%
      fitBounds(lng1=lng1(), lng2=lng2(), lat1=lat1(), lat2=lat2()) %>%
      # setMaxBounds(lng1=min(aa$Longitude), lng2=max(aa$Longitude), lat1=min(aa$Latitiude), lat2=max(aa$Latitiude))
      addCircles(lng=fishmatleaf()$lon, lat=fishmatleaf()$lat, color="black", 
                 fillOpacity=fishmatleaf()$sig/max(fishmatleaf()$sig), 
                 opacity=fishmatleaf()$sig/max(fishmatleaf()$sig),
                 label=paste0("row ",fishmatleaf()$row, ", signal ",fishmatleaf()$sig)) %>%
      addMarkers(lng=fishmat()$lon[fishmat()$sel], lat=fishmat()$lat[fishmat()$sel]) %>% # , label=fishmat()$ID[fishmat()$sel]
      addScaleBar(position="bottomright")
  })
  output$fishtable <- renderTable(fishmat_tbl())
  
  # (zoom sliders??)
  
  
  ### data download
  
  DLall <- reactive(cbind(DATA_ROW=datamat()$row, SELECTED=sel(),ID=ID(), tableinput(), COMMENT=comment()))
  DLsel <- reactive(DLall()[DLall()$SELECTED,])
  output$allDL <- downloadHandler(
    filename=function() paste0(input$fileID,"AllRecords_read",Sys.Date(),".csv"),
    content=function(file) write.csv(DLall(), file)
  )
  output$selDL <- downloadHandler(
    filename=function() paste0(input$fileID,"SelectedRecords_read",Sys.Date(),".csv"),
    content=function(file) write.csv(DLsel(), file)
  )
}
# Run the app ----
shinyApp(ui, server)
