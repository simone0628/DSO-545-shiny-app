library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(shiny)
library(jsonlite)
library(leaflet)
library(promises)
library(stringr)
library(lubridate)
library(shinycssloaders)
library(geosphere)
library(DT)
library(data.table)
library(shinyjs)

DEBUG = F
## For testing to quickly get data
if(DEBUG){
    start_date = "27/05/2019"
    return_date = "01/06/2019"
    input = data.frame(airport_start = "LAX")
    url = fromJSON(gsub("(\\d)\\/","\\1%2F",sprintf(
        "https://api.skypicker.com/flights?sort=price&v=3&asc=1&fly_from=%s&curr=USD&adults=1&date_from=%s&date_to=%s&return_from=%s&return_to=%s&one_for_city=0&one_per_date=0&max_stopovers=5&limit=300",
        input$airport_start,start_date,start_date,return_date,return_date)))
    flight_info = url$data 
    airport_info = read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/airport_info.csv")
    USmap = leaflet(width=900, height=650) %>% setView(lng=-118.243683,lat=34.052235,5) %>% addTiles() 
}


# Get data about airports
# airport_endpoint = fromJSON("https://prod.greatescape.co/api/flights/airports")
airport_info = read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/airport_info.csv")

# Get airline company data
airline_carriers = read_csv("~/Dropbox/Spring 2019/DSO 545/shiny_app/airline_carriers.csv")
airline_carriers = na.omit(airline_carriers)

# We should probably make this layout look better
ui <- shinyUI(
    fluidPage(
    useShinyjs(),
    tags$head(
        tags$style(HTML("@import url('https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css');"))
    ),
    navbarPage("DSO 545 Final Project",
                   tabPanel("Wanna Get Away?",
                         sidebarPanel(   
                            selectInput("airport_start", "Origin Airport", selected = 'LAX (Los Angeles International)',
                                        choices = sort(unique(airport_info$idname))),
                            selectInput("selectinputid", "Select Holiday:",choices = NULL),
                            dateInput("departureDate", "Select Departure Date:"),
                            dateInput("returnDate", "Select Return Date:"),
                            actionButton("goButton1", "Submit Query")),
               mainPanel(
                   shinyjs::hidden(
                       div(id = "leaflet_output",
                           shinycssloaders::withSpinner(leafletOutput("mymap"))
                       )),
                   # shinycssloaders::withSpinner(leafletOutput("mymap")),
                   dataTableOutput("result")
                   
               )
                   ),
                   tabPanel("Second tab name",
                            sidebarPanel(),
                            mainPanel(
                                plotOutput("secondplot")
                            )
                   )
)
)
)


# ui <- shinyUI(
#     
#     fluidPage(
#         useShinyjs(),
#         tags$head(
#             tags$style(HTML("@import url('https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css');"))
#         ),
#         titlePanel("Wanna Get Away?"),
#         sidebarLayout(
#             sidebarPanel(
#                 helpText("Finals stressing you out? Want to get away from all of your responsibilities? We can help!"),
#                 selectInput("airport_start", "Origin Airport", selected = 'LAX (Los Angeles International)',
#                             choices = sort(unique(airport_info$idname))),
#                 selectInput("selectinputid", "Select Holiday:",choices = NULL),
#                 dateInput("departureDate", "Select Departure Date:"),
#                 dateInput("returnDate", "Select Return Date:"),
#                 actionButton("goButton1", "Submit Query")),
#                 mainPanel(
#                     shinyjs::hidden(
#                     div(id = "leaflet_output",
#                     shinycssloaders::withSpinner(leafletOutput("mymap"))
#                     )),
#                     # shinycssloaders::withSpinner(leafletOutput("mymap")),
#                     dataTableOutput("result"),
#                     plotOutput("secondplot")
#                 )
#         )
#     )
# )

server <- function(input, output,session) {
    
    output$secondplot = renderPlot({
        # Put plot code here so that it returns the ggplot2 object
    })
    
    shinyjs::onclick("goButton1",
                     shinyjs::show(id = "leaflet_output",anim = T))
    
    
    observeEvent(D1(),{
        updateSelectInput(session, "selectinputid", "Select Holiday:",  choices = unique(D1()$name),selected = unique(D1()$name)[1])
    })
    
    # Maybe we should give users the option to either select a holiday OR choose a date themselves DONE
    D1  <- reactive({
        selected_airport = airport_info %>% filter( idname == input$airport_start)
        # Get the possible holidays given the airport from today to 6 months in the future
        today = paste0(as.character(Sys.Date()),"T00:00:00")
        future = paste0(as.character(lubridate::ymd(Sys.Date()) + months(6)),"T00:00:00")
        
        # Get a dataframe of major holidays in the next 6 months
        holidays = fromJSON(sprintf("https://prod.greatescape.co/api/travel/cities/%s/holidays?startDate=%s&endDate=%s",selected_airport$id,today,future))
        
        holidays
    })
    
    observe({
        updateDateInput(session, "departureDate","Select Departure Date:",value = as.character(as.Date(D1()[D1()$name== input$selectinputid,]$date) ))
    })
    observe({
        updateDateInput(session, "returnDate","Select Return Date:",value = as.character(input$departureDate + 5 ))
    })
    
    # start_date = eventReactive(input$goButton1,{
    #     (D1()[D1()$name== input$selectinputid,]$date)
    # })

    result_val <- reactiveVal()
    observeEvent(input$goButton1,{
        shinyjs::show("mymap")
        result_val(NULL)
        start_date = format(as.Date(input$departureDate),'%d/%m/%Y')
        return_date = format(as.Date(input$returnDate),'%d/%m/%Y')
        airport_start = airport_info[airport_info$idname == input$airport_start,]$id
        
        # Promises are kind of cool but I don't know if we really need it now that we are using a normal API and not a websocket
        dat = promise(function(resolve,reject){
            #date is day/month/year
            # Limiting to 100 flights because otherwise the map is just too busy
            url = fromJSON(gsub("(\\d)\\/","\\1%2F",sprintf(
"https://api.skypicker.com/flights?sort=price&v=3&asc=1&fly_from=%s&curr=USD&adults=1&date_from=%s&date_to=%s&return_from=%s&return_to=%s&one_for_city=0&one_per_date=0&max_stopovers=5&limit=300",
airport_start,start_date,start_date,return_date,return_date)))
            resolve(url$data)
        })
        # Here we pass on the value of the promise to result_val
        dat %...>% result_val()
     
    
    output$mymap = req(renderLeaflet({
        # This means that it won't try to do anything until it gets the result_val
        flight_info = req(result_val())
        flight_info = flight_info %>% select(-one_of(c("baglimit","bags_price","duration","conversion","countryTo", "countryFrom")))
        airport_start = airport_info[airport_info$idname == input$airport_start,]$id
        
        # This is how we bin the flights by price. Since there can be several flights to the same location we just average those prices
        getColor <- function(flight_info) {
            flight = flight_info %>% group_by(flyTo) %>% summarize(price=mean(price)) 
            quants = flight %>% do(data.frame(t(quantile(.$price, probs = c(0.25,0.5,0.75)))))
            flight$color = sapply(flight$price, function(price) {
                if(price <= quants$X25.) {
                    "green"
                } else if(price > quants$X25. & price <= quants$X75.) {
                    "orange"
                } else if(price > quants$X75.) {
                    "red"
                } })
            return(flight)
        }
        
        
        flight_color = getColor(flight_info)
        # Just takes the output of our color function and returns a list of icons
        icons <- awesomeIcons(
            icon = 'ios-close',
            iconColor = 'black',
            library = 'ion',
            markerColor = "green"
        )
        
        # Get unique destinations
        destinations = unique(flight_info$flyTo)
        
        # starting coordinates
        lat_start = airport_info[airport_info$id == airport_start,]$lat
        lon_start = airport_info[airport_info$id == airport_start,]$lon
        
        # Create the map here and we will center it at the lat and lon of the starting coordinates
        temp_map = leaflet() %>% setView(lng=lon_start,lat=lat_start,5) %>% addTiles() 
        
        lat_end = sapply(destinations,function(x){ airport_info[airport_info$id == x,]$lat})
        lon_end = sapply(destinations,function(x){ airport_info[airport_info$id == x,]$lon})
        
        # This is a super ugly way to get the routes from each row. If you want to come up with a better way, look at the flight_info dataframe
        # You can just run the code in the promise wrapper and then do flight_info = url$data
        marker_icon = icons
        for(i in 1:length(destinations)){
            # Marker and line color
            marker_icon$markerColor = flight_color[flight_color$flyTo == destinations[i],]$color
            
            # Add transfers
            each_flight = flight_info %>% filter(flyTo==destinations[i])
            transfer_df = suppressWarnings(bind_rows(lapply(1:nrow(each_flight),function(j){
                if(nrow(each_flight[j,]$route[[1]]) > 2){
                    # Has transfers; get latTo lngTo cityTo, flyTo
                    dat = each_flight[j,]$route[[1]]
                    dat = dat[1:(nrow(dat)/2),]
                    data.frame(lat=dat$latTo,lon=dat$lngTo,id=dat$flyTo)
                }
            })))
            if(nrow(transfer_df) > 0){
                full_flight = suppressWarnings(bind_rows(data.frame(lat=lat_start,lon=lon_start,id = airport_start),transfer_df))
            }else{
                end_df = data.frame(lat=as.numeric(lat_end[i]),lon=as.numeric(lon_end[i]),id = destinations[i])
                full_flight = suppressWarnings(bind_rows(data.frame(lat=lat_start,lon=lon_start,id = airport_start),end_df) )
            }
            
            
            avg_price =  flight_info %>% filter(flyTo == full_flight$id[nrow(full_flight)]) %>% select(price) %>% summarise(price = mean(price))
            # String to print out when user clicks on line
            detail = paste('Final Destination: ', unique(flight_info[flight_info$flyTo == full_flight$id[nrow(full_flight)],]$cityTo), "<br>" ,
                           'Airport Code: ', full_flight$id[nrow(full_flight)], "<br>",
                           'Average Price = ', "$",round(avg_price$price,2),
                           sep = '')
            # Another hacky way to only show the markers on the last stop if the flight has transfers
            for(k in 1:(nrow(full_flight)-1)){
                
                # create great circle lines
                inter <- gcIntermediate(c(full_flight$lon[k],full_flight$lat[k]), c(full_flight$lon[k+1], full_flight$lat[k+1]), n=50, addStartEnd=TRUE)
                
                # add lines with leaflet
                temp_map = temp_map %>% addPolylines(data = inter, weight = 2, color = marker_icon$markerColor, opacity = 0.7,fillOpacity = 0.5 ,popup = detail,layerId = full_flight$id[nrow(full_flight)]) 
            }
            # Add the markers
            temp_map = temp_map %>% addAwesomeMarkers(lng=full_flight$lon[nrow(full_flight)],lat=full_flight$lat[nrow(full_flight)],popup=detail,icon = marker_icon,layerId = full_flight$id[nrow(full_flight)])
        }
        # Render the map
        temp_map
    }))
    })
    
    # We want to observe when people click on the markers and display a table with the different flights available for that particular location
    observeEvent(input$mymap_marker_click, { 
        p <- input$mymap_marker_click$id
        flight_info = result_val()
        flight_info = flight_info %>% select(-one_of(c("baglimit","bags_price","duration","conversion","countryTo", "countryFrom")))
        # Spread airlines column, change values to the names and then gather back
        flight_data = flight_info %>% filter(flyTo == p) %>% select(flyTo,dTime,aTime,airlines,fly_duration,price,route)
        for(i in 1:nrow(flight_data)){
            airline_vals = unlist(flight_data[i,]$airlines)
            airline_vals = airline_carriers$name[airline_carriers$id %in% airline_vals]
            flight_data[i,]$airlines = list(airline_vals)
        }
        
        flight_data = flight_data %>% 
            distinct(flyTo,dTime,aTime,.keep_all = T) %>%
            mutate(dTime = as.character(as_datetime(dTime,tz="America/Los_Angeles")), aTime = as.character(as_datetime(aTime,tz="America/Los_Angeles")))%>%
            select(Destination=flyTo,`Departure Time`= dTime,`Arrival Time` = aTime, `Airlines` = airlines, `Flight Duration` = fly_duration, `Price`=price, route = route)
        
        for(i in 1:nrow(flight_data)){
            flight_data$route[[i]] = flight_data$route[[i]] %>% select(cityFrom,flyFrom,cityTo,flyTo,aTimeUTC,dTimeUTC,operating_carrier,return) %>%
                mutate(dTimeUTC = as.character(as_datetime(dTimeUTC,tz="America/Los_Angeles")), aTimeUTC= as.character(as_datetime(aTimeUTC,tz="America/Los_Angeles")) ) %>%
                select(`Departing City` = cityFrom, `Departing Airport Code` = flyFrom, `Departure Time` = dTimeUTC , `Arrival City`= cityTo,
                       `Arrival Airport Code`= flyTo, `Arrival Time` = aTimeUTC , `Return Flight?`= return)
        }
        # Change the column names
        output$result <- renderDataTable({
            DT::datatable(
                data = data.table(flight_data),
                rownames = FALSE,
                escape = FALSE,
                extensions = c('Scroller'),
                options = list(
                    server = TRUE,
                    searching = FALSE,
                    order = list(list(4 , 'desc')),
                    dom = 'tfrilp',
                    autoWidth = TRUE,
                    stripeClasses = list(),
                    deferRender = TRUE,
                    scrollX = TRUE,
                    lengthChange = FALSE,
                    scrollY = "51vh",
                    scroller = TRUE,
                    scollCollapse = TRUE,
                    columnDefs = list(
                        list(visible = FALSE, targets = c(-1,6 )),
                        list(orderable = FALSE, className = 'details-control',  targets = 0, render = JS("function(data, type, full){ return '<i class=\"fa fa-plus\" aria-hidden=\"true\">' + data + '</i>' }") ),
                        list(targets = 5, render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }")),
                        list(targets = 7, render = JS("function(data, type, full){ return '<span class=sparkSamples>' + data + '</span>' }"))          
                    )
                ),
                callback = JS("
                              table.column(1).nodes().to$().css({cursor: 'pointer'})
                              
                              // Generate the child table layout
                              var generateChildTableLayout = function(parentRowData) {
                              if (parentRowData != null){
                              var result = ('<table id=\"' + parentRowData[0] + '\"><thead><tr>')
                              for (var col in parentRowData[parentRowData.length - 1][0]){
                              result += '<th>' + col + '</th>'
                              }
                              result += '</tr></thead></table>'
                              return result
                              } else {
                              return ''
                              }
                              }
                              
                              var converter = function(x) {
                              var res = [];
                              for (var i = 0; i < x[Object.keys(x)[0]].length; ++i) res.push({});
                              for (var key in x) {
                              var elem = x[key];
                              for (var i = 0; i < elem.length; ++ i) {
                              res[i][key] = elem[i];
                              }
                              }
                              return res;
                              }  
                              
                              var cols = function(x) {
                              var res = [];
                              for (var key in x) {
                              res.push({'data': key,'title': key});
                              }
                              return res;
                              }
                              
                              // Generate the child table content
                              var generateChildTableContent = function(parentRowData) {
                              var childRowData = converter(parentRowData[parentRowData.length - 1]);
                              var childColumns = cols(parentRowData[parentRowData.length - 1]);
                                console.log(childRowData)
console.log(childColumns)
                              var subtable = $('table#' + parentRowData[0]).DataTable({
                              'data': childRowData,
                              'columns': childColumns,
                              'autoWidth': false,
                              'deferRender': true,
                              'class': 'stripe',
                              'info': true,
                              'lengthChange': false,
                              'ordering': false,
                              'paging': false,
                              'scrollX': true,
                              'scrollY': true,
                              'searching': false
                              }).draw()
                              }
                              
                              table.on('click', 'td.details-control', function() {
                              var table = $(this).closest('table')
                              var tr = $(this).closest('tr');
                              var td = $(this)
                              var tdi = tr.find('i.fa').first();
                              var row = $(table).DataTable().row(td.closest('tr'))
                              if (row.child.isShown()) {
                              row.child.hide()
                              tdi.removeClass('fa-minus');
                              tdi.addClass('fa-plus');
                              tdi.innerText = ' ';
                              } else {
                              var parentRowData = row.data();
                              row.child(generateChildTableLayout(parentRowData)).show()
                              generateChildTableContent(parentRowData)
                              tdi.removeClass('fa-plus');
                              tdi.addClass('fa-minus');
                              tdi.innerText = ' ';
                              }
                              })
                              ")      ) 
        }, server = FALSE)
        })

    
}  

    
shinyApp(ui,server)
