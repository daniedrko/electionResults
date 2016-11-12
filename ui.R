library(shiny)
load("votepct.rdata")
load("pastResults.rdata")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  tags$head(
    tags$title("Election Trends"),
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Lato|Roboto+Slab');
                    h1, h2 {
                      font-family: 'Roboto Slab';

                    }
                    body {
                      font-family: 'Lato';
                    }
                    .table.data.table.table-bordered.table-condensed td {
                      text-align: 'center';
                      font-family: 'Lato';
                      padding: 20px;
                    }
                    "))
  ),
  
  # Application title
  headerPanel(HTML('Past Presidential Election Trends')),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("state",
                   "Select a State:",
                   c("National" = "All", "Alabama" = "AL", "Alaska" = "AK", "Arizona" = "AZ", "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC", "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Iowa" = "IA", "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME", "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS", "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY", "North Carolina" = "NC", "North Dakota" = "ND", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Tennesse" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI", "Wyoming" = "WY"),
                   selected = "All"),
       HTML("Select a Population to View the State's demographics in detail in the bar chart below. <br/><br/>"),
       selectInput("type",
                   "Type of Population", 
                   c("General Population", "Registered Voters", "Actual Voter Turnout"),
                   selected = "Actual Voter Turnout"),
      HTML("<p>Pollsters offer subsets of data based on state and national population demographics, but those populations may not be reflective of true populations. These graphs, reflecting 2012 Census voting data, are meant to serve as a quick reference when evaluating a pollster's crosstabulation and methodology.</p> <p>These population percentage graphs were created by Danielle Keeton-Olsen of Talking Points Memo, to be used for <a href='http://polltracker.talkingpointsmemo.com'>PollTracker</a>.</p> <p>Election results pulled from the <a href='http://www.fec.gov/pubrec/electionresults.shtml'>Federal Election Commission</a>; Voter turnout demographic data pulled from <a href='http://www.census.gov/topics/public-sector/voting/data/tables.html'>U.S. Census Voting and Registration</a> database.</p>")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h2(textOutput("stateTitle")),
      fluidRow(
        plotOutput("results",
                   hover = hoverOpts("plot_hover", delay = 90, delayType = "debounce")),
        uiOutput("hover_info")
      ),
      fluidRow(
        column(9, plotOutput("demog")),
        column(3, 
               #add some text here?
               tableOutput("turnout"))
      )
    )
  )
))
