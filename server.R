library(shiny)
library(ggplot2)
library(xtable)
load("votepct.rdata")
load("pastResults.rdata")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$stateTitle <- renderText({
    if(input$state == "All") {
      print("National Statistics")
    } else {
      print(input$state)
    }
  })
 
  output$demog <- renderPlot({
    if(input$type == "General Population") {
      
      ggplot(subset(gen, Abbreviation == input$state), aes(population, percent2)) +
        geom_bar(stat = "identity", fill = c("#ffcc66")) +
        geom_text(aes(label = paste0(percent2, "%")), nudge_y = 3) +
        scale_x_discrete(labels = c("Men", "Women", "White", "Nonwhite", "Black", "Hispanic")) +
        ylim(c(0,100)) +
        ggtitle("General Population Percent By Demographics, 2012") +
        xlab(NULL) +
        ylab("Percent")
      
    } else if(input$type == "Registered Voters") {
      
      ggplot(subset(rv, Abbreviation == input$state), aes(population, percent2)) +
        geom_bar(stat = "identity", fill = c("#00cccc")) +
        geom_text(aes(label = paste0(percent2, "%")), nudge_y = 3) +
        scale_x_discrete(labels = c("Men", "Women", "White", "Nonwhite", "Black", "Hispanic")) +
        ylim(c(0,100)) +
        ggtitle("Percent of Registered Voter Population By Demographics, 2012") +
        xlab(NULL) +
        ylab("Percent")
      
    } else if(input$type == "Actual Voter Turnout") {
      
      ggplot(subset(vote, Abbreviation == input$state), aes(population, percent2)) +
        geom_bar(stat = "identity", fill = c("#6600cc")) +
        geom_text(aes(label = paste0(percent2, "%")), nudge_y = 2) +
        scale_x_discrete(labels = c("Men", "Women", "White", "Nonwhite", "Black", "Hispanic")) +
        ylim(c(0,100)) +
        ggtitle("Percent of 2012 Voters By Demographics") +
        xlab(NULL) +
        ylab("Percent")
      
    }

  })
  
  output$results <- renderPlot({
    ggplot(subset(res, State == input$state), aes(year, percent)) +
      geom_point(stat = "identity", aes(colour = party)) +
      geom_line(aes(group = party, colour = party)) +
      scale_colour_manual(values = c("#0066cc", "#008000", "#cc0000")) +
      ggtitle("Presidential Candidate Votes By Year")
  })
  
  output$turnout <- renderTable({
    resx <- subset(res2, State == input$state)
    my.vars = c("year", "Total.Votes")
    resx <- resx[my.vars]
    rownames(resx) <- 1:nrow(resx)
    colnames(resx) <- c("Year", "Total Voter Turnout")
    xtable(resx)
  }, digits = 0)
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(subset(res, State == input$state), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Candidate: </b>", point$choice, "<br/>",
                    "<b> Year: </b>", point$year, "<br/>",
                    "<b> Percent of Vote Received: </b>", round(point$percent, digits = 1), "%")))
    )
  })
  
})
