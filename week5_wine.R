library(shiny)
library(ggplot2)
library(shinyWidgets)
library(dplyr)

dataset <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", sep=";")


# Define the UI
ui <- fluidPage(
  titlePanel("Choose your options:"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_type", "Select plot type:", choices = c("Scatter", "Pairwise", "geom_bin2d", "geom_hex", "geom_count", "aesthetic", "stacked bar chart", "side-by-side bar chart", "filled bar chart")),
      # put panel based on plot_type
      conditionalPanel(
        condition = "input.plot_type == 'Pairwise'",
        pickerInput(
          inputId = "selected_attributes",
          label = "Select attributes:",
          choices = names(dataset),
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      ),
      conditionalPanel(
        condition = "input.plot_type == 'Scatter' || input.plot_type == 'geom_bin2d' || input.plot_type == 'geom_hex' || input.plot_type == 'geom_count'||input.plot_type == 'aesthetic'",
        selectInput("x_attr", "Select x-axis attribute:", names(dataset)),
        selectInput("y_attr", "Select y-axis attribute:", names(dataset))
      ),
      conditionalPanel(
        condition = "input.plot_type == 'aesthetic'",
        selectInput("factor", "Select factor attribute:", names(dataset))
      )
    ),
    
    mainPanel(
      plotOutput("scatterplot",height = "400px", width = "600px")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Render the scatterplot
  output$scatterplot <- renderPlot({
    if (input$plot_type == "Scatter") {
      # Scatterplot based on x and y attributes: E3
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(dataset, aes_string(x = x_attr, y = y_attr)) +
        geom_point()+geom_smooth()
    } else if (input$plot_type == "Pairwise") {
      # pair-wise plot based on selected attributes: E1&E2
      selected_attrs <- input$selected_attributes
      if (length(selected_attrs) < 2) {
        return(NULL)
      }
      pairs(dataset[, selected_attrs])
    }else if (input$plot_type == "geom_bin2d"){
      # Two dimensional binning (geom_bin2d): E3
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(dataset, aes_string(x=x_attr, y=y_attr) ) +
        geom_bin2d()
    }else if (input$plot_type == "geom_hex"){
      # Two dimensional binning (geom_hex): E4
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(dataset, aes_string(x=x_attr, y=y_attr) ) +
        geom_hex()
    }
    else if (input$plot_type == "geom_count"){
      # geom_count(): E5
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      ggplot(data = dataset, aes_string(x = x_attr, y=y_attr)) +
        geom_count(alpha=0.25)
    }else if (input$plot_type == "aesthetic"){
      #E6&E7&E8, please uncomment to see different plots (for shape, for color, for wrap)
      x_attr <- input$x_attr
      y_attr <- input$y_attr
      factor_attr <- factor(dataset[, input$factor])
      
      # for shape
      #factor_shapes <- c(16, 17, 18)
      #ggplot(data = dataset) + geom_point(aes_string(x = x_attr, y = y_attr, shape = factor_attr), size = 2.0)
      
      # for color
      #ggplot(data=dataset, mapping = aes_string(x=x_attr, y=y_attr)) + geom_point(mapping = aes_string(colour = factor_attr), size=2.0)
      
      # wrap
      ggplot(data=dataset, mapping = aes_string(x=x_attr, y=y_attr)) + geom_point(mapping = aes_string(colour = factor_attr), size=2.0)+facet_wrap(~quality, scales = "free") 
      
    }else if(input$plot_type == "stacked bar chart"){
      a <-dataset
      a$density_new <- cut(a$density, breaks=c(seq(from = min(a$density), to = max(a$density), length.out = 5)))
      ggplot(data = a, aes(x = density_new, fill = factor(quality))) +
        geom_bar() +
        labs(x = "density_new", y = "count", fill = "quality") +
        theme_minimal()
    }else if(input$plot_type =="side-by-side bar chart"){
      a <-dataset
      a$density_new <- cut(a$density, breaks=c(seq(from = min(a$density), to = max(a$density), length.out = 5)))
      ggplot(a, aes(x = density_new, fill = factor(quality))) + 
        geom_bar(position = "dodge", stat = "count") +
        labs(x = "density_new",
             y = "count",
             fill = "quality")
    }
    else if(input$plot_type=='filled bar chart'){
      a <-dataset
      a$density_new <- cut(a$density, breaks=c(seq(from = min(a$density), to = max(a$density), length.out = 5)))
      a_percent <- a %>%
        group_by(density_new, quality) %>%
        summarise(Count = n()) %>%
        group_by(density_new) %>%
        mutate(Percentage = Count / sum(Count))
      
      # Create the filled bar chart with colors representing quality levels
      ggplot(a_percent, aes(x = density_new, y = Percentage, fill = factor(quality))) + 
        geom_bar(stat = "identity") +
        labs(
          x = "Density",
          y = "Percentage",
          fill = "Quality") +
        coord_cartesian(ylim = c(0, 1))  # Set y-axis limit from 0 to 1
    }
  })
}

# Run the app
shinyApp(ui, server)
