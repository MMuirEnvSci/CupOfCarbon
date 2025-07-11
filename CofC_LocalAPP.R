#Install R packages on first use (remove the # symbol in front of each line). These instructions can be ignored on subsequent uses by replacing the "#" symbols.

#install.packages("shiny")
#install.packages("imager")
#install.packages("shinyFiles")

library(shiny)
library(imager)
library(shinyFiles)

# Define UI
ui <- fluidPage(
  titlePanel(
    title = div(
      h1("Cup of Carbon - Estimation of Dissolved Organic Carbon (DOC) from images"),
      h4("Michael Muir, University of Glasgow, 2025. v1.0")  # Subtitle added here
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("single_image", "Choose a single image file", accept = c('image/png', 'image/jpeg', 'image/jpg')),
      shinyDirButton("folder", "Select Folder", "Choose a folder containing images"),
      p("Instructions:"),
      p("1. Use the file input to select a single image, or select a folder to analyze multiple images."),
      p("2. For each image, click on two regions:"),
      tags$ul(
        tags$li("First click: Select the 'Water' region."),
        tags$li("Second click: Select the 'Paper' region.")
      ),
      p("3. The app automatically proceeds to the next image after analysis."),
      p("4. Use 'Skip to Next Image' to bypass an image manually."),
      p("5. Use 'Redo Last Measurement' to reselect areas for the previous image."),
      p("6. Click 'Download Results as CSV' to save the results table."),
      p("7. Click 'Delete Last Line' to remove the last entry from the results table."),
      p("(Please ensure your computer screen scale is set to 100 % for the region selection to work properly)"),
      actionButton("start_analysis", "Start Folder Analysis"),
      actionButton("next_image", "Skip to Next Image"),
      actionButton("redo_measurement", "Redo Last Measurement"),
      actionButton("delete_last_line", "Delete Last Line"),
      actionButton("reset", "Reset Table"),
      downloadButton("downloadData", "Download Results as CSV"),
      tableOutput("rgb_table")
    ),
    
    mainPanel(
      plotOutput("imagePlot", click = "plot_click")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Allow folder selection
  volumes <- c(Home = fs::path_home(), getVolumes()())
  shinyDirChoose(input, "folder", roots = volumes)
  
  # Reactive variable to store the list of image files
  image_files <- reactiveVal(NULL)
  
  # Reactive variable to track the current image index
  current_image_index <- reactiveVal(0)
  
  # Reactive variable to track the previously analyzed image
  previous_image_index <- reactiveVal(NULL)
  
  # Reactive variable to store the loaded image
  img <- reactiveVal(NULL)
  
  # Reactive variable to store the coordinates of the selected regions
  roi_coords <- reactiveVal(list())
  
  # Reactive variable to store the results
  results <- reactiveVal(data.frame(ImageName = character(), 
                                    Water_R = numeric(), Water_G = numeric(), Water_B = numeric(),
                                    Paper_R = numeric(), Paper_G = numeric(), Paper_B = numeric(),
                                    Estimated_DOC = numeric()))
  
  # Load images from the selected folder
  observeEvent(input$folder, {
    req(input$folder)
    folder_path <- parseDirPath(volumes, input$folder)
    image_files(list.files(folder_path, pattern = "\\.(jpg|jpeg|png)$", full.names = TRUE))
    current_image_index(0)  # Reset index
  })
  
  # Load a single image when selected
  observeEvent(input$single_image, {
    req(input$single_image)
    img(load.image(input$single_image$datapath))
    image_files(NULL)  # Clear folder files to indicate single-image mode
    roi_coords(list())  # Reset coordinates for the single image
  })
  
  # Start folder analysis
  observeEvent(input$start_analysis, {
    req(image_files())
    current_image_index(1)  # Start from the first image
    img(load.image(image_files()[1]))
    roi_coords(list())  # Reset coordinates for the first image
  })
  
  # Skip to the next image
  observeEvent(input$next_image, {
    req(image_files())
    previous_image_index(current_image_index())  # Save the current index as previous
    next_index <- current_image_index() + 1
    if (next_index <= length(image_files())) {
      current_image_index(next_index)
      img(load.image(image_files()[next_index]))
      roi_coords(list())  # Reset coordinates for the next image
    } else {
      showModal(modalDialog(
        title = "No More Images",
        "You have reached the end of the folder. All images have been analyzed.",
        easyClose = TRUE,
        footer = NULL
      ))
      img(NULL)  # Clear the image display
    }
  })
  
  # Automatically proceed to the next image after analysis
  observeEvent(results(), {
    req(image_files(), current_image_index() > 0)
    
    # If analyzing images in a folder and not at the last image
    if (current_image_index() < length(image_files())) {
      next_index <- current_image_index() + 1
      current_image_index(next_index)
      img(load.image(image_files()[next_index]))
      roi_coords(list())  # Reset coordinates for the next image
    }
  }, ignoreInit = TRUE)
  
  # Redo the last measurement
  observeEvent(input$redo_measurement, {
    req(previous_image_index())
    img(load.image(image_files()[previous_image_index()]))
    current_image_index(previous_image_index())  # Set current index to previous
    roi_coords(list())  # Reset coordinates
    showModal(modalDialog(
      title = "Redo Measurement",
      "Please reselect the regions on the previous image.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Render image plot
  output$imagePlot <- renderPlot({
    if (is.null(img())) return(NULL)
    plot(img())
  })
  
  # Handle image clicks to select regions
  observeEvent(input$plot_click, {
    req(img())  # Ensure image is loaded
    click_coords <- input$plot_click
    
    # Access current coordinates
    coords <- roi_coords()
    
    if (length(coords) < 2) {
      # Store click coordinates in the list
      coords[[length(coords) + 1]] <- click_coords
      roi_coords(coords)
      
      # Automatically proceed to extract RGB values when two points are selected
      if (length(coords) == 2) {
        Water_coords <- coords[[1]]  # First region (Water)
        pap_coords <- coords[[2]]  # Second region (Paper)
        
        # Process the image
        cup1 <- img()
        
        # Split the image into channels
        spl <- imsplit(cup1, "c")
        
        patch_size <- 200 # Define the patch size
        
        # Helper function to calculate RGB values
        extract_rgb <- function(image, x, y, width, height) {
          r_mean <- patchstat(image$`c = 1`, "mean", x, y, width, height)
          g_mean <- patchstat(image$`c = 2`, "mean", x, y, width, height)
          b_mean <- patchstat(image$`c = 3`, "mean", x, y, width, height)
          
          # Scale values to 0-255 range
          rgb_values <- as.integer(c(r_mean, g_mean, b_mean) * 255)
          return(rgb_values)
        }
        
        # Extract RGB values for the two selected regions
        Water_rgb <- extract_rgb(spl, Water_coords$x, Water_coords$y, patch_size, patch_size)
        pap_rgb <- extract_rgb(spl, pap_coords$x, pap_coords$y, patch_size, patch_size)
        
        # Get the image name
        image_name <- ifelse(is.null(image_files()),
                             basename(input$single_image$name),
                             basename(image_files()[current_image_index()]))
        
        # Calculate the Estimated DOC using the provided formula
        #estimated_doc <- ((log(Water_rgb[3] + (255 - pap_rgb[3]))) - 5.122) / -0.038 ### Old cal
        #estimated_doc <-exp((((Water_rgb[3] + (255 - pap_rgb[3]))-178.36)/-37.23)) ###B_ws cal data (Figure 3)
        estimated_doc <-exp((((Water_rgb[3] + (255 - pap_rgb[3]))-199.92)/-41.45)) ###All Silver Flowe samples cal (Figure)
        
        # Store the results in a table with image name
        result <- data.frame(
          ImageName = image_name,
          Water_R = Water_rgb[1], Water_G = Water_rgb[2], Water_B = Water_rgb[3],
          Paper_R = pap_rgb[1], Paper_G = pap_rgb[2], Paper_B = pap_rgb[3],
          Estimated_DOC = estimated_doc
        )
        
        # Update the results with the new data
        current_results <- results()
        updated_results <- rbind(current_results, result)
        results(updated_results)
        
        # Save the current index as previous after analysis
        previous_image_index(current_image_index())
      }
    }
  })
  
  # Delete the last line from the results table
  observeEvent(input$delete_last_line, {
    current_results <- results()
    if (nrow(current_results) > 0) {
      updated_results <- current_results[-nrow(current_results), ]
      results(updated_results)
    }
  })
  

  # Reset the results table
  observeEvent(input$reset, {
    results(data.frame(ImageName = character(), 
                       Water_R = numeric(), Water_G = numeric(), Water_B = numeric(),
                       Paper_R = numeric(), Paper_G = numeric(), Paper_B = numeric(),
                       Estimated_DOC = numeric()))  # Clear results
    current_image_index(0)  # Reset index
    previous_image_index(NULL)  # Clear previous index
    roi_coords(list())  # Clear coordinates
  })
  
  # Display the results table
  output$rgb_table <- renderTable({
    results()
  })
  
  # Allow users to download the results as CSV
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("rgb_values_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(results(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
