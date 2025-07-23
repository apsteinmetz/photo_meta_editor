# install.packages(c("shiny", "shinydashboard", "DT", "leaflet", 
#                    "exifr", "magick", "shinyFiles", "shinyWidgets", 
#                    "htmltools", "dplyr", "lubridate", "jsonlite"))
# You'll also need to install exiftool on your system for metadata writing:
# 
# Windows: Download from https://exiftool.org/
# macOS: brew install exiftool
# Linux: sudo apt-get install libimage-exiftool-perl
# Claude Prompt: You a Shiny for R developer. Write a shiny app to manage photo
# metadata.  the app should load a photo, preview it, offer the ability to
# change the file name, the metadata concerning creation date, location and
# additional text descriptions.  The location should be selectable via clicking
# on a map or typing a place name.  Include a checkbox to select "Date is
# Approximate."  If checked, add "Date is approximate" to the text metadata. By
# default the first photo in a folder should be loaded and the user should have
# the ability to save and automatically load the next photo in the folder.

# Required libraries
# Required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(leaflet)
library(exifr)
library(magick)
library(shinyFiles)
library(shinyWidgets)
library(htmltools)
library(dplyr)
library(lubridate)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Photo Metadata Manager"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Photo Manager", tabName = "photos", icon = icon("camera"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .photo-preview {
          max-width: 100%;
          max-height: 400px;
          border: 2px solid #ddd;
          border-radius: 8px;
        }
        .metadata-panel {
          background: white;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
        }
        .navigation-buttons {
          margin: 10px 0;
        }
        .map-container {
          height: 300px;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "photos",
              fluidRow(
                # Photo selection and navigation
                box(
                  title = "Photo Selection", status = "primary", solidHeader = TRUE,
                  width = 12,
                  fluidRow(
                    column(4,
                           shinyDirButton("selectDir", "Select Photo Folder", "Choose folder containing photos")
                    ),
                    column(8,
                           verbatimTextOutput("currentFolder")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(12,
                           div(class = "navigation-buttons",
                               actionButton("prevPhoto", "Previous", icon = icon("arrow-left")),
                               actionButton("nextPhoto", "Next", icon = icon("arrow-right")),
                               actionButton("saveMetadata", "Save & Next", icon = icon("save"), class = "btn-success"),
                               span(style = "margin-left: 20px;", textOutput("photoCounter", inline = TRUE))
                           )
                    )
                  )
                )
              ),
              
              fluidRow(
                # Photo preview
                column(6,
                       box(
                         title = "Photo Preview", status = "info", solidHeader = TRUE,
                         width = NULL, height = "500px",
                         div(
                           style = "text-align: center; padding: 20px;",
                           imageOutput("photoPreview", height = "400px")
                         )
                       )
                ),
                
                # Metadata editing
                column(6,
                       box(
                         title = "Metadata Editor", status = "warning", solidHeader = TRUE,
                         width = NULL, height = "500px",
                         
                         # File name
                         textInput("fileName", "File Name:", value = ""),
                         
                         # Date inputs
                         fluidRow(
                           column(8,
                                  dateInput("creationDate", "Creation Date:", value = Sys.Date())
                           ),
                           column(4,
                                  br(),
                                  checkboxInput("dateApproximate", "Date is Approximate", value = FALSE)
                           )
                         ),
                         
                         # Time input
                         timeInput("creationTime", "Creation Time:", value = "12:00:00"),
                         
                         # Location input methods
                         h4("Location"),
                         tabsetPanel(
                           tabPanel("Search Place",
                                    textInput("locationSearch", "Search Location:", 
                                              placeholder = "Enter city, address, or landmark"),
                                    actionButton("searchLocation", "Search", icon = icon("search"))
                           ),
                           tabPanel("Click on Map",
                                    p("Click on the map to set location")
                           )
                         ),
                         
                         # Current coordinates display
                         fluidRow(
                           column(6,
                                  numericInput("latitude", "Latitude:", value = NULL, step = 0.000001)
                           ),
                           column(6,
                                  numericInput("longitude", "Longitude:", value = NULL, step = 0.000001)
                           )
                         ),
                         
                         # Description
                         textAreaInput("description", "Description:", 
                                       placeholder = "Add any additional notes or descriptions",
                                       rows = 3)
                       )
                )
              ),
              
              fluidRow(
                # Map
                column(12,
                       box(
                         title = "Location Map", status = "success", solidHeader = TRUE,
                         width = NULL,
                         div(class = "map-container",
                             leafletOutput("locationMap", height = "300px")
                         )
                       )
                )
              ),
              
              fluidRow(
                # Current metadata display
                column(12,
                       box(
                         title = "Current Photo Information", status = "info", solidHeader = TRUE,
                         width = NULL,
                         verbatimTextOutput("currentMetadata")
                       )
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    photoFolder = NULL,
    photoFiles = NULL,
    currentPhotoIndex = 1,
    currentPhoto = NULL,
    originalMetadata = NULL
  )
  
  # Directory selection
  shinyDirChoose(input, "selectDir", roots = c(home = "~", root = "/"))
  
  # Observe directory selection
  # Observe directory selection
  observeEvent(input$selectDir, {
    # Check if selectDir is not NULL and is a proper list structure
    if (!is.null(input$selectDir) && 
        !is.atomic(input$selectDir) && 
        !is.null(input$selectDir$path) && 
        length(input$selectDir$path) > 0) {
      
      values$photoFolder <- parseDirPath(c(home = "~", root = "/"), input$selectDir)
      
      # Only proceed if parseDirPath returned a valid path
      if (length(values$photoFolder) > 0 && nchar(values$photoFolder) > 0) {
        # Get photo files
        photo_extensions <- c("jpg", "jpeg", "png", "tiff", "tif", "bmp")
        all_files <- list.files(values$photoFolder, full.names = TRUE)
        values$photoFiles <- all_files[tolower(tools::file_ext(all_files)) %in% photo_extensions]
        
        if (length(values$photoFiles) > 0) {
          values$currentPhotoIndex <- 1
          loadCurrentPhoto()
        } else {
          showNotification("No photo files found in selected folder!", type = "warning")
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # Function to load current photo
  loadCurrentPhoto <- function() {
    if (!is.null(values$photoFiles) && length(values$photoFiles) > 0) {
      current_file <- values$photoFiles[values$currentPhotoIndex]
      values$currentPhoto <- current_file
      
      # Load existing metadata
      tryCatch({
        values$originalMetadata <- read_exif(current_file)
        
        # Update UI with existing metadata
        updateTextInput(session, "fileName", value = basename(current_file))
        
        # Extract date/time if available
        if (!is.null(values$originalMetadata$DateTime)) {
          dt <- ymd_hms(values$originalMetadata$DateTime, quiet = TRUE)
          if (!is.na(dt)) {
            updateDateInput(session, "creationDate", value = as.Date(dt))
            updateTimeInput(session, "creationTime", value = format(dt, "%H:%M:%S"))
          }
        }
        
        # Extract GPS coordinates if available
        if (!is.null(values$originalMetadata$GPSLatitude) && !is.null(values$originalMetadata$GPSLongitude)) {
          updateNumericInput(session, "latitude", value = values$originalMetadata$GPSLatitude)
          updateNumericInput(session, "longitude", value = values$originalMetadata$GPSLongitude)
        } else {
          updateNumericInput(session, "latitude", value = NULL)
          updateNumericInput(session, "longitude", value = NULL)
        }
        
        # Extract description if available
        desc <- values$originalMetadata$ImageDescription %||% ""
        updateTextAreaInput(session, "description", value = desc)
        
        # Check if date is marked as approximate
        if (grepl("Date is approximate", desc, ignore.case = TRUE)) {
          updateCheckboxInput(session, "dateApproximate", value = TRUE)
        } else {
          updateCheckboxInput(session, "dateApproximate", value = FALSE)
        }
        
      }, error = function(e) {
        showNotification(paste("Error reading metadata:", e$message), type = "warning")
        values$originalMetadata <- NULL
        
        # Reset form fields
        updateTextInput(session, "fileName", value = basename(current_file))
        updateNumericInput(session, "latitude", value = NULL)
        updateNumericInput(session, "longitude", value = NULL)
        updateTextAreaInput(session, "description", value = "")
        updateCheckboxInput(session, "dateApproximate", value = FALSE)
      })
    }
  }
  
  # Photo navigation
  observeEvent(input$prevPhoto, {
    if (!is.null(values$photoFiles) && values$currentPhotoIndex > 1) {
      values$currentPhotoIndex <- values$currentPhotoIndex - 1
      loadCurrentPhoto()
    }
  })
  
  observeEvent(input$nextPhoto, {
    if (!is.null(values$photoFiles) && values$currentPhotoIndex < length(values$photoFiles)) {
      values$currentPhotoIndex <- values$currentPhotoIndex + 1
      loadCurrentPhoto()
    }
  })
  
  # Save and next
  observeEvent(input$saveMetadata, {
    saveCurrentMetadata()
    if (!is.null(values$photoFiles) && values$currentPhotoIndex < length(values$photoFiles)) {
      values$currentPhotoIndex <- values$currentPhotoIndex + 1
      loadCurrentPhoto()
    }
  })
  
  # Function to save metadata using exiftool
  saveMetadataWithExiftool <- function(file_path, datetime_string, lat, lon, description) {
    # Build exiftool command
    cmd_args <- c(
      "-overwrite_original",
      paste0('-DateTime="', datetime_string, '"'),
      paste0('-DateTimeOriginal="', datetime_string, '"'),
      paste0('-CreateDate="', datetime_string, '"')
    )
    
    # Add GPS coordinates if provided
    if (!is.null(lat) && !is.null(lon) && !is.na(lat) && !is.na(lon)) {
      cmd_args <- c(cmd_args,
                    paste0('-GPSLatitude=', abs(lat)),
                    paste0('-GPSLongitude=', abs(lon)),
                    ifelse(lat >= 0, '-GPSLatitudeRef=N', '-GPSLatitudeRef=S'),
                    ifelse(lon >= 0, '-GPSLongitudeRef=E', '-GPSLongitudeRef=W')
      )
    }
    
    # Add description if provided
    if (!is.null(description) && nchar(description) > 0) {
      # Escape quotes in description
      clean_description <- gsub('"', '\\"', description)
      cmd_args <- c(cmd_args, paste0('-ImageDescription="', clean_description, '"'))
    }
    
    # Add the file path
    cmd_args <- c(cmd_args, shQuote(file_path))
    
    # Execute exiftool command
    result <- system2("exiftool", args = cmd_args, stdout = TRUE, stderr = TRUE)
    
    return(list(
      success = attr(result, "status") %||% 0 == 0,
      output = result
    ))
  }
  
  # Function to save metadata using magick (fallback)
  saveMetadataWithMagick <- function(file_path, datetime_string, lat, lon, description) {
    tryCatch({
      img <- image_read(file_path)
      
      # Set basic metadata
      img <- image_set_attribute(img, "date:create", datetime_string)
      img <- image_set_attribute(img, "date:modify", datetime_string)
      
      # Set description if provided
      if (!is.null(description) && nchar(description) > 0) {
        img <- image_set_attribute(img, "comment", description)
      }
      
      # Note: magick has limited GPS support, so we'll add GPS info to comment
      if (!is.null(lat) && !is.null(lon) && !is.na(lat) && !is.na(lon)) {
        gps_info <- paste("GPS:", lat, ",", lon)
        current_comment <- image_get_attribute(img, "comment") %||% ""
        new_comment <- if(nchar(current_comment) > 0) {
          paste(current_comment, gps_info, sep = " | ")
        } else {
          gps_info
        }
        img <- image_set_attribute(img, "comment", new_comment)
      }
      
      # Write the image back
      image_write(img, file_path)
      
      return(list(success = TRUE, output = "Metadata saved with magick"))
      
    }, error = function(e) {
      return(list(success = FALSE, output = paste("Magick error:", e$message)))
    })
  }
  
  # Function to save metadata
  # Function to save metadata
  saveCurrentMetadata <- function() {
    if (!is.null(values$currentPhoto)) {
      tryCatch({
        # Prepare metadata
        new_description <- input$description
        if (input$dateApproximate && !grepl("Date is approximate", new_description, ignore.case = TRUE)) {
          new_description <- paste(new_description, "Date is approximate", sep = if(nchar(new_description) > 0) ". " else "")
        }
        
        # Combine date and time
        datetime_string <- paste(input$creationDate, input$creationTime)
        
        # Handle file renaming
        old_path <- values$currentPhoto
        new_filename <- input$fileName
        if (!grepl("\\.", new_filename)) {
          new_filename <- paste0(new_filename, ".", tools::file_ext(old_path))
        }
        new_path <- file.path(dirname(old_path), new_filename)
        
        if (old_path != new_path && file.exists(old_path)) {
          success <- file.rename(old_path, new_path)
          if (success) {
            values$currentPhoto <- new_path
            values$photoFiles[values$currentPhotoIndex] <- new_path
          } else {
            showNotification("Failed to rename file", type = "error")
            return()
          }
        }
        
        # Use exiftool to write metadata
        if (Sys.which("exiftool") != "") {
          cmd_parts <- c("exiftool", "-overwrite_original")
          
          # Set date/time
          cmd_parts <- c(cmd_parts, paste0('-DateTime="', datetime_string, '"'))
          cmd_parts <- c(cmd_parts, paste0('-DateTimeOriginal="', datetime_string, '"'))
          cmd_parts <- c(cmd_parts, paste0('-CreateDate="', datetime_string, '"'))
          
          # Set GPS coordinates if both are provided and valid
          if (!is.null(input$latitude) && !is.null(input$longitude) && 
              !is.na(input$latitude) && !is.na(input$longitude) &&
              is.numeric(input$latitude) && is.numeric(input$longitude)) {
            cmd_parts <- c(cmd_parts, paste0('-GPSLatitude=', input$latitude))
            cmd_parts <- c(cmd_parts, paste0('-GPSLongitude=', input$longitude))
            
            # Set GPS reference based on coordinate signs
            lat_ref <- if(input$latitude >= 0) "N" else "S"
            lon_ref <- if(input$longitude >= 0) "E" else "W"
            cmd_parts <- c(cmd_parts, paste0('-GPSLatitudeRef=', lat_ref))
            cmd_parts <- c(cmd_parts, paste0('-GPSLongitudeRef=', lon_ref))
          }
          
          # Set description
          if (nchar(new_description) > 0) {
            # Escape quotes in description
            escaped_description <- gsub('"', '\\"', new_description)
            cmd_parts <- c(cmd_parts, paste0('-ImageDescription="', escaped_description, '"'))
          }
          
          # Add the file path
          cmd_parts <- c(cmd_parts, shQuote(values$currentPhoto))
          
          # Execute the command
          cmd_string <- paste(cmd_parts, collapse = " ")
          result <- system(cmd_string, intern = TRUE)
          
          # Check if command was successful
          if (length(result) == 0 || !any(grepl("error", result, ignore.case = TRUE))) {
            showNotification("Metadata saved successfully!", type = "message")
          } else {
            showNotification(paste("exiftool warning:", paste(result, collapse = " ")), type = "warning")
          }
          
        } else {
          showNotification("exiftool not found. Please install exiftool to save metadata to files.", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error saving metadata:", e$message), type = "error")
      })
    }
  }
  # Location search
  observeEvent(input$searchLocation, {
    if (nchar(input$locationSearch) > 0) {
      # Simple geocoding using Nominatim (OpenStreetMap)
      tryCatch({
        url <- paste0("https://nominatim.openstreetmap.org/search?format=json&q=", 
                      URLencode(input$locationSearch))
        response <- jsonlite::fromJSON(url)
        
        if (length(response) > 0) {
          lat <- as.numeric(response$lat[1])
          lon <- as.numeric(response$lon[1])
          
          updateNumericInput(session, "latitude", value = lat)
          updateNumericInput(session, "longitude", value = lon)
          
          showNotification("Location found!", type = "success")
        } else {
          showNotification("Location not found. Try a different search term.", type = "warning")
        }
      }, error = function(e) {
        showNotification("Error searching location. Check internet connection.", type = "error")
      })
    }
  })
  
  # Outputs
  output$currentFolder <- renderText({
    if (!is.null(values$photoFolder)) {
      paste("Current folder:", values$photoFolder)
    } else {
      "No folder selected"
    }
  })
  
  output$photoCounter <- renderText({
    if (!is.null(values$photoFiles)) {
      paste("Photo", values$currentPhotoIndex, "of", length(values$photoFiles))
    } else {
      ""
    }
  })
  
  output$photoPreview <- renderImage({
    if (!is.null(values$currentPhoto) && file.exists(values$currentPhoto)) {
      list(src = values$currentPhoto,
           alt = "Photo preview",
           width = "100%",
           height = "400px",
           style = "object-fit: contain;")
    } else {
      list(src = "",
           alt = "No photo selected")
    }
  }, deleteFile = FALSE)
  
  output$locationMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })
  
  # Update map when coordinates change
  observe({
    # Only update map if both coordinates are valid numbers
    if (!is.null(input$latitude) && !is.null(input$longitude) && 
        !is.na(input$latitude) && !is.na(input$longitude) &&
        is.numeric(input$latitude) && is.numeric(input$longitude) &&
        input$latitude >= -90 && input$latitude <= 90 &&
        input$longitude >= -180 && input$longitude <= 180) {
      
      leafletProxy("locationMap") %>%
        clearMarkers() %>%
        addMarkers(lng = input$longitude, lat = input$latitude) %>%
        setView(lng = input$longitude, lat = input$latitude, zoom = 10)
    }
  })
  
  # Handle map clicks
  observeEvent(input$locationMap_click, {
    click <- input$locationMap_click
    if (!is.null(click$lat) && !is.null(click$lng)) {
      updateNumericInput(session, "latitude", value = round(click$lat, 6))
      updateNumericInput(session, "longitude", value = round(click$lng, 6))
    }
  })
  
  output$currentMetadata <- renderText({
    if (!is.null(values$originalMetadata)) {
      metadata_text <- c(
        paste("File:", basename(values$currentPhoto)),
        paste("Original DateTime:", values$originalMetadata$DateTime %||% "Not available"),
        paste("GPS Coordinates:", 
              if (!is.null(values$originalMetadata$GPSLatitude) && !is.null(values$originalMetadata$GPSLongitude)) {
                paste(values$originalMetadata$GPSLatitude, ",", values$originalMetadata$GPSLongitude)
              } else {
                "Not available"
              }),
        paste("Description:", values$originalMetadata$ImageDescription %||% "Not available"),
        paste("Camera:", values$originalMetadata$Make %||% "Unknown", values$originalMetadata$Model %||% "")
      )
      paste(metadata_text, collapse = "\n")
    } else if (!is.null(values$currentPhoto)) {
      paste("File:", basename(values$currentPhoto), "\nNo EXIF metadata available")
    } else {
      "No photo selected"
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)