# Required libraries
library(shiny)
library(bslib)
library(leaflet)
library(exifr)
library(magick)
library(shinyFiles)
library(shinyWidgets)
library(dplyr)
library(lubridate)

# UI
ui <- page_navbar(
  title = "Photo Metadata Manager",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3498db",
    secondary = "#95a5a6",
    success = "#2ecc71",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  
  nav_panel(
    title = "Photo Manager",
    icon = icon("camera"),
    
    page_sidebar(
      sidebar = sidebar(
        title = "Photos",
        width = 150,
        
        # Folder selection
        shinyDirButton("selectDir", "Select Folder", 
                       "Choose folder containing photos",
                       class = "btn-primary w-100 btn-sm"),
        
        br(), br(),
        
        # Current folder display
        card(
          height = "60px",
          card_body(
            padding = "8px",
            verbatimTextOutput("currentFolder")
          )
        ),
        
        br(),
        
        # Navigation controls
        actionButton("prevPhoto", "Previous", 
                     icon = icon("arrow-left"),
                     class = "btn-outline-secondary btn-sm w-100"),
        
        br(),
        
        actionButton("nextPhoto", "Next", 
                     icon = icon("arrow-right"),
                     class = "btn-outline-secondary btn-sm w-100"),
        
        br(),
        
        actionButton("saveMetadata", "Save & Next", 
                     icon = icon("save"),
                     class = "btn-success btn-sm w-100"),
        
        br(), br(),
        
        # Photo counter
        card(
          height = "50px",
          card_body(
            padding = "8px",
            textOutput("photoCounter")
          )
        )
      ),
      
      # Main content area
      # Photo preview, metadata display, and metadata editing row
      layout_columns(
        col_widths = c(5, 3, 4),
        fill = FALSE,
        gap = "10px",
        
        # Photo preview
        card(
          full_screen = FALSE,
          height = "400px",
          card_header("Photo Preview"),
          card_body(
            padding = "8px",
            imageOutput("photoPreview", height = "330px")
          )
        ),
        
        # Current metadata display
        card(
          full_screen = FALSE,
          height = "400px",
          card_header("Current Photo Info"),
          card_body(
            padding = "10px",
            verbatimTextOutput("currentMetadata")
          )
        ),
        
        # Metadata editing
        card(
          full_screen = FALSE,
          height = "400px",
          card_header("Metadata Editor"),
          card_body(
            padding = "10px",
            
            textInput("fileName", "File Name:", value = ""),
            
            layout_columns(
              col_widths = c(8, 4),
              fill = FALSE,
              dateInput("creationDate", "Creation Date:", value = Sys.Date()),
              checkboxInput("dateApproximate", "Approximate", value = FALSE)
            ),
            
            timeInput("creationTime", "Creation Time:", value = "12:00:00"),
            
            h6("Location"),
            navset_card_tab(
              height = "100px",
              nav_panel(
                "Search",
                textInput("locationSearch", "", 
                          placeholder = "Enter location"),
                actionButton("searchLocation", "Search", 
                             icon = icon("search"),
                             class = "btn-outline-primary btn-sm")
              ),
              nav_panel(
                "Map Click",
                p("Click map below", class = "small text-muted")
              )
            ),
            
            layout_columns(
              col_widths = c(6, 6),
              fill = FALSE,
              numericInput("latitude", "Lat:", value = NULL, step = 0.000001),
              numericInput("longitude", "Lng:", value = NULL, step = 0.000001)
            )
          )
        )
      ),
      
      br(),
      
      # Description and Map row
      layout_columns(
        col_widths = c(4, 8),
        fill = FALSE,
        gap = "10px",
        
        # Description card
        card(
          full_screen = FALSE,
          height = "350px",
          card_header("Description"),
          card_body(
            padding = "10px",
            textAreaInput("description", "", 
                          placeholder = "Add any additional notes or descriptions",
                          rows = 12)
          )
        ),
        
        # Map section
        card(
          full_screen = FALSE,
          height = "350px",
          card_header("Location Map"),
          card_body(
            padding = "8px",
            leafletOutput("locationMap", height = "280px")
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
  
  # Function to convert GPS coordinates with proper sign
  convertGPSCoordinate <- function(coord_value, ref_value) {
    if (is.null(coord_value) || is.na(coord_value)) return(NULL)
    
    # Convert to numeric if it's not already
    coord_num <- as.numeric(coord_value)
    if (is.na(coord_num)) return(NULL)
    
    # Apply sign based on reference
    if (!is.null(ref_value) && !is.na(ref_value)) {
      ref_upper <- toupper(as.character(ref_value))
      if (ref_upper %in% c("S", "W")) {
        coord_num <- -abs(coord_num)
      } else if (ref_upper %in% c("N", "E")) {
        coord_num <- abs(coord_num)
      }
    }
    
    return(coord_num)
  }
  
  # Function to generate unique filename
  generateUniqueFilename <- function(desired_path, original_path) {
    # If the desired path is the same as original, no change needed
    if (desired_path == original_path) {
      return(desired_path)
    }
    
    # If the desired file doesn't exist, use it
    if (!file.exists(desired_path)) {
      return(desired_path)
    }
    
    # Extract directory, filename without extension, and extension
    dir_path <- dirname(desired_path)
    filename_full <- basename(desired_path)
    filename_parts <- tools::file_path_sans_ext(filename_full)
    file_ext <- tools::file_ext(filename_full)
    
    # Start with increment 1
    increment <- 1
    
    # Keep trying until we find a unique name
    repeat {
      new_filename <- paste0(filename_parts, "_", increment, ".", file_ext)
      new_path <- file.path(dir_path, new_filename)
      
      # If this path doesn't exist, we found our unique name
      if (!file.exists(new_path)) {
        return(new_path)
      }
      
      # Otherwise, increment and try again
      increment <- increment + 1
      
      # Safety check to prevent infinite loop (shouldn't happen in practice)
      if (increment > 9999) {
        # Fall back to timestamp-based naming
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        new_filename <- paste0(filename_parts, "_", timestamp, ".", file_ext)
        new_path <- file.path(dir_path, new_filename)
        return(new_path)
      }
    }
  }
  
  # Directory selection
  shinyDirChoose(input, "selectDir", roots = c(home = "~", root = "/"))
  
  # Observe directory selection
  observeEvent(input$selectDir, {
    if (!is.null(input$selectDir)) {
      # Handle both list and atomic vector cases
      if (is.list(input$selectDir) && !is.null(input$selectDir$path) && length(input$selectDir$path) > 0) {
        values$photoFolder <- parseDirPath(c(home = "~", root = "/"), input$selectDir)
      } else if (is.atomic(input$selectDir) && length(input$selectDir) > 0) {
        tryCatch({
          values$photoFolder <- parseDirPath(c(home = "~", root = "/"), input$selectDir)
        }, error = function(e) {
          showNotification("Error parsing directory path", type = "warning")
          return()
        })
      } else {
        return()
      }
      
      # Validate that we got a valid folder path
      if (!is.null(values$photoFolder) && length(values$photoFolder) > 0 && dir.exists(values$photoFolder)) {
        # Get photo files
        photo_extensions <- c("jpg", "jpeg", "png", "tiff", "tif", "bmp")
        all_files <- list.files(values$photoFolder, full.names = TRUE)
        values$photoFiles <- all_files[tolower(tools::file_ext(all_files)) %in% photo_extensions]
        
        if (length(values$photoFiles) > 0) {
          values$currentPhotoIndex <- 1
          loadCurrentPhoto()
          showNotification(paste("Loaded", length(values$photoFiles), "photos"), type = "message")
        } else {
          showNotification("No photo files found in selected folder!", type = "warning")
        }
      } else {
        showNotification("Invalid directory selected", type = "warning")
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
        
        # Extract and set date/time if available
        date_set <- FALSE
        time_set <- FALSE
        
        # Try different date/time fields in order of preference
        date_fields <- c("DateTime", "DateTimeOriginal", "CreateDate", "ModifyDate")
        
        for (field in date_fields) {
          if (!is.null(values$originalMetadata[[field]]) && !date_set) {
            dt <- ymd_hms(values$originalMetadata[[field]], quiet = TRUE)
            if (!is.na(dt)) {
              updateDateInput(session, "creationDate", value = as.Date(dt))
              updateTimeInput(session, "creationTime", value = format(dt, "%H:%M:%S"))
              date_set <- TRUE
              time_set <- TRUE
              break
            }
          }
        }
        
        # If no date found, try date-only fields
        if (!date_set) {
          date_only_fields <- c("DateTimeDigitized", "FileModifyDate")
          for (field in date_only_fields) {
            if (!is.null(values$originalMetadata[[field]])) {
              dt <- ymd(values$originalMetadata[[field]], quiet = TRUE)
              if (!is.na(dt)) {
                updateDateInput(session, "creationDate", value = dt)
                date_set <- TRUE
                break
              }
            }
          }
        }
        
        # Set default time if date was found but time wasn't
        if (date_set && !time_set) {
          updateTimeInput(session, "creationTime", value = "12:00:00")
        }
        
        # Extract GPS coordinates with proper sign handling
        lat <- convertGPSCoordinate(values$originalMetadata$GPSLatitude, values$originalMetadata$GPSLatitudeRef)
        lng <- convertGPSCoordinate(values$originalMetadata$GPSLongitude, values$originalMetadata$GPSLongitudeRef)
        
        updateNumericInput(session, "latitude", value = lat)
        updateNumericInput(session, "longitude", value = lng)
        
        # Extract description from various possible fields
        description <- ""
        desc_fields <- c("ImageDescription", "UserComment", "Caption", "Description", "Subject")
        
        for (field in desc_fields) {
          if (!is.null(values$originalMetadata[[field]]) && 
              !is.na(values$originalMetadata[[field]]) && 
              nchar(trimws(values$originalMetadata[[field]])) > 0) {
            description <- trimws(values$originalMetadata[[field]])
            break
          }
        }
        
        updateTextAreaInput(session, "description", value = description)
        
        # Check if date is marked as approximate in any description field
        approximate_date <- FALSE
        for (field in desc_fields) {
          if (!is.null(values$originalMetadata[[field]]) && 
              grepl("date is approximate|approximate date", values$originalMetadata[[field]], ignore.case = TRUE)) {
            approximate_date <- TRUE
            break
          }
        }
        updateCheckboxInput(session, "dateApproximate", value = approximate_date)
        
      }, error = function(e) {
        showNotification(paste("Error reading metadata:", e$message), type = "warning")
        values$originalMetadata <- NULL
        
        # Reset form fields to defaults
        updateTextInput(session, "fileName", value = basename(current_file))
        updateDateInput(session, "creationDate", value = Sys.Date())
        updateTimeInput(session, "creationTime", value = "12:00:00")
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
  
  # Function to save metadata
  # Function to save metadata
  # Function to save metadata
  saveCurrentMetadata <- function() {
    if (!is.null(values$currentPhoto)) {
      tryCatch({
        # Check if there are any changes to save
        changes_detected <- FALSE
        
        # Check filename change
        current_filename <- basename(values$currentPhoto)
        new_filename <- input$fileName %||% basename(values$currentPhoto)
        if (!grepl("\\.", new_filename)) {
          new_filename <- paste0(new_filename, ".", tools::file_ext(values$currentPhoto))
        }
        
        if (new_filename != current_filename) {
          changes_detected <- TRUE
        }
        
        # Check date/time changes
        if (!is.null(values$originalMetadata)) {
          # Get current date/time from metadata
          current_datetime <- NULL
          date_fields <- c("DateTime", "DateTimeOriginal", "CreateDate", "ModifyDate")
          
          for (field in date_fields) {
            if (!is.null(values$originalMetadata[[field]])) {
              current_datetime <- ymd_hms(values$originalMetadata[[field]], quiet = TRUE)
              if (!is.na(current_datetime)) break
            }
          }
          
          # Compare with form values
          new_datetime <- ymd_hms(paste(input$creationDate, input$creationTime), quiet = TRUE)
          
          if (is.null(current_datetime) || is.na(current_datetime) || 
              (!is.na(new_datetime) && abs(as.numeric(difftime(current_datetime, new_datetime, units = "secs"))) > 1)) {
            changes_detected <- TRUE
          }
        } else {
          # If no metadata exists, consider it a change if date/time is set
          changes_detected <- TRUE
        }
        
        # Check GPS coordinates changes
        if (!is.null(values$originalMetadata)) {
          current_lat <- convertGPSCoordinate(values$originalMetadata$GPSLatitude, values$originalMetadata$GPSLatitudeRef)
          current_lng <- convertGPSCoordinate(values$originalMetadata$GPSLongitude, values$originalMetadata$GPSLongitudeRef)
          
          new_lat <- input$latitude
          new_lng <- input$longitude
          
          # Check if coordinates changed (with small tolerance for floating point precision)
          lat_changed <- FALSE
          lng_changed <- FALSE
          
          if (is.null(current_lat) && !is.null(new_lat) && !is.na(new_lat)) {
            lat_changed <- TRUE
          } else if (!is.null(current_lat) && (is.null(new_lat) || is.na(new_lat))) {
            lat_changed <- TRUE
          } else if (!is.null(current_lat) && !is.null(new_lat) && !is.na(new_lat)) {
            if (abs(current_lat - new_lat) > 0.000001) {
              lat_changed <- TRUE
            }
          }
          
          if (is.null(current_lng) && !is.null(new_lng) && !is.na(new_lng)) {
            lng_changed <- TRUE
          } else if (!is.null(current_lng) && (is.null(new_lng) || is.na(new_lng))) {
            lng_changed <- TRUE
          } else if (!is.null(current_lng) && !is.null(new_lng) && !is.na(new_lng)) {
            if (abs(current_lng - new_lng) > 0.000001) {
              lng_changed <- TRUE
            }
          }
          
          if (lat_changed || lng_changed) {
            changes_detected <- TRUE
          }
        } else {
          # If no metadata exists, consider it a change if coordinates are set
          if ((!is.null(input$latitude) && !is.na(input$latitude)) || 
              (!is.null(input$longitude) && !is.na(input$longitude))) {
            changes_detected <- TRUE
          }
        }
        
        # Check description changes
        if (!is.null(values$originalMetadata)) {
          current_description <- ""
          desc_fields <- c("ImageDescription", "UserComment", "Caption", "Description", "Subject")
          
          for (field in desc_fields) {
            if (!is.null(values$originalMetadata[[field]]) && 
                !is.na(values$originalMetadata[[field]]) && 
                nchar(trimws(values$originalMetadata[[field]])) > 0) {
              current_description <- trimws(values$originalMetadata[[field]])
              break
            }
          }
          
          # Remove "Date is approximate" from current description for comparison
          current_description_clean <- gsub("\\s*\\.?\\s*Date is approximate\\.?\\s*", "", current_description, ignore.case = TRUE)
          current_description_clean <- trimws(current_description_clean)
          
          new_description <- trimws(input$description %||% "")
          
          if (current_description_clean != new_description) {
            changes_detected <- TRUE
          }
        } else {
          # If no metadata exists, consider it a change if description is provided
          if (nchar(trimws(input$description %||% "")) > 0) {
            changes_detected <- TRUE
          }
        }
        
        # Check approximate date flag changes
        if (!is.null(values$originalMetadata)) {
          current_approximate <- FALSE
          desc_fields <- c("ImageDescription", "UserComment", "Caption", "Description", "Subject")
          
          for (field in desc_fields) {
            if (!is.null(values$originalMetadata[[field]]) && 
                grepl("date is approximate|approximate date", values$originalMetadata[[field]], ignore.case = TRUE)) {
              current_approximate <- TRUE
              break
            }
          }
          
          if (current_approximate != input$dateApproximate) {
            changes_detected <- TRUE
          }
        } else {
          # If no metadata exists, consider it a change if approximate is checked
          if (input$dateApproximate) {
            changes_detected <- TRUE
          }
        }
        
        # If no changes detected, don't save
        if (!changes_detected) {
          showNotification("No Changes Made to Photo. Not Saving.", type = "message")
          return()
        }
        
        # Proceed with saving if changes were detected
        # Prepare metadata
        new_description <- input$description %||% ""
        if (input$dateApproximate && !grepl("Date is approximate", new_description, ignore.case = TRUE)) {
          new_description <- paste(new_description, "Date is approximate", sep = if(nchar(new_description) > 0) ". " else "")
        }
        
        # Combine date and time
        datetime_string <- paste(input$creationDate, input$creationTime)
        
        # Handle file renaming with duplicate checking
        old_path <- values$currentPhoto
        new_filename_final <- new_filename
        
        # Create desired new path
        desired_path <- file.path(dirname(old_path), new_filename_final)
        
        # Generate unique filename if needed
        final_path <- generateUniqueFilename(desired_path, old_path)
        
        # Rename file if path changed
        if (old_path != final_path && file.exists(old_path)) {
          success <- file.rename(old_path, final_path)
          if (success) {
            values$currentPhoto <- final_path
            values$photoFiles[values$currentPhotoIndex] <- final_path
            
            # Update filename input to reflect the actual filename used
            actual_filename <- basename(final_path)
            if (actual_filename != new_filename_final) {
              updateTextInput(session, "fileName", value = actual_filename)
              showNotification(paste("File renamed to", actual_filename, "to avoid duplicate"), type = "message")
            } else {
              showNotification("File renamed successfully", type = "message")
            }
          } else {
            showNotification("Failed to rename file", type = "error")
            final_path <- old_path
          }
        }
        
        # Use exiftool to write metadata if available
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
            
            # Set latitude with correct reference
            lat_ref <- if(input$latitude >= 0) "N" else "S"
            lng_ref <- if(input$longitude >= 0) "E" else "W"
            
            cmd_parts <- c(cmd_parts, paste0('-GPSLatitude=', abs(input$latitude)))
            cmd_parts <- c(cmd_parts, paste0('-GPSLongitude=', abs(input$longitude)))
            cmd_parts <- c(cmd_parts, paste0('-GPSLatitudeRef=', lat_ref))
            cmd_parts <- c(cmd_parts, paste0('-GPSLongitudeRef=', lng_ref))
          }
          
          # Set description
          if (nchar(new_description) > 0) {
            cmd_parts <- c(cmd_parts, paste0('-ImageDescription="', new_description, '"'))
            cmd_parts <- c(cmd_parts, paste0('-UserComment="', new_description, '"'))
          }
          
          cmd_parts <- c(cmd_parts, shQuote(values$currentPhoto))
          
          # Execute the command
          result <- system(paste(cmd_parts, collapse = " "), intern = TRUE)
          
          showNotification("Metadata saved successfully!", type = "message")
        } else {
          showNotification("exiftool not found. Install exiftool to save metadata to files.", type = "warning")
        }
        
      }, error = function(e) {
        showNotification(paste("Error saving metadata:", as.character(e$message)), type = "error")
      })
    }
  }  # Location search
  observeEvent(input$searchLocation, {
    if (nchar(input$locationSearch %||% "") > 0) {
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
          
          showNotification("Location found!", type = "message")
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
      basename(values$photoFolder)
    } else {
      "None"
    }
  })
  
  output$photoCounter <- renderText({
    if (!is.null(values$photoFiles)) {
      paste(values$currentPhotoIndex, "/", length(values$photoFiles))
    } else {
      "0/0"
    }
  })
  
  output$photoPreview <- renderImage({
    if (!is.null(values$currentPhoto) && file.exists(values$currentPhoto)) {
      list(src = values$currentPhoto,
           alt = "Photo preview",
           width = "100%",
           height = "330px",
           style = "object-fit: contain; border: 1px solid #dee2e6; border-radius: 0.375rem;")
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
        paste("DateTime:", values$originalMetadata$DateTime %||% "N/A"),
        paste("GPS:", 
              if (!is.null(values$originalMetadata$GPSLatitude) && !is.null(values$originalMetadata$GPSLongitude)) {
                lat_display <- convertGPSCoordinate(values$originalMetadata$GPSLatitude, values$originalMetadata$GPSLatitudeRef)
                lng_display <- convertGPSCoordinate(values$originalMetadata$GPSLongitude, values$originalMetadata$GPSLongitudeRef)
                paste(round(lat_display, 4), ",", round(lng_display, 4))
              } else {
                "N/A"
              }),
        paste("Description:", substr(values$originalMetadata$ImageDescription %||% "N/A", 1, 50)),
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