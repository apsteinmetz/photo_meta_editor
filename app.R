# Load Required libraries ======================================================
library(shiny)
library(bslib)
library(leaflet)
library(exifr)
library(magick)
library(shinyFiles)
library(shinyWidgets)
library(dplyr)
library(lubridate)

# Try to load elmer for AI features (optional)
ai_available <- requireNamespace("ellmer", quietly = TRUE)
if (ai_available) {
  library(ellmer)
}


# # css header and footer ===================================================
# # this is css code to show the image popup
# sidebar contents =============================================================
sidebar_contents <- sidebar(
  title = "Photos",
  width = 150,

  # Folder selection
  shinyDirButton(
    "selectDir",
    "Select Folder",
    "Choose folder containing photos",
    class = "btn-primary w-100 btn-sm"
  ),

  verbatimTextOutput("currentFolder"),
  # Current folder display
  # card(
  #  height = "40px",
  #  card_body(
  #    # padding = "8px",
  #    verbatimTextOutput("currentFolder")
  #  )
  # ),

  br(),
  # Navigation controls
  actionButton(
    "prevPhoto",
    "Previous",
    icon = icon("arrow-left"),
    class = "btn-outline-secondary btn-sm w-100"
  ),

  br(),

  actionButton(
    "nextPhoto",
    "Next",
    icon = icon("arrow-right"),
    class = "btn-outline-secondary btn-sm w-100"
  ),
  br(),
  actionButton(
    "saveMetadata",
    "Save & Next",
    icon = icon("save"),
    class = "btn-success btn-sm w-100"
  ),
  br(),
  br(),
  # Photo counter
  card(
    height = "50px",
    card_body(
      padding = "8px",
      textOutput("photoCounter")
    )
  )
)

# cards ========================================================================
# Photo preview
card_preview <- card(
  full_screen = FALSE,
  height = "550px",
  card_header("Photo Preview (Click to Enlarge)"),
  card_body(
    imageOutput("photoPreview", click = "enlarge_photo", height = "480px")
  )
)
# Current metadata display
card_show_metadata <- card(
  full_screen = FALSE,
  height = "550px",
  card_header("Current Photo Info"),
  card_body(
    padding = "10px",
    verbatimTextOutput("currentMetadata")
  )
)
# Metadata editing
card_edit_metadata <- card(
  full_screen = FALSE,
  height = "400px",
  card_header("Metadata Editor"),
  card_body(
    padding = "15px",

    # Top row: File name and copy button
    layout_columns(
      col_widths = c(6, 3, 3),
      fill = FALSE,
      textInput("fileName", "File Name:", value = ""),
      shinyFilesButton(
        "selectSourceFile",
        "Copy From File",
        "Choose photo to copy metadata from",
        multiple = FALSE,
        class = "btn-outline-info btn-sm",
        style = "margin-top: 25px;"
      ),
      div(
        style = "margin-top: 30px; font-size: 0.85em; color: #666;",
        textOutput("selectedSourceFile", inline = TRUE)
      )
    ),

    # Second row: Date, time, and approximate checkbox
    layout_columns(
      col_widths = c(4, 4, 4),
      fill = TRUE,
      dateInput("photoTakenDate", "Photo Taken Date:", value = Sys.Date()),
      timeInput("photoTakenTime", "Photo Taken Time:", value = "12:00:00"),
      div(
        style = "padding-top: 25px;",
        checkboxInput("dateApproximate", "Date is Approximate", value = FALSE)
      )
    ),

    # Third row: Location section
    h6("Location", style = "margin-top: 20px; margin-bottom: 15px;"),

    layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      # Location search
      div(
        textInput(
          "locationSearch",
          "Search Location:",
          placeholder = "Enter city, address, or landmark"
        ),
        actionButton(
          "searchLocation",
          "Search",
          icon = icon("search"),
          class = "btn-outline-primary btn-sm w-100"
        )
      ),
      # Coordinates
      div(
        numericInput("latitude", "Latitude:", value = NULL, step = 0.000001),
        numericInput("longitude", "Longitude:", value = NULL, step = 0.000001)
      ),
      # Map instruction and coordinate info
      div(
        style = "padding-top: 10px;",
        p("Click on the map below to set location", class = "text-muted small"),
        br(),
        div(
          id = "coordinateDisplay",
          style = "font-size: 0.85em; color: #666; min-height: 40px;",
          "Coordinates will appear here when set"
        )
      )
    )
  )
)
# Description card
card_description <- card(
  full_screen = FALSE,
  height = "350px",
  card_header("Description"),
  card_body(
    padding = "10px",
    textAreaInput(
      "description",
      "",
      placeholder = "Add any additional notes or descriptions",
      rows = 12
    )
  )
)

# AI Date Estimation card
card_ai_date <- card(
  full_screen = FALSE,
  height = "350px",
  card_header(
    div(
      style = "display: flex; align-items: center; gap: 8px;",
      icon("magic"),
      "AI Date Estimation"
    )
  ),
  card_body(
    padding = "15px",
    textInput(
      "aiContext",
      "Additional Context (optional):",
      placeholder = "e.g., 'This is my grandfather' or 'Photo taken in New York'"
    ),
    layout_columns(
      col_widths = c(5, 4, 3),
      fill = FALSE,
      actionButton(
        "estimateDate",
        "Estimate Date with AI",
        icon = icon("magic"),
        class = "btn-info btn-sm w-100"
      ),
      uiOutput("aiResultButton"),
      uiOutput("aiApplyButton")
    ),
    uiOutput("aiSpinner")
  )
)

# Map section card
card_map <- card(
  full_screen = FALSE,
  height = "350px",
  card_header("Location Map"),
  card_body(
    padding = "8px",
    leafletOutput("locationMap", height = "280px")
  )
)
# about card
card_about <- card(
  full_screen = FALSE,
  height = "300px",
  card_header("About this App"),
  card_body(
    p(
      "This app allows you to choose a folder containting photos, edit certain photo metadata, date taken, location and descriptive text. The file name can also be changed"
    ),
    p("Developed by Art Steinmetz using Anthropic Claude-Sonnet 4."),
    p(
      "Development facts: 1000 lines of code, 1mm input tokens, $5 in Claude costs, 4 hours of back and forth prompting."
    )
  )
)

# UI ===========================================================================
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

  # Add custom CSS and JavaScript for the popup in header
  #tab panel to view and edit metadata
  nav_panel(
    title = "Photo Manager",
    icon = icon("camera"),
    page_sidebar(
      # select and load folder with photos
      sidebar = sidebar_contents,
      # Main content area
      # First row. Photo preview, metadata display, and metadata editing.
      layout_columns(
        col_widths = c(4, 2, 6),
        fill = FALSE,
        gap = "10px",
        card_preview,
        card_show_metadata,
        card_edit_metadata
      ),
      br(),
      # Second row. Description metadata, AI date, and Map
      layout_columns(
        col_widths = c(3, 3, 6),
        fill = FALSE,
        gap = "10px",
        card_description,
        card_ai_date,
        card_map,
      )
    )
  ),
  # nav panel "about" page
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card_about
  ),
)

# Server =======================================================================
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    photoFolder = NULL,
    photoFiles = NULL,
    currentPhotoIndex = 1,
    currentPhoto = NULL,
    originalMetadata = NULL,
    sourceFile = NULL, # Add this line
    aiResult = NULL, # Full AI response text
    aiEstimatedDate = NULL, # Parsed date from AI response
    aiRunning = FALSE # TRUE while AI query is in flight
  )

  # Custom write_exif function using exifr::exiftool_call()
  write_exif <- function(path, tags, overwrite_original = TRUE, quiet = TRUE) {
    # Validate inputs
    if (!file.exists(path)) {
      stop("File does not exist: ", path)
    }

    if (!is.list(tags) || length(tags) == 0) {
      stop("Tags must be a non-empty list")
    }

    # Build exiftool arguments
    args <- character(0)

    # Add overwrite original flag if requested
    if (overwrite_original) {
      args <- c(args, "-overwrite_original")
    }

    # List of tags that may generate warnings but still work
    warning_tags <- c("GPSPosition")

    # Add each tag/value pair
    for (tag_name in names(tags)) {
      tag_value <- tags[[tag_name]]

      # Handle empty/NULL values (clear the tag)
      if (
        is.null(tag_value) ||
          is.na(tag_value) ||
          (is.character(tag_value) && nchar(trimws(tag_value)) == 0)
      ) {
        args <- c(args, paste0("-", tag_name, "="))
      } else {
        # Properly quote values that contain spaces or special characters
        args <- c(
          args,
          paste0("-", tag_name, "=", shQuote(as.character(tag_value)))
        )
      }
    }

    # Add the file path
    args <- c(args, shQuote(path))

    # Execute exiftool command
    tryCatch(
      {
        result <- exifr::exiftool_call(args = args, quiet = TRUE) # Always quiet to capture output
        exit_code <- attr(result, "status")

        # Filter out known warnings that don't indicate actual failures
        if (length(result) > 0) {
          # Remove lines containing warnings about tags that we know work anyway
          filtered_result <- result[
            !grepl(
              paste(
                paste0("Sorry, ", warning_tags, " is not writable"),
                collapse = "|"
              ),
              result,
              ignore.case = TRUE
            )
          ]

          # Only show remaining messages if not in quiet mode
          if (!quiet && length(filtered_result) > 0) {
            cat(paste(filtered_result, collapse = "\n"), "\n")
          }
        }

        # Check if the operation was successful (exit code 0 or NULL means success)
        if (is.null(exit_code) || exit_code == 0) {
          return(TRUE)
        } else {
          if (!quiet) {
            cat("ExifTool returned non-zero exit status:", exit_code, "\n")
          }
          return(FALSE)
        }
      },
      error = function(e) {
        if (!quiet) {
          cat("Error calling exiftool:", e$message, "\n")
        }
        return(FALSE)
      }
    )
  } # Function to safely get metadata field with multiple attempts
  get_metadata_field <- function(metadata, field_names) {
    if (is.null(metadata)) {
      return(NULL)
    }

    # If field_names is a single string, convert to vector
    if (is.character(field_names) && length(field_names) == 1) {
      field_names <- c(field_names)
    }

    # Try each field name in order
    for (field_name in field_names) {
      if (field_name %in% names(metadata)) {
        value <- metadata[[field_name]]
        if (
          !is.null(value) &&
            !is.na(value) &&
            !(is.character(value) && nchar(trimws(value)) == 0)
        ) {
          return(value)
        }
      }
    }
    return(NULL)
  }

  # Function to convert GPS coordinates with proper sign
  convertGPSCoordinate <- function(coord_value, ref_value) {
    if (is.null(coord_value) || is.na(coord_value)) {
      return(NULL)
    }

    # Convert to numeric if it's not already
    coord_num <- as.numeric(coord_value)
    if (is.na(coord_num)) {
      return(NULL)
    }

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

  # Function to parse various date formats
  parse_exif_datetime <- function(datetime_str) {
    if (
      is.null(datetime_str) ||
        is.na(datetime_str) ||
        nchar(trimws(datetime_str)) == 0
    ) {
      return(NA)
    }

    # Try different parsing methods
    datetime_str <- trimws(datetime_str)

    # Try standard EXIF format first (YYYY:MM:DD HH:MM:SS)
    dt <- ymd_hms(gsub(":", "-", datetime_str, fixed = TRUE), quiet = TRUE)
    if (!is.na(dt)) {
      return(dt)
    }

    # Try ISO format
    dt <- ymd_hms(datetime_str, quiet = TRUE)
    if (!is.na(dt)) {
      return(dt)
    }

    # Try with different separators
    dt <- dmy_hms(datetime_str, quiet = TRUE)
    if (!is.na(dt)) {
      return(dt)
    }

    # Try date only formats
    dt <- ymd(gsub(":", "-", datetime_str), quiet = TRUE)
    if (!is.na(dt)) {
      return(as.POSIXct(paste(dt, "12:00:00")))
    }

    return(NA)
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

  # Directory selection with improved navigation and custom options
  pictures_path <- file.path(Sys.getenv("USERPROFILE"), "Pictures")
  if (!dir.exists(pictures_path)) {
    pictures_path <- normalizePath("~", winslash = "/")
  }

  # Allow navigation to parent folders by including root paths
  root_paths <- c(
    Pictures = pictures_path,
    Documents = file.path(Sys.getenv("USERPROFILE"), "Documents"),
    Desktop = file.path(Sys.getenv("USERPROFILE"), "Desktop"),
    Home = normalizePath("~", winslash = "/")
  )

  # Add drive roots on Windows
  if (.Platform$OS.type == "windows") {
    drives <- paste0(LETTERS[1:26], ":/")
    existing_drives <- drives[sapply(drives, dir.exists)]
    if (length(existing_drives) > 0) {
      names(existing_drives) <- paste0("Drive_", substr(existing_drives, 1, 1))
      root_paths <- c(root_paths, existing_drives)
    }
  } else {
    root_paths <- c(root_paths, root = "/")
  }

  # Custom shinyDirChoose with restricted options
  observe({
    shinyDirChoose(
      input,
      "selectDir",
      roots = root_paths,
      allowDirCreate = FALSE,
      restrictions = system.file(package = "base")
    )
  })

  observeEvent(
    input$selectDir,
    {
      if (!is.null(input$selectDir)) {
        if (
          is.list(input$selectDir) &&
            !is.null(input$selectDir$path) &&
            length(input$selectDir$path) > 0
        ) {
          values$photoFolder <- parseDirPath(root_paths, input$selectDir)
        } else if (is.atomic(input$selectDir) && length(input$selectDir) > 0) {
          tryCatch(
            {
              values$photoFolder <- parseDirPath(root_paths, input$selectDir)
            },
            error = function(e) {
              showNotification("Error parsing directory path", type = "warning")
              return()
            }
          )
        } else {
          return()
        }
        if (
          !is.null(values$photoFolder) &&
            length(values$photoFolder) > 0 &&
            dir.exists(values$photoFolder)
        ) {
          photo_extensions <- c("jpg", "jpeg", "png", "tiff", "tif", "bmp")
          all_files <- list.files(values$photoFolder, full.names = TRUE)
          values$photoFiles <- all_files[
            tolower(tools::file_ext(all_files)) %in% photo_extensions
          ]

          if (length(values$photoFiles) > 0) {
            values$currentPhotoIndex <- 1
            loadCurrentPhoto()
            showNotification(
              paste("Loaded", length(values$photoFiles), "photos"),
              type = "message"
            )
          } else {
            showNotification(
              "No photo files found in selected folder!",
              type = "warning"
            )
          }
        } # else {
        #  cat(values$photoFolder, "\n")
        #  showNotification("Invalid directory selected", type = "warning")
        #}
      }
    },
    ignoreInit = TRUE
  )

  # Function to load current photo
  loadCurrentPhoto <- function() {
    if (!is.null(values$photoFiles) && length(values$photoFiles) > 0) {
      current_file <- values$photoFiles[values$currentPhotoIndex]
      values$currentPhoto <- current_file
      # Clear AI results from previous photo
      values$aiResult <- NULL
      values$aiEstimatedDate <- NULL

      # Load existing metadata
      tryCatch(
        {
          # Request specific EXIF tags
          values$originalMetadata <- read_exif(
            current_file,
            tags = c(
              "CreateDate",
              "DateTimeOriginal",
              "GPSLatitude",
              "GPSLongitude",
              "GPSLatitudeRef",
              "GPSLongitudeRef",
              "GPSPosition",
              "ImageDescription",
              "UserComment",
              "Make",
              "Model"
            )
          )

          # Update UI with existing metadata
          updateTextInput(session, "fileName", value = basename(current_file))

          # Extract and set photo taken date/time from CreateDate or DateTimeOriginal
          date_set <- FALSE
          time_set <- FALSE

          # Try CreateDate first, then DateTimeOriginal
          datetime_fields <- c("CreateDate", "DateTimeOriginal")
          for (field in datetime_fields) {
            datetime_value <- get_metadata_field(values$originalMetadata, field)
            if (!is.null(datetime_value)) {
              dt <- parse_exif_datetime(datetime_value)
              if (!is.na(dt)) {
                updateDateInput(session, "photoTakenDate", value = as.Date(dt))
                updateTimeInput(
                  session,
                  "photoTakenTime",
                  value = format(dt, "%H:%M:%S")
                )
                date_set <- TRUE
                time_set <- TRUE
                break
              }
            }
          }

          # Set default time if date was found but time wasn't
          if (date_set && !time_set) {
            updateTimeInput(session, "photoTakenTime", value = "12:00:00")
          }

          # Extract GPS coordinates with proper sign handling
          gps_lat <- get_metadata_field(values$originalMetadata, "GPSLatitude")
          gps_lat_ref <- get_metadata_field(
            values$originalMetadata,
            "GPSLatitudeRef"
          )
          gps_lng <- get_metadata_field(values$originalMetadata, "GPSLongitude")
          gps_lng_ref <- get_metadata_field(
            values$originalMetadata,
            "GPSLongitudeRef"
          )

          lat <- convertGPSCoordinate(gps_lat, gps_lat_ref)
          lng <- convertGPSCoordinate(gps_lng, gps_lng_ref)

          updateNumericInput(session, "latitude", value = lat)
          updateNumericInput(session, "longitude", value = lng)

          # Extract description, including filesystem date
          description <- ""
          desc_fields <- c(
            "ImageDescription",
            "UserComment",
            "Caption",
            "Description",
            "Subject"
          )

          for (field in desc_fields) {
            field_value <- get_metadata_field(values$originalMetadata, field)
            if (!is.null(field_value) && nchar(trimws(field_value)) > 0) {
              description <- trimws(field_value)
              break
            }
          }

          # Get filesystem date and add to description if not already present
          file_info <- file.info(current_file)
          if (!is.null(file_info$mtime)) {
            filesystem_date <- format(file_info$mtime, "%Y-%m-%d %H:%M:%S")
            filesystem_text <- paste("File date:", filesystem_date)

            # Only add filesystem date if it's not already in the description
            if (!grepl("File date:", description, fixed = TRUE)) {
              if (nchar(description) > 0) {
                description <- paste(description, filesystem_text, sep = ". ")
              } else {
                description <- filesystem_text
              }
            }
          }

          updateTextAreaInput(session, "description", value = description)

          # Check if date is marked as approximate in any description field
          approximate_date <- FALSE
          for (field in desc_fields) {
            field_value <- get_metadata_field(values$originalMetadata, field)
            if (
              !is.null(field_value) &&
                grepl(
                  "date is approximate|approximate date",
                  field_value,
                  ignore.case = TRUE
                )
            ) {
              approximate_date <- TRUE
              break
            }
          }
          updateCheckboxInput(
            session,
            "dateApproximate",
            value = approximate_date
          )
        },
        error = function(e) {
          showNotification(
            paste("Error reading metadata:", e$message),
            type = "warning"
          )
          values$originalMetadata <- NULL

          # Reset form fields to defaults and add filesystem date
          updateTextInput(session, "fileName", value = basename(current_file))
          updateDateInput(session, "photoTakenDate", value = Sys.Date())
          updateTimeInput(session, "photoTakenTime", value = "12:00:00")
          updateNumericInput(session, "latitude", value = NULL)
          updateNumericInput(session, "longitude", value = NULL)

          # Get filesystem date for description
          file_info <- file.info(current_file)
          description <- ""
          if (!is.null(file_info$mtime)) {
            filesystem_date <- format(file_info$mtime, "%Y-%m-%d %H:%M:%S")
            description <- paste("File date:", filesystem_date)
          }
          updateTextAreaInput(session, "description", value = description)
          updateCheckboxInput(session, "dateApproximate", value = FALSE)
        }
      )
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
    if (
      !is.null(values$photoFiles) &&
        values$currentPhotoIndex < length(values$photoFiles)
    ) {
      values$currentPhotoIndex <- values$currentPhotoIndex + 1
      loadCurrentPhoto()
    }
  })

  # Function to save metadata using the custom write_exif function
  saveCurrentMetadata <- function() {
    if (!is.null(values$currentPhoto)) {
      tryCatch(
        {
          # Prepare metadata
          new_description <- input$description %||% ""
          if (
            input$dateApproximate &&
              !grepl("Date is approximate", new_description, ignore.case = TRUE)
          ) {
            new_description <- paste(
              new_description,
              "Date is approximate",
              sep = if (nchar(new_description) > 0) ". " else ""
            )
          }

          # Combine date and time for photo taken date (use standard EXIF format)
          datetime_obj <- ymd_hms(
            paste(input$photoTakenDate, input$photoTakenTime),
            quiet = TRUE
          )
          if (is.na(datetime_obj)) {
            showNotification("Invalid date/time format", type = "error")
            return(FALSE)
          }
          datetime_string <- format(datetime_obj, "%Y:%m:%d %H:%M:%S")

          # Handle file renaming with duplicate checking
          old_path <- values$currentPhoto
          new_filename <- input$fileName %||% basename(values$currentPhoto)
          if (!grepl("\\.", new_filename)) {
            new_filename <- paste0(
              new_filename,
              ".",
              tools::file_ext(values$currentPhoto)
            )
          }

          # Create desired new path
          desired_path <- file.path(dirname(old_path), new_filename)

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
              if (actual_filename != new_filename) {
                updateTextInput(session, "fileName", value = actual_filename)
                showNotification(
                  paste(
                    "File renamed to",
                    actual_filename,
                    "to avoid duplicate"
                  ),
                  type = "message"
                )
              }
            } else {
              showNotification("Failed to rename file", type = "error")
              return(FALSE)
            }
          }

          # Prepare tags list for write_exif function
          tags_to_write <- list()

          # Set BOTH CreateDate and DateTimeOriginal to the same value
          tags_to_write$CreateDate <- datetime_string
          tags_to_write$DateTimeOriginal <- datetime_string

          # Set GPS coordinates if both are provided and valid
          if (
            !is.null(input$latitude) &&
              !is.null(input$longitude) &&
              !is.na(input$latitude) &&
              !is.na(input$longitude) &&
              is.numeric(input$latitude) &&
              is.numeric(input$longitude)
          ) {
            # Set latitude/longitude references based on sign
            lat_ref <- if (input$latitude >= 0) "N" else "S"
            lng_ref <- if (input$longitude >= 0) "E" else "W"

            # Set all GPS metadata tags
            tags_to_write$GPSLatitude <- abs(input$latitude)
            tags_to_write$GPSLongitude <- abs(input$longitude)
            tags_to_write$GPSLatitudeRef <- lat_ref
            tags_to_write$GPSLongitudeRef <- lng_ref

            # Create GPSPosition as space-separated string of latitude and longitude
            gps_position <- paste(input$latitude, input$longitude)
            tags_to_write$GPSPosition <- gps_position
          } else {
            # Clear GPS data if coordinates are not valid
            tags_to_write$GPSLatitude <- NULL
            tags_to_write$GPSLongitude <- NULL
            tags_to_write$GPSLatitudeRef <- NULL
            tags_to_write$GPSLongitudeRef <- NULL
            tags_to_write$GPSPosition <- NULL
          }

          # Set description
          if (nchar(new_description) > 0) {
            tags_to_write$ImageDescription <- new_description
            tags_to_write$UserComment <- new_description
          }

          # Use the custom write_exif function
          success <- write_exif(
            path = values$currentPhoto,
            tags = tags_to_write,
            overwrite_original = TRUE,
            quiet = FALSE
          )

          if (success) {
            showNotification("Metadata saved successfully!", type = "message")
            return(TRUE)
          } else {
            showNotification("Failed to save metadata", type = "error")
            return(FALSE)
          }
        },
        error = function(e) {
          showNotification(
            paste("Error saving metadata:", as.character(e$message)),
            type = "error"
          )
          return(FALSE)
        }
      )
    }
    return(FALSE)
  }

  # Save and next
  observeEvent(input$saveMetadata, {
    # Always try to save metadata first
    save_success <- saveCurrentMetadata()

    # Move to next photo regardless of save success (but show appropriate message)
    if (
      !is.null(values$photoFiles) &&
        values$currentPhotoIndex < length(values$photoFiles)
    ) {
      # Small delay to ensure file operations complete
      Sys.sleep(0.2)
      values$currentPhotoIndex <- values$currentPhotoIndex + 1
      loadCurrentPhoto()
    } else if (!is.null(values$photoFiles)) {
      showNotification("Reached the last photo in the folder", type = "message")
    }
  })

  # Location search
  observeEvent(input$searchLocation, {
    if (nchar(input$locationSearch %||% "") > 0) {
      # Simple geocoding using Nominatim (OpenStreetMap)
      tryCatch(
        {
          url <- paste0(
            "https://nominatim.openstreetmap.org/search?format=json&q=",
            URLencode(input$locationSearch)
          )
          response <- jsonlite::fromJSON(url)

          if (length(response) > 0) {
            lat <- as.numeric(response$lat[1])
            lon <- as.numeric(response$lon[1])

            updateNumericInput(session, "latitude", value = lat)
            updateNumericInput(session, "longitude", value = lon)

            showNotification("Location found!", type = "message")
          } else {
            showNotification(
              "Location not found. Try a different search term.",
              type = "warning"
            )
          }
        },
        error = function(e) {
          showNotification(
            "Error searching location. Check internet connection.",
            type = "error"
          )
        }
      )
    }
  })

  # AI spinner output — driven reactively by aiRunning / aiResult flags
  output$aiSpinner <- renderUI({
    if (isTRUE(values$aiRunning)) {
      div(
        style = "margin-top: 8px; color: #666; font-size: 0.9em;",
        icon("spinner", class = "fa-spin"),
        " Analyzing photo with AI..."
      )
    } else if (!is.null(values$aiResult)) {
      range_match <- regmatches(
        values$aiResult,
        regexpr("Date Range:\\s*([^.]+)", values$aiResult)
      )
      date_range_text <- if (length(range_match) > 0) {
        trimws(sub("Date Range:\\s*", "", range_match[1]))
      } else {
        "Unknown"
      }
      div(
        style = "margin-top: 10px; padding: 8px; background-color: #e8f4f8; border-left: 3px solid #3498db; border-radius: 3px; font-size: 0.9em;",
        div(tags$strong("Date Range: "), date_range_text),
        div(
          tags$strong("Best Date: "),
          if (!is.null(values$aiEstimatedDate)) {
            format(values$aiEstimatedDate, "%B %d, %Y")
          } else {
            "Unknown"
          }
        )
      )
    } else {
      NULL
    }
  })

  # AI result and apply buttons (hidden until AI returns a result)
  output$aiResultButton <- renderUI({
    req(values$aiResult)
    actionButton(
      "showAiResult",
      "View Reasoning",
      icon = icon("eye"),
      class = "btn-outline-secondary btn-sm w-100"
    )
  })

  output$aiApplyButton <- renderUI({
    req(values$aiEstimatedDate)
    actionButton(
      "applyAiDate",
      "Apply AI Date",
      icon = icon("check"),
      class = "btn-success btn-sm w-100"
    )
  })

  # Show AI reasoning in a modal
  observeEvent(input$showAiResult, {
    req(values$aiResult)
    showModal(modalDialog(
      title = "AI Date Estimation",
      pre(
        style = "white-space: pre-wrap; font-size: 0.9em;",
        values$aiResult
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })

  # Apply AI estimated date to metadata fields
  observeEvent(input$applyAiDate, {
    req(values$aiEstimatedDate)
    updateDateInput(session, "photoTakenDate", value = values$aiEstimatedDate)
    updateCheckboxInput(session, "dateApproximate", value = TRUE)
    # Append AI note to description
    current_desc <- input$description %||% ""
    ai_note <- paste0(
      "AI estimated date: ",
      format(values$aiEstimatedDate, "%Y-%m-%d")
    )
    if (!grepl("AI estimated date", current_desc, fixed = TRUE)) {
      updated_desc <- if (nchar(trimws(current_desc)) > 0) {
        paste(current_desc, ai_note, sep = ". ")
      } else {
        ai_note
      }
      updateTextAreaInput(session, "description", value = updated_desc)
    }
    showNotification("AI date applied.", type = "message")
  })

  # AI Date Estimation
  observeEvent(input$estimateDate, {
    req(values$currentPhoto)

    if (!ai_available) {
      showNotification(
        "AI features require the 'ellmer' package. Install with: install.packages('ellmer')",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Clear previous result and signal spinner to show
    values$aiResult <- NULL
    values$aiEstimatedDate <- NULL
    values$aiRunning <- TRUE

    # Capture values needed in the later callback (avoid reactive context issues)
    current_photo <- values$currentPhoto
    ai_context <- input$aiContext

    # Capture the reactive domain so later::later() can update reactive values
    # and call showNotification() from outside a reactive context
    domain <- shiny::getDefaultReactiveDomain()

    # Defer the blocking AI call so the spinner renders first
    later::later(
      function() {
        shiny::withReactiveDomain(domain, {
          tryCatch(
            {
              # Build the prompt
              base_prompt <- paste(
                "Analyze this photograph and estimate when it was taken based on:",
                "1. Clothing styles and fashion",
                "2. Background elements (cars, buildings, technology visible)",
                "3. Photo quality and characteristics",
                "4. Any other visible time period indicators",
                "5. Do not rely on metadata or file information, only visual clues in the image.",
                "\nProvide your best estimate as a year or range of years (e.g., '1985' or '1975-1980').",
                "Also provide a single date estimate as the most likely point within the range.",
                "Also provide a brief explanation of the key indicators you used.",
                "Format your response EXACTLY as: 'Date Range: [date range]. Best Date: [YYYY-MM-DD]. Reasoning: [explanation]'"
              )

              # Add user context if provided
              if (!is.null(ai_context) && nchar(trimws(ai_context)) > 0) {
                base_prompt <- paste0(
                  base_prompt,
                  "\n\nAdditional context provided by user: ",
                  ai_context
                )
              }

              # Create chat and send image
              chat <- chat_openai(
                system_prompt = "You are an expert at dating historical photographs based on visual clues."
              )

              # Read the image and encode it
              img_data <- base64enc::base64encode(current_photo)

              # Send text prompt and image as separate arguments to chat
              result <- chat$chat(
                base_prompt,
                content_image_url(paste0("data:image/jpeg;base64,", img_data))
              )

              # Store the full result
              values$aiResult <- result

              # Parse "Best Date:" (YYYY-MM-DD) from the structured response
              date_match <- regmatches(
                result,
                regexpr("Best Date:\\s*(\\d{4}-\\d{2}-\\d{2})", result)
              )
              if (length(date_match) > 0) {
                estimated_date_str <- trimws(sub(
                  "Best Date:\\s*",
                  "",
                  date_match[1]
                ))
                values$aiEstimatedDate <- tryCatch(
                  as.Date(estimated_date_str),
                  error = function(e) NULL
                )
              } else {
                # Fallback: first 4-digit year in response
                year_match2 <- regmatches(
                  result,
                  regexpr("\\b(19|20)\\d{2}\\b", result)
                )
                values$aiEstimatedDate <- if (length(year_match2) > 0) {
                  as.Date(paste0(year_match2[1], "-07-01"))
                } else {
                  NULL
                }
              }

              # Parse "Date Range:" from the structured response
              range_match <- regmatches(
                result,
                regexpr("Date Range:\\s*([^.]+)", result)
              )
              date_range_text <- if (length(range_match) > 0) {
                trimws(sub("Date Range:\\s*", "", range_match[1]))
              } else {
                "Unknown"
              }

              # Signal done — spinner renderUI will reactively update
              values$aiRunning <- FALSE
              showNotification(
                "AI analysis complete! Click 'Apply AI Date' to use the estimate.",
                type = "message",
                duration = 4
              )
            },
            error = function(e) {
              values$aiRunning <- FALSE
              showNotification(
                paste("AI estimation failed:", e$message),
                type = "error",
                duration = 5
              )
            }
          )
        }) # end withReactiveDomain
      },
      delay = 0
    )
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

  output$photoPreview <- renderImage(
    {
      if (!is.null(values$currentPhoto) && file.exists(values$currentPhoto)) {
        list(
          src = values$currentPhoto,
          alt = "Photo preview - Click to enlarge",
          width = "100%",
          height = "480px",
          style = "object-fit: contain; border: 1px solid #dee2e6; border-radius: 0.375rem;"
        )
      } else {
        list(src = "", alt = "No photo selected")
      }
    },
    deleteFile = FALSE
  )

  # Photo popup image
  output$img_large <- renderImage(
    {
      if (!is.null(values$currentPhoto) && file.exists(values$currentPhoto)) {
        list(
          src = values$currentPhoto,
          alt = "Full size photo preview",
          style = "max-width: 100%; max-height: 80vh; object-fit: contain;"
        )
      } else {
        list(src = "", alt = "")
      }
    },
    deleteFile = FALSE
  )

  observeEvent(input$enlarge_photo, {
    req(values$currentPhoto)
    showModal(modalDialog(
      imageOutput("img_large"),
      title = values$currentPhoto,
      size = "xl",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  # File picker for copying metadata - dynamic roots based on current folder
  observe({
    # Safely check if photoFolder exists and is valid
    folder_is_valid <- !is.null(values$photoFolder) &&
      length(values$photoFolder) > 0 &&
      !is.na(values$photoFolder) &&
      nchar(values$photoFolder) > 0 &&
      dir.exists(values$photoFolder)

    if (folder_is_valid) {
      # Create roots with current folder as primary option
      current_roots <- c(
        "Current Folder" = values$photoFolder,
        "Parent Folder" = dirname(values$photoFolder),
        root_paths
      )

      shinyFileChoose(
        input,
        "selectSourceFile",
        roots = current_roots,
        filetypes = c("jpg", "jpeg", "png", "tiff", "tif", "bmp")
      )
    } else {
      # Fallback to original roots if no folder is loaded
      shinyFileChoose(
        input,
        "selectSourceFile",
        roots = root_paths,
        filetypes = c("jpg", "jpeg", "png", "tiff", "tif", "bmp")
      )
    }
  })

  # Handle file selection for copying metadata
  observeEvent(input$selectSourceFile, {
    if (!is.null(input$selectSourceFile)) {
      # Use the appropriate roots for parsing based on current state
      parse_roots <- if (
        !is.null(values$photoFolder) && dir.exists(values$photoFolder)
      ) {
        c("Current Folder" = values$photoFolder, root_paths)
      } else {
        root_paths
      }

      source_file_path <- parseFilePaths(parse_roots, input$selectSourceFile)

      if (
        nrow(source_file_path) > 0 && file.exists(source_file_path$datapath[1])
      ) {
        # Store the selected file path
        values$sourceFile <- source_file_path$datapath[1]

        # Update the display text
        output$selectedSourceFile <- renderText({
          basename(values$sourceFile)
        })

        # Copy metadata from the selected file
        copyMetadataFromFile(values$sourceFile)

        showNotification(
          paste("Metadata copied from", basename(values$sourceFile)),
          type = "message"
        )
      }
    }
  })

  # Function to copy metadata from selected file
  copyMetadataFromFile <- function(source_file_path) {
    if (!file.exists(source_file_path)) {
      showNotification("Source file not found", type = "error")
      return()
    }

    tryCatch(
      {
        # Read metadata from source file
        source_metadata <- read_exif(
          source_file_path,
          tags = c(
            "CreateDate",
            "DateTimeOriginal",
            "GPSLatitude",
            "GPSLongitude",
            "GPSLatitudeRef",
            "GPSLongitudeRef",
            "ImageDescription",
            "UserComment"
          )
        )

        # Copy date/time information
        datetime_fields <- c("CreateDate", "DateTimeOriginal")
        for (field in datetime_fields) {
          datetime_value <- get_metadata_field(source_metadata, field)
          if (!is.null(datetime_value)) {
            dt <- parse_exif_datetime(datetime_value)
            if (!is.na(dt)) {
              updateDateInput(session, "photoTakenDate", value = as.Date(dt))
              updateTimeInput(
                session,
                "photoTakenTime",
                value = format(dt, "%H:%M:%S")
              )
              break
            }
          }
        }

        # Copy GPS coordinates
        gps_lat <- get_metadata_field(source_metadata, "GPSLatitude")
        gps_lat_ref <- get_metadata_field(source_metadata, "GPSLatitudeRef")
        gps_lng <- get_metadata_field(source_metadata, "GPSLongitude")
        gps_lng_ref <- get_metadata_field(source_metadata, "GPSLongitudeRef")

        lat <- convertGPSCoordinate(gps_lat, gps_lat_ref)
        lng <- convertGPSCoordinate(gps_lng, gps_lng_ref)

        updateNumericInput(session, "latitude", value = lat)
        updateNumericInput(session, "longitude", value = lng)

        # Copy description (but preserve existing filesystem date)
        current_description <- input$description %||% ""
        source_description <- ""

        desc_fields <- c("ImageDescription", "UserComment")
        for (field in desc_fields) {
          field_value <- get_metadata_field(source_metadata, field)
          if (!is.null(field_value) && nchar(trimws(field_value)) > 0) {
            source_description <- trimws(field_value)
            break
          }
        }

        if (nchar(source_description) > 0) {
          # Remove filesystem date from source description
          source_desc_clean <- gsub(
            "\\s*\\.?\\s*File date:[^.]*\\.?",
            "",
            source_description
          )
          source_desc_clean <- trimws(source_desc_clean)

          # Preserve existing filesystem date from current description
          filesystem_match <- regmatches(
            current_description,
            regexpr("File date:[^.]*", current_description)
          )

          if (length(filesystem_match) > 0 && nchar(source_desc_clean) > 0) {
            new_description <- paste(
              source_desc_clean,
              filesystem_match,
              sep = ". "
            )
          } else if (length(filesystem_match) > 0) {
            new_description <- filesystem_match
          } else {
            new_description <- source_desc_clean
          }

          updateTextAreaInput(session, "description", value = new_description)
        }

        # Check if source date is marked as approximate
        approximate_date <- grepl(
          "date is approximate|approximate date",
          source_description,
          ignore.case = TRUE
        )
        updateCheckboxInput(
          session,
          "dateApproximate",
          value = approximate_date
        )
      },
      error = function(e) {
        showNotification(
          paste("Error copying metadata:", e$message),
          type = "error"
        )
      }
    )
  }

  # Output for displaying selected source file
  output$selectedSourceFile <- renderText({
    if (!is.null(values$sourceFile)) {
      paste("Selected:", basename(values$sourceFile))
    } else {
      "No file selected"
    }
  })

  output$locationMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 0, zoom = 2)
  })

  # Update map when coordinates change
  observe({
    # Only update map if both coordinates are valid numbers
    if (
      !is.null(input$latitude) &&
        !is.null(input$longitude) &&
        !is.na(input$latitude) &&
        !is.na(input$longitude) &&
        is.numeric(input$latitude) &&
        is.numeric(input$longitude) &&
        input$latitude >= -90 &&
        input$latitude <= 90 &&
        input$longitude >= -180 &&
        input$longitude <= 180
    ) {
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
      # Get filesystem date
      file_info <- file.info(values$currentPhoto)
      filesystem_date <- if (!is.null(file_info$mtime)) {
        format(file_info$mtime, "%Y-%m-%d %H:%M:%S")
      } else {
        "N/A"
      }

      # Safely get metadata fields - prioritize CreateDate and DateTimeOriginal
      create_date <- get_metadata_field(values$originalMetadata, "CreateDate")
      datetime_original <- get_metadata_field(
        values$originalMetadata,
        "DateTimeOriginal"
      )
      photo_taken <- create_date %||% datetime_original

      gps_lat <- get_metadata_field(values$originalMetadata, "GPSLatitude")
      gps_lat_ref <- get_metadata_field(
        values$originalMetadata,
        "GPSLatitudeRef"
      )
      gps_lng <- get_metadata_field(values$originalMetadata, "GPSLongitude")
      gps_lng_ref <- get_metadata_field(
        values$originalMetadata,
        "GPSLongitudeRef"
      )
      gps_position <- get_metadata_field(values$originalMetadata, "GPSPosition")
      image_desc <- get_metadata_field(
        values$originalMetadata,
        c("ImageDescription", "UserComment")
      )
      make <- get_metadata_field(values$originalMetadata, "Make")
      model <- get_metadata_field(values$originalMetadata, "Model")

      metadata_text <- c(
        paste("File:", basename(values$currentPhoto)),
        paste("Photo Taken:", photo_taken %||% "N/A"),
        paste("File Date:", filesystem_date),
        paste(
          "GPS:",
          if (!is.null(gps_lat) && !is.null(gps_lng)) {
            lat_display <- convertGPSCoordinate(gps_lat, gps_lat_ref)
            lng_display <- convertGPSCoordinate(gps_lng, gps_lng_ref)
            if (!is.null(lat_display) && !is.null(lng_display)) {
              paste(round(lat_display, 4), ",", round(lng_display, 4))
            } else {
              "N/A"
            }
          } else {
            "N/A"
          }
        ),
        paste("GPS Position:", gps_position %||% "N/A"),
        paste("Description:", substr(image_desc %||% "N/A", 1, 50)),
        paste("Camera:", make %||% "Unknown", model %||% "")
      )
      paste(metadata_text, collapse = "\n")
    } else if (!is.null(values$currentPhoto)) {
      # Get filesystem date even when no EXIF data
      file_info <- file.info(values$currentPhoto)
      filesystem_date <- if (!is.null(file_info$mtime)) {
        format(file_info$mtime, "%Y-%m-%d %H:%M:%S")
      } else {
        "N/A"
      }

      paste(
        "File:",
        basename(values$currentPhoto),
        "\nFile Date:",
        filesystem_date,
        "\nNo EXIF metadata available"
      )
    } else {
      "No photo selected"
    }
  })

  # Clean up when session ends (browser closed or tab closed)
  session$onSessionEnded(function() {
    # Clean up temp directory if it exists (cloud mode)
    # Use isolate() to access reactive values in callback
    temp_dir <- isolate(values$tempDir)
    if (!is.null(temp_dir) && dir.exists(temp_dir)) {
      tryCatch(
        {
          unlink(temp_dir, recursive = TRUE)
        },
        error = function(e) {
          # Silently fail if cleanup doesn't work
        }
      )
    }

    # Note: stopApp() is commented out as it may not be desired in all contexts
    # Uncomment the following line if you want the app to stop when browser closes
    stopApp()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
