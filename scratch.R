# this is an R code snippet to read metadata from a photo file
# read all the metadata from a photo file
library(exiftoolr)
# Specify the path to the photo file
photo_file <- "path/to/your/photo.jpg"

# show  file picker to select photo
if (interactive()) {
  photo_file <- file.choose()
}

photo_path <- "C:\\Users\\Apste\\Pictures\\ControlCenter4\\Scan\\"

# Read the metadata
metadata <- read_exif(photo_file)
# Print the metadata
print(metadata)
 # convert all columns to character
metadata[] <- lapply(metadata, as.character)

# pivot to long format
library(tidyr)
metadata_long <- pivot_longer(metadata, 
                              cols = everything(), 
                              names_to = "Metadata_Field", 
                              values_to = "Value")

metadata %>% filter(is.na(GPSLatitude)) %>% select(FileName) %>% pull()

names(metadata)

exifr::exiftool_call()