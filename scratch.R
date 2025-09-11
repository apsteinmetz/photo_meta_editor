# this is an R code snippet to read metadata from a photo file
# read all the metadata from a photo file
library(exiftoolr)
# Specify the path to the photo file
# photo_file <- "path/to/your/photo.jpg"

# show  file picker to select photo
if (interactive()) {
  photo_file <- file.choose()
}

# photo_path <- "C:/Users/Apste/Pictures/to_process/peter art and caroline in backyard.png"
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


detect_image_format <- function(path) {
  f <- function(p) {
    con <- file(p, "rb")
    on.exit(close(con), add = TRUE)
    sig <- readBin(con, what = "raw", n = 32)
    is_pref <- function(prefix) length(sig) >= length(prefix) && all(sig[seq_along(prefix)] == prefix)
    if (is_pref(as.raw(c(0xFF, 0xD8, 0xFF)))) return("JPEG")
    if (is_pref(as.raw(c(0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A)))) return("PNG")
    if (is_pref(charToRaw("GIF87a")) || is_pref(charToRaw("GIF89a"))) return("GIF")
    if (is_pref(as.raw(c(0x49, 0x49, 0x2A, 0x00))) || is_pref(as.raw(c(0x4D, 0x4D, 0x00, 0x2A)))) return("TIFF")
    if (is_pref(charToRaw("BM"))) return("BMP")
    if (length(sig) >= 12 && all(sig[1:4] == charToRaw("RIFF")) && all(sig[9:12] == charToRaw("WEBP"))) return("WEBP")
    if (length(sig) >= 12 && all(sig[5:8] == charToRaw("ftyp"))) {
      brand <- rawToChar(sig[9:12])
      if (brand %in% c("heic", "heif", "heix", "hevc", "hevx", "mif1")) return("HEIF")
      if (brand %in% c("avif", "avis")) return("AVIF")
      if (brand %in% c("jp2 ", "jpx ")) return("JP2")
    }
    "unknown"
  }
  if (length(path) == 1) f(path) else vapply(path, f, character(1))
}
detect_image_format(photo_file)
