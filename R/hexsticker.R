library(hexSticker)
library(magick)
library(showtext)
library(ggplot2)

# Download a font (optional)
# font_add_google("Roboto", "roboto")
# Automatically use showtext for device
showtext_auto()

# Path to your axolotl image
# Replace with your actual image path
img_path <- here::here("man", "figures", "axolotl.png")

# Process the image to remove white background
img <- image_read(img_path)
# Convert white background to transparent
img <- image_transparent(img, "white", fuzz = 10)

# Save the processed image temporarily
tmp_img_path <- "axolotl_transparent.png"
image_write(img, path = tmp_img_path)

# Create the sticker
sticker <- sticker(
  # Use the processed image
  subplot = tmp_img_path,
  # Package name
  package = "axolotr",
  # Size of the package name
  p_size = 40,
  # Color of the package name (green)
  p_color = "#7AA661",
  # Adjust the position of the subplot
  s_x = 1.0,
  s_y = 0.9,
  # Adjust the size of the subplot
  s_width = 0.8,
  s_height = 0.8,
  # Hexagon fill color (#e5edf4)
  h_fill = "#e5edf4",
  # Hexagon border color
  h_color = "#4D6D8E",
  # Size of the hexagon border
  h_size = 1.5,
  # File to save the sticker to
  filename = "man/figures/logo.png",
  # Resolution of the output file
  dpi = 600
)

# Display the sticker
plot(sticker)

# Clean up the temporary file
file.remove(tmp_img_path)

# Print message
cat("Logo created successfully as logo.png\n")
