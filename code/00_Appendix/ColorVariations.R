library(colorspace)

# Your original colors
original_colors <- c("#5EF1F2", "#2BCE48", "#990000", "#F0A3FF")

# Function to generate variations
generate_variations <- function(color) {
  # Lighter version (adjust luminance)
  lighter <- lighten(color, amount = 0.4)  # 0.2 means 20% lighter
  
  # Darker version (adjust luminance)
  darker <- darken(color, amount = 0.4) # 0.2 means 20% darker
  
  return(c(lighter, darker))
}

# Generate variations for all colors
color_variations <- lapply(original_colors, generate_variations)

# Convert to a matrix for easier access
color_matrix <- matrix(unlist(color_variations), ncol = 2, byrow = TRUE)

# Name the rows for easy access
rownames(color_matrix) <- original_colors

# Print the variations
print(color_matrix)