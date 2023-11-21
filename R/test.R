# Install and load required packages
install.packages("tidyr")
library(tidyr)

# Create a sample data frame
data <- data.frame(
  ID = 1:3,
  Value1_A = c(10, 20, 30),
  Value2_A = c(40, 50, 60),
  Value1_B = c(70, 80, 90),
  Value2_B = c(100, 110, 120)
)

# Display the original data frame
print("Original Data:")
print(data)

# Pivot longer using two sets of values
data_long <- data %>%
  pivot_longer(
    cols = starts_with("Value"),
    names_to = c(".value", "Set"),
    names_sep = "_"
  )

# Display the resulting long-format data frame
print("\nData Long:")
print(data_long)
