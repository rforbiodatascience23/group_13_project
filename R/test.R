library(stringr)

# Example string
text <- "This is an example (123) with some (456) groups."

# Define a regular expression with capturing groups
pattern <- "\\((\\d+)\\)"

# Use str_match to extract the first capturing group
matches <- str_match(text, pattern)

matches

# Extract the first capturing group
if (!is.na(matches[1, 2])) {
  result <- matches[1, 2]
  cat("Extracted value:", result, "\n")
} else {
  cat("No match found.\n")
}
