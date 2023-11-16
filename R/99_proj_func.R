# Function for cleaning names based on index
clean_selected_cols <- function(data, index, ...) {
  names(data)[index] <- janitor::make_clean_names(names(data)[index], ...)
  return(data)
}

# Function for cleaning names based on index
clean_selected_cols <- function(data, index, ...) {
  names(data)[index] <- janitor::make_clean_names(names(data)[index], ...)
  return(data)
}

# Function to extract libraries from a file
extract_libraries <- function(file_path) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr")
  }
  
  file_content <- readLines(file_path)
  
  # Extract regex matches for libraries
  library_pattern = 'library\\(["\']([^"\']+)["\']\\)'
  library_matches <- stringr::str_extract_all(file_content,
                                              library_pattern)
  
  # Extract library names from regex matches
  name_pattern = '["\']([^"\']+)["\']'
  library_names <- unlist(lapply(library_matches, 
                                 function(match) stringr::str_match(match, name_pattern)[, 2]))
  
  return(library_names)
}

# Function to check and install missing libraries from entire project
check_and_install_libraries <- function(dir_path) {
  for (file_path in list.files(dir_path)){
    # Extract library names
    required_libraries <- extract_libraries(paste0(dir_path, '/', file_path))
    
    # Install libraries if not installed
    for (library_name in required_libraries) {
      if (!requireNamespace(library_name, quietly = TRUE)) {
        install.packages(library_name, dependencies = TRUE)
      }
    }
  }
  print("All required libraries checked and installed.")
}

