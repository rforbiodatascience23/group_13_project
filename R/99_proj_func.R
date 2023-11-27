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

# Function to extract packages used in a file via regex match
extract_packages <- function(file_path) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr")
  }

  file_content <- readLines(file_path)

  # Extract regex matches for libraries
  library_pattern <- 'library\\(["\']([^"\']+)["\']\\)'
  library_matches <- stringr::str_extract_all(
    file_content,
    library_pattern
  )

  # Extract library names from regex matches
  name_pattern <- '["\']([^"\']+)["\']'
  package_names <- unlist(lapply(
    library_matches,
    function(match) stringr::str_match(match, name_pattern)[, 2]
  ))

  return(package_names)
}

# Function to check if a package is on CRAN
is_cran_package <- function(package_name) {
  available_cran_packages <- rownames(available.packages())
  return(package_name %in% available_cran_packages)
}

# Function to check if a package is on Bioconductor
is_bioconductor_package <- function(package_name) {
  available_bioconductor_packages <- BiocManager::available()
  return(package_name %in% available_bioconductor_packages)
}

# Function to check and install missing libraries from entire project
BiocManager::repositories()
check_and_install_packages <- function(dir_path) {
  # Install Biocmanager if not already installed
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
    BiocManager::install()
  }
  
  # Install devtools if not already installed
  if (!requireNamespace("devtools", quietly = TRUE)) {
    install.packages("devtools")
  }
  
  # Install installing ggdendroplot via devtools
  if (!requireNamespace("ggdendroplot", quietly = TRUE)) {
    devtools::install_github("nicolash2/ggdendroplot")
  }
  
  # For each file find all libraries
  for (file_path in list.files(dir_path)) {
    # Extract library names
    required_packages <- extract_packages(paste0(dir_path, "/", file_path))

    # Install libraries if not installed
    for (package_name in required_packages) {
      if (!requireNamespace(package_name, quietly = TRUE)) {
        # Check if part of CRAN or bioconductor and install from these
        if (is_cran_package(package_name)) {
          install.packages(package_name, dependencies = TRUE)
        } 
        else if (is_bioconductor_package(package_name)) {
          BiocManager::install(package_name)
        } 
        else {
          cat(sprintf("%s is not on CRAN or Bioconductor.\n", package_name))
        }
      }
    }
  }
}


# Clustering function to perform kmeans clustering on a row of expression values
kmeans_cluster_row <- function(transcript_expr_data) {
  # Transpose tibble to fit into the kmeans function
  transposed_tibble <- transcript_expr_data |>
    pivot_longer(
      cols = everything(),
      names_to = "sample_id"
    )

  # Clustering
  cl <- transposed_tibble |>
    select(value) |>
    kmeans(centers = 2)

  # Chain clustering states to each expression value
  cluster_tibble <- transposed_tibble |>
    mutate(cluster_int = cl$cluster) |>
    pivot_wider(
      names_from = sample_id,
      values_from = value
    )

  return(cluster_tibble)
}

# Average expression fold change check
fold_change <- function(val_1, val_2) {
  fold_change <- max(val_1, val_2) - min(val_1, val_2)
  return(fold_change)
}
