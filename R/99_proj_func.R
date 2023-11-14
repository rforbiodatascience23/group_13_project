# Function for cleaning names based on index
clean_selected_cols <- function(data, index, ...) {
  names(data)[index] <- janitor::make_clean_names(names(data)[index], ...)
  return(data)
}
