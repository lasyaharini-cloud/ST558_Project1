# Variables required by the project
pums_variables <- c(
  "AGEP",
  "GASP",
  "GRPIP",
  "JWAP",
  "JWDP",
  "JWMNP",
  "PWGTP",
  "FER",
  "HHL",
  "SCH",
  "SCHL",
  "SEX"
)


# Download metadata for one year
get_variable_metadata <- function(year) {
  
  # Only these years are allowed
  valid_years <- 2021:2024
  
  if (length(year) != 1 || !year %in% valid_years) {
    stop(
      "year must be one of 2021, 2022, 2023, or 2024."
    )
  }
  
  
  # The state variable has a different name depending on year
  if (year %in% c(2021, 2022)) {
    state_variable <- "ST"
  } else {
    state_variable <- "STATE"
  }
  
  
  # Complete list of variables to retain
  variables_to_keep <- c(
    pums_variables,
    state_variable,
    "REGION",
    "DIVISION"
  )
  
  
  # Construct the appropriate Census metadata URL
  url <- paste0(
    "https://api.census.gov/data/",
    year,
    "/acs/acs1/pums/variables.json"
  )
  
  
  # Send the request to the Census API
  response <- httr::GET(url)
  
  
  # Stop if the request was unsuccessful
  httr::stop_for_status(response)
  
  
  # Convert the response into text
  response_text <- httr::content(
    response,
    as = "text",
    encoding = "UTF-8"
  )
  
  
  # Parse the JSON text into an R list
  response_list <- jsonlite::fromJSON(
    response_text,
    simplifyVector = FALSE
  )
  
  
  # Extract the variables section
  all_variables <- response_list$variables
  
  
  # Check whether all required variables were found
  missing_variables <- setdiff(
    variables_to_keep,
    names(all_variables)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "The following variables were not found for ",
      year,
      ": ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  
  # Return only the variables needed for this project
  all_variables[variables_to_keep]
}


# Years required by the assignment
years <- 2021:2024


# Run the function once for each year
census_variable_values <- setNames(
  lapply(years, get_variable_metadata),
  as.character(years)
)


# Save all four yearly lists in one RDS file
saveRDS(
  census_variable_values,
  file = "data/census_variable_values.rds"
)