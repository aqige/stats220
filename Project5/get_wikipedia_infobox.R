require("httr", character.only = TRUE)
require("rvest", character.only = TRUE)
require("tidyverse", character.only = TRUE)

text <- "Running this code will get data from the Wikipedia API.\nYou should only take what you need, once.\nDo you want to continue?"
user_input <- menu(c("Yes", "No"), title=text)

get_wikipedia_infobox <- function(search_query) {
  
  search_query_no_hon <- str_remove_all(search_query, "Hon |Rt ")
  message("Searching for: ", search_query_no_hon)
  
  wp_url <- "https://en.wikipedia.org/w/api.php"
  
  # Search for the most likely article
  search_response <- GET(
    url = wp_url,
    query = list(
      action = "query",
      list = "search",
      srsearch = paste(search_query_no_hon, "New Zealand politician"),
      format = "json",
      srlimit = 1
    )
  )
  
  search_data <- content(search_response, as = "parsed", type = "application/json")
  
  # If no match is found, return an empty tibble and move on
  if (length(search_data$query$search) == 0) {
    warning("No results found for query: ", search_query_no_hon)
    return(tibble())
  }
  
  resolved_title <- search_data$query$search[[1]]$title
  
  # Parse the resolved article page
  response <- GET(
    url = wp_url,
    query = list(
      action = "parse",
      page = resolved_title,
      format = "json",
      prop = "text",
      redirects = "true"
    )
  )
  
  data <- content(response, as = "parsed", type = "application/json")
  html_content <- data$parse$text$`*`
  page_html <- read_html(html_content)
  infobox_table <- html_element(page_html, "table.infobox")
  
  # If the page doesn't have a standard infobox, skip it
  if (is.na(infobox_table)) {
    warning("No infobox table found on page: ", resolved_title)
    return(tibble())
  }
  
  rows <- html_elements(infobox_table, "tr")
  results_list <- list()
  current_role <- "General Info"  
  
  # Loop through rows to extract structural data
  for (i in seq_along(rows)) {
    row <- rows[i]
    th_element <- row %>% html_element("th")
    td_element <- row %>% html_element("td")
    
    header_text <- th_element %>% html_text2()
    value_text  <- td_element %>% html_text2()
    
    if (is.na(header_text)) header_text <- ""
    if (is.na(value_text)) value_text <- ""
    
    has_colspan <- !is.na(html_attr(th_element, "colspan"))
    if (has_colspan && value_text == "" && header_text != "") {
      if (!header_text %in% c("Political career")) {
        current_role <- header_text
      }
      next 
    }
    
    if (header_text == "In office" && value_text == "") {
      results_list[[length(results_list) + 1]] <- tibble(role = current_role, label = "Status", value = "In office")
      next
    }
    
    if (header_text == "" && value_text != "" && !is.na(html_attr(td_element, "colspan"))) {
      results_list[[length(results_list) + 1]] <- tibble(role = current_role, label = "Dates", value = value_text)
      next
    }
    
    if (header_text != "" && value_text != "") {
      results_list[[length(results_list) + 1]] <- tibble(role = current_role, label = header_text, value = value_text)
    }
  }
  
  # Combine row results and add a column for the query context
  if (length(results_list) > 0) {
    output_df <- bind_rows(results_list) %>% 
      mutate(minister = search_query, page_name = resolved_title) %>% 
      select(minister, page_name, role, label, value) # Nicely ordered columns
    return(output_df)
  } else {
    return(tibble())
  }
}
