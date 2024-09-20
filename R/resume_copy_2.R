library(collapse)
library(htmltools)
library(tidyverse)
#############
# Generics
#############
read_data <- function(path = "data/"){
  path <- fs::path_wd(path)
  filepaths <- fs::dir_ls(path, type = "file", regexp = "[.]yml$|[.]yaml$⁠⁠")
  yml_list <- purrr::map(filepaths, yaml::read_yaml)
  return(yml_list)
}
get_section <- function(section, resume_data, drop_internals = T){
  section_data <- collapse::get_elem(resume_data, section, regex = T, ignore.case = T)
  if(length(section_data) == 1){
    if(section == names(section_data)){
      section_data <- section_data[[section]]
    }  
  }
  section_data[stringr::str_detect(names(section_data), pattern = "_$", negate = T)]
  return(section_data)
}
glue_id <- function(section, selector){
  glue::glue("{section}-{selector}")
}

main_or_aside <- function(section, resume_data){
  main_data <- get_section("main", resume_data)
  aside_data <- get_section("aside", resume_data)
  if(section %in% stringr::str_to_lower(names(main_data))){
    main_or_aside_ <- "main"
  } else if(section %in% stringr::str_to_lower(names(aside_data))){
    main_or_aside_ <- "resume-aside"
  } else {
    stop("`main_or_aside` can't find section `{section}`")
  }
  return(main_or_aside_)
}
create_section_div <- function(section, main_or_aside, section_data){
  
    div(
      class = list("section"),
      id = glue_id(section, "section"),
      h2(
        class = list("section-header"),
        id = glue_id(section, "section-header"),
        if(!is.null(icon)){
          fontawesome::fa_i(name = icon, id = glue_id(section, glue::glue("fa-{icon}")))
        },
        stringr::str_to_title(section)
      ),
      div(
        class = list("section-details", main_or_aside_),
        id = glue_id(selector = "section-details", section_),
        ...
      )
    )
  }
}
details_selector <- "section-details-entry"
create_section_config <- function(section){
  resume_data <- read_data()
  main_or_aside_ <- main_or_aside(section, resume_data)
  section_data <- get_section(section, resume_data)
  section_div <- create_section_div(section, main_or_aside_, section_data)
  entries_data <- section_data[stringr::str_detect(names(section_data), pattern = "_$", negate = T)]
  return(
    list(section = section, main_or_aside = main_or_aside_, section_data = section_data, section_div = section_div, entries_data = entries_data)
  )
}
#############
# Header
#############

header_div <- function(header_data =  get_section("header", resume_data = read_data())){
  contact_icons <- collapse::get_elem(header_data, "contact", recursive = T, regex = T, ignore.case = T) 
  icon_elements <- icons <- purrr::map(contact_icons, function(icon){
    icon_element <- htmltools::tags$a(
      fontawesome::fa_i(
        name = icon$icon, 
        class = list("fa-icon", "main-fa-icon", "resume-header-fa-icon"),
      ),
      href=icon$href,
      target="_blank"
    )
    return(icon_element)
  })
  resume_header_div_ <- div(
    class = list("resume-header", "flex-column", "main-flex-column"),
    div(
      class = list("resume-header-name"),
      h1(header_data$name)
    ),
    div(
      class = list("resume-header-title"),
      h2(header_data$title)
    ),
    div(
      class = list("resume-header-contact", "flex-row-center-center"),
      icon_elements
    )
  )
  return(resume_header_div_)
}
#############
# Experience
#############
split_jobs <- function(yml){
  jobs <- purrr::map2(names(yml), yml, function(company_name, yml){
    yml$company_name = company_name
    return(yml)
  })
  return(jobs)
}
job_divs <- function(jobs){
  job_divs_ <- purrr::map(jobs, function(job){
    company_div <- div(
      class = "experience-company", 
      h3(
        class = ".h3-left",
        job$company_name
      ),
      p(
        class = ".p-left",
        fontawesome::fa_i("location-dot", class = "fa-icon"),
        job$location
      )
    )
    is_role <- unlist(purrr::map(job, is.list)) |> as.logical()
    roles <- job[is_role]
    role_names <- names(roles)
    role_divs <- purrr::map2(role_names, roles, function(role_name, role){
      bullets <- purrr::map(role$bullets, htmltools::tags$li, class=".li-left")
      role_div <- div(
        class = "experience-role", 
        div(
          class = "experience-role-title", 
          h4(role_name,  class = ".h4-left"),
          p(role$dates, class = ".p-left")
        ),
        div(
          class = "experience-role-details", 
          p(role$desc, bullets, class=".p-left"),
        )
      )
      return(role_div)
    })
    job_div <- div(
      class = "job", 
      company_div,
      role_divs
    )
    return(job_div)
  })
  return(job_divs_)
}
#############
# Education
#############
education_config <- create_section_config("education")
details_div <- purrr::map2(names(education_config$entries_data), education_config$entries_data, function(name, entry){
  div(
    class = list(details_selector, education_config$main_or_aside),
    h4(
      class = list("h4", education_config$main_or_aside),
      name
    ),
    div(
      class = "flex-row-space-between-center",
      p(
        class = list("p", education_config$main_or_aside),
        entry$degree
      ),
      p(
        class = list("p", education_config$main_or_aside),
        entry$date
      )
    )
  )
})
education_div <- education_config$section_div(
  details_div
)
#############
# Coding
#############

coding_df <- function(sidebar_sections){
  coding_sections <- sidebar_sections[["Skills"]][["Coding"]]
  scoring_scale <- coding_sections$scoring_scale
  languages <- coding_sections[names(coding_sections) != "scoring_scale"] 
  coding_df <- tibble::enframe(languages) |> 
    dplyr::mutate(value = as.numeric(value) / scoring_scale) |> 
    mutate(perfect = 1.0) 
  coding_df$order <- seq.int(from =1 , to = length(languages), by = 1)
  return(coding_df)
}
coding_barchart <- function(coding_df, margin_ = margin(0, 2, 0, 0.5, "cm")){
  barchart <- coding_df |> 
    mutate(barfill = 1) |> 
    ggplot(aes(y = name)) +
    geom_col(aes(x = barfill), fill = "#606060",  width = .55) +
    geom_col(aes(x = value), fill = "#ffffff",  width = .55) +
    theme_void() +
    labs(y = NULL, x = NULL) +
    theme(
      axis.text.y = element_text(size = 15, family = "Overpass", face = "bold", color = "#ffffff", hjust = 1),
      plot.background = element_rect(fill = "#333333", color = "#333333"),
      panel.background = element_rect(fill = "#333333", color = "#333333"),
      plot.margin = margin_
    )
  return(barchart)
}

coding_lollipop <- function(coding_df, margin_ = margin(0, 2, 0, 0.5, "cm")){
  chart <- coding_df |> 
    mutate(perfect = 1.0) |> 
    ggplot(aes(x=value, y=reorder(name, -order))) +
    geom_segment( aes(x=0, xend=perfect, y=name, yend=name), color="#606060") +
    geom_point(color = "#ffffff", fill = "#ffffff", size = 4, alpha = 1, shape = "\u275A") +
    theme_void() +
    labs(y = NULL, x = NULL) +
    theme(
      axis.text.y = element_text(size = 20, family = "Overpass", face = "bold", color = "#ffffff", hjust = 1),
      plot.background = element_rect(fill = "#333333", color = "#333333"),
      panel.background = element_rect(fill = "#333333", color = "#333333"),
      plot.margin = margin_
    )
  return(chart)
}


#############
# Tools / Frameworks
#############



read_data <- function(path = "data/"){
  path <- fs::path_wd(path)
  filepaths <- fs::dir_ls(path, type = "file", regexp = "[.]yml$|[.]yaml$⁠⁠")
  yml_list <- purrr::map(filepaths, yaml::read_yaml)
  return(yml_list)
}
get_section <- function(section, resume_data, drop_internals = T){
  section_data <- collapse::get_elem(resume_data, section, regex = T, ignore.case = T)
  if(length(section_data) == 1){
    if(section == names(section_data)){
      section_data <- section_data[[section]]
    }  
  }
  section_data[stringr::str_detect(names(section_data), pattern = "_$", negate = T)]
  return(section_data)
}
glue_id <- function(section, selector){
  glue::glue("{section}-{selector}")
}

main_or_aside <- function(section, resume_data){
  main_data <- get_section("main", resume_data)
  aside_data <- get_section("aside", resume_data)
  if(section %in% stringr::str_to_lower(names(main_data))){
    main_or_aside_ <- "main"
  } else if(section %in% stringr::str_to_lower(names(aside_data))){
    main_or_aside_ <- "resume-aside"
  } else {
    stop("`main_or_aside` can't find section `{section}`")
  }
  return(main_or_aside_)
}
create_section_div <- function(section, main_or_aside, section_data){
  
  div(
    class = list("section"),
    id = glue_id(section, "section"),
    h2(
      class = list("section-header"),
      id = glue_id(section, "section-header"),
      if(!is.null(icon)){
        fontawesome::fa_i(name = icon, id = glue_id(section, glue::glue("fa-{icon}")))
      },
      stringr::str_to_title(section)
    ),
    div(
      class = list("section-details", main_or_aside_),
      id = glue_id(selector = "section-details", section_),
      ...
    )
  )
}
}
details_selector <- "section-details-entry"
create_section_config <- function(section){
  resume_data <- read_data()
  main_or_aside_ <- main_or_aside(section, resume_data)
  section_data <- get_section(section, resume_data)
  section_div <- create_section_div(section, main_or_aside_, section_data)
  entries_data <- section_data[stringr::str_detect(names(section_data), pattern = "_$", negate = T)]
  return(
    list(section = section, main_or_aside = main_or_aside_, section_data = section_data, section_div = section_div, entries_data = entries_data)
  )
}
