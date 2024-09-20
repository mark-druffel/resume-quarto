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
get_section <- function(yml_data, section){
  yml_section <- collapse::get_elem(yml_data, section, regex = T, ignore.case = T)
  if(section == names(yml_section)){
    yml_section <- yml_section[[section]]
  }
  return(yml_section)
}
main_or_aside <- function(){}



section_class <- function(main_or_aside){
  list("section-div", glue::glue("{main_or_aside}-section-div"))
}
section_id <- function(lower_title){
  glue::glue("{lower_title}-section-div")
}
section_header_class <- function(main_or_aside){
  list("section-header-div", glue::glue("{main_or_aside}-section-header-div"))
}
section_header_id <- function(lower_title){
  glue::glue("{lower_title}-section-header-div")
}
section_header_heading_class <- function(main_or_aside){
  list("h2", glue::glue("{main_or_aside}-h2"))
}
section_header_heading_id <- function(lower_title){
  glue::glue("{lower_title}-h2")
}
section_details_class <- function(main_or_aside){
  list("section-details-div", glue::glue("{main_or_aside}-section-details-div"))
}
section_details_entry_header_class <- function(main_or_aside){
  list("section-details-div", glue::glue("{main_or_aside}-section-details-div"))
}
section_details_id <- function(lower_title){
  glue::glue("{lower_title}-section-details-div")
}
icon_class <- function(main_or_aside){
  list("fa-icon", glue::glue("{main_or_aside}-fa-icon"))
}
icon_id <- function(icon){
  glue::glue("{icon}-fa-icon")
}
section_header_div <- function(section, lower_section, main_or_aside){
  section_header_div_ <- div( 
    class = section_header_class(main_or_aside),
    id = section_header_id(lower_section),
    h2(
      class = section_header_heading_class(main_or_aside),
      id = section_header_heading_id(lower_section),
      if(!is.null(icon)){fontawesome::fa_i(name = icon, class = icon_class(main_or_aside), id = icon_id(icon))},
      section
    )
  )
  return(section_header_div_)
}
section_header_padding_div <- function(){
  div(class = "section-header-border-padding")
}
section_div <- function(..., section, icon = NULL, main_or_aside){
  lower_section <- stringr::str_to_lower(section)
  section_div_ <- div(
    class = section_class(main_or_aside),
    id = section_id(lower_section),
    section_header_div(section, lower_section, main_or_aside), 
    section_header_padding_div(),
    ...
  )
  return(section_div_)
}


#############
# Header
#############
resume_header_div <- function(sections_list, main_or_aside){
  resume_header_div_ <- div( 
    class = list("resume-header", glue::glue("{main_or_aside}-section-header")),
    id = glue::glue("resume-section-header"),
    h1(
      class = glue::glue("{main_or_aside}-h1"),
      collapse::get_elem(yml, "name", recursive = T, regex = T, ignore.case = T) 
    )
  )
  return(section_header_div_)
}
contact_div <- function(sections_list){
  contact_list <- collapse::get_elem(yml, "contact", recursive = T, regex = T, ignore.case = T) 
  icon_elements <- icons <- purrr::map(contact_list, function(contact){
    icon_element <- htmltools::tags$a(
      fontawesome::fa_i(
        name = contact$icon, 
        class = "fa-icon",
        id = glue::glue("fa-{contact$icon}")
      ),
      href=contact$href,
      target="_blank"
    )
    return(icon_element)
  })
  div(
    class = "header-contacts",
    icon_elements
  )
}
header_div <- function(yml){
  contacts <- yml$contact
  icons <- purrr::map(contacts, function(contact){
    link <- htmltools::tags$a(
      fontawesome::fa_i(name = contact$icon, class = list("fa-icon-left", paste0("fa-", contact$icon))),
      href=contact$href,
      target="_blank"
    )
    return(link)
  })
  header_div_ <- div(
    class = list("section-top", "section-left", "header"),
    div(
      class = "header-name",
      h1(yml$name)
    ),
    div(
      class = "header-title",
      h2(yml$title)
    ),
    div(
      class = "header-icons",
      icons
    )
  )
  return(header_div_)
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
education_divs <- function(sections_list){
  edu_section <- get_section("Education")
  university_names <- names(edu_section)
  edu_divs <- purrr::map2(university_names, edu_sections, function(name, edu){
    div(
      class = "education-entry",
      h4(
        class = "h4-right",
        name
      ),
      div(
        class = "education-entry-flex",
        p(
          class = "p-right",
          edu$degree
        ),
        p(
          class = "p-right",
          edu$date
        )
      )
    )
  })
  return(edu_divs)
}
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


#library(collapse)
#library(tidyverse)
#coding_df <- function(yml){
#  coding_yml <- get_elem(yml, "Coding", recursive = T, regex = T) 
#  scoring_scale <- coding_yml$scoring_scale
#  languages <- coding_yml[names(coding_yml) != "scoring_scale"] 
#  coding_df <- tibble::enframe(languages) |> 
#    dplyr::mutate(value = as.numeric(value) / scoring_scale)
#  return(coding_df)
#}

#sidebar_yaml <- yaml::read_yaml("data/sidebar.yml")
#sidebar_yaml |> 
#  split_sidebar() |> 
#  coding_df()


yml_data <- read_data()
yml_main <- get_section(yml_data, "main")
yml_aside <- get_section(yml_data, "aside")
section <- "Education"
main_or_aside <- function(yml_main, section){
  sect
  if(section %in% names(yml_main)){
    main_or_aside <- "main"
  } else if(stringr::str_to_lower(section) %in% stringr::str_to_lower(names(yml_aside))){
    main_or_aside <- "aside"
  } else {
    stop("`main_or_aside` can't find section `{section}`")
  }
}


education_divs <- function(yml_data){
  edu_section <- get_section(yml_data, "Education")
  university_names <- names(edu_section)
  edu_divs <- purrr::map2(university_names, edu_sections, function(name, edu){
    div(
      class = "education-entry",
      h4(
        class = "h4-right",
        name
      ),
      div(
        class = "education-entry-flex",
        p(
          class = "p-right",
          edu$degree
        ),
        p(
          class = "p-right",
          edu$date
        )
      )
    )
  })
  return(edu_divs)
}


details_with_bullets_div <- function(..., section, icon = NULL, main_or_aside){
  lower_section <- stringr::str_to_lower(section)
  section_div_ <- div(
    class = section_class(main_or_aside),
    id = section_id(lower_section),
    section_header_div(section, lower_section, main_or_aside), 
    section_header_padding_div(),
    ...
  )
  return(section_div_)
}

assign()
search()



section_div <- function(..., section, icon = NULL, main_or_aside){
  lower_section <- stringr::str_to_lower(section)
  section_div_ <- div(
    class = section_class(main_or_aside),
    id = section_id(lower_section),
    section_header_div(section, lower_section, main_or_aside), 
    section_header_padding_div(),
    ...
  )
  return(section_div_)
}



glue_class <- function(selector, main_or_aside){
  list(selector, glue::glue("{main_or_aside}-{selector}"))
}
glue_id <- function(selector, section){
  glue::glue("{section}-{selector}")
}
create_section_divs <- function(section){
  section <- stringr::str_to_lower(section)
  main_or_aside <- main_or_aside(section)
  div( 
    class = section_header_class(main_or_aside),
    id = section_header_id(lower_section),
    ...
  )
}

section_header_div <- function(section, lower_section, main_or_aside){
  section_header_div_ <- div( 
    class = section_header_class(main_or_aside),
    id = section_header_id(lower_section),
    h2(
      class = section_header_heading_class(main_or_aside),
      id = section_header_heading_id(lower_section),
      if(!is.null(icon)){fontawesome::fa_i(name = icon, class = icon_class(main_or_aside), id = icon_id(icon))},
      section
    )
  )
  return(section_header_div_)
}
section_header_padding_div <- function(){
  div(class = "section-header-border-padding")
}
section_div <- function(..., section, icon = NULL, main_or_aside){
  lower_section <- stringr::str_to_lower(section)
  section_div_ <- div(
    class = section_class(main_or_aside),
    id = section_id(lower_section),
    section_header_div(section, lower_section, main_or_aside), 
    section_header_padding_div(),
    ...
  )
  return(section_div_)
}
