library(htmltools)
library(tidyverse)
#############
# Generics
#############
section_div <- function(..., title, icon = NULL, top = F, right = F, bottom = F, left = F){
  side <- ifelse(left, "-left", "-right")
  section_classes <- list(
    "section-vbreaker",
    ifelse(top, "section-top", ""), 
    ifelse(right, "section-right", ""),
    ifelse(bottom, "section-bottom", ""), 
    ifelse(left, "section-left", ""),
    paste0(stringr::str_to_lower(title), "-section")
  )
  section_classes[section_classes != ""]
  section_div_ <- div(
    class = section_classes,
    div( 
      class = list("section-header", paste0(stringr::str_to_lower(title), "-header")),
      h2(
        class = glue::glue("h2{side}"),
        fontawesome::fa_i(name = icon, class = glue::glue("fa-icon{side}")), 
        title
      )
    ),
    div(class = "section-header-border-padding"),
    ...
  )
  return(section_div_)
}
split_sidebar <- function(yml){
  sidebar_sections <- yml[["sidebar"]]
  return(sidebar_sections)
}
#############
# Header
#############
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
education_divs <- function(sidebar_sections){
  edu_sections <- sidebar_sections[["Education"]]
  edu_names <- names(edu_sections)
  edu_divs <- purrr::map2(edu_names, edu_sections, function(name, edu){
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

