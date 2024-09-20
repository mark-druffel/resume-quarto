library(collapse)
library(htmltools)
library(tidyverse)
library(lorem)
##########################################
# Data Generics
##########################################
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
check_main_or_aside <- function(section, resume_data, skip = NULL){
  if(is.null(skip)){
    main_data <- get_section("main", resume_data)
    aside_data <- get_section("aside", resume_data)
    purrr::map(aside_data, function(section_data){
      section_names <- names(section_data)
      return(section_names)
    }) 
    if(section %in% stringr::str_to_lower(names(main_data))){
      main_or_aside <- "main"
    } else if(section %in% stringr::str_to_lower(names(aside_data))){
      main_or_aside <- "aside"
    } else {
      stop(glue::glue("`main_or_aside` can't find section `{section}`"))
    }
  } else {
    main_or_aside <- skip
  }
  return(main_or_aside)
}
glue_class <- function(main_or_aside,selector = ""){
  # Wanted to list any html selectors, but paged.js adds in selectors over mine so it's not working 
  #html_selectors <- purrr::map(unlist(stringr::str_split(selector, "-")), function(selector_part){
  #  names(htmltools::tags)[names(htmltools::tags) == selector_part]
  #}) |> unlist()
  full_class <- stringr::str_remove_all(
    glue::glue("{main_or_aside}-{stringr::str_to_lower(selector)}"), 
    "^-|-$"
  )
  return(full_class)
}
glue_id <- function(main_or_aside, selector, name){
  glue::glue("{main_or_aside}-{stringr::str_to_lower(selector)}-{name}")
}
get_div_data <- function(section, ...){
  section <- stringr::str_to_lower(section)
  resume_data <- read_data()
  section_data <- get_section(section = section, resume_data = resume_data) 
  main_or_aside <- check_main_or_aside(section = section, resume_data = resume_data, ...)
  div_data <- list(
    resume_data = resume_data, 
    section_data = section_data, 
    main_or_aside = main_or_aside,
    section_title = stringr::str_to_title(section)
  )
  return(div_data)
}
##########################################
# HTML Generics
##########################################
get_div_section_details_with_two_p_flexrow <- function(section){
  div_data <- get_div_data(section)
  details_div <- purrr::map2(names(div_data$section_data), div_data$section_data, function(name, entry){
    div(
      class = glue_class(div_data$main_or_aside, glue::glue("{section}-details")),
      h4(
        class = glue_class(div_data$main_or_aside, "h4"),
        name
      ),
      div(
        class = "flex-row-space-between-center",
        p(
          class = glue_class(div_data$main_or_aside, "p"),
          entry$degree
        ),
        p(
          class = glue_class(div_data$main_or_aside, "p"),
          entry$date
        )
      )
    )
  })
  return(details_div)
}
section_div <- function(section, icon = NULL){
  div_data <- get_div_data(section)
  function(...){
    section_div_ <- div(
      class = glue_class(div_data$main_or_aside, section),
      h2(
        class = glue_class(div_data$main_or_aside, "h2"),
        id = glue_id(div_data$main_or_aside, "h2", glue::glue("{section}-title")),
        if(!is.null(icon)){
          fontawesome::fa_i(name = icon, class = glue_class(div_data$main_or_aside, "fa-icon"), id = glue_id(div_data$main_or_aside, "fa-icon", glue::glue("{section}-icon")))
        },
        div_data$section_title
      ),
      div(class = "section-header-border-padding"),
      ...
    )
    return(section_div_)
  }
}
resume_data <- read_data()
##########################################
# Header
##########################################
header_div <- function(resume_data){
  header_data <- get_section("header", resume_data)
  contact_icons <- get_section("contact", header_data)
  icon_elements <- icons <- purrr::map(contact_icons, function(icon){
    icon_element <- htmltools::tags$a(
      class = list("contact-icon", "contact-icon-main"),
      fontawesome::fa_i(
        name = icon$icon, 
        class = list("fa-icon", "fa-icon-main"),
        id = glue::glue("fa-header-{icon$icon}")
      ),
      href=icon$href,
      target="_blank"
    )
    return(icon_element)
  })
  resume_header_div_ <- div(
    class = list("resume-header"),
    h1(header_data$name, class = list("main-h1","resume-header-name")),
    h2(header_data$title, class = list("main-h2", "resume-header-title")),
    div(
      class = list("resume-header-contact"),
      icon_elements
    )
  )
  return(resume_header_div_)
}
##########################################
# Objective
##########################################
get_objective_desc_p <-function(section = "objective"){
  div_data <- get_div_data(section)
  p(
    class = glue_class(div_data$main_or_aside, "p"),
    div_data$section_data$desc
  )
}
objective_div <- section_div("objective", icon = "location-crosshairs")
objective_details_div <- get_objective_desc_p()

##########################################
# Experience
##########################################
get_experience_details_div <-function(section = "experience"){
  div_data <- get_div_data(section)
  company_divs <- purrr::map2(names(div_data$section_data), div_data$section_data, function(company_name, entry){
    company_div <- div(
      class = glue_class(div_data$main_or_aside, glue::glue("{section}-company-header")), 
      h3(
        class = glue_class(div_data$main_or_aside, glue::glue("{section}-company-name")),
        id = glue_id(div_data$main_or_aside, glue::glue("{section}-company-name"), company_name),
        company_name
      ),
      p(
        class = glue_class(div_data$main_or_aside, glue::glue("{section}-company-location")),
        id = glue_id(div_data$main_or_aside, glue::glue("{section}-company-location"), company_name),
        fontawesome::fa_i(
          "location-dot", 
          class = glue_class(div_data$main_or_aside, glue::glue("{section}-company-location-icon")),
          id = glue_id(div_data$main_or_aside, glue::glue("{section}-company-location"), company_name)
        ),
        entry$location
      )
    )
    has_shared_role_desc <- !is.null(entry$desc)
    if(has_shared_role_desc){
      shared_role_desc <- p(
        class=glue_class(div_data$main_or_aside, glue::glue("{section}-role-desc")),
        entry$desc
      ) 
    }
    is_role <- unlist(purrr::map(entry, is.list)) |> as.logical()
    roles <- entry[is_role]
    role_names <- names(roles)
    role_divs <- purrr::map2(role_names, roles, function(role_name, role){
      bullets <- purrr::map(role$bullets, htmltools::tags$li, class = glue_class(div_data$main_or_aside, "li"))
      role_div <- div(
        class = glue_class(div_data$main_or_aside, glue::glue("{section}-role")), 
        div(
          class = glue_class(div_data$main_or_aside, glue::glue("{section}-role-title")),
          h4(
            class = glue_class(div_data$main_or_aside, glue::glue("{section}-role-name")),
            role_name
          ),
          p(
            class = glue_class(div_data$main_or_aside, glue::glue("{section}-role-dates")),
            role$dates
          ),
        ),
        div(
          class = "experience-role-details", 
          p(
            class=glue_class(div_data$main_or_aside, glue::glue("{section}-role-desc")),
            role$desc
          ),
          p(
            class = glue_class(div_data$main_or_aside, glue::glue("{section}-role-bullets")),
            bullets
          )
        )
      )
      return(role_div)
    })
    company_div <- div(
      class = glue_class(div_data$main_or_aside, "company"), 
      company_div,
      role_divs,
      if(has_shared_role_desc){shared_role_desc},
    )
    return(company_div)
  })
  return(company_divs)
}
experience_div <- section_div("experience", "route")
experience_details_div <- get_experience_details_div()

##########################################
# Education
##########################################
education_div <- section_div("education", "graduation-cap")
education_details_div <- get_div_section_details_with_two_p_flexrow("education")
##########################################
# Skills
# Using ggplot with htmltools presents some challenges, but Quarto handles all this for me if 
# I just output ggplot with no html. I'm going to use a separate code block for now to avoid the 
# htmltools challenge. This post gives a solution that might be useful if I decide to try to use 
# ggplot with htmltools - https://stackoverflow.com/questions/50244709/how-to-store-r-ggplot-graph-as-html-code-snippet
# 
# Now I'm wanting to consolidate tools and coding into technical skills with subsections, but ggplot print makes that very clumsy
# Going to just write scripts for now and redesign this later. 
##########################################
section <- "Soft Skills"
soft_skills_div <- section_div(section, "comments")
soft_skills_div <- htmltools::withTags(
  soft_skills_div(
    ul(
      class = "aside-tools-section-list",
      li(
        class = "aside-tools-section-list-item",
        "Strategy"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Presentation"
      ),
      li(
        class = "aside-tools-section-list-item",
        "People Mgmt"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Project Mgmt"
      )
    )
  )
)

section <- "Coding"
coding_div <- section_div(section, "laptop-code")
div_data <- get_div_data(section)
plot_skills <- function(section = "coding"){
  div_data <- get_div_data(section, skip = "aside")
  scoring_scale <- div_data$section_data$scoring_scale
  skills <- div_data$section_data[names(div_data$section_data) != "scoring_scale"] 
  skills_df <- tibble::enframe(skills) |> 
    dplyr::mutate(value = as.numeric(value) / scoring_scale) |> 
    mutate(perfect = 1.0) 
  skills_df$order <- seq.int(from =1 , to = length(skills), by = 1)
  # Eventually would like to adjust this based on parsing css
  margin_ <-  margin(0, 2, 0, 0, "cm")
  lollipop_chart <- skills_df |> 
    ggplot(aes(x=value, y=reorder(name, -order))) +
    geom_segment( aes(x=0, xend=perfect, y=name, yend=name), linewidth = 2, color="#606060") +
    geom_point(color = "#ffffff", fill = "#ffffff", size = 7, alpha = 1, shape = "\u275A") +
    theme_void() +
    labs(title = NULL, y = NULL, x = NULL) +
    theme(
      #plot.title = element_text(size = 32, family = "Overpass", color = "#ffffff", hjust = .5),
      axis.text.y = element_text(size = 28, family = "Overpass", face = "bold", color = "#ffffff", hjust = 1),
      plot.background = element_rect(fill = "#333333", color = "#333333"),
      panel.background = element_rect(fill = "#333333", color = "#333333"),
      plot.margin = margin_
    )
  purrr::walk(list(lollipop_chart), print)
}
section <- "Tools"
tools_div <- section_div(section, icon = "wrench")
div_data <- get_div_data(section, skip = "aside")
tools_details <- purrr::map2(names(div_data$section_data), div_data$section_data, function(section, tools){
  div(
    class = glue_class(div_data$main_or_aside, "tools-section"),
    h4(
      class = glue_class(div_data$main_or_aside, "tools-subsection-header"),
      section
    ),
    htmltools::tags$ul(
      class = glue_class(div_data$main_or_aside, "tools-section-list"),
      purrr::map(tools, htmltools::tags$li, class = glue_class(div_data$main_or_aside, "tools-section-list-item"))
    )
  )
})
tools_div <- tools_div(
  tools_details
)
# coding_barchart <- function(coding_df, margin_ = margin(0, 2, 0, 0.5, "cm")){
#   barchart <- coding_df |> 
#     mutate(barfill = 1) |> 
#     ggplot(aes(y = name)) +
#     geom_col(aes(x = barfill), fill = "#606060",  width = .55) +
#     geom_col(aes(x = value), fill = "#ffffff",  width = .55) +
#     theme_void() +
#     labs(y = NULL, x = NULL) +
#     theme(
#       axis.text.y = element_text(size = 15, family = "Overpass", face = "bold", color = "#ffffff", hjust = 1),
#       plot.background = element_rect(fill = "#333333", color = "#333333"),
#       panel.background = element_rect(fill = "#333333", color = "#333333"),
#       plot.margin = margin_
#     )
#   return(barchart)
# }
# 
# coding_lollipop <- function(coding_df, margin_ = margin(0, 2, 0, 0.5, "cm")){
#   chart <- coding_df |> 
#     mutate(perfect = 1.0) |> 
#     ggplot(aes(x=value, y=reorder(name, -order))) +
#     geom_segment( aes(x=0, xend=perfect, y=name, yend=name), color="#606060") +
#     geom_point(color = "#ffffff", fill = "#ffffff", size = 4, alpha = 1, shape = "\u275A") +
#     theme_void() +
#     labs(y = NULL, x = NULL) +
#     theme(
#       axis.text.y = element_text(size = 20, family = "Overpass", face = "bold", color = "#ffffff", hjust = 1),
#       plot.background = element_rect(fill = "#333333", color = "#333333"),
#       panel.background = element_rect(fill = "#333333", color = "#333333"),
#       plot.margin = margin_
#     )
#   return(chart)
# }
##########################################
# Tools
##########################################
#tools_div <- section_div("Tools", "wrench")
# get_tools_details_div <- function(section = "tools"){
#   div_data <- get_div_data(section)
#   tool_divs <- purrr::map2(names(div_data$section_data), div_data$section_data, function(tool_section, tool_data){
#     div(
#       class = glue_class(div_data$main_or_aside, glue::glue("tools-section")),
#       h3(
#         class = glue_class(div_data$main_or_aside, "tools-h3"),
#         tool_section
#       ),
#       purrr::map2(names(tool_data), tool_data, function(tool_subsection, tool_subdata){
#         div(
#           class = glue_class(div_data$main_or_aside, glue::glue("tools-subsection")), 
#           h4(
#             class = glue_class(div_data$main_or_aside, "tools-h4"),
#             tool_subsection
#           ),
#           p(
#             class = glue_class(div_data$main_or_aside, "p"),
#             paste(tool_subdata, collapse = ", ")
#           )
#         )
#       })
#     )
#   })
#   return(tool_divs)
# }

# div_data <- get_div_data("tools")
# tools_details <- purrr::map2(names(div_data$section_data), div_data$section_data, function(section, tools){
#   div(
#     class = glue_class(div_data$main_or_aside, "tools-section"),
#     h4(
#       class = glue_class(div_data$main_or_aside, "tools-section-header"),
#       section
#     ),
#     htmltools::tags$ul(
#       class = glue_class(div_data$main_or_aside, "tools-section-list"),
#       purrr::map(tools, htmltools::tags$li, class = glue_class(div_data$main_or_aside, "tools-section-list-item"))
#     )
#   )
# })

##########################################
# Volunteer
##########################################
volunteer_div <- section_div("Volunteer", "hand")
div_data <- get_div_data("volunteer")
volunteer_details <- purrr::map2(names(div_data$section_data), div_data$section_data, function(org, org_data){
  role_name <- names(org_data)
  div(
    class = glue_class(div_data$main_or_aside, "volunteer-section"),
    h3(
      class = glue_class(div_data$main_or_aside, "volunteer-section-org"),
      org
    ),
    h4(
      class = glue_class(div_data$main_or_aside, "volunteer-section-role"),
      role_name
    ),
    p(
      class = glue_class(div_data$main_or_aside, "p"),
      org_data
    )
  )
})

hobbies_div <- section_div("Hobbies", "mountain")
hobbies_div <- htmltools::withTags(
  hobbies_div(
    ul(
      class = "aside-tools-section-list",
      li(
        class = "aside-tools-section-list-item",
        "Rock Climbing"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Hiking"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Biking"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Cooking"
      ),
      li(
        class = "aside-tools-section-list-item",
        "Reading"
      )
    )
  )
)
