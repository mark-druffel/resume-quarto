if(F){
get_socials <- function(..., class = "header-icons"){
  ilist <- list(...)
  div <- htmltools::tags$div(
    class = "header-icons", 
    purrr::map(ilist, function(x){
      htmltools::tags$a(
        fontawesome::fa_i(x[["icon"]], class = x[["class"]]),
        href=x[["href"]],
        target=x[["target"]]
      )
    })
  )
  htmltools::HTML(div)
}
get_socials(
  website = list(icon = "home", href = 'https://www.mark-druffel.com/', class = "fa-icon", target = "_blank"),
  email = list(icon = "envelope", href = 'mailto:mark.druffel@gmail.com', class = "fa-icon", target = "_blank"),
  linkedin = list(icon = "linkedin", href = 'https://www.linkedin.com/in/markdruffel/', class = "fa-icon", target = "_blank"),
  gh = list(icon = "github", href = 'https://github.com/mark-druffel', class = "fa-icon", target = "_blank"),
  so = list(icon = "stack-overflow", href = 'https:/stackoverflow.com/users/6213809/mark-druffel', class = "fa-icon", target = "_blank")
)
}
htmltools::tags$div(
  class = "header-icons", 
  htmltools::tags$a(
    fontawesome::fa_i("home", class = "fa-icon-left"),
    href='https://www.mark-druffel.com/',
    target='_blank'
  ),
  htmltools::tags$a(
    fontawesome::fa_i("envelope", class = "fa-icon"),
    href='mailto:mark.druffel@gmail.com',
    target='_blank'
  ),
  htmltools::tags$a(
    fontawesome::fa_i("linkedin", class = "fa-icon"),
    href='https://www.linkedin.com/in/markdruffel/',
    target='_blank'
  ),
  htmltools::tags$a(
    fontawesome::fa_i("github", class = "fa-icon"),
    href='https://github.com/mark-druffel',
    target='_blank'
  ),
  htmltools::tags$a(
    fontawesome::fa_i("stack-overflow", class = "fa-icon"),
    href='https:/stackoverflow.com/users/6213809/mark-druffel',
    target='_blank'
  ) 
)

