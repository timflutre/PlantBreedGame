
tooltip_label <- function(label, description){

  tooltip <- tags$details(
    tags$summary(style = "cursor: pointer;",
     label, bsicons::bs_icon("question-circle")
    ),
    tags$blockquote(style = "font-weight: normal; font-size: inherit; font-style: italic;",
      description
    )
  )
}

collapible_section <- function(title, title_id, content) {

  tags$details(style = "margin-top: 30px;",
    tags$summary(
      h3(title, "(click to expand)",
         style = "margin-top: 0px; display: inline-block;",
         id = title_id
      )
    ),
    content
  )

}
