library(shiny)

none_value <- "-- None --"

data_viz_ui <- function(id) {
  ns <- NS(id)
  div(
    div(style = "display: flex;",
      div(style = "flex: 0;",
        selectInput(ns("x_var"),
          "X variable",
          choices = list(none_value),
          multiple = FALSE
        ),

        selectInput(ns("y_var"),
          "Y variable",
          choices = list(none_value),
          multiple = FALSE
        ),
        selectInput(ns("col_var"),
          "Color variable",
          choices = list(none_value),
          multiple = FALSE
        ),
        uiOutput(ns("plotWarning"))
      ),
      div(style = "flex: 1;",
        withSpinner(plotlyOutput(ns("plot")))
      )
    ),
    hr(),
    div(style = "margin-top: 30px;",
      id = ns("data-table"),
      withSpinner(dataTableOutput(ns("dataTable")))
    )
  )
}

data_viz_server <- function(id, plot_data) {
  moduleServer(id, function(input, output, session) {

    observe({
      data <- plot_data()
      if (is.null(data)) {
        updateSelectInput(session, "x_var",
          choices = none_value,
        )
        updateSelectInput(session, "y_var",
          choices = none_value,
        )
        updateSelectInput(session, "col_var",
          choices = none_value,
        )
        return(NULL)
      }

      var_list <- colnames(data)
      # browser()
      var_list <- c(none_value, var_list)
      updateSelectInput(session, "x_var",
        choices = var_list,
        selected = ifelse(input$x_var %in% var_list, input$x_var, none_value)
      )
      updateSelectInput(session, "y_var",
        choices = var_list,
        selected = ifelse(input$y_var %in% var_list, input$y_var, none_value)
      )
      updateSelectInput(session, "col_var",
        choices = var_list,
        selected = ifelse(input$col_var %in% var_list, input$col_var, none_value)
      )
    })

    output$plotWarning <- renderUI({
      warning_messages <- plot_warning(
        data = plot_data(),
        x_var = input$x_var,
        y_var = input$y_var,
        col_var = input$col_var
      )

      if (length(warning_messages) == 0) {
        return(span())
      }

      div(class = "warning",
        p(bsicons::bs_icon("exclamation-diamond-fill"), "Warning:"),
        tags$ul(
          lapply(warning_messages, tags$li)
        )
      )
    })

    output$plot <- renderPlotly({
      data <- plot_data()
      if ((input$x_var == none_value && input$y_var == none_value) || is.null(data)) {
        return(empty_plot("No data to show"))
      }

      if (input$x_var == none_value || input$y_var == none_value) {
        return(
          plot_1D(
            data = data,
            x_var = input$x_var,
            y_var = input$y_var,
            col_var = input$col_var
          )
        )
      }

      return(
        plot_2D(
          data = data,
          x_var = input$x_var,
          y_var = input$y_var,
          col_var = input$col_var
        )
      )
    })

    output$dataTable <- renderDataTable({
      data <- plot_data()
      filter <- "top"
      if (is.null(data)) {
        data <- data.frame(`Variable` = numeric())
        filter <- "none"
      }
      DT::datatable(data,
        filter = filter,
        style = "bootstrap4",
        options = list(
          language = list(emptyTable = 'Empty'),
          pageLength = 10,
          lengthMenu = c(10, 25, 50, 100)
        )
      )
    })
  })
}

empty_plot <- function(info = "") {
  return(plot_ly(type = "scatter", mode = "markers") %>%
    add_annotations(
      x=0.5, y=0.5, xref = "paper", yref = "paper",
      text = info,
      xanchor = 'center',
      showarrow = FALSE
    ))
}

plot_1D <- function(data, x_var, y_var, col_var) {
  var_of_interest <- c(x_var, y_var)[ which(!is_null_var(c(x_var, y_var))) ]

  if (is.numeric(data[, var_of_interest])) {
    return(histogram(data, x_var, y_var, col_var))
  }
  barplot(data, x_var, y_var, col_var)
}

plot_2D <- function(data, x_var, y_var, col_var) {
  if (is.numeric(data[, x_var]) && is.numeric(data[, y_var])) {
    return(scatter_plot(data, x_var, y_var, col_var))
  }
  if (any(c(is.numeric(data[, x_var]), is.numeric(data[, y_var])))) {
    return(box_plot(data, x_var, y_var, col_var))
  }
  empty_plot("Plot for two categorical variables is not implemented.\nYou could rather use one variable as X axis and the other as color.")
}

scatter_plot <- function(data, x_var, y_var, col_var) {
  color <- NULL
  if (!is_null_var(col_var)) {
    color <- data[, col_var]
  }
  p <- plot_ly(type = "scatter",
               mode = "markers",
               data = data,
               x = data[, x_var],
               y = data[, y_var],
               color = color,
               hoverinfo = "text",
               text = apply(data, 1, function(l) {
                 paste(names(l), ":", l, collapse = "\n")
               }))
  p <- layout(p,
              yaxis = list(title = y_var),
              xaxis = list(title = x_var),
              legend = list(title = list(text = col_var))
              )
  p
}

box_plot <- function(data, x_var, y_var, col_var) {
  color <- NULL
  if (!is_null_var(col_var)) {
    if (!is.numeric(data[, col_var])) {
      # TODO, since this boxplot have jitter points it may be possible
      # to use quantitiative variables for color but it does not seems
      # straightforward currently
      color <- data[, col_var]
    }
  }

  x_values <- NULL
  if (!is_null_var(x_var)) {
    x_values <- data[, x_var]
  }
  y_values <- NULL
  if (!is_null_var(y_var)) {
    y_values <- data[, y_var]
  }

  p <- plot_ly(
    type = "box",
    data = data,
    y = y_values,
    x = x_values,
    color = color,
    boxpoints = "all",
    jitter = 0.3,
    pointpos = 0,
    hoverinfo = "text",
    text = apply(data, 1, function(l) {
      paste(names(l), ":", l, collapse = "\n")
    })
  )
  p <- layout(p,
              boxmode = "group",
              yaxis = list(title = y_var),
              xaxis = list(title = x_var),
              legend = list(title = list(text = col_var))
              )
  p
}

histogram <- function(data, x_var, y_var, col_var) {
  hist_axis_title <- "Number of observations"

  if (!is_null_var(col_var)) {
    if (is.numeric(data[, col_var])) {
      col_var <- none_value
    }
  }

  data_list <- list(data)

  alpha <- 1
  if (!is_null_var(col_var)) {
    data_list <- split(data, data[, col_var])
    alpha <- 0.6
  }

  x_values <- NULL
  x_axis_title <- hist_axis_title
  if (!is_null_var(x_var)) {
    x_axis_title <- x_var
  }

  y_values <- NULL
  y_axis_title <- hist_axis_title
  if (!is_null_var(y_var)) {
    y_axis_title <- y_var
  }

  p <- plot_ly(
    type = "histogram",
    alpha = alpha
  )
  for (data_index in seq_along(data_list)) {
    data <- data_list[[data_index]]
    x_values <- NULL
    if (!is_null_var(x_var)) {
      x_values <- data[, x_var]
    }
    y_values <- NULL
    if (!is_null_var(y_var)) {
      y_values <- data[, y_var]
    }
    p <- add_histogram(
      p,
      data = data,
      y = y_values,
      x = x_values,
      name = names(data_list)[data_index],
      marker = list(
        line = list(color = 'rgb(235, 237, 235)', width = 1)
      )
    )
  }

  p <- layout(
    p,
    barmode = "overlay",
    showlegend = !is_null_var(col_var),
    yaxis = list(title = y_axis_title),
    xaxis = list(title = x_axis_title),
    legend = list(title = list(text = col_var))
  )
  p
}


barplot <- function(data, x_var, y_var, col_var) {
  hist_axis_title <- "Number of observations"

  data <- data %>%
    dplyr::mutate_if(is.character, as.factor)

  if (!is_null_var(col_var)) {
    if (!is.numeric(data[, col_var])) {
      data <- dplyr::group_by(data, !!sym(col_var), .drop = FALSE)
    }
  }

  x_axis_title <- hist_axis_title
  if (!is_null_var(x_var)) {
    data <- dplyr::group_by(data, !!sym(x_var), .add = TRUE, .drop = FALSE)
    x_axis_title <- x_var
  }

  y_axis_title <- hist_axis_title
  if (!is_null_var(y_var)) {
    data <- dplyr::group_by(data, !!sym(y_var), .add = TRUE, .drop = FALSE)
    y_axis_title <- y_var
  }

  plt_data <- as.data.frame(dplyr::summarise(data, n = dplyr::n(), .groups = "drop"), stringsAsFactors = FALSE)

  if (!is_null_var(x_var)) {
    x_values <- plt_data[[x_var]]
    y_values <- plt_data[["n"]]
  } else {
    x_values <- plt_data[["n"]]
    y_values <- plt_data[[y_var]]
  }

  col_values <- NULL
  if (!is_null_var(col_var)) {
    col_values <- plt_data[[col_var]]
  }

  p <- plot_ly(plt_data,
               x = x_values,
               y = y_values,
               color = col_values,
               type = 'bar')

  p <- layout(
    p,
    showlegend = !is_null_var(col_var),
    yaxis = list(title = y_axis_title),
    xaxis = list(title = x_axis_title),
    barmode = 'stack',
    legend = list(title = list(text = col_var))
  )
  p
}

plot_warning <- function(data, x_var, y_var, col_var) {

  warnings_messages <- c()
  if (is.null(data)) {
    return(warnings_messages)
  }

  n_factor_max <- 20
  for (var in c(x_var, y_var, col_var)) {
    if (!is_null_var(var)) {
      if (is.character(data[, var])) {
        n_factor <- length(unique(data[, var]))
        if (n_factor > n_factor_max) {
          warnings_messages <- c(warnings_messages,
            "Categorical variable with a large amount of levels is detected, plot may not render correctly."
          )
          break()
        }
      }
    }
  }


  if (x_var == none_value || y_var == none_value) {
    # 1D plot
    if (!is_null_var(col_var)) {
      if (is.numeric(data[, col_var])) {
        warnings_messages <- c(warnings_messages,
          "Quantitative variables cannot be used for colors with histograms and barplots"
        )
      }
    }
  } else {
    # 2D plot
    if (is.numeric(data[, x_var]) && is.numeric(data[, y_var])) {
      # scatter plot
    }
    if (any(c(is.numeric(data[, x_var]), is.numeric(data[, y_var])))) {
      # box plot
      if (!is_null_var(col_var)) {
        if (is.numeric(data[, col_var])) {
          warnings_messages <- c(warnings_messages,
                                 "Quantitative variables cannot be used for colors with boxplot"
          )
        }
      }
    }
  }
  warnings_messages
}

is_null_var <- function(vars) {
  sapply(vars, function(var) { identical(var, none_value) })
}

