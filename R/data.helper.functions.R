# Data Helper Functions

## Model training --------------------------------------------------------------
union_cv_results <- function(data_list) {
  out_dat <- data_list[[1]]

  for (i in 2:CVFolds_N){
    out_dat <- union(out_dat,
                     data_list[[i]])
  }

  out_dat
}
## Data Visualization ----------------------------------------------------------
data.plot.left_detailed <- function(data,
                                    left.n = 0,
                                    title = NULL,
                                    title.left = NULL,
                                    xname,
                                    yname,
                                    xlabel1 = NULL,
                                    xlabel2 = NULL,
                                    ylabel1 = NULL,
                                    ylabel2 = NULL,
                                    line_col1 = "blue",
                                    line_col2 = "red",
                                    normalize = FALSE) {
  if(is.null(xlabel1)) {
    xlabel1 <- xname
  }
  if(is.null(xlabel2)) {
    xlabel2 <- str_glue(xlabel1, " (left part)")
  }
  if(is.null(ylabel1)) {
    ylabel1 <- yname
  }
  if(is.null(ylabel2)) {
    ylabel2 <- ylabel1
  }

  p1 <- data |>
    data.plot(title = title,
              xname = xname,
              yname = yname,
              xlabel = xlabel1,
              ylabel = ylabel1,
              line_col = line_col1)

  p2 <- data |>
    data.plot.left.n(left.n = left.n,
                     title = title.left,
                     xname = xname,
                     yname = yname,
                     xlabel = xlabel2,
                     ylabel = ylabel2,
                     line_col = line_col2,
                     normalize = normalize)
  grid.arrange(p1, p2)
}
data.plot.left.n <- function(data,
                             left.n = 0,
                             title,
                             xname,
                             yname,
                             xlabel,
                             ylabel,
                             line_col = "red",
                             normalize = FALSE) {
  x_col <- data[, xname]
  y_col <- data[, yname]


  data.left <- data

  if (left.n > 0) {
    data.left <- data |>
      head(left.n)
  }

  data.left |>
    data.plot(title = title,
              xname = xname,
              yname = yname,
              xlabel = xlabel,
              ylabel = ylabel,
              line_col = line_col,
              normalize = normalize)
}

data.plot.right_detailed <- function(data,
                                     shift = 1,
                                     title = NULL,
                                     title.right = NULL,
                                     xname,
                                     yname,
                                     xlabel1 = NULL,
                                     xlabel2 = NULL,
                                     ylabel1 = NULL,
                                     ylabel2 = NULL,
                                     line_col1 = "blue",
                                     line_col2 = "red",
                                     normalize = FALSE) {
  if(is.null(xlabel1)) {
    xlabel1 <- xname
  }
  if(is.null(xlabel2)) {
    xlabel2 <- str_glue(xlabel1, " (shifted right)")
  }
  if(is.null(ylabel1)) {
    ylabel1 <- yname
  }
  if(is.null(ylabel2)) {
    ylabel2 <- ylabel1
  }

  p1 <- data |>
    data.plot(title = title,
              xname = xname,
              yname = yname,
              xlabel = xlabel1,
              ylabel = ylabel1,
              line_col = line_col1)

  p2 <- data |>
    data.plot.shifted.right(shift = shift,
                            title = title.right,
                            xname = xname,
                            yname = yname,
                            xlabel = xlabel2,
                            ylabel = ylabel2,
                            line_col = line_col2,
                            normalize = normalize)
  grid.arrange(p1, p2)
}
data.plot.shifted.right <- function(data,
                                    shift = 1,
                                    title,
                                    xname,
                                    yname,
                                    xlabel,
                                    ylabel,
                                    line_col = "red",
                                    normalize = FALSE) {
  x_col <- data[, xname]
  y_col <- data[, yname]

  data.right <- data |>
    mutate(x_right = lead(x_col, shift),
           y_right = lead(y_col, shift)) |>
    # filter(!is.na(x_right)) |>
    data.plot(title = title,
              xname = "x_right",
              yname = "y_right",
              xlabel = xlabel,
              ylabel = ylabel,
              line_col = line_col,
              normalize = normalize)
}
data.plot <- function(data,
                      title,
                      xname,
                      yname,
                      xlabel = NULL,
                      ylabel = NULL,
                      line_col = "blue",
                      # scale = 1,
                      normalize = FALSE) {
  y <- data[, yname]

  if (normalize) {
    y <- y - min(y)
  }

  if (is.null(xlabel)) {
    xlabel = xname
  }
  if (is.null(ylabel)) {
    ylabel = yname
  }

  aes_mapping <- aes(x = data[, xname], y = y)

  data |>
    ggplot(mapping = aes_mapping) +
    ggtitle(title) +
    xlab(xlabel) +
    ylab(ylabel) +
    geom_point() +
    geom_line(color=line_col)
}
