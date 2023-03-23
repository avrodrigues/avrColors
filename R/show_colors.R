#' Show colors in the plot
#'
#'

show_colors <- function(col_vec){
  graphics::image(1:length(col_vec), 1, as.matrix(1:length(col_vec)),
                  col = col_vec, xlab = "", ylab = "", xaxt = "n", yaxt = "n",
                  bty = "n")
}


show_in_context <- function(col_vec, type = "area"){
  require(ggplot2)

  names(col_vec) <- col_vec

  if(type == "area"){


    df.palette <- data.frame(
      group = 1:length(col_vec),
      fill = col_vec
    )

    set.seed(2416)
    df.palette$bar <- runif(length(col_vec), 10, 20)

    showPlot <-
    ggplot(df.palette, aes(y = bar, x = group, fill = fill)) +
      geom_col() +
      scale_fill_manual(values = col_vec) +
      theme_light()
  }

  if(type == "points"){
    set.seed(2416)
    x <-  rnorm(50, 10)
    y <- (x * 1.5) + 13 + rnorm(50, 2)
    df.points <- data.frame(x, y)
    df.points$color <- sample(col_vec, size = 50, replace = T )

    showPlot <-
    ggplot(df.points, aes(x = x, y = y, color=color)) +
      geom_point(size = 2) +
      scale_color_manual(values = col_vec) +
      theme_light()

  }

  if(type == "lines"){

    y <- unlist(
      lapply(seq_along(col_vec), function(i){
        set.seed(25*i)
        rnorm(10, i, 1)
      })
    )

    df.lines <- data.frame(
      group = as.factor(rep(col_vec, each = 10)),
      x = rep(1:10, length(col_vec)),
      y
    )

    showPlot <-
    ggplot(df.lines, aes(x = x, y = y, color = group, group = group)) +
      geom_line(size = 0.8) +
      scale_color_manual(values = col_vec) +
      theme_light()

  }

  if(type == "raster"){
    library(datasets)
    data("volcano")
    x <- 1:dim(volcano)[1]
    y <- 1:dim(volcano)[2]


    r.df <- expand.grid(x = x,y = y)
    r.df$value <- as.numeric(volcano)
    showPlot <-
    ggplot(r.df, aes(x = x, y = y, fill = value)) +
      geom_raster() +
      coord_equal() +
      scale_fill_gradientn(colours = col_vec) +
      theme_light()
  }

return(showPlot)


}
