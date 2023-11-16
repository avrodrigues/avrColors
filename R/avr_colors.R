## Palettes

avr_colors <- list()


avr_colors$blue_gold <- c(
  "#363870",
  "#589eab",
  "#d3a838",
  "#3d291a"
)

avr_colors$blue_gold_red <- c(
  "#363870",
  "#589eab",
  "#d3a838",
  "#3d291a",
  "#BB4455",
  "#f9c0c2"

)

avr_colors$five_hues <- c(
  "#3d291a",
  "#a9344f",
  "#578a5b",
  "#83a6c4",
  "#fcc573"
)

avr_colors$blue_red_div <- c(
  "#303260",
  "#4574AD",
  "#ACCDF2",
  "#FFFFCC",
  "#FEA194",
  "#C62F22",
  "#901B1B"
)

avr_colors$greys <- c(
  "#040400",
  "#1F1F1B",
  "#3B3B37",
  "#575753",
  "#73736F",
  "#8E8E8A",
  "#AAAAA6",
  "#C6C6C2",
  "#E2E2DE",
  "#FEFEFA"
)


get_avr_colors <- function(name, n, direction = 1){

  if(missing(n)){
    the_colors <- avr_colors[[name]]
  }

  if(!missing(n)){
    if(n <= length(avr_colors[[name]])){
      the_colors <- avr_colors[[name]][1:n]
    }
    else{
      the_colors <- grDevices::colorRampPalette(avr_colors[[name]])(n)
    }
  }

  if(direction == -1){
    return(rev(the_colors))
  }
  return(the_colors)


}

