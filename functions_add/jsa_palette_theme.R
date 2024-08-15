
# Load Required packages
packages <- c(
  "ggplot2",
  "ggtext"
)

package_check <- lapply(
  packages,
  function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#### JSA Colour Palette
# Define colours
jsa_colours <- list(
  primary = c("#6929c4", "#009d9a", "#012749", "#ee538b", "#1192e8",
              "#9f1853", "#005d5d","#fa4d56", "#570408", "#198038",
              "#002d9c", "#b28600", "#8a3800", "#a56eff"),
  fav =  c("#2f005f","#4b0985","#d5a3f9","#d2de5a","#0f2532", "#b91c1c")
)
# Generate a palette class
jsa_palettes <- function(name, n, 
                        all_palettes = jsa_colours, 
                        type = c("discrete", "continuous")) {
  palette <- all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type <- match.arg(type)
  out <- switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

jsa_palettes("primary", type = "discrete")

## Update ggplot colour and fill 
# Discrete 
scale_colour_jsa_d <- function(name) {
  ggplot2::scale_colour_manual(values = jsa_palettes(name,
                                                     type = "discrete")
                               )
}

scale_fill_jsa_d <- function(name) {
  ggplot2::scale_fill_manual(values = jsa_palettes(name,
                                                   type = "discrete")
                             )
}
# Continuous
scale_colour_jsa_c <- function(name) {
  ggplot2::scale_colour_gradientn(colours = jsa_palettes(name = name,
                                                         type = "continuous")
                                  )
}

scale_fill_jsa_c <- function(name) {
  ggplot2::scale_fill_gradientn(colours = jsa_palettes(name = name,
                                                        type = "continuous")
                                )
}
# Ensure sale_colour_*() function work with Britsh or American spelling
scale_color_jsa_d <- scale_colour_jsa_d
scale_color_jsa_c <- scale_colour_jsa_c

## Alternative method
palette_alt <- grDevices::colorRampPalette(
  colors = c(
    "#2f005f","#4b0985","#d5a3f9","#d2de5a","#0f2532", "#b91c1c",
    "#6929c4", "#009d9a", "#012749", "#ee538b", "#1192e8","#9f1853",
    "#005d5d","#fa4d56", "#570408", "#198038","#002d9c", "#b28600",
    "#8a3800", "#a56eff"))(20)


### Create a theme
jsa_theme <- function(){
  ## Generic theme function
  # - Font size 11
  # - background is white 
  # - Title size is 18, adjusted to top left
  # - Subtitle size 10, adjusted to top left
  # - caption size 8, colour gray and bottom right
  #
  theme_light(base_size = 11) +
    theme(
      plot.title = ggtext::element_textbox_simple(
        size = 18, margin = margin(t = 12)),
      plot.subtitle = ggtext::element_textbox_simple(
        size = 10, margin = margin(b = 12)),
      plot.title.position = "plot",
      plot.caption = element_text(size = 8, color = "gray50"),
      axis.title = element_text(size = 11),
      axis.text = element_text(lineheight = 1),
      axis.text.x = element_text(margin = margin(10, 0, 20, 0),
                                 size = 10), ## trbl
      axis.line.x = element_line(color = "grey40"),
      legend.position = "top"
    )
}

jsa_theme_clean <- function() {
  ## Generic theme function
  # - Legend is disable
  # - background is white without grid
  # - Title size is 18, adjusted to top left
  # - Subtitle size 10, adjusted to top left
  # - caption size 8, colour gray and bottom right
  #
  theme(
    plot.title =ggtext::element_textbox_simple(
      size = 18, margin = margin(t = 12)),
    plot.subtitle = ggtext::element_textbox_simple(
      size = 10, margin = margin(b = 12)),
    plot.title.position = "plot",
    plot.caption = element_text(size = 8, color = "gray50"),
    panel.background = element_rect(fill="white"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(color = "grey40"),
    axis.text.y = element_text(size = 11),
    axis.ticks.length = unit(0, "lines"),
    panel.border = element_blank(),
    plot.margin = margin(10, 25, 10, 25),
    legend.position = 'none'
  )
}


### Example
# df <- data.frame(x = c("A", "B", "C", "D"),
#                 y = 1:4)
# g <- ggplot(data = df,
#            mapping = aes(x = x, y = y)) +
#   theme_minimal() +
#   theme(legend.position = c(0.05, 0.95),
#         legend.justification = c(0, 1),
#         legend.title = element_blank(),
#         axis.title = element_blank()
#   )
# 
# 
## Discrete
# g +
#   geom_col(aes(fill = x), size = 3) +
#   labs(title = "***Top left adjusted***", subtitle = "subtile adjusted to the left") +
#   scale_fill_jsa_d("fav") +
#   jsa_theme_clean()
# 
# g +
#   geom_col(aes(fill = x), size = 3) +
#   labs(title = "Top left adjusted", subtitle = "subtile adjusted to the left") +
#   scale_fill_jsa_d("primary") +
#   jsa_theme()
# 
## Continuous
# g +
#   geom_point(aes(colour = y), size = 3) +
#   labs(title = "Top left adjusted", subtitle = "subtile adjusted to the left") +
#   scale_colour_jsa_c("primary") +
#   jsa_theme()