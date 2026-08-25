# annual incidcen in line plot 
# author: CG 
# v1

#######################################################
library(ggplot2)
library(rlang)

make_lineplot <- function(
    data,
    x, y,                  # bare column names or expressions (e.g. factor(year))
    facet = NULL,          # optional facet variable
    color = NULL,          # optional color variable
    title = NULL,
    xlab = NULL, ylab = NULL,
    outfile = NULL,        # e.g. "figs/annual_ir.pdf"
    width = 10, height = 6, dpi = 600
) {
  # capture symbols/expressions
  x_quo     <- enquo(x)
  y_quo     <- enquo(y)
  facet_quo <- enquo(facet)
  color_quo <- enquo(color)
  
  # sensible default axis labels
  if (is.null(xlab)) xlab <- as_label(x_quo)
  if (is.null(ylab)) ylab <- as_label(y_quo)
  
  p <- ggplot(data, aes(x = !!x_quo, y = !!y_quo, group = 1)) +
    geom_line(linewidth = 0.5) +
    geom_point(size = 0.7) +
    labs(x = xlab, y = ylab, title = title) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title         = element_text(hjust = 0.5, face = "bold"),
      axis.line = element_line(linewidth = 0.4, colour = "grey30"),
      axis.ticks = element_line(linewidth = 0.4, colour = "grey30"),
      axis.ticks.length = unit(0.7, "mm"),
      axis.ticks.x = element_line(),
      axis.ticks.y = element_line(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    )
  x_vec <- rlang::eval_tidy(x_quo, data)
  x_vec <- as.numeric(as.character(x_vec))
  
  p <- p +
    scale_x_discrete(
      labels = function(x) {
        yr <- suppressWarnings(as.integer(as.character(x)))
        ifelse(!is.na(yr) & yr %% 2 == 0, as.character(yr), "")
      }
    ) +
    expand_limits(y = 0)
  
  
  # optional color mapping (similar style to facet block)
  if (!quo_is_null(color_quo)) {
    # map color and also group by it so lines connect within each color level
    p <- p + aes(color = !!color_quo, group = !!color_quo) +
      labs(color = as_label(color_quo))
    
    p <- p + ggthemes::scale_color_tableau("Tableau 20")
  }
  
  # optional facet
  if (!quo_is_null(facet_quo)) {
    p <- p + facet_wrap(vars(!!facet_quo))
  }
  
  # save, if requested
  if (!is.null(outfile)) {
    ggsave(filename = outfile, plot = p, width = width, height = height, dpi = dpi)
  }
  
  return(p)
}


################ heatmap ###############################
# deps
library(ggplot2)
library(rlang)

make_heatmap <- function(
    data,
    x, y, fill,                 # bare column names or expressions (e.g., factor(year))
    facet = NULL,               # optional facet variable (or leave NULL for no facet)
    facet_nrow = 1,             # rows for facet_wrap when used
    palette = NULL,             # e.g., tailwind_cols; if NULL uses viridis-like default
    legend_name = NULL,         # legend title for 'fill'
    title = NULL,
    xlab = NULL, ylab = NULL,
    tile_border = "white",      # tile edge color
    outfile = NULL,             # e.g., "figs/annual_heatmap.pdf"
    width = 12, height = 6, dpi = 600,
    limits = NULL,              # optional c(min, max) for the fill scale
    na_value = "grey50"         # tile color for NA
) {
  # capture
  x_quo     <- enquo(x)
  y_quo     <- enquo(y)
  fill_quo  <- enquo(fill)
  facet_quo <- enquo(facet)
  
  # default labels
  if (is.null(xlab)) xlab <- as_label(x_quo)
  if (is.null(ylab)) ylab <- as_label(y_quo)
  if (is.null(legend_name)) legend_name <- as_label(fill_quo)
  
  p <- ggplot(data, aes(x = !!x_quo, y = !!y_quo, fill = !!fill_quo)) +
    geom_tile(color = tile_border) +
    labs(x = xlab, y = ylab, title = title, fill = legend_name) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title  = element_text(hjust = 0.5, face = "bold")
    )
  
  # optional facet
  if (!quo_is_null(facet_quo)) {
    p <- p + facet_wrap(vars(!!facet_quo), nrow = facet_nrow)
  }
  
  # fill scale
  if (is.null(palette)) {
    # sensible default if no palette supplied
    p <- p + scale_fill_viridis_c(option = "D", limits = limits, na.value = na_value)
  } else {
    p <- p + scale_fill_gradientn(colours = palette, limits = limits, na.value = na_value)
  }
  
  # save, if requested
  if (!is.null(outfile)) {
    ggsave(filename = outfile, plot = p, width = width, height = height, dpi = dpi)
  }
  
  return(p)
}


