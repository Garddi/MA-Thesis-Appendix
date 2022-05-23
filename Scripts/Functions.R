## Function Scripts

#The functions in this script have all been used at some point,
# However, not all of them ended up being actually used to produce the final
# thesis. I still include them here.

# Returns a TRUE if a combination of values are completely unique, meaning
# no two observations in the data have the same values on inserted variables

unique_id <- function(x, ...) {
  id_set <- x %>% dplyr::select(...)
  id_set_dist <- id_set %>% distinct
  if (nrow(id_set) == nrow(id_set_dist)) {
    TRUE
  } else {
    non_unique_ids <- id_set %>%
      filter(id_set %>% duplicated()) %>%
      distinct()
    suppressMessages(
      inner_join(non_unique_ids, x) %>% arrange(...)
    )
  }
}


# Aligns legend in ggplot functions

align_legend <- function(p, hjust = 0.5)
{
  # extract legend
  g <- cowplot::plot_to_gtable(p)
  grobs <- g$grobs
  legend_index <- which(sapply(grobs, function(x) x$name) == "guide-box")
  legend <- grobs[[legend_index]]

  # extract guides table
  guides_index <- which(sapply(legend$grobs, function(x) x$name) == "layout")

  # there can be multiple guides within one legend box
  for (gi in guides_index) {
    guides <- legend$grobs[[gi]]

    # add extra column for spacing
    # guides$width[5] is the extra spacing from the end of the legend text
    # to the end of the legend title. If we instead distribute it by `hjust:(1-hjust)` on
    # both sides, we get an aligned legend
    spacing <- guides$width[5]
    guides <- gtable::gtable_add_cols(guides, hjust*spacing, 1)
    guides$widths[6] <- (1-hjust)*spacing
    title_index <- guides$layout$name == "title"
    guides$layout$l[title_index] <- 2

    # reconstruct guides and write back
    legend$grobs[[gi]] <- guides
  }

  # reconstruct legend and write back
  g$grobs[[legend_index]] <- legend
  g
}


# Converts logit vectors into predicted probabilities.

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
