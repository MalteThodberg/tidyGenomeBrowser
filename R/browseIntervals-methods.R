#' @export
#' @rdname browseIntervals
setMethod("browseIntervals", signature(object = "ANY"),
          function(object, region=NULL){
              stop("object must be a GenomicRangesList, GenomicRanges or data.frame!")
          })

#' @export
#' @rdname browseIntervals
setMethod("browseIntervals", signature(object = "GenomicRangesList"),
          function(object, region = NULL, plot = TRUE){
              # Coerce to GenomicRanges
              o <- unlist(object, use.names=FALSE)

              # Add group
              if(is.null(names(object))){
                  o$facet <- rep(x = seq_along(object),
                                 times = elementNROWS(object))
              }else{
                  o$facet <- rep(x = names(object),
                                 times = elementNROWS(object))
              }

              # Next method
              browseIntervals(object = o, region = region, plot = plot)
          })

#' @export
#' @rdname browseIntervals
setMethod("browseIntervals", signature(object = "GenomicRanges"),
          function(object, region = NULL, plot = TRUE){
              stopifnot(is.logical(plot))

              # Get plotting regions
              region <- flattenRegion(region = region, object = object)

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Features within region: ", length(o))

              # Add y
              o$bin <- as.integer(disjointBins(o + getOption("tidyGenomeBrowser.wiggle"),
                                               ignore.strand = TRUE))

              # Transfer name to a column
              if(is.null(o$name) && !is.null(names(o))){
                  o$name <- names(o)
              }

              # Coerce
              o <- as.data.frame(o)

              # Plot of necessary
              if(isTRUE(plot)){
                  o <- browseIntervals(object = o, region = region)
              }

              # Return
              o
          })


#' @export
#' @rdname browseIntervals
setMethod("browseIntervals", signature(object = "data.frame"),
          function(object, region){
              # Decide color
              if(!is.null(object$color)){
                  message("Found custom colors...")
                  color_var <- "color"
              }else{
                  color_var <- "strand"
              }

              # Setup plot
              o <- ggplot(object)

              # Add ranges
              o <- o + geom_rect(aes(xmin = .data$start - 0.5,
                                        xmax = .data$end + 0.5,
                                        ymin = .data$bin - getOption("tidyGenomeBrowser.height"),
                                        ymax = .data$bin + getOption("tidyGenomeBrowser.height"),
                                        fill = .data[[color_var]]),
                                    color = NA)

              # Add optional thick ranges
              if(!is.null(object$thick.start) &&
                     !is.null(object$thick.end)){
                      message("Found thickStart/End...")
                      o <- o + geom_rect(aes(xmin = .data$thick.start - 0.5,
                                             xmax = .data$thick.end + 0.5,
                                             ymin = .data$bin - getOption("tidyGenomeBrowser.thick"),
                                             ymax = .data$bin + getOption("tidyGenomeBrowser.thick"),
                                             fill = .data[[color_var]]),
                                         color = NA)
                      }

              # Add labels
              if(!is.null(object$name) && getOption("tidyGenomeBrowser.name")){
                  message("Found names...")
                  o <- o + geom_text_repel(aes(x = ifelse(.data$strand == "-",
                                                        .data$end,
                                                        .data$start),
                                               y = .data$bin,
                                               label=.data$name),
                                           size = getOption("tidyGenomeBrowser.fontsize"),
                                           nudge_y = getOption("tidyGenomeBrowser.fontnudge"),
                                           force = getOption("tidyGenomeBrowser.fontforce"),
                                           force_pull = getOption("tidyGenomeBrowser.fontpull"),
                                           min.segment.length = getOption("tidyGenomeBrowser.fontsegment"),
                                           fontface = getOption("tidyGenomeBrowser.fontface"),
                                           box.padding = getOption("tidyGenomeBrowser.fontpad"),
                                           point.size = NA, # Don't repulse from points
                                           direction = "x")

                  }

              # Add facetting
              if(!is.null(object$facet) & nrow(object) != 0L){
                  message("Found facets...")
                  o <- o + facet_grid(facet~.)
              }

              # Add strand coloring
              if(is.null(object$color)){
                  o <- o +
                      scale_fill_manual("strand", drop=FALSE,
                                         values=getOption("tidyGenomeBrowser.strand"))
              }

              # Remove y-axis
              o <- o +
                  scale_y_continuous(expand = expansion(add = getOption("tidyGenomeBrowser.expansion"))) +
                  ylab("") +
                  theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.grid.minor.y = element_blank(),
                        panel.grid.major.y = element_blank())

              # Add layout
              o <- o +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = getOption("tidyGenomeBrowser.breaks")),
                                     labels = scales::unit_format(unit = "MB",
                                                                  scale = 1e-6,
                                                                  accuracy=getOption("tidyGenomeBrowser.decimals")),
                                     expand = expansion(add = c(0, 0))) +
                  coord_cartesian(xlim = c(start(region),
                                           end(region))) +
                  xlab(paste0(getOption("tidyGenomeBrowser.prefix"),
                              as.character(seqnames(region))))

              # Return
              o
          })
