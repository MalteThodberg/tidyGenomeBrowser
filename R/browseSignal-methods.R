#' @export
#' @rdname browseSignal
setMethod("browseSignal", signature(object = "ANY"),
          function(object){
              stop("object must be a GenomicRangesList, GenomicRanges or data.frame!")
          })

#' @export
#' @rdname browseSignal
setMethod("browseSignal", signature(object = "GenomicRangesList"),
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
                  o$facet <- factor(o$facet, levels=names(object))
              }


              # Next method
              browseSignal(object = o, region = region, plot = plot)
          })

#' @export
#' @rdname browseSignal
setMethod("browseSignal", signature(object = "GenomicRanges"),
          function(object, region = NULL, plot = TRUE){
              stopifnot(!is.null(score(object)),
                        is.logical(plot),
                        length(plot) == 1L)

              if(!isDisjoint(object)){
                  warning("object is not disjoint" ,
                  "This may cause strange behavior if not using facets!")
              }

              # Get plotting regions
              region <- flattenRegion(region = region, object = object)

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Features within region: ", length(o))

              # Flip strand
              if(isTRUE(getOption("tidyGenomeBrowser.flip"))){
                  if(any(strand(o) == "-")){
                      message("Flipping signal on minus strand...")
                      score(o) <- ifelse(strand(o) == "-", -score(o), score(o))
                  }
              }

              # Coerce
              o <- as.data.frame(o)

              # Plot of necessary
              if(isTRUE(plot)){
                  o <- browseSignal(object = o, region = region)
              }

              # Return
              o
          })

#' @export
#' @rdname browseSignal
setMethod("browseSignal", signature(object = "data.frame"),
          function(object, region){
              # Decide color
              if(!is.null(object$color)){
                  color_var <- "color"
              }else{
                  color_var <- "strand"
              }

              # Setup plot
              o <- ggplot(object)

              # Decide based on position type
              if(!is.null(object$pos)){
                  message("Found single-bp ranges...")

                  # Add positions as points
                  o <- o + geom_bar(aes(x = .data$pos,
                                        y = .data$score,
                                        fill = .data[[color_var]]),
                                    stat="identity",
                                    alpha=getOption("tidyGenomeBrowser.alpha"))

                  # # Add custom coloring
                  # if(is.null(object$color)){
                  #     o <- o + scale_fill_manual("Strand",
                  #                                values = getOption("tidyGenomeBrowser.strand"))
                  # }
              }else{
                  # Add ranges
                  o <- o +
                      geom_rect(aes(xmin=.data$start-0.5,
                                    xmax=.data$end+0.5,
                                    ymin=0,
                                    ymax=.data$score,
                                    fill = .data[[color_var]]),
                                alpha=getOption("tidyGenomeBrowser.alpha"))
                      # geom_area(aes(x= start + ((end - start) / 2),
                      #               y = score,
                      #               fill = .data[[color_var]]),
                      #           alpha=getOption("tidyGenomeBrowser.alpha"))
                      # geom_area(aes(x= start,
                      #               y = score,
                      #               color = .data[[color_var]]),
                      #           alpha=0.75) +
                      # geom_point(aes(x= start,
                      #               y = score,
                      #               color = .data[[color_var]]),
                      #           alpha=0.75)
#
#                   # Add custom coloring
#                   if(is.null(object$color)){
#                       o <- o + scale_color_manual("Strand",
#                                                  values = getOption("tidyGenomeBrowser.strand"))
#                   }
              }

              # Add color
              if(is.null(object$color)){
                  o <- o +
                      scale_fill_manual("strand", drop=FALSE,
                                        values = getOption("tidyGenomeBrowser.strand"))
              }

              # Add facetting
              if(!is.null(object$facet)){
                  message("Facetting on group...")
                  o <- o + facet_grid(facet~.)
              }

              # Add layout
              o <- o +
                  scale_x_continuous(labels = scales::unit_format(unit = "MB",
                                                                  scale = 1e-6)) +
                  coord_cartesian(xlim = c(start(region),
                                           end(region))) +
                  labs(x = as.character(seqnames(region)),
                       y = "Signal")

              # Return
              o
          })
