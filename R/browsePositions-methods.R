#' @export
#' @rdname browsePositions
setMethod("browsePositions", signature(object = "ANY"),
          function(object, region=NULL){
              stop("object must be a GenomicRangesList, GPos or data.frame!")
          })

#' @export
#' @rdname browsePositions
setMethod("browsePositions", signature(object = "GenomicRangesList"),
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
              browsePositions(object = o, region = region, plot = plot)
          })

#' @export
#' @rdname browsePositions
setMethod("browsePositions", signature(object = "GenomicRanges"),
          function(object, region = NULL, plot = TRUE){
              stopifnot(is.logical(plot))

              # Get plotting regions
              region <- flattenRegion(region = region, object = object)

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Features within region: ", length(o))

              # Add y
              if(is.null(score(object))){
                  o$bin <- as.integer(disjointBins(methods::as(o, "GRanges") + getOption("tidyGenomeBrowser.wiggle"),
                                                   ignore.strand = TRUE))
              }

              # Coerce
              o <- as.data.frame(o)

              # Plot of necessary
              if(isTRUE(plot)){
                  o <- browsePositions(object = o, region = region)
              }

              # Return
              o
          })


#' @export
#' @rdname browsePositions
setMethod("browsePositions", signature(object = "data.frame"),
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

              # Determine y-axis
              if(is.null(object$score)){
                  y_var <- "bin"
              }else{
                  message("Found scores...")
                  y_var <- "score"
              }

              # Add points
              if(!is.null(object$shape)){
                  message("Found shapes...")
                  o <- o + geom_point(aes(x = .data$pos,
                                          y= .data[[y_var]],
                                          shape= .data$shape,
                                          color = .data[[color_var]]),
                                      alpha=getOption("tidyGenomeBrowser.alpha"))
              }else{
                  o <- o + geom_point(aes(x = .data$pos,
                                          y= .data[[y_var]],
                                          color = .data[[color_var]]),
                                      alpha=getOption("tidyGenomeBrowser.alpha"))
              }

              # Add labels
              if(!is.null(object$name)){
                  o <- o + geom_text_repel(aes(x=.data$pos,
                                               y=.data[[y_var]],
                                               label=.data$name),
                                           size= getOption("tidyGenomeBrowser.size"))
              }

              # Add facetting
              if(!is.null(object$facet) & nrow(object) != 0L){
                  message("Found facets...")
                  o <- o + facet_grid(facet~.)
              }

              # Add strand coloring
              if(is.null(object$color)){
                  o <- o +
                      scale_color_manual("strand", drop=FALSE,
                                         values=getOption("tidyGenomeBrowser.strand"))
              }

              # Remove y-axis
              if(is.null(object$score)){
                  o <- o +
                      scale_y_continuous(expand = expansion(add = getOption("tidyGenomeBrowser.expansion"))) +
                      ylab("") +
                      theme(axis.text.y = element_blank(),
                            axis.ticks.y = element_blank(),
                            panel.grid.minor.y = element_blank(),
                            panel.grid.major.y = element_blank())

              }else{
                  o <- o + ylab("Score")
              }

              # Add layout
              o <- o +
                  scale_x_continuous(labels = scales::unit_format(unit = "MB",
                                                                  scale = 1e-6),
                                     expand = expansion(add = c(0, 0))) +
                  coord_cartesian(xlim = c(start(region),
                                           end(region))) +
                  xlab(paste0(getOption("tidyGenomeBrowser.prefix"),
                              as.character(seqnames(region))))

              # Return
              o
          })
