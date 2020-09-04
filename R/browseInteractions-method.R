#' @export
#' @rdname browseInteractions
setMethod("browseInteractions", signature(object = "ANY"),
          function(object){
              stop("object must be a GInteractions or data.frame!")
          })

#' @export
#' @rdname browseInteractions
setMethod("browseInteractions", signature(object = "GInteractions"),
          function(object, region = NULL, plot = TRUE){
              stopifnot(is.logical(plot),
                        length(plot) == 1L)

              # Update this for GI-specific?
              region <- flattenRegion(region = region,
                                      object = object)

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Interactions within region: ", length(o))


              # Detect whether interactions fit region
              o$interactions <-  ifelse(overlapsAny(anchors(o, "first"), region) &
                                            overlapsAny(anchors(o, "second"), region),
                                        "inside", "outside")

              # Add score if missing
              if(is.null(o$score)){
                  message("No score present: Setting arch heights to distance.")
                  o$score <- pairdist(o)
              }

              # Reduce to midpoints
              o <- resize(o, width=1, fix="center")

              # Linearize: Seems buggy?
              # o <- linearize(o, regions(o))

              # Format arches or empty
              if(length(o) == 0){
                  # placeholder
                  empty <- data.frame(arch_id=integer(0),
                                      arch_type=character(0),
                                      arch_x=integer(0),
                                      arch_y=integer(0))

                  # Bind to make sure mcols survive
                  o <- cbind(empty, mcols(o))
              }else{
                  # Stack
                  o <- stack(reduce(methods::as(o, "GRangesList"),
                                    min.gapwidth=1e9))

                  # Save meta data
                  m <- methods::as(mcols(o), "data.frame")
                  m$arch_id <- seq_len(nrow(m))

                  # Control points
                  o$arch_id <- seq_along(o)
                  o$arch_pos <- start(resize(o,
                                            fix="center",
                                            width=1))
                  # o$control <- resize(pgap(first(o),
                  #                           second(o)),
                  #                      fix="center",
                  #                     width=1)

                  # Coerce to data.frame
                  o <- unname(o)
                  o <- as.data.frame(o)

                  # Poor mans gather
                  o <- lapply(o$arch_id, function(i) data.frame(arch_id=i,
                                                               arch_type=c("start",
                                                                         "control",
                                                                         "end"),
                                                               arch_x=c(o$start[i],
                                                                         o$arch_pos[i],
                                                                         o$end[i]),
                                                               arch_y=c(0,
                                                                         o$score[i],
                                                                         0)))
                  o <- do.call(rbind, o)

                  # Reattach info
                  o <- merge(o, m, by="arch_id")
              }
              # }else{
              #     message("No interaction score present: Setting arch heights to distance.")
              #     o <- unname(o)
              #     o$score <- pairdist(o)
              #     o <- as.data.frame(o)

              # Plot of necessary
              if(isTRUE(plot)){
                  o <- browseInteractions(object = o, region = region)
              }

              # Return
              o
          })

#' @export
#' @rdname browseInteractions
setMethod("browseInteractions", signature(object = "data.frame"),
          function(object, region){
              # Decide color
              if(!is.null(object$color)){
                  color_var <- "color"
              }else{
                  color_var <- "interactions"
              }

              # Setup plot
              o <- ggplot(object)

              # Add arches
              o <- o + geom_line(aes(x = .data$arch_x,#bezierx,
                                     y = .data$arch_y,
                                     group = .data$arch_id,
                                     color = .data[[color_var]]),
                                 stat = "smooth",
                                 method = stats::lm,
                                 formula = y ~ poly(x, 2),
                                 se = FALSE,
                                 alpha=getOption("tidyGenomeBrowser.alpha"))

              # Decide based on position type
              #if(!is.null(object$score)){
              #message("Found scores as arch heights...")

              # o <- o + geom_line(aes(x = .data$arch_x,#bezierx,
              #                        y = .data$arch_y,
              #                        group = .data$arch_id,
              #                        color = .data[[color_var]]),
              #                    stat = "smooth",
              #                    method = stats::lm,
              #                    formula = y ~ poly(x, 2),
              #                    se = FALSE,
              #                    alpha=getOption("tidyGenomeBrowser.alpha"))

                  # o <- o + stat_smooth(aes(x = bezierx,
                  #                          y = beziery,
                  #                          group = bezier,
                  #                          color = .data[[color_var]]),
                  #                      method = "lm",
                  #                      formula = y ~ poly(x, 2),
                  #                      se = FALSE,
                  #                      alpha=getOption("tidyGenomeBrowser.alpha"))

                  # o <- o + geom_bezier(aes(x = bezierx,
                  #                          y = beziery,
                  #                          group=bezier,
                  #                          color = .data[[color_var]]),
                  #                      alpha=getOption("tidyGenomeBrowser.alpha"))
              # }else{
              #     #message("No interaction score present: Setting arch heights to distance.")
              #     o <- o +
              #         geom_curve(aes(x= .data$start,
              #                        xend = .data$end,
              #                        y=0,
              #                        yend=0,
              #                        color = .data[[color_var]]),
              #                    alpha=getOption("tidyGenomeBrowser.alpha"),
              #                    curvature=getOption("tidyGenomeBrowser.curvature")) +
              #         theme(axis.title.y = element_blank(),
              #               axis.text.y = element_blank(),
              #               axis.ticks.y = element_blank(),
              #               panel.grid.minor.y = element_blank(),
              #               panel.grid.major.y = element_blank())
              # }

              # Add color
              if(is.null(object$color)){
                  o <- o +
                      scale_color_manual("interactions",
                                         values = getOption("tidyGenomeBrowser.interactions"))
              }

              # Add facetting
              if(!is.null(object$facet) & nrow(object) != 0L){
                  message("Found facets...")
                  o <- o + facet_grid(facet~.)
              }

              # Add layout
              o <- o +
                  scale_x_continuous(breaks = scales::pretty_breaks(n = getOption("tidyGenomeBrowser.breaks")),
                                     labels = scales::unit_format(unit = "MB",
                                                                  scale = 1e-6,
                                                                  accuracy=getOption("tidyGenomeBrowser.decimals")),
                                     expand = expansion(add = c(0, 0))) +
                  coord_cartesian(xlim = c(start(region),
                                           end(region))) +
                  labs(x = paste0(getOption("tidyGenomeBrowser.prefix"),
                                  as.character(seqnames(region))),
                       y = "Interactions")

              # Return
              o
          })