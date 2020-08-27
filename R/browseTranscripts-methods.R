#' @export
#' @rdname browseTranscripts
setMethod("browseTranscripts", signature(object = "ANY"),
          function(object){
              stop("object must be a GRangesList or data.frame!")
          })

#' @export
#' @rdname browseTranscripts
setMethod("browseTranscripts", signature(object = "GRangesList"),
          function(object, region = NULL, CDS = NULL, plot = TRUE){
              stopifnot(is.logical(plot),
                        !is.null(names(object)))

              # Get plotting regions
              region <- flattenRegion(region = region,
                                      object = unlist(object))

              # Subset down
              o <- subsetByOverlaps(object, region, ignore.strand = TRUE)
              message("Features within region: ", length(o))

              if(length(o) == 0L){
                  # placeholder
                  d <- as.data.frame(o)

                  # Format thickness
                  d$tx <- factor(d$tx, levels=c("Intron", "Exon", "CDS"))

                  # Rename
                  d$name <- d$group_name
                  d$group_name <- NULL

                  # Bind to make sure mcols survive
                  d <- cbind(d, mcols(o))
              }else{
                  # Extract transcripts
                  tx_frame <- unlist(reduce(o, min.gapwidth=1e9), use.names=FALSE)
                  #tx_frame$bin <- disjointBins(tx_frame)
                  tx_frame$bin <- as.integer(disjointBins(tx_frame + getOption("tidyGenomeBrowser.wiggle"),
                                                   ignore.strand = TRUE))
                  tx_frame$group_name <- names(o)

                  # Coerce to data frames
                  tx_frame <- as.data.frame(tx_frame)
                  exon_frame <- as.data.frame(o)

                  # Add bins to exons
                  exon_frame <- merge(exon_frame, tx_frame[,c("group_name", "bin")])

                  # Stack
                  tx_frame$tx <- "Intron"
                  exon_frame$tx <- "Exon"
                  i <- intersect(colnames(tx_frame), colnames(exon_frame))
                  d <- rbind(tx_frame[i], exon_frame[i])

                  # Add CDS if present
                  if(methods::is(CDS, "GRangesList")){
                      message("Found CDS regions...")
                      stopifnot(!is.null(names(CDS)))

                      # Extract
                      cds_frame <- subsetByOverlaps(CDS,
                                                    region,
                                                    ignore.strand = TRUE)

                      # Coerce and add bins
                      cds_frame <- as.data.frame(cds_frame)
                      cds_frame <- merge(cds_frame, tx_frame[,c("group_name", "bin")])

                      # Add too stack
                      cds_frame$tx <- "CDS"
                      d <- rbind(d, cds_frame[i])
                  }

                  # Format thickness
                  d$tx <- factor(d$tx, levels=c("Intron", "Exon", "CDS"))

                  # Reattach mcols
                  d <- merge(d, as.data.frame(cbind(group_name=names(o),
                                                    mcols(o))))

                  # Rename
                  d$name <- d$group_name
                  d$group_name <- NULL
              }

              # Plot of necessary
              if(isTRUE(plot)){
                  d <- browseTranscripts(object = d, region = region)
              }

              # Return
              d
          })

#' @export
#' @rdname browseTranscripts
setMethod("browseTranscripts", signature(object = "data.frame"),
          function(object, region){
              # Decide color
              if(!is.null(object$color)){
                  message("Found custom colors...")
                  color_var <- "color"
              }else{
                  color_var <- "strand"
              }

              # Setup plot
              o <- ggplot(object) +
                  geom_segment(aes(x=.data$start, xend=.data$end,
                                   y=.data$bin, yend=.data$bin,
                                   size=.data$tx,
                                   color=.data[[color_var]])) +
                  scale_size_manual(values=getOption("tidyGenomeBrowser.tx"),
                                    guide=FALSE)

              # Add names
              if(isTRUE(getOption("tidyGenomeBrowser.name"))){
                  o <- o + geom_text_repel(aes(x=ifelse(.data$strand == "-",
                                                        .data$end,
                                                        .data$start),
                                               y=.data$bin,
                                               label=ifelse(.data$tx == "Intron",
                                                            .data$name,
                                                            NA)),
                                           nudge_y= getOption("tidyGenomeBrowser.nudge"),
                                           size= getOption("tidyGenomeBrowser.size"),
                                           direction="x",
                                           min.segment.length=  2,
                                           #vjust = 1,
                                           #angle        = 45,
                                           #xlim=c(0.2, 0.8),
                                           point.padding = NA)

              }else{
                  message("Skipping adding transcript names...")
              }

              # Add facetting
              if(!is.null(object$facet) & nrow(object) != 0L){
                  message("Found facets...")
                  o <- o + facet_grid(facet ~ .)
              }

              # Add strand coloring
              if(is.null(object$color)){
                  o <- o +
                      scale_color_manual("strand", drop=FALSE,
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