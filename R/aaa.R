.onLoad <- function(libname, pkgname) {
    op <- options()
    op.tidyGenomeBrowser <- list(
        tidyGenomeBrowser.breaks = 5,
        tidyGenomeBrowser.decimals = 0.01,
        tidyGenomeBrowser.prefix = c("Genomic position on: "),
        tidyGenomeBrowser.strand = c(`+`="tomato",
                                     `-`="cornflowerblue",
                                     `*`="hotpink"),
        tidyGenomeBrowser.interactions = c(inside="forestgreen",
                                            outside="grey75"),
        tidyGenomeBrowser.expansion=c(0.5, 0.5),
        tidyGenomeBrowser.alpha = 0.75,
        tidyGenomeBrowser.nudge = 0.3,
        tidyGenomeBrowser.wiggle = 0,
        tidyGenomeBrowser.height = 0.1,
        tidyGenomeBrowser.thick = 0.2,
        tidyGenomeBrowser.flip = TRUE,
        tidyGenomeBrowser.name = TRUE,
        tidyGenomeBrowser.size = 3,
        tidyGenomeBrowser.tx = c(Intron=0.5, Exon=2, CDS=3),
        tidyGenomeBrowser.fontface = "plain"
    )
    toset <- !(names(op.tidyGenomeBrowser) %in% names(op))
    if(any(toset)) options(op.tidyGenomeBrowser[toset])

    invisible()
}