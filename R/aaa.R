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
        tidyGenomeBrowser.wiggle = 0,
        tidyGenomeBrowser.height = 0.1,
        tidyGenomeBrowser.thick = 0.2,
        tidyGenomeBrowser.flip = TRUE,
        tidyGenomeBrowser.name = TRUE,
        tidyGenomeBrowser.size = 3,
        tidyGenomeBrowser.tx = c(Intron=0.5, Exon=2, CDS=3),
        tidyGenomeBrowser.fontface = "plain",
        tidyGenomeBrowser.fontsize = 3,
        tidyGenomeBrowser.fontforce = 1,
        tidyGenomeBrowser.fontpull = 1,
        tidyGenomeBrowser.fontnudge = 0.3,
        tidyGenomeBrowser.fontpad = 0,
        tidyGenomeBrowser.fontsegment = 2
        # tidyGenomeBrowser.label = c(size=3,
        #                             nudge=0.3,
        #                             force=0.5,
        #                             segment=2)
    )
    toset <- !(names(op.tidyGenomeBrowser) %in% names(op))
    if(any(toset)) options(op.tidyGenomeBrowser[toset])

    invisible()
}