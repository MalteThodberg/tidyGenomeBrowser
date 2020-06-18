## code to prepare `DATASET` dataset goes here

#usethis::use_data("DATASET")
library(ThodbergMisc)
collections("Genomics")
library(magrittr)

#### Genome ####

hg19 <- SeqinfoForUCSCGenome("hg19")

#### GWAS ####

url <- "ftp://ftp.ebi.ac.uk/pub/databases/gwas/summary_statistics/FerreiraMA_19853236_GCST008421/MONO.assoc.gz"
filename <-  tempfile(fileext=".assoc.gz")
download.file(url, destfile=filename)
gwas0 <- read.table(filename, header=TRUE, comment.char="")

# Format
gwas <- makeGRangesFromDataFrame(gwas0,
                                 seqnames.field="CHR",
                                 start.field="BP",
                                 end.field="BP",
                                 keep.extra.columns=TRUE)
newStyle <- mapSeqlevels(seqlevels(gwas), "UCSC")
gwas <- renameSeqlevels(gwas, newStyle)
seqlevels(gwas, pruning.mode="coarse") <- seqlevels(hg19)
seqinfo(gwas) <- hg19
gwas <- as(gwas, "GPos")

#### FANTOM5 ####

url <- "https://fantom.gsc.riken.jp/5/datafiles/latest/extra/CAGE_peaks/hg19.cage_peak_phase1and2combined_coord.bed.gz"
filename <-  tempfile(fileext=".bed.gz")
download.file(url, destfile=filename)
FANTOM5 <- import(filename, genome="hg19")

#usethis::use_data(FANTOM5, overwrite=TRUE)

#### CTSSs ####
url <- "http://fantom.gsc.riken.jp/5/datafiles/latest/basic/human.primary_cell.hCAGE/CD14%252b%2520Monocytes%252c%2520donor1.CNhs10852.11224-116B9.hg19.ctss.bed.gz"
filename <-  tempfile(fileext=".bed.gz")
download.file(url, destfile=filename)
CTSSs <- import(filename, genome="hg19")


# Corces
url <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE74912&format=file&file=GSE74912%5FATACseq%5FAll%5FCounts%2Etxt%2Egz"
filename <-  tempfile(fileext=".gz")
download.file(url, destfile=filename)
heap <- vroom::vroom(filename, col_select=c("Chr", "Start", "End"))
ATACSeq <- makeGRangesFromDataFrame(as.data.frame(heap),
                                    seqinfo=SeqinfoForUCSCGenome("hg19"))

#usethis::use_data(ATACSeq, overwrite=TRUE)

# PCHiC (Download DATA S1 from paper)
big <- data.table::fread("~/DATA/SCRATCH/GenomeBrowser/PCHiC_peak_matrix_cutoff5.tsv")

# Coerce to GRanges
bait <- makeGRangesFromDataFrame(df=big,
                                   seqnames.field="baitChr",
                                   start.field="baitStart",
                                   end.field="baitEnd",
                                   ignore.strand=TRUE)

other <- makeGRangesFromDataFrame(df=big,
                                    seqnames.field="oeChr",
                                    start.field="oeStart",
                                    end.field="oeEnd",
                                    ignore.strand=TRUE)

# Coerce to interactions
pchic <- GInteractions(bait, other)
pchic$score <- big$Mon

# Fix seqlevels
newStyle <- mapSeqlevels(seqlevels(pchic), "UCSC")
pchic <- renameSeqlevels(pchic, newStyle)
seqlevels(pchic, pruning.mode="coarse") <- seqlevels(hg19)
seqinfo(pchic) <- hg19

#### gene19 ####

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
library(TxDb.Hsapiens.UCSC.hg38.knownGene)

#OLD:  23186 / 445815

gene19 <- unstrand(genes(TxDb.Hsapiens.UCSC.hg19.knownGene)["23186"] + 1e3)
gene38 <- unstrand(genes(TxDb.Hsapiens.UCSC.hg38.knownGene)["23186"]) + 1e3

#### UCSC ####

url <- "https://atac-blood-hg38.s3.amazonaws.com/hg38/Mono.bw"
BW38 <- import(url, which=gene38)

# Liftover
path = system.file(package="liftOver", "extdata", "hg38ToHg19.over.chain")
ch = import.chain(path)
seqlevelsStyle(BW38) = "UCSC"
BW19 = rtracklayer::liftOver(BW38, ch)
BW19 <- unlist(BW19[elementNROWS(BW19) == 1])

#### Save only needed ####

CAGE_TCs <- subsetByOverlaps(FANTOM5, gene19) %>%
    transform(name = NULL,
              itemRgb=NULL)
CAGE_CTSSs <- subsetByOverlaps(CTSSs, gene19) %>%
    transform(name=NULL) %>%
    as("GPos")
ATAC_peaks <- subsetByOverlaps(ATACSeq, gene19) %>%
    transform(name=NULL)
ATAC_signal <- BW19

PCHiC <- subsetByOverlaps(pchic, gene19) %>%
    reduceRegions()

GWAS <- gwas %>%
    subsetByOverlaps(gene19)

#### Write objects ####

usethis::use_data(CAGE_TCs, overwrite=TRUE)
usethis::use_data(CAGE_CTSSs, overwrite=TRUE)
usethis::use_data(ATAC_peaks, overwrite=TRUE)
usethis::use_data(ATAC_signal, overwrite=TRUE)
usethis::use_data(PCHiC, overwrite=TRUE)
usethis::use_data(GWAS, overwrite=TRUE)


#### Find common set ####

# d <- subsetByOverlaps(FANTOM5, ATACSeq)
# d <- subsetByOverlaps(d, PCHiC)




#
# newStyle <- mapSeqlevels(seqlevels(gi), "UCSC")
# gi <- renameSeqlevels(gi, newStyle)
#
#
#
# # Fix chroms
# HiC <- subset(HiC, baitChr %in% as.character(1:22))
# HiC <- subset(HiC, oeChr %in% as.character(1:22))
#
# HiC$baitChr <- paste0("chr", HiC$baitChr)
# HiC$oeChr <- paste0("chr", HiC$oeChr)

# epiFiles <- query(ah, "EpigenomeRoadMap")


#
#
#
# library(GenomicRanges)
# library(Sushi)
#
# # Copy Sushi data
# Sushi_data = data(package = 'Sushi')
# data(list = Sushi_data$results[,3])
#
# # Example ranges
# exRanges <- makeGRangesFromDataFrame(transform(Sushi_ChIPSeq_pol2.bed,
#                                    strand=ifelse(strand == -1, "-", "+")),
#                                    keep.extra.columns=FALSE)
#
#
# region <-  GRanges("chr11:2282500-2285000")
# browseRanges <- function(object, region, plot=TRUE){
#     # Subset down
#     o <- subsetByOverlaps(object, region)
#
#     # Add y
#     o$bin <- factor(as.integer(disjointBins(o, ignore.strand=TRUE)))
#
#     # Return
#     o <- as.data.frame(o)
# }
#
# usethis::use_data(exRanges, overwrite=TRUE)
#
# ggplot(as.data.frame(o),
#        aes(x=start, y=bin, xend=end, yend=bin, color=strand)) +
#     geom_segment(size=7.5, alpha=0.75) +
#     coord_cartesian(xlim = c(start(region), end(region))) +
#     scale_color_manual("Strand", values=c(`+`="tomato",
#                                           `-`="cornflowerblue",
#                                           `*`="hotpink")) +
#     labs(x="Position", y=NULL) +
#     theme(axis.title.y=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank(),
#           panel.grid.minor.y=element_blank(),
#           panel.grid.major.y=element_blank())
