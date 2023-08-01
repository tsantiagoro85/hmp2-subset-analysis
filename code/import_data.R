# Import data and metadata

# Read taxa tables
DNA <- read.table("data/HMP2_subsample.csv", sep = ",", quote = "", header = T, check.names = F, row.names = 1, comment.char = "", stringsAsFactors = F)

dim(DNA)
# [1] 1147  8

# Read metadata files.
metadata_DNA <- read.table("data/HMP2_subsample_metadata.csv", sep = ",", quote = "", header = T, check.names = F, row.names = 1, comment.char = "", stringsAsFactors = F)
metadata_DNA$Host_Age <- as.factor(metadata_DNA$Host_Age)

dim(metadata_DNA)
# [1] 8 5

# Set color palette
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Create outputs directories
dir.create("outputs/")
dir.create("outputs/alpha_diversity_taxa")
dir.create("outputs/beta_diversity_taxa")
dir.create("outputs/taxonomy")
dir.create("outputs/aldex2")
dir.create("outputs/aldex2/taxa")
