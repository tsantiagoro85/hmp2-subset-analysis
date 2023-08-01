# Alpha diversity taxa

# This code will determine alpha diversity values and generate boxplots

# Determine read counts and the lowest number for the subsampling step below
sumdata.DNA <- data.frame(Read_Counts = colSums(d.1.DNA))
sumdata.DNA$Sample_Name <- rownames(sumdata.DNA)

# Make a phyloseq object
phyobj.DNA <- phyloseq(otu_table(d.1.DNA, taxa_are_rows = TRUE), taxa_names(as.matrix(taxonomy.DNA)), sample_data(metadata_DNA))

phyobj.DNA
# phyloseq-class experiment-level object
# otu_table()   OTU Table:         [ 271 taxa and 8 samples ]
# sample_data() Sample Data:       [ 8 samples by 5 sample variables ]

# Rarefy table to the lowest read count based on sumdata.DNA
min.read.counts.DNA <- min(colSums(d.1.DNA)) 
sv.subsampled.DNA <- Subsample.Table(d.1.DNA, min.read.counts.DNA) 

# Subsampling feature table to 469636, currently has  271  taxa.
# ...sampled to 469636 reads with 271 taxa

# Create phyloseq objects with rarefied table
phyobj2.DNA <- phyloseq(otu_table(sv.subsampled.DNA, taxa_are_rows = TRUE), taxa_names(as.matrix(taxonomy.DNA)), sample_data(metadata_DNA))
sample_data(phyobj2.DNA)$Host_Disease <- as(sample_data(phyobj2.DNA)$host_disease, "character")

phyobj2.DNA
# phyloseq-class experiment-level object
# otu_table()   OTU Table:         [ 271 taxa and 8 samples ]
# sample_data() Sample Data:       [ 8 samples by 6 sample variables ]

# Estimate richness
alpha.DNA <- estimate_richness(phyobj2.DNA, measures = c("Observed","Shannon","Chao1"))

# Order alpha diversity file by sample ID name before adding metadata
alpha.DNA <- alpha.DNA[order(row.names(alpha.DNA)), ]

# Order metadata file to match alpha diversity file
metadata_DNA <- metadata_DNA[order(row.names(metadata_DNA)), ]

# Add columns to alpha diversity file with metadata of interest
alpha.DNA$Host_Sex <- metadata_DNA$host_sex
alpha.DNA$Host_Age <- metadata_DNA$Host_Age
alpha.DNA$Host_Disease <- metadata_DNA$host_disease
alpha.DNA$Host_Sex_Host_Disease <- metadata_DNA$host_sex_host_disease

alpha.DNA$se.chao1 <- NULL # Remove se.chao1 column

# Add a SampleName column:
alpha.DNA['SampleName'] <- row.names(alpha.DNA)

# Duplicate/melt alpha dataframe for plotting
new_alpha_DNA <- reshape2::melt(alpha.DNA, value.name = "Alpha_Diversity", variable.name = "Type")

# Plot alpha diversity
default_theme <- theme_classic() + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        legend.position = "none",
        axis.line = element_line(linewidth = 1, lineend = "square"), 
        axis.title = element_text(size = 12), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 14))

boxplot <- ggplot(data = new_alpha_DNA,
       aes(x = Host_Sex_Host_Disease, y = Alpha_Diversity)) +
  geom_boxplot(position = position_dodge(1), outlier.shape = NA, aes(fill = Host_Sex)) +
  geom_jitter(position = position_jitter(0), color = "black") +
  scale_fill_manual(values = c("#1DB17E","#FFC75C")) +
  default_theme + xlab("Host Sex and Disease") + ylab("Alpha Diversity") +
  facet_wrap(~Type, scales = "free")

pdf("outputs/alpha_diversity_taxa/alpha_diversity.pdf", height = 6, width = 9)
print(boxplot)
dev.off()

# Test for significance
# Observed
test.alpha.observed <- wilcox.test(alpha.DNA$Observed, g = alpha.DNA$Host_Sex_Host_Disease, p.adjust.method = "fdr")
chars.observed <- capture.output(print(test.alpha.observed))
writeLines(chars.observed, con = file("outputs/alpha_diversity_taxa/wilcox_observed.txt"))

# Chao1
test.alpha.chao1 <- wilcox.test(alpha.DNA$Chao1, g = alpha.DNA$Host_Sex_Host_Disease, p.adjust.method = "fdr")
chars.chao1 <- capture.output(print(test.alpha.chao1))
writeLines(chars.chao1, con = file("outputs/alpha_diversity_taxa/wilcox_chao1.txt"))

# Shannon
test.alpha.shannon <- wilcox.test(alpha.DNA$Shannon, g = alpha.DNA$Host_Sex_Host_Disease, p.adjust.method = "fdr")
chars.shannon <- capture.output(print(test.alpha.shannon))
writeLines(chars.shannon, con = file("outputs/alpha_diversity_taxa/wilcox_shannon.txt"))