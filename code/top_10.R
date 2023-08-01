# Plotting differentially abundant features from aldex2 analysis

# This code will generate boxplots from CLR values of the top 10 taxa with the highest absolute effect values.

# Get the CLR values of the collapsed categories for each skin site. This was also done in beta_diversity_taxa.R
d.clr.1 <- get.clr(d.1.DNA)

# Read aldex significant results, and add a column with the absolute effect values for later parsing of the top ten absolute values. The selected dataset did not generate significantly abundant taxa, so we will use the original aldex2 results.
taxa.effect <- read.table("outputs/aldex2/taxa/Males vs Females CD_aldex_all.txt", sep = "\t", quote = "", header = T, check.names = F, row.names = 1, comment.char = "", stringsAsFactors = F)
taxa.effect$effect.abs <- abs(taxa.effect$effect) # Add column with absolute effect values

# Order absolute effect values in descending order and extract top 10 values
taxa.effect.order <- taxa.effect[order(-taxa.effect$effect.abs), drop = FALSE, ]
taxa.effect.order <- as.data.frame(taxa.effect.order)
taxa.effect.order.top.10 <- taxa.effect.order %>% slice(1:10)

# Extract the top 10 absolute effect values from the original CLR datasets and add metadata information
taxa.effect.clr.top.10 <- d.clr.1[, colnames(d.clr.1) %in% rownames(taxa.effect.order.top.10)]
taxa.effect.clr.top.10.metadata <- merge(taxa.effect.clr.top.10, metadata_DNA, by.x = "row.names", by.y = "row.names", all = T)  %>% column_to_rownames("Row.names")
taxa.effect.clr.top.10.metadata.2 <- reshape2::melt(taxa.effect.clr.top.10.metadata, value.name = "CLR", variable.name = "Taxa")

# Reduce taxonomy string to species level for better aesthetics
taxa.effect.labels <- taxa.effect.clr.top.10.metadata.2$Taxa
taxa.effect.sep <- as.data.frame(taxa.effect.labels) %>% separate(taxa.effect.labels, into = paste("V", 1:7, sep=";"), sep = ";")
colnames(taxa.effect.sep) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus","Species")
taxa.effect.clr.top.10.metadata.2$Taxa <- taxa.effect.sep$Species 

# Plot CLR values as boxplots
default_theme <- theme_classic() + 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line = element_line(size = 1, lineend = "square"), 
        axis.title = element_text(size = 14), 
        axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10))

boxplot_taxa_top_10_clr <- ggplot(taxa.effect.clr.top.10.metadata.2, 
                          aes(x = Taxa, y = CLR, fill = host_sex_host_disease)) + 
  geom_boxplot(position = position_dodge(1), outlier.shape = NA) +
  scale_fill_manual(values = c("#1DB17E","#FFC75C")) +
  default_theme + xlab("Taxa") + ylab("CLR-transformed values")

pdf("outputs/aldex2/taxa/taxa-clr-values.pdf", height = 8, width = 12)
print(boxplot_taxa_top_10_clr)

dev.off()
