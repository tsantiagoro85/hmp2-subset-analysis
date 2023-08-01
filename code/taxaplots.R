# Taxa plots

# This code will generate stacked barplots for the top 20 most abundant taxa

# Shorten taxonomy string at the species level for better aesthetics
tax.DNA <- rownames(d.1.DNA)
taxonomy.DNA <- as.data.frame(tax.DNA) %>% separate(tax.DNA, into = paste("V", 1:7, sep = ";"), sep = ";")
rownames(taxonomy.DNA) <- rownames(d.1.DNA)
colnames(taxonomy.DNA) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus","Species")
taxa.summaries.DNA <- Summarize.Taxa(d.1.DNA, taxonomy.DNA)
merged.species <- taxa.summaries.DNA$Species # Collapse table to species level
rownames(merged.species) <- make.names(taxonomy.DNA[, 7], TRUE) # Change row names to shorter species names

# Plot taxonomy
pdf("outputs/taxonomy/taxaplots.pdf", width = 15)
print(
  Microbiome.Barplot2(merged.species, metadata_DNA, CATEGORY = "host_sex") + theme(axis.text.x = element_text(angle = 65,size = 12),legend.text = element_text(size = 10)) + theme(strip.text.x = element_text(size = 14)) + theme(axis.text.y = element_text(size = 14)) + theme(axis.title.y = element_text(size = 16))
)

dev.off()
