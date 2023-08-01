# Data manipulation

# Collapse table to species level
tax.DNA <- rownames(DNA)
taxonomy.DNA <- as.data.frame(tax.DNA) %>% separate(tax.DNA, into = paste("V", 1:7, sep=";"), sep = ";")
rownames(taxonomy.DNA) <- rownames(DNA)
colnames(taxonomy.DNA) <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus","Species")
taxa.summaries.DNA <- Summarize.Taxa(DNA, taxonomy.DNA)
DNA.species <- taxa.summaries.DNA$Species

# Filter tables for further processing
# Keep bacteria only
d.b.DNA <- DNA.species[grepl("^k_Bacteria",rownames(DNA.species)),] 

# Remove taxa present in relative abundances < 0.001%. Filter can be changed as needed.
d.1.DNA <- Fraction.Filter(d.b.DNA, 0.00001, VERBOSE=T)
# [1] "Filtering table at a min fraction of 1e-05 of feature table..."
# [1] "...There are 25303425 reads and 985  features"
# [1] "...After filtering there are 25279223 reads and 271 OTUs"
