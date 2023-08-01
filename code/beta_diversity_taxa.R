# Beta diversity taxa

# This code will calculate Aitchison values and generate biplots and dendrograms from taxa tables

## Biplots
# CLR transformation
d.clr.1 <- get.clr(d.1.DNA)

d.pca.1 <- prcomp(d.clr.1)

d.mvar.1 <- sum(d.pca.1$sdev^2)

# Calculate principal components
PC1 <- round(sum(d.pca.1$sdev[1]^2)/d.mvar.1, 4)*100
PC2 <- round(sum(d.pca.1$sdev[2]^2)/d.mvar.1, 4)*100
PC3 <- round(sum(d.pca.1$sdev[3]^2)/d.mvar.1, 4)*100

# Add sample labels, and color by metadata
# Need to put metadata on the same table
new.metadata <- metadata_DNA[rownames(d.pca.1$x), ]

# Needs to be in the same order
d2 <- cbind(d.pca.1$x, new.metadata)

xlab <- paste("PC1: ", PC1, "%", sep = "")
ylab <- paste("PC2: ", PC2, "%", sep = "")

scale.factor <- abs(max(d.pca.1$x[,2]))/abs(max(d.pca.1$rotation[,2]))*2

# Generate figure
pdf("outputs/beta_diversity_taxa/biplot_DNA.pdf", width = 14)
p.DNA <- ggplot() +
  #geom_point(data = data.frame(d.pca.1$rotation), aes(x = PC1, y = PC2), alpha = 1/10) +
  geom_point(data = d2, aes(x = PC1/scale.factor, y = PC2/scale.factor, colour = host_sex_host_disease), size = 5)

print(
  p.DNA + ggtitle("Compositional PCA biplot") +
  xlab(xlab) + ylab(ylab) +
  theme_bw()
)

dev.off()

# Generate figure with taxa driving the separation
# Add taxa
tax.in <- rownames(d.pca.1$rotation)
tax.group <- split_taxa(tax.in)

# Species level
tnames <- tax.group[,7]

pdf("outputs/beta_diversity_taxa/biplot_DNA_with_taxa.pdf", width = 14)
p.taxa <-ggplot() +
  geom_point(data = data.frame(d.pca.1$rotation), aes(x = PC1, y = PC2), alpha = 1/10, size = 5) +
  geom_text(data = data.frame(d.pca.1$rotation), aes(x = PC1, y = PC2), label = tnames, check_overlap = TRUE,  alpha = 1/2, size = 4, hjust = 0, nudge_x = 0.005, show.legend = FALSE) +
  geom_point(data = d2, aes(x = PC1/scale.factor, y = PC2/scale.factor, colour = host_sex_host_disease), size = 5)
             
print (
  p.taxa + ggtitle("") +
  xlab(xlab) + ylab(ylab) +
  theme_bw()
)

dev.off()
