# Differential abundance analysis (ALDEx2)

# This code will perform differential abundance analysis and generate associated aldex plots

# Naming the output files
i.taxa <- "Males vs Females CD"

# Naming the output directories
out.taxa <- "outputs/aldex2/taxa/"

# Order dataset and metadata by sample ID to do a paired comparison
d.1.DNA.2 <- d.1.DNA[, order(colnames(d.1.DNA))]
taxa.ordered <- metadata_DNA[order(row.names(metadata_DNA)), ]

# Get the conditions matching donor order
conds.DNA <- taxa.ordered[colnames(d.1.DNA.2), "host_sex"]

# Perform aldex analysis
x.all <- aldex(d.1.DNA.2, conds.DNA, denom = "all", paired = TRUE, effect = TRUE)

# Write table of the aldex analysis
write.table(x.all, file = paste(out.taxa, i.taxa, "_aldex_all",".txt", sep = ""), sep = "\t", quote = F, col.names = NA)

# Get significant differences, if any, and write in a table. If no significant differences are found, no table will be generated.
sig.taxa <- which(x.all$wi.eBH < 0.1 & abs(x.all$effect) > 1) # These parameters can be changed as needed
if (length(sig.taxa) != 0){
  x.sig.taxa <- x.all[sig.taxa, ]
  write.table(x.sig.taxa, file = paste(out.taxa, i.taxa, "_aldex_all","_sig",".txt", sep = ""), sep = "\t", quote = F, col.names = NA)}

sig.taxa <- which(abs(x.all$effect) > 1)
rownames(x.all)[sig.taxa]

# Plot results
pdf(paste(out.taxa, i.taxa, "_effect", ".pdf", sep = ""))
par(mfrow = c(1,1))
aldex.plot2(x.all, type = "MW", test = "both", cutoff.pval = 0.1, cutoff.effect = 1)
title(i.taxa, outer = TRUE, line = -2)
dev.off()
