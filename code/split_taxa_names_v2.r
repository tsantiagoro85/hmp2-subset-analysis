# Split taxa names function

split_taxa <- function(tax.in){
# Need to get the taxonomies into a group.table...separate columns for each level
	# tax<-rownames(tax.in)

	# Get the max number of splits for the number of columns needed
	maxsplit <- max(sapply(strsplit(as.character(tax.in),';'),length))

	# https://stackoverflow.com/questions/4350440/split-data-frame-string-column-into-multiple-columns/4351918
	tax.group <- str_split_fixed(tax.in, ";", maxsplit)
	colnames(tax.group) <- (seq(1:maxsplit))
	# rownames for both tables have to match
	rownames(tax.group) <- rownames(tax.in)
	return(tax.group)
}
