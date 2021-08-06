	
	print("made it here first")
	data = read.table (file="table_annoterm_pairwise_v2.1_pvalues.txt", sep="\t", header=TRUE)
	qvalue = p.adjust (data$pvalue, method="fdr")
	data = cbind (data, qvalue)
	print("made it here second")
	write.table (data, file="table_annoterm_pairwise_v2.1_pqvalues.txt", sep="\t", quote=FALSE)
	