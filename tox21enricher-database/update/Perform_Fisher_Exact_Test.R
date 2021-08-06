#setwd("E:/Dropbox/WORKSPACE/!HurLab/Tox21Enricher/Junguk Hur/3_MySQL-Database_20161003")

data = read.table (file="table_annoterm_pairwise_v2.1.txt", sep="\t", header=TRUE)

# ----------------------------------------
# if we have multiple matrices and want to collect p.values, then
# initialize a vector for p.values 
p.values = c ()

for (i in 1:length(data[,1]))
{
  dat = c(data[i,6]-1, data[i,4]-data[i,6], data[i,5]-data[i,6], 8949-data[i,4]-data[i,5]+data[i,6])
  dat.mat = matrix(dat, nrow=2)
  fisher.output = fisher.test(dat.mat)
  p.values = c (p.values, fisher.output$p.value)
}

q.values = p.adjust (p.values, method = "fdr")
data.new = cbind (data, p.values, q.values)

write.table (data.new, file="table_annoterm_pairwise_v2.1_calculated.txt", sep="\t", quote=FALSE)

