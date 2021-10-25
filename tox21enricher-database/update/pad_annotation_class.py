# Adds placeholder columns to annotation_Class file

f = open('./table_annotation_class_v2.1.txt', 'r')
datafile = f.readlines()
f.close()

writeOut = ""
for line in datafile:
	x = line.replace("\n", "")
	x += "\tplaceholder\tplaceholder\tplaceholder\tplaceholder\n"
	writeOut += x

f = open('./table_annotation_class_v2.1.txt', 'w')
f.write(writeOut)
f.close()