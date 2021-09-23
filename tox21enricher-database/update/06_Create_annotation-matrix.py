import re
mat = open("chem_annotation_matrix_v1.0.txt","r")
out = open("newmatrix.sql", "w+")

matrix = mat.readlines()
mat.close()

header = "INSERT INTO annotation_matrix (casrn, name, annotation) VALUES"

firstLine = matrix[0]
annoList = firstLine.split("\t")

for line in matrix:
    tmp = line.split("\t")  
    
    #   OLD TABLE FORMAT   
    #       0       1           2...
    #       casrn   term        everything else
    
    annoStr = ""
    j = 0
    for i in tmp:
        if i == "1":
            annoStr = annoStr + str(j-2) + "|"
            #annoStr = annoStr + annoList[j].replace("I.", "") + "|"
        j = j + 1

    query = header + "('"+tmp[0]+"', '"+tmp[1].replace("'","")+"', '"+annoStr+"');\n"
    #print(query)
    if(query != "INSERT INTO annotation_matrix (casrn, name, annotation) VALUES('CASRN', 'Name', '');\n"):    
	    out.writelines(query)
out.close()

