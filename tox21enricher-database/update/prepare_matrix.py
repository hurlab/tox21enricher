import re
mat = open("chem_annotation_matrix_v1.0.txt","r")
out = open("out.txt", "w+")

matrix = mat.readlines()
mat.close()

header = "INSERT INTO annotation_matrix (casrn, name, annotation) VALUES"
for line in matrix:
    tmp = line.split("\t")  
    
    #   OLD TABLE FORMAT   
    #       0       1           2...
    #       casrn   term        everything else
    
    annoStr = ""
    j = 1
    for i in tmp:
        if i == str(1):
            annoStr = annoStr + str(j-2) + "|"
        j = j + 1

    query = header + "('"+tmp[0]+"', '"+tmp[1].replace("'","")+"', '"+annoStr+"');\n"
    #print(query)
    out.writelines(query)
out.close()