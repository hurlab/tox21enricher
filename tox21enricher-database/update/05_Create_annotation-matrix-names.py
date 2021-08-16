import re
mat = open("chem_annotation_matrix_column_names_v1.0.txt","r")
out = open("annotation-matrix-names.sql", "w+")

matrix = mat.readlines()
mat.close()

header = "INSERT INTO annotation_matrix_terms (id, term) VALUES"
for line in matrix:
    tmp = line.split("\t")  
    
    #   OLD TABLE FORMAT   
    #       0       1           2...
    #       id   term        everything else
    

    query = header + "("+tmp[0]+", '"+tmp[1].rstrip().replace("'","")+"');\n"
    #print(query)
    out.writelines(query)
out.close()