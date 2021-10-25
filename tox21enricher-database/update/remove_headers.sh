#!/bin/bash
# Remove headers from database text files
sed -i 1,1d table_annotation_class_v2.1.txt
sed -i 1,1d table_annotation_detail_v2.1.txt
sed -i 1,1d table_annoterm_pairwise_v2.1_FINAL.txt
sed -i 1,1d table_chemical_detail_v2.1.txt
sed -i 1,1d term2casrn_mapping_v2.1.txt
