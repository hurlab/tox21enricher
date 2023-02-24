#!/bin/bash
curl --header "Content-Type: application/json" \
	--request POST \
	--data '{"mode":"casrn", "input":"#Set1\n965-90-2\n50-50-0\n#Set2\n979-32-8\n#Set3\n4245-41-4\n143-50-0\n17924-92-4", "annotations":"MESH,PHARMACTIONLIST,ACTIVITY_CLASS,ADVERSE_EFFECT,INDICATION,KNOWN_TOXICITY,MECH_LEVEL_1,MECH_LEVEL_2,MECH_LEVEL_3,MECHANISM,MODE_CLASS,PRODUCT_CLASS,STRUCTURE_ACTIVITY,TA_LEVEL_1,TA_LEVEL_2,TA_LEVEL_3,THERAPEUTIC_CLASS,TISSUE_TOXICITY,DRUGBANK_ATC,DRUGBANK_ATC_CODE,DRUGBANK_CARRIERS,DRUGBANK_ENZYMES,DRUGBANK_TARGETS,DRUGBANK_TRANSPORTERS,CTD_CHEMICALS_DISEASES,CTD_CHEMICALS_GENES,CTD_CHEMICALS_GOENRICH_CELLCOMP,CTD_CHEMICALS_GOENRICH_MOLFUNCT,CTD_CHEMICALS_PATHWAYS,CTD_GOSLIM_BIOPROCESS,HTS_ACTIVE,LEADSCOPE_TOXICITY,MULTICASE_TOX_PREDICTION,TOXCAST_ACTIVE,TOXINS_TARGETS,TOXPRINT_STRUCTURE,TOXREFDB", "cutoff":10, "tanimoto":0.5}' \
	http://<api_addr>:<port>/submit
	
	
# json params
# 	mode			The request mode
#	- casrn		Enrich from user-provided CASRN list
#	- substructure		Enrich from chemicals with shared substructures
#	- similarity		Enrich from chemicals with structural similarity
#	- annotation		View annotations for user-provided CASRN list

#	input			The input to perform enrichment on/view annotations for. Separate lines with
#				newline character (\n) and start set names with pound symbol (#)
#	- example: #Set1\n965-90-2\n50-50-0\n979-32-8\n#Set2\n4245-41-4\n143-50-0\n17924-92-4

#	annotations		The comma-separated list of annotations to use in this request. A full list of
#				annotations can be found by using the query: http://<tox21enricher_api_addr>:<port>/annotationList
#				If not supplied, will just use the default annotations
#				(everything except CTD_GOFAT_BIOPROCESS).

#	cutoff			Node cutoff value. Integers only between 1 - 50 inclusive. If not supplied, will default to 
#				10.

#	tanimoto		Tanimoto threshold value (0.01 - 1.00) to be used with similarity search only. If provided with
#				a different mode, this value is ignored. If not supplied, will default to 0.5.