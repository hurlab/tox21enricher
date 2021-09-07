# Python script that uses rdkit substructure search to identify certain hyperreactive functional groups in submitted chemicals

from rdkit import Chem
import re
import sys

def calcReactiveGroups(inputChem):
	# 1) Define functional groups
	# Nitrile (Cyanide) functional group
	g1 = Chem.rdmolfiles.MolFromSmarts("[NX1]#[CX2]")
	# Isocyanate functional group
	g2 = Chem.rdmolfiles.MolFromSmarts("[NX2]=[CX2]=[OX1]")
	# Aldehyde functional group                           
	g3 = Chem.rdmolfiles.MolFromSmarts("[CX3H1](=O)[#6]")
	# Epoxide functional group
	g4 = Chem.rdmolfiles.MolFromSmarts("[OX2]1[CX4][CX4]1")

	# 2) Convert to SMILES if InChI
	pattern = re.compile("^InChI=.+")
	if pattern.match(inputChem) != None:
		inputChem = Chem.rdmolfiles.MolToSmiles(Chem.MolFromInchi(inputChem,True,True,None,False))

	# 3) Convert input to an Rdkit molecule and run a substructure match search
	mol = Chem.rdmolfiles.MolFromSmiles(str(inputChem).rstrip())

	#check for Nitrile
	m1 = mol.GetSubstructMatch(g1)
	#check for Isocyanate
	m2 = mol.GetSubstructMatch(g2)
	#check for Aldehyde
	m3 = mol.GetSubstructMatch(g3)
	#check for Epoxide
	m4 = mol.GetSubstructMatch(g4)

	r1 = 0
	r2 = 0
	r3 = 0
	r4 = 0

	if len(m1) > 0:
		r1 = 1
	if len(m2) > 0:
		r2 = 1
	if len(m3) > 0:
		r3 = 1
	if len(m4) > 0:
		r4 = 1
	
	return str(r1) + "," + str(r2) + "," + str(r3) + "," + str(r4)

def main():
	if len(sys.argv) != 2:
		print("Usage: ./calcReativeGroups.py <SMILES or InChI string>")
		return False
	return calcReactiveGroups(sys.argv[1])
	
if __name__ == "__main__":
	main()
