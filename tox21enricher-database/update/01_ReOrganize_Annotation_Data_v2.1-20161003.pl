#!/usr/bin/perl 

# ToDo: deal the complete list of CASRN (not only those in tox21)
#
# Version 2.1: 	This version uses only the CASRN defined in the tox21 platform
#				while calculating the overlap between terms

use warnings;
use strict;


# ----------------------------------------------------
# Load tox21 chem IDs
# ----------------------------------------------------
open (CHEMINFO, "Annotation/Tox21_CASRN_Names_Full.anno");
open (CHEMINFOTABLE, ">table_chemical_detail_v2.1.txt");
print CHEMINFOTABLE "CASRNUID	CASRN	TestSubstance_ChemName	Molecular_Formular	IUPAC_Name	InChI	InChIKey	SMILES\n";

my %CASRN2name				= ();
my %CASRN2UID				= ();
my %UID2CASRN				= ();
my %CASRN2ChemDetailLine	= ();
# my %CASRN2molecularFormular	= ();
# my %CASRN2IUPACName			= ();
# my %CASRN2InChI				= ();
# my %CASRN2InChIKey			= ();
# my %CASRN2SMILES			= ();
my $CASRNUID				= 1;

my $chemInfoHeaderLine	= <CHEMINFO>;	$chemInfoHeaderLine =~ s/\r|\n//g;
my @chemInfoHeaderSplit = split (/\t/, $chemInfoHeaderLine);

my $casrnIndex				= get_column_index (\@chemInfoHeaderSplit, ("TestSubstance_CASRN", "TS_CASRN"));
my $chemicalNameIndex		= get_column_index (\@chemInfoHeaderSplit, ("TestSubstance_ChemName", "TS_ChemName"));
my $MolecularFormularIndex	= get_column_index (\@chemInfoHeaderSplit, "STRUCTURE_Formula");
my $IUPACNameIndex			= get_column_index (\@chemInfoHeaderSplit, "IUPACName");
my $InChIIndex				= get_column_index (\@chemInfoHeaderSplit, "InChI");
my $InChIKeyIndex			= get_column_index (\@chemInfoHeaderSplit, "InChIKey");
my $SMILESIndex				= get_column_index (\@chemInfoHeaderSplit, "STRUCTURE_SMILES");

if (! $casrnIndex) {	die "! Missing casrnIndex\n";	}
elsif (! $chemicalNameIndex) {	die "! Missing chemicalNameIndex\n";	}
elsif (! $MolecularFormularIndex) {	die "! Missing MolecularFormularIndex\n";	}
elsif (! $IUPACNameIndex) {	die "! Missing IUPACNameIndex\n";	}
elsif (! $InChIIndex) {	die "! Missing InChIIndex\n";	}
elsif (! $InChIKeyIndex) {	die "! Missing InChIKeyIndex\n";	}
elsif (! $SMILESIndex) {	die "! Missing SMILESIndex\n";	}

# Process the cheminfo content
while(<CHEMINFO>)
{	my $line = $_;	 $line =~ s/\r|\n//g;
	if ($line ne "")
	{	my @tmpSplit 		= split (/\t/, $line);
		my $tmpOutputLine	= '';
		if ((defined $tmpSplit[$casrnIndex]) && ($tmpSplit[$casrnIndex] ne "") && ($tmpSplit[$casrnIndex] =~ /\d/))
		{	$tmpOutputLine .= $tmpSplit[$casrnIndex]."\t".$tmpSplit[$chemicalNameIndex];
			$tmpOutputLine .= "\t".((defined $tmpSplit[$MolecularFormularIndex]) ? $tmpSplit[$MolecularFormularIndex] : "");
			$tmpOutputLine .= "\t".((defined $tmpSplit[$IUPACNameIndex]) ? $tmpSplit[$IUPACNameIndex] : "");
			$tmpOutputLine .= "\t".((defined $tmpSplit[$InChIIndex]) ? $tmpSplit[$InChIIndex] : "");
			$tmpOutputLine .= "\t".((defined $tmpSplit[$InChIKeyIndex]) ? $tmpSplit[$InChIKeyIndex] : "");
			$tmpOutputLine .= "\t".((defined $tmpSplit[$SMILESIndex]) ? $tmpSplit[$SMILESIndex] : "");
			
			$CASRN2ChemDetailLine{$tmpSplit[$casrnIndex]} = $tmpOutputLine;
			$CASRN2name{$tmpSplit[$casrnIndex]} = $tmpSplit[$chemicalNameIndex];
		}
	}
}	close CHEMINFO;


my @sortedCASRN = sort keys %CASRN2name;
foreach my $CASRN (@sortedCASRN)
{	print CHEMINFOTABLE $CASRNUID."\t".$CASRN2ChemDetailLine{$CASRN}."\n";
	
	$CASRN2UID{$CASRN} = $CASRNUID;
	$UID2CASRN{$CASRNUID} = $CASRN;
	$CASRNUID++;
}	close CHEMINFOTABLE;




# ----------------------------------------------------
# Load Drug Annotation data
# ----------------------------------------------------
my @annoFiles 	= glob ("Annotation/DrugMatrix_*.txt");
my $outputDir	= "ReOrganized/";
mkdir ($outputDir) || print "";

open (ANNOSUMMARY, ">table_annotation_class_v2.1.txt");
print ANNOSUMMARY "annoClassID\tannoClassName\tfirstTermID\tlastTermID\tnumberOfTermIDs\n";

open (ANNODETAILS, ">table_annotation_detail_v2.1.txt");
print ANNODETAILS "annoTermID\tannoClassID\tannoTerm\n";

open (COMPLETEANNO, ">term2casrn_mapping_v2.1.txt");
print COMPLETEANNO "term2casrnMappingUID\tannoTermID\tannoClassID\tCASRN2UID\n";


open (UNDEFINED, ">WARNING_UNDEFINED-CASRN_v2.1.txt");
open (REGENERATE, ">WARNING_Annotation-file-to-be-regenerated.txt");

				
my $annoTermID				= 1;
my $annoClassID				= 1;
my $annoTermDetailUID		= 1;
my %termID2annoTerm			= ();
my %undefinedCASRN			= ();
my %annoClassToRegenrate	= ();
my %termIDID2CASRNArray		= ();
my %termIDID2CASRNHash		= ();

foreach my $infile (@annoFiles)
{	my @tmp1 = split (/\//, $infile);
	my @tmp2 = split (/\.txt/, $tmp1[$#tmp1]);
	my @tmp3 = split (/DrugMatrix_/, $tmp2[0]);
	my $annoClassName 		= uc($tmp3[1]);
	my $beginningTermID		= $annoTermID;
	my $regenerateFileTag	= 0;
	
	my $reorganizedFile = $outputDir.$tmp3[0].'_'.$tmp2[0].'_ReOrganized.txt';
	open (REORGANIZED, ">".$reorganizedFile);
	print "! Processing $tmp2[0] ...\n";
	
	my %localTerm2FullyExtractWithTox21 = ();
	my %localTerm2Chem	= ();
	open (INFILE, $infile);
	while(<INFILE>)
	{	my $line = $_;	 $line =~ s/\r|\n//g;
		if ($line ne "")
		{	my @tmpSplit = split (/\t/, $line);
		
			if ((not defined $tmpSplit[1]) || ($tmpSplit[1] eq ""))
			{	next;
			}
			
			my @termSplit = split (/\; /, $tmpSplit[1]);
			foreach my $term (@termSplit)
			{	$localTerm2Chem{$term}{$tmpSplit[0]} = 1;
				# mark for term to extract with matching tox21 CASRN
				if (defined $CASRN2UID{$tmpSplit[0]})
				{	$localTerm2FullyExtractWithTox21{$term}{$tmpSplit[0]} = 1;
				}
			}
			if ($tmpSplit[0] =~ /\//)
			{	$regenerateFileTag = 1;
			}
			
			
			
		}
	}	close INFILE;
	
	my @sortedConceptTerm = sort keys %localTerm2FullyExtractWithTox21;
	foreach my $term (@sortedConceptTerm)
	{	if (defined $localTerm2FullyExtractWithTox21{$term})
		{	my @CASRNs = keys %{$localTerm2FullyExtractWithTox21{$term}};
			my $termCount	= scalar @CASRNs;
			
			
			foreach my $CASRN (@CASRNs)
			{	if (not defined $CASRN2UID{$CASRN})
				{	$undefinedCASRN{$CASRN} = 1;
					print UNDEFINED "$tmp2[0]\t$CASRN\n";
				}
				else
				{
					print REORGANIZED $annoTermID."\t".$annoClassID."\t".$term."\t".$CASRN2UID{$CASRN}."\t".$CASRN."\n";
					print COMPLETEANNO $annoTermDetailUID++."\t".$annoTermID."\t".$annoClassID."\t".$CASRN2UID{$CASRN}."\n";
				}
			}
			
			$termID2annoTerm{$annoTermID} 			= $term;
			$termIDID2CASRNHash{$annoTermID} 		= $localTerm2FullyExtractWithTox21{$term};
			$termIDID2CASRNArray{$annoTermID} 		= \@CASRNs; 
			
			# print out annotation term detail
			print ANNODETAILS $annoTermID++."\t".$annoClassID."\t".$term."\n";
		}
	}	close REORGANIZED;

	# Summary of anno class
	print ANNOSUMMARY $annoClassID."\t".$annoClassName."\t".$beginningTermID."\t".($annoTermID-1)."\t".($annoTermID-$beginningTermID)."\n";
	$annoClassID++;
	
	# Regenerate File Tag
	if ($regenerateFileTag)
	{	print REGENERATE $tmp2[0]."\n";
	}
}	close COMPLETEANNO;	close ANNOSUMMARY;	close REGENERATE;	close ANNODETAILS;


open (PAIRWISE, ">table_annoterm_pairwise_v2.1.txt");
print PAIRWISE "termPairUID\tterm1UID\tterm2UID\tterm1Size\tterm2Size\tcommon\tunion\tjaccardIndex\n";
print "\n\n! Performing Pairwise comparison ...\n";
# Create a pair data
my $termPairUID = 1;

my @sortedTermIDs = sort {$a <=> $b} keys %termID2annoTerm;
for (my $i=0; $i < $#sortedTermIDs; $i++)
{	print "! Processing index $i (out of ".($#sortedTermIDs+1).")\n";
	for (my $j=$i+1; $j <= $#sortedTermIDs; $j++)
	{	# perform pairwise comparison
		my ($term1Size, $term2Size, $common, $unionCount, $jaccardIndex) = compare_two_terms_for_overlap($sortedTermIDs[$i],$sortedTermIDs[$j]);
		
		# Minimum overlap is 2
		if ($common > 1)
		{	print PAIRWISE $termPairUID++."\t".$sortedTermIDs[$i]."\t".$sortedTermIDs[$j]."\t".$term1Size."\t".$term2Size."\t".$common."\t".$unionCount."\t".$jaccardIndex."\n";
		
		# .
			               # ($unionCount/$common)."\t";		# this needs to be corrected to reflect actual odds ratio
			# my $randNumber 	= (rand(1000)+1);
			# my $pvalue 			= 1/$randNumber;
			# my $qvalue			= $pvalue*10;
			# if ($qvalue >1)	
			# {	$qvalue = 1;
			# }
			              
			# print PAIRWISE $pvalue."\t".$qvalue."\n"; 
		}
	}
}	close PAIRWISE;


sub compare_two_terms_for_overlap
{	my $annoTermID1	= shift;
	my $annoTermID2	= shift;
	
	my $common			= 0;
	my %unionData		= ();
	my $term1Size		= scalar @{$termIDID2CASRNArray{$annoTermID1}};
	my $term2Size		= scalar @{$termIDID2CASRNArray{$annoTermID2}};
	
	# first set
	foreach my $CASRN (@{$termIDID2CASRNArray{$annoTermID1}})
	{	$unionData{$CASRN} = 1;
		if (defined $termIDID2CASRNHash{$annoTermID2}{$CASRN})
		{	$common++;
		}
	}
	
	# second set
	foreach my $CASRN (@{$termIDID2CASRNArray{$annoTermID2}})
	{	$unionData{$CASRN} = 1;
	}
	
	my $unionCount		= scalar (keys %unionData);
	my $jaccardIndex	= sprintf("%.5f",$common/$unionCount);
	
	return ($term1Size, $term2Size, $common, $unionCount, $jaccardIndex);
}



# ---------------------------------------------------------------------------
# Return the index from an array
# ---------------------------------------------------------------------------
# The function returns the index of specified element. 
#
# Date: 10/10/2014
# Change: 1) the function now accepts multiple specific elements (terms)
#		  2) non-existent elements will result in an empty value ""
# ---------------------------------------------------------------------------
sub get_column_index
{	my @localArray		= @_;

	my $headerRef		= shift @localArray;
	my $status			= "";
	
	if ((not defined $localArray[0]) || ($localArray[0] eq ""))
	{	return ($status);
	}
	
	# Check exact match
	for (my $i=0; $i < scalar (@{$headerRef}); $i++)
	{	for (my $j=0; $j < scalar @localArray; $j++)
		{	if ($$headerRef[$i] eq $localArray[$j])
			{	return ($i);
			}
		}
	}
	
	# partial match
	for (my $i=0; $i < scalar (@{$headerRef}); $i++)
	{	for (my $j=0; $j < scalar @localArray; $j++)
		{	if ($$headerRef[$i] =~ /$localArray[$j]/i)
			{	return ($i);
			}
		}
	}
	
	return ($status);
}



# open (UNDEFINED, ">Undefined_CASRN.txt");
# foreach my $CASRN (keys %undefinedCASRN)
# {	print UNDEFINED $CASRN."\n";
# }	close UNDEFINED;