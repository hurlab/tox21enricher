#!/usr/bin/perl -w

use warnings;
use strict;

## read tox21 data
open (CHEMINFO, "Annotation/Tox21_CASRN_Names_Full.anno");
my %CASRN2name = ();
my $headerLine = <CHEMINFO>;
while(<CHEMINFO>) {
	my $line = $_;	$line =~ s/\r|\n//g;
	my @tmpSplit = split (/\t/, $line);
	if ((defined $tmpSplit[4]) && ($tmpSplit[4] ne "")) {
		$CASRN2name{$tmpSplit[4]} = $tmpSplit[3];
	}
}	close CHEMINFO;

# ----------------------------------------------------
# Load Drug Annotation data
# ----------------------------------------------------
my @annoFiles 	= glob ("Annotation/DrugMatrix_*.txt");
my %term2CASRN				= ();
my %CASRN2term				= ();

foreach my $infile (@annoFiles)
{	my @tmp1 = split (/\//, $infile);
	my @tmp2 = split (/\.txt/, $tmp1[$#tmp1]);
	my @tmp3 = split (/DrugMatrix_/, $tmp2[0]);
	my $annoClassName 		= uc($tmp3[1]);
	print "! Loading $annoClassName\n";
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
			{	my $classTerm = $annoClassName.'__'.$term;
				$term2CASRN{$classTerm}{$tmpSplit[0]} = 1;
				$CASRN2term{$tmpSplit[0]}{$classTerm} = 1;
			}
		}
	}	close INFILE;
}	

my @classTerms 	= sort keys %term2CASRN;
my @casrns		= sort keys %CASRN2term;
my $classTermCount	= scalar @classTerms;
my $casrnsCount		= scalar @casrns;
print "# of classTerms: $classTermCount\n";
print "# of casrns: $casrnsCount\n";

## create output files
open (COLUMNINDEX, ">chem_annotation_matrix_column_names_v1.0.txt");
my $indexHeaderString	= "";
for (my $i=1; $i <= $classTermCount; $i++) {
	print COLUMNINDEX $i."\t".$classTerms[($i-0)]."\n";
	$indexHeaderString .= "\t"."I.".$i;
}	close COLUMNINDEX;

open (MATRIX, 	">chem_annotation_matrix_v1.0.txt");
print MATRIX 	"CASRN\tName".$indexHeaderString."\n";
foreach my $casrn (@casrns) {
	if (defined $CASRN2name{$casrn}) {
		print MATRIX $casrn."\t".$CASRN2name{$casrn};
		
		foreach my $classTerm (@classTerms) {
			print MATRIX "\t".((defined $CASRN2term{$casrn}{$classTerm})? 1 : 0);
		}	print MATRIX "\n";
	}else{
		print $casrn."\t not in Tox21.\n";
	}
}	close MATRIX;

exit;
