#!/usr/bin/perl 

use warnings;
use strict;

my @outputFiles = glob ("tmp/*.out");

# Reorder outputFiles
my %outputFile2fileNumber	= ();
foreach my $file (@outputFiles)
{	my @tmp1 = split (/\//, $file);
	my @tmp2 = split (/\.out/, $tmp1[$#tmp1]);
	my @tmp3 = split (/\_/, $tmp2[0]);
	$outputFile2fileNumber{$file} = $tmp3[1];
}

@outputFiles	= sort {$outputFile2fileNumber{$a} <=> $outputFile2fileNumber{$b}} keys %outputFile2fileNumber;
	

	
	
# Merge processed output files and calculate Q-values
my $mergedOutputPvalueFile	= "table_annoterm_pairwise_v2.1_pvalues.txt";
my $mergedOutputPQvalueFile	= "table_annoterm_pairwise_v2.1_pqvalues.txt";
my $mergedOutputFinalFile 	= "table_annoterm_pairwise_v2.1_FINAL.txt";

open (MERGEDOUTPUT, ">$mergedOutputPvalueFile");
for (my $i=0; $i <= $#outputFiles; $i++)
{	open (INFILE, $outputFiles[$i]);
	print "! Loading $outputFiles[$i]\n";
	my $headerLine = <INFILE>;
	if ($i==0)
	{	print MERGEDOUTPUT $headerLine;
	}
	
	while(<INFILE>)
	{	my @tmpSplit = split (/\t/, $_);
		shift @tmpSplit;
		print MERGEDOUTPUT join("\t", @tmpSplit);
	}	close INFILE;
}	close MERGEDOUTPUT;


my $calculateQValueCommandFile	= "TmpCommandToCalculateQValues.R";
open (CMDFILE, ">".$calculateQValueCommandFile);
print CMDFILE "	
	print(\"made it here first\")
	data = read.table (file=\"$mergedOutputPvalueFile\", sep=\"\\t\", header=TRUE)
	qvalue = p.adjust (data\$pvalue, method=\"fdr\")
	data = cbind (data, qvalue)
	print(\"made it here second\")
	write.table (data, file=\"$mergedOutputPQvalueFile\", sep=\"\\t\", quote=FALSE)
	";
close CMDFILE;
 `R --no-save --quiet < $calculateQValueCommandFile`; 
 
print STDOUT "! Finished running R script.\n";

 
 
 
open (INFILE, "$mergedOutputPQvalueFile");
open (OUTFILE, ">$mergedOutputFinalFile");

my $finalHeaderLine = <INFILE>;
print OUTFILE $finalHeaderLine;

while(<INFILE>)
{	my @tmpSplit = split (/\t/, $_);
	shift @tmpSplit;
	
	print OUTFILE join("\t", @tmpSplit);
}	close OUTFILE;	close INFILE;





	
# # Merge processed output files and calculate Q-values
# my $mergedOutputPvalueFile	= "table_annoterm_pairwise_v2.1_pvalues.txt";
# my $mergedOutputPQvalueFile	= "table_annoterm_pairwise_v2.1_pqvalues.txt";
# open (MERGEDOUTPUT, ">table_annoterm_pairwise_v2.1_pvalues.txt");
# for (my $i=0; $i <= $#outputFiles; $i++)
# {	open (INFILE, $outputFiles[$i]);
	# print "! Loading $outputFiles[$i]\n";
	# my $headerLine = <INFILE>;
	# if ($i==0)
	# {	print MERGEDOUTPUT $headerLine;
	# }
	
	# while(<INFILE>)
	# {	my $line = $_;
		# my @tmpSplit = split (/\t/, $line);
		# shift @tmpSplit;
		# print MERGEDOUTPUT join("\t", @tmpSplit);
	# }	close INFILE;
# }	close MERGEDOUTPUT;

# print "!----!\n".
	  # "! calculating FDR \n";
# my $calculateQValueCommandFile	= "TmpCommandToCalculateQValues.R";
# open (CMDFILE, ">".$calculateQValueCommandFile);
# print CMDFILE "	
	# data = read.table (file=\"$mergedOutputPvalueFile\", sep=\"\\t\", header=TRUE)
	# q.values = p.adjust (data\$p.values, method=\"fdr\")
	# data = cbind (data, q.values)
	
	# write.table (data, file=\"$mergedOutputPQvalueFile\", sep=\"\\t\", quote=FALSE)
	# ";
# close CMDFILE;
 # `R --no-save --quiet < $calculateQValueCommandFile`; 
 

print "\n\n! All Process has completed. \n\n";
exit;

