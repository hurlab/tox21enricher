#!/usr/bin/perl 

use warnings;
use strict;
use Parallel::ForkManager;


# ---------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------
my $inputDir     			= "";
my $cpuCores    			= 28;
my $tmpDir 					= "tmp/";
mkdir ($tmpDir) || print "";

open (INFILE, "table_annoterm_pairwise_v2.1.txt");
my $headerLine = <INFILE>;
my @lines	= ();

print "going to read file\n";
my $lineCt = 0;

while(<INFILE>)
{	
	print "$lineCt\n";
	push @lines, $_;
	$lineCt = $lineCt + 1;
}	close INFILE;

print "successfully read file\n";

# Initiate parallel processing
my $parallelProcess = Parallel::ForkManager->new($cpuCores);

# Process the actual files
my $lineCount		= scalar @lines;
my $linesPerFile	= int ($lineCount/$cpuCores) + 1;


my @outputFiles		= ();
for (my $fileNumber	= 1; $fileNumber <= $cpuCores; $fileNumber++)
{
	$parallelProcess->start and next;
    
	# Create temporary input file
	my $tmpInputFile	= $tmpDir."Temporary_$fileNumber.txt";
	my $tmpOutputFile	= $tmpDir."Temporary_$fileNumber.out";
	my $tmpCommandFile	= $tmpDir."Temporary_$fileNumber.R";
	push @outputFiles, $tmpOutputFile;
	
	open (TMPINPUT, ">".$tmpInputFile);
	print TMPINPUT $headerLine;
	
	print "! Processing $fileNumber file $tmpInputFile\n";
	my $currentIndex	= ($fileNumber-1)*$linesPerFile;
	for (my $j=0; $j<$linesPerFile; $j++)
	{	my $lineIndex = $currentIndex+$j;
		if ($lineIndex < $lineCount)
		{	print TMPINPUT $lines[$lineIndex];
		}
	}	close TMPINPUT;
	

	# Create R command File
	open (CMDFILE, ">".$tmpCommandFile);
	print CMDFILE "	
		data = read.table (file=\"$tmpInputFile\", sep=\"\\t\", header=TRUE)
		pvalue = c ()
		for (i in 1:length(data[,1]))
		{
		  dat = c(data[i,6]-1, data[i,4]-data[i,6], data[i,5]-data[i,6], 8949-data[i,4]-data[i,5]+data[i,6])
		  dat.mat = matrix(dat, nrow=2)
		  fisher.output = fisher.test(dat.mat)
		  pvalue = c (pvalue, fisher.output\$p.value)
		}

		data.new = cbind (data, pvalue)

		write.table (data.new, file=\"$tmpOutputFile\", sep=\"\\t\", quote=FALSE)
		";
	close CMDFILE;
	
	# Run R command File
    `R --no-save --quiet < $tmpCommandFile`; 
    
    $parallelProcess->finish;
}
$parallelProcess->wait_all_children;




# Merge processed output files and calculate Q-values
my $mergedOutputPvalueFile	= "table_annoterm_pairwise_v2.1_pvalues.txt";
my $mergedOutputPQvalueFile	= "table_annoterm_pairwise_v2.1_pqvalues.txt";
my $mergedOutputFinalFile 	= "table_annoterm_pairwise_v2.1_FINAL.txt";

open (MERGEDOUTPUT, ">$mergedOutputPvalueFile");
for (my $i=0; $i <= $#outputFiles; $i++)
{	open (INFILE, $outputFiles[$i]);
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
	data = read.table (file=\"$mergedOutputPvalueFile\", sep=\"\\t\", header=TRUE)
	qvalue = p.adjust (data\$pvalue, method=\"fdr\")
	data = cbind (data, qvalue)
	
	write.table (data, file=\"$mergedOutputPQvalueFile\", sep=\"\\t\", quote=FALSE)
	";
close CMDFILE;
 `R --no-save --quiet < $calculateQValueCommandFile`; 
 

 
 
 
open (INFILE, "$mergedOutputPQvalueFile");
open (OUTFILE, ">$mergedOutputFinalFile");

my $finalHeaderLine = <INFILE>;
print OUTFILE $finalHeaderLine;

while(<INFILE>)
{	my @tmpSplit = split (/\t/, $_);
	shift @tmpSplit;
	
	print OUTFILE join("\t", @tmpSplit);
}	close OUTFILE;	close INFILE;


print "\n\n! All Process has completed. \n\n";
exit;

