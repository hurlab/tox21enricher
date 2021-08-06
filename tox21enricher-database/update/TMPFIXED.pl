#!/usr/bin/perl 

use warnings;
use strict;
use Parallel::ForkManager;


# ---------------------------------------------------------------------
# Parameters
# ---------------------------------------------------------------------
my $inputDir     			= "";
my $cpuCores    			= 4;
my $tmpDir 					= "tmp/";

# Initiate parallel processing
my $parallelProcess = Parallel::ForkManager->new($cpuCores);

for (my $fileNumber	= 1; $fileNumber <= $cpuCores; $fileNumber++)
{
	$parallelProcess->start and next;
	
	my $tmpCommandFile	= $tmpDir."Temporary_$fileNumber.R";
	
	# Run R command File
    	`R --no-save --quiet < $tmpCommandFile`; 
    
    	$parallelProcess->finish;
}
$parallelProcess->wait_all_children;



print "\n\n! All Process has completed. \n\n";
exit;

