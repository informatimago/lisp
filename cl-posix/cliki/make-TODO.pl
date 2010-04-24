#!/usr/bin/perl -w

# for each line containing XXX or TODO, print it and all the
# immediately following lines that have the same comment introducer

my $introducer=undef;
while(<>) {
    if(/(XXX|TODO)/) {
	($introducer)=m/(;+\s*)/;
	print "\n";
    };
    if((defined $introducer) && /$introducer/) {
	print "$ARGV:$.:$_";
    } else {
	$introducer=undef;
    }
} continue {
    # reset line numbers between files
    close ARGV  if eof;     # Not eof()!
}

