#!/usr/bin/perl

print "Running: $0 @ARGV\n";

my $nb_args = @ARGV;
print "Running with $nb_args args\n";

foreach my $arg (@ARGV)
{
    print "$arg\n";
}

my $filename = shift or die "Usage: $0 FILENAME";

print "Processing file $filename\n";
