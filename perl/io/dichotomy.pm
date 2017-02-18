#!/usr/bin/perl

use Math::Random;

my $line;

print "Enter a number: ";
while (<STDIN>)
{
    chomp;
    $number = $_;

    if ($number =~ /^[0-9]+$/) {
	print "NUMBER OK ($number)\n";
    }
    else
    {
	print "That's not a number...\n";
    }

    print "Enter a number: ";
}
