#!/usr/bin/perl

my @numbers = @ARGV;

my @doubles = map { $_ * 2 } @numbers;
my @triples = map { $_ * 3 } @numbers;
my @squares = map { $_ * $_ } @numbers;

my @even = map { (not $_ % 2) ? ($_) : () } @numbers;
my @odd = map { $_ % 2 ? ($_) : () } @numbers;

my @characters = map(chr, @numbers);

printf "Numbers:\t@numbers\n\n";
printf "Doubles:\t@doubles\n";
printf "Triples:\t@triples\n";
printf "Squares:\t@squares\n";

printf "Even:\t\t@even\n";
printf "Odd:\t\t@odd\n";

printf "Characters:\t\t%s\n", @characters;

my @test = (0..100);
printf "Test: @test";
