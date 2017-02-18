#!/usr/bin/perl

my $filename = "out.txt";

open(my $fh, ">>", $filename) or die "Can't open >> '$filename'";

foreach my $idx (0..9)
{
    printf $fh "#$idx Writing annoying things to my file $filename\n"
	or die "printf failed";
}

printf($fh "\n");

printf($fh "This is a float %f\n", 3.14159);
printf($fh "%c, %d, %u, %e, %o, %x, %b\n",
       65, 65, 65, 65, 65, 65, 65);

close $fh or die "Failed to close file '$filename'\n";

open($fh, "<", $filename) or die "Can't open < '$filename'";

my $line;
my $line_number = 0;

while (!eof($fh))
{
    defined($line = readline $fh)
	or die "Failed to readline from file $filename";

    printf "\t#$line_number => $line";

    $line_number++;
}

close $fh or die "Failed to close file '$filename'\n";
unlink $filename or die "Failed to unlink $filename";
