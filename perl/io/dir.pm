#!/usr/bin/perl

my ($dir_name) = @ARGV;

die "Usage: $0 DIR_NAME" if not defined $dir_name;

sub recursive_readdir
{
    my $dir_name = @_;

    print "DIR_NAME: $dir_name\n";

    opendir(my $dh, $dir_name)
	or return 0;

    print "AZERTY";

    while (readdir $dh)
    {
	printf "\t$dir/$_\n";

	# opendir(my $sub_dh, $dir) or next;

	recursive_readdir($_);
    }

    print "QWERTY";

    closedir $dh;

    return 1;
}

print "XXX\n";
recursive_readdir $dir_name;
print "YYY\n";
