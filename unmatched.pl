#!/usr/bin/perl

use strict;
use warnings;

sub print_lines_with_paren_count {
    my ($filename) = @_;
    my $line_number = 0;
    my $open_paren_count = 0;

    open my $fh, '<', $filename or die "Could not open file '$filename': $!";
    while (my $line = <$fh>) {
        $line_number++;
        my $open_count = () = $line =~ /\(/g;
        my $close_count = () = $line =~ /\)/g;
        $open_paren_count += $open_count;
        $open_paren_count -= $close_count; # Decrement by the number of closing parens
        print "$open_paren_count\t$line";
    }
    close $fh;
}

# Get the filename from the command line argument.
if (@ARGV != 1) {
    die "Usage: $0 <lisp_file>\n";
}
my $filename = $ARGV[0];

print_lines_with_paren_count($filename);
