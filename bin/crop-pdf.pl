#!/usr/bin/env perl
use strict;
use warnings;

my $offset = shift or die;
my $file   = shift or die;

my @offsets = split /,/, $offset;
my ($top, $right, $bottom, $left) = @offsets;
if (@offsets == 1) {
    $right = $bottom = $left = $top;
}
elsif (@offsets == 2) {
    $bottom = $top;
    $left   = $right;
}
elsif (@offsets == 3) {
    $left = $right;
}

(my $ofile = $file) =~ s{\.pdf$}{-cropped\.pdf};
open my $in, '<', $file or die;
open my $out, '>', $ofile or die;
while (my $line = <$in>) {
    $line =~ s{(/CropBox\s*\[\s*([^\[]+)\])}{crop($1, $2)}eg;
    print $out $line;
}
close $in;
close $out;

sub crop {
    my ($str, $box) = @_;

    my @off   = ($left, $bottom, -$right, -$top);
    my @pos   = map { int $_ + shift @off } split /\s+/, $box;
    my $crop  = sprintf '/CropBox[%d %d %d %d]', @pos;
    my $blank = length($str) - length $crop;
    warn $str and return $str if $blank < 0;

    return $crop . (' ' x $blank);
}

__END__

=pod

=head1 NAME

crop-pdf.pl - Adjust CropBox of PDF file

=head1 SYNOPSIS

 % perl crop-pdf.pl <offset> <file>

 e.g.

   % crop-pdf.pl 80,68,42,68 HighPerformanceMySQL.pdf

=head1 OPTIONS

=over

=item I<offset>

Specify offset numbers to crop margin. top,right,bottom,left

=item I<file>

Filename to convert CropBox. Generate <file>-cropped.pdf

=back

=cut
