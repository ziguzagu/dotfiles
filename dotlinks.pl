#!/usr/bin/perl
# $Id$

use strict;
use warnings;

use File::Basename;

my %none_dot = map { $_ => 1 } qw(bin);
my %except   = map { $_ => 1 } (basename $0);
my @targets  = glob "*";

chdir $ENV{HOME};
for my $file (@targets) {
    next if $except{$file};

    my $link = $none_dot{$file} ? $file : ".$file";
    next if -e $link or -l $link;

    symlink "svn/dots/$file", "$link"
        or die "cannot make symlink: $!";
    print $link, "\n";
}
