#!/usr/bin/perl -w

use v5.32;
use utf8;
use warnings;
use strict;
use Getopt::Long 'HelpMessage';
use Time::Piece;
use File::Find;

# CLI PARSING

GetOptions(
    'from=s' => \(my $from_name = ''),
    'to=s' => \(my $to_name = '/home/'),
    'create' => sub { create() },
    'remove' => \(my $remove = 0),
    'pretend' => \(my $pretend = 0),
    'overwrite' => \(my $overwrite = 0),
    'info' =>  sub { say info() },
    'version' =>  sub { say "0.0.1" },
    'help' => sub { HelpMessage(0) }
) or HelpMessage(1);

die HelpMessage unless $from_name;

# die unless we got the mandatory argument
HelpMessage(1) unless $from_name;

sub info
{
    return "-- general information --
from: $from_name
to: $to_name"
}

# tbc
sub print_license { ... }

=head1 NAME

license - get license texts at the command line!

=head1 SYNOPSIS

  --from,-f        target folder with dotfiles
  --to,-to         destination folder to deliver links
  --create,-c      create links of dotfiles
  --remove,-r      remove links from target folder
  --pretend,-p     demonstrate files linking
  --overwrite,-o   overwrite existent links
  --info,-i        provide additional information
  --help,-h        display help usage
  --version,-v     display version

=head1 VERSION

0.01

=cut


# MIDDLEWARE
sub walk {
  find(\&wanted, $from_name);
}

sub wanted {
    print "$File::Find::name\n ";
}

# ACTIONS
sub create {
  walk
}
