#!/usr/bin/perl -w

use v5.32;
use utf8;
use warnings;
use strict;
use Getopt::Long 'HelpMessage';
use Time::Piece;

# CLI PARSER

GetOptions(
    'from=s' => \my $from_name,
    'to=s' => \(my $to_name = '/home/'),
    'deploy' => \(my $deploy = 0),
    'pretend' => \(my $pretend = 0),
    'overwrite' => \(my $overwrite = 0),
    'info' =>  sub { say info() },
    'help' => sub { HelpMessage(0) }
) or HelpMessage(1);

die "\nFROM folder is required (--to)\n" unless $from_name;

# die unless we got the mandatory argument
HelpMessage(1) unless $from_name;

sub info
{
    return "-- general information --
from:
to: $to_name
deploy: $deploy
pretend: $pretend
overwrite: $overwrite"
}


# tbc
sub print_license { ... }

=head1 NAME

license - get license texts at the command line!

=head1 SYNOPSIS

  --from,-f        folder with dotfiles (required)
  --to,-to         location where to link files (defaults to $HOME)
  --pretend,-p     mimic deployment of symbolic links
  --deploy,-d      deploy dotfiles links
  --overwrite,-o   force redeployment of dotfiles links
  --info,-i        general information of internals commands
  --help,-h        Print this help

=head1 VERSION

0.01

=cut
