#!/Users/endress/bin/perl -l 

use strict;
use warnings;

my %words = (
	     "set1" =>  [qw/new6:new11:new1 new8:new11:new7 new6:new22:new7 new12:new19:new1 new8:new19:new4 new12:new22:new4/],
	     "set2" => [qw/new10:new14:new23 new24:new14:new13 new10:new16:new13 new9:new3:new23 new24:new3:new15 new9:new16:new15/]
	    );

my %phantoms = (
		"set1" => [qw/new6:new11:new7 new12:new19:new4/],
		"set2" => [qw/new10:new14:new13 new9:new3:new15/]
	       );


foreach my $s (keys %words) {

  map {
    make_file ("word",
	       $s,
	       [split (/:/, $_)],
	       "h")
  } @{$words{$s}};

  map {
    make_file ("word",
	       $s,
	       [split (/:/, $_)],
	       "v")
  } @{$words{$s}};
  
}


foreach my $s (keys %phantoms) {

  map {
    make_file ("phantom",
	       $s,
	       [split (/:/, $_)],
	       "h")
  } @{$phantoms{$s}};

  map {
    make_file ("phantom",
	       $s,
	       [split (/:/, $_)],
	       "v")
  } @{$phantoms{$s}};
  
}




sub make_file {

  my ($type, $set, $files, $orientation) = @_;
 
  my $tile = ($orientation =~ /h/) ? "3x1" : "1x3";

  my $cmd = "/usr/local/bin/montage " . join (".bmp ", @$files) .
    ".bmp -tile $tile $type" . "_$set" . "_" . "$orientation" . "_" . join ("_", @$files) . ".bmp";

  #print ($cmd);
  system ($cmd);
}
