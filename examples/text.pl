#!/usr/bin/perl
use strict;
use warnings;

use SVG::Rasterize;
use SVG;

my $rasterize;
my $svg;
my $points;

$svg = SVG->new;

$svg->rect('width'  => 100,
	   'height' => 130,
	   'fill'   => 'white');

$svg->text('x' => 10, 'y' => 10)->cdata('Hello World');

$rasterize = SVG::Rasterize->new;
$rasterize->rasterize(width => 100, height => 130, svg => $svg);

$rasterize->write(type => 'png', file_name => 'text.png');
