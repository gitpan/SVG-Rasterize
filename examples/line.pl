#!/usr/bin/perl
use strict;
use warnings;

use SVG::Rasterize;
use SVG;

my $rasterize;
my $svg;
my $engine;

$rasterize = SVG::Rasterize->new;
$svg       = SVG->new;
$svg->line(x1 => 10, y1 => 10, x2 => 80, y2 => 20,
	   style => 'stroke:rgb(255, 0, 0);stroke-width:5');
$svg->line(x1 => 50, y1 => 50, x2 => 100, y2 => 100,
	   style => 'stroke:rgb(0, 0, 100%)');
$rasterize->rasterize(width => 100, height => 100, svg => $svg);

$rasterize->write(type => 'png', file_name => 'line.png');
