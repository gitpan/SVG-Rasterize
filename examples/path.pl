#!/usr/bin/perl
use strict;
use warnings;

use SVG::Rasterize;
use SVG;

my $rasterize;
my $svg;
my $engine;

$svg = SVG->new;
$svg->path('stroke'       => 'cornflowerblue',
	   'stroke-width' => 2,
	   'd'            => 'M10 10 L40 10 L50 90 Z');
$svg->path('stroke'       => 'yellow',
	   'stroke-width' => 2,
	   'd'            => 'M20 15 l30 0 l10 80 Z',
	   'transform'    => 'rotate(-10)');

$rasterize = SVG::Rasterize->new;
$rasterize->rasterize(width => 100, height => 100, svg => $svg);
$rasterize->write(type => 'png', file_name => 'path.png');
