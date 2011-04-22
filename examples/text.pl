#!/usr/bin/perl
use strict;
use warnings;

use SVG::Rasterize;
use SVG;

my $rasterize;
my $svg;
my $points;
my $text;

$svg = SVG->new;

$svg->rect('width'  => 150, 'height' => 150, 'fill'   => 'white');

$svg->text('x' => 30, 'y' => 20, 'fill' => 'red')->cdata('Hello World');
$svg->line(x1 => 30, y1 => 20, x2 => 104, y2 => 20, 'stroke' => 'black');

$text = $svg->text('x' => 30, 'y' => 60);
$text->tspan->cdata('foo');
$text->a('xlink:href' => 'foo')->circle
    (cx => 50, cy => 60, r => 7, 'fill' => 'yellow');
$text->tspan('font-size' => 'large')->cdata('bar');

$rasterize = SVG::Rasterize->new;
$rasterize->rasterize(width => 150, height => 150, svg => $svg);

$rasterize->write(type => 'png', file_name => 'text.png');
