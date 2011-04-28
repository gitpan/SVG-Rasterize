#!/usr/bin/perl
use strict;
use warnings;

use SVG::Rasterize;
use SVG;

my $rasterize;
my $svg;
my $g;
my $points;
my $text;
my $node;

$svg = SVG->new;

$svg->rect('width' => 300, 'height' => 300, 'fill' => 'white');

# baseline test
$svg->text('x' => 30, 'y' => 20, 'fill' => 'red')->cdata('Hello World');
$svg->line(x1 => 30, y1 => 20, x2 => 104, y2 => 20, 'stroke' => 'black');

# deferred rasterization, flush in right order, text carries on
$text = $svg->text('x' => 30, 'y' => 60);
$text->tspan->cdata('foo');
$text->a('xlink:href' => 'foo')->circle
    (cx => 50, cy => 60, r => 7, 'fill' => 'yellow');
$text->tspan('font-size' => 'large')->cdata('bar');

# text-anchor
$svg->line(x1 => 80, y1 => 100, x2 => 80, y2 => 140, 'stroke' => 'blue');
$svg->text('x' => 80, 'y' => 110)->cdata('baz');
$text = $svg->text('x' => 80, 'y' => 122, 'text-anchor' => 'middle');
$text->cdata('qux');
$text = $svg->text('x' => 80, 'y' => 134, 'text-anchor' => 'end');
$text->cdata('corge');

# positioning of single characters, directly from SVG spec
$g    = $svg->group(transform => 'translate(50, 200) scale(0.2)');
$g    = $g->group('font-family' => 'Verdana', 'font-size' => 45);
$text = $g->text(fill => 'rgb(255, 164, 0)');
$node = $text->tspan(x => '300 350 400 450 500 550 600 650', 'y' => "100");
$node->cdata('Cute and');
$node = $text->tspan(x => '375 425 475 525 575', 'y' => "200");
$node->cdata('fuzzy');
$g->rect(x => "1", 'y' => "1", width => "998", height => "298",
	 fill => "none", stroke => "blue", 'stroke-width' => "2");

# new chunks are started only due to y
$text = $svg->text(x => 150, 'y' => '50 52 54 56 58 60');
$text->cdata('stairs');

$rasterize = SVG::Rasterize->new;
$rasterize->rasterize(width => 300, height => 300, svg => $svg);

$rasterize->write(type => 'png', file_name => 'text.png');
