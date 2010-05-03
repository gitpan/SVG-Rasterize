#!perl -T
use strict;
use warnings;

use Test::More tests => 10;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub tree_traversal {
    my $rasterize;
    my $svg;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->group(id => 'foo')->group(id => 'bar')->line
	(x1 => 0, y1 => 100, x2 => 50, y2 => 140, id => 'baz');
    @expected = ('svg', 'g', 'g', 'line');
    $rasterize->before_node_hook
	(sub { is($_[2], shift(@expected), 'node name') });
    $rasterize->rasterize(svg => $svg);
    is(scalar(@expected), 0, 'all used up');

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->group(id => 'foo')->group(id => 'bar')->line
	(x1 => 0, y1 => 100, x2 => 50, y2 => 140, id => 'baz');
    @expected = (undef, 'foo', 'bar', 'baz');
    $rasterize->before_node_hook
	(sub { is($_[3]->{id}, shift(@expected), 'node id') });
    $rasterize->rasterize(svg => $svg);
    is(scalar(@expected), 0, 'all used up');
}

tree_traversal;
