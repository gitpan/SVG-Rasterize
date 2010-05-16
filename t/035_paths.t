#!perl -T
use strict;
use warnings;

use Test::More tests => 4;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub path_data {
    my $rasterize;
    my $svg;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $svg->path(id => 'p01', d => 'M100 100 1.3 0.14 -15 31');
    @expected = ('svg', 'p01');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $svg->path(id => 'p01', d => 'M100 100 0.1 1 3L1 -0.1 12.34.1 5 M12');
    @expected = ('svg', 'p01');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);
}

path_data;
