#!perl -T
use strict;
use warnings;

# $Id$

use Test::More tests => 1;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub state_cdata {
    my $rasterize;
    my $svg;
    my $node;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $node = $svg->group(id => 'g01');
    $node->text(id => 'te01')->cdata('Hello World');
    $rasterize->start_node_hook(sub {
	my ($rasterize, $state) = @_;
	if($state->node_name eq '#text') {
	    is($state->cdata, 'Hello World', 'cdata arrived at State');
	}
    });
    $rasterize->rasterize(svg => $svg);
}

state_cdata;
