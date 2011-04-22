#!perl -T
use strict;
use warnings;

# $Id: 080_text.t 6323 2010-06-25 09:26:13Z mullet $

use Test::More tests => 2;

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

sub font_size {
    my $rasterize;
    my $svg;
    my $node;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $node = $svg->text(id => 'te01', 'font-size' => 'large');
    $node->cdata('Hello World');
    $rasterize->start_node_hook(sub {
	my ($rasterize, $state) = @_;
	if($state->node_attributes->{id}) {
	    if($state->node_attributes->{id} eq 'te01') {
		is($state->properties->{'font-size'}, 18,
		   'font-size large');
	    }
	}
    });
    $rasterize->rasterize(svg => $svg);
}

state_cdata;
font_size;
