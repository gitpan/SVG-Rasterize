#!perl -T
use strict;
use warnings;

use Test::More tests => 13;

use SVG::Rasterize;
use SVG;

sub svg_rasterize {
    my $rasterize;

    $rasterize = SVG::Rasterize->new;
    ok(defined($rasterize), 'rasterize defined');
    isa_ok($rasterize, 'SVG::Rasterize');
    can_ok($rasterize,
	   'engine',
	   'rasterize');
}

sub svg_rasterize_engine {
    my $rasterize;
    my $svg;
    my $engine;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new()->firstChild;
    is($svg->getNodeName, 'svg', 'node name control');
    $rasterize->rasterize(width => 100, height => 100, svg => $svg);
    $engine = $rasterize->engine;
    ok(defined($engine), 'engine defined');
    isa_ok($engine, 'SVG::Rasterize::Cairo');
    can_ok($engine,
	   'width', 'height', 'context');
}

sub svg_rasterize_state {
    my $rasterize;
    my $svg;
    my $state;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new->firstChild;
    $state     = SVG::Rasterize::State->new
	(rasterize          => $rasterize,
	 node            => $svg,
	 node_name       => $svg->getNodeName,
	 node_attributes => {$svg->getAttributes},
	 cdata           => undef,
	 child_nodes     => undef);
    ok(defined($state), 'state defined');
    isa_ok($state, 'SVG::Rasterize::State');
    can_ok($state,
	   'node',
	   'node_name',
	   'node_attributes',
	   'shift_child_node');
}

sub svg_rasterize_textnode {
    my $node;

    $node = SVG::Rasterize::TextNode->new(data => '');
    ok(defined($node), 'node defined');
    isa_ok($node, 'SVG::Rasterize::TextNode');
    can_ok($node,
	   'getNodeName',
	   'getAttributes',
	   'getChildNodes',
	   'getData');
}

svg_rasterize;
svg_rasterize_engine;
svg_rasterize_state;
svg_rasterize_textnode;
