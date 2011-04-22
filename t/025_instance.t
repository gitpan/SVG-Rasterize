#!perl -T
use strict;
use warnings;

use Test::More tests => 18;
use Test::Exception;

use SVG::Rasterize;
use SVG;
use SVG::Rasterize::Cairo;

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
    my $engine;
    my $context;

    $engine = SVG::Rasterize::Cairo->new(width => 10, height => 20);
    ok(defined($engine), 'engine defined');
    isa_ok($engine, 'SVG::Rasterize::Cairo');
    can_ok($engine,
	   'width', 'height', 'context',
	   'draw_path', 'draw_text',
	   'write');
    is($engine->width, 10, 'width accessor control');
    is($engine->height, 20, 'height accessor control');
    $context = $engine->context;
    ok(defined($context), 'context defined');
    isa_ok($context, 'Cairo::Context');
}

sub svg_rasterize_state {
    my $rasterize;
    my $svg;
    my $state;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new->firstChild;
    $state     = SVG::Rasterize::State->new
	(rasterize          => $rasterize,
	 node               => $svg,
	 node_name          => $svg->getNodeName,
	 node_attributes    => {$svg->getAttributes},
	 cdata              => undef,
	 child_nodes        => undef);
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

    eval { SVG::Rasterize::TextNode->new };
    ok(defined($@), 'node without data fails');
    isa_ok($@, 'SVG::Rasterize::Exception::ParamsValidate');

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
