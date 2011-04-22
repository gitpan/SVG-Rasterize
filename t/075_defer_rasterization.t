#!perl -T
use strict;
use warnings;

# $Id: 075_defer_rasterization.t 6473 2011-04-21 07:38:30Z powergnom $

use Test::More tests => 28;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub defer_attribute {
    my $rasterize;
    my $svg;
    my $node;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $node = $svg->group(id => 'g01');
    $node = $node->text(id => 'te01');
    $node->tspan(id => 'ts01');
    @expected = ('svg', 'g01', 'te01', 'ts01');
    $hook = sub {
	my ($rasterize, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'g01') {
	    ok(!$state->defer_rasterization,
		'group does not defer rasterization');
	}
	if($state->node_attributes->{id} eq 'te01') {
	    is($state->defer_rasterization, 1,
		'text defers rasterization');
	}
	if($state->node_attributes->{id} eq 'ts01') {
	    is($state->defer_rasterization, 1,
		'tspan defers rasterization');
	}
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);
}

sub child_states {
    my $rasterize;
    my $svg;
    my $node;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $node = $svg->group(id => 'g01');
    $node = $node->text(id => 'te01');
    $node->tspan(id => 'ts01');
    @expected = ('svg', 'g01', 'te01', 'ts01');
    $rasterize->start_node_hook(sub {
	my ($rasterize, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	ok(!defined($state->child_states),
	    'child_states undef at start');
	ok(!defined($state->shift_child_state),
	   'shift_child_state works and returns undef');
    });
    $rasterize->end_node_hook(sub {
	my ($rasterize, $state) = @_;
	if($state->node_attributes->{id} eq 'g01') {
	    ok(!defined($state->child_states),
	       'child_states undef at end of g01');
	}
	if($state->node_attributes->{id} eq 'te01') {
	    ok(defined($state->child_states),
	       'child_states defined at end of te01');
	    is(scalar(@{$state->child_states}), 0,
	       'but no child states (any more) at end of te01');
	}
	if($state->node_attributes->{id} eq 'ts01') {
	    ok(defined($state->parent->child_states),
	       'te01 child_states defined at end of ts01');
	    is(scalar(@{$state->parent->child_states}), 1,
	       'one child states (any more) at end of te01');
	    is($state->parent->shift_child_state->node_attributes->{id},
	       'ts01',
	       'child state is the ts01 state');
	    ok(!defined($state->parent->shift_child_state),
	       'next shift_child_state is undef');
	}
    });
    $rasterize->rasterize(svg => $svg);
}

defer_attribute;
child_states;
