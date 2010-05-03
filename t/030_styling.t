#!perl -T
use strict;
use warnings;

use Test::More tests => 38;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub set_property {
    my $rasterize;
    my $svg;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('stroke-width' => '9pt');
    is($svg->firstChild->attrib('stroke-width'), '9pt', 'check attrib');
    $hook      = sub {
	if($_[2] eq 'svg') {
	    is($_[3]->{'stroke-width'}, '9pt', 'xsl stroke-width');
	}
    };
    $rasterize->before_node_hook($hook);
    $rasterize->rasterize(svg => $svg);

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('stroke-width' => '9pt');
    $svg->firstChild->attrib('id'           => 'svg');
    $svg->group(id => 'g01');
    is($svg->firstChild->attrib('stroke-width'), '9pt', 'check attrib');
    is($svg->firstChild->attrib('id'), 'svg', 'check attrib');
    $hook = sub {
	if($_[3]->{id} eq 'svg') {
	    is($_[3]->{'stroke-width'}, '9pt', 'xsl stroke-width');
	}
	if($_[3]->{id} eq 'g01') {
	    ok(!defined($_[3]->{'stroke-width'}),
	       'xsl stroke-width not on group');
	}
    };
    $rasterize->before_node_hook($hook);
    $hook = sub {
	my ($render, $state) = @_;
	if($state->node_attributes->{id} eq 'svg') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on svg');
	}	
	if($state->node_attributes->{id} eq 'g01') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on g01');
	}	
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);

    $svg->group(id => 'g02', 'stroke-width' => '10px');
    @expected = ('svg', 'g01', 'g02');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'svg') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on svg');
	}	
	if($state->node_attributes->{id} eq 'g01') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on g01');
	}	
	if($state->node_attributes->{id} eq 'g02') {
	    is($state->properties->{'stroke-width'}, 10,
	       'property stroke-width on g02');
	}	
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);

    $svg->group(id => 'g03', 'stroke-width' => '10px',
		style => 'stroke-width:1in');
    @expected = ('svg', 'g01', 'g02', 'g03');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'svg') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on svg');
	}	
	if($state->node_attributes->{id} eq 'g01') {
	    is($state->properties->{'stroke-width'}, 11.25,
	       'property stroke-width on g01');
	}	
	if($state->node_attributes->{id} eq 'g02') {
	    is($state->properties->{'stroke-width'}, 10,
	       'property stroke-width on g02');
	}	
	if($state->node_attributes->{id} eq 'g03') {
	    is($state->properties->{'stroke-width'}, 90,
	       'property stroke-width on g03');
	}
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);
}

sub color {
    my $rasterize;
    my $svg;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $svg->group(id => 'g01');
    $svg->group(id => 'g02', 'stroke' => 'rgb(20, 255, 1)');
    $svg->group(id => 'g03', 'stroke' => 'rgb(13%, -10%, 120%)',
		style => 'stroke-width:1in');
    @expected = ('svg', 'g01', 'g02', 'g03');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'g01') {
	    is($state->properties->{'stroke'}, 'none',
	       'property stroke is "none" on g01');
	}
	if($state->node_attributes->{id} eq 'g02') {
	    is_deeply($state->properties->{'stroke'}, [20, 255, 1],
	       'property stroke on g02');
	}
	if($state->node_attributes->{id} eq 'g03') {
	    is_deeply($state->properties->{'stroke'}, [33, 0, 255],
	       'property stroke on g03');
	}
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);
}

sub whitespace {
    my $rasterize;
    my $svg;
    my $hook;
    my @expected;

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 400, height => 300);
    $svg->firstChild->attrib('id' => 'svg');
    $svg->group(id => 'g01', 'stroke' => "\trgb(20, 255, 1) ");
    @expected = ('svg', 'g01');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'g01') {
	    is_deeply($state->properties->{'stroke'}, [20, 255, 1],
	       'property stroke on g01');
	}
    };
    $rasterize->start_node_hook($hook);
    $rasterize->rasterize(svg => $svg);

    $rasterize->normalize_attributes(0);
    @expected = ('svg', 'g01');
    $hook = sub {
	my ($render, $state) = @_;
	is($state->node_attributes->{id}, shift(@expected),
	   'expected id');
	if($state->node_attributes->{id} eq 'g01') {
	    is_deeply($state->properties->{'stroke'}, [20, 255, 1],
	       'property stroke on g01 with attribute normalization');
	}
    };
    $rasterize->start_node_hook($hook);
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Invalid color specification/,
	      'without attribute normalization');
}

set_property;
color;
whitespace;
