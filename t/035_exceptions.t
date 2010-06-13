#!perl -T
use strict;
use warnings;

use Test::More tests => 34;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub test_ex_pa {
    my $ex;
    my $rasterize;

    $rasterize = SVG::Rasterize->new;
    $rasterize->{state} = 'foo';
    throws_ok(sub { $rasterize->ex_pa('path data', 'bar') },
	      qr/Failed to process the path data string \'bar\' /,
	      'ex_pa message');
    $ex = $@;
    isa_ok($ex, 'SVG::Rasterize::Exception::Parse');
    isa_ok($ex, 'SVG::Rasterize::Exception::Base');
    isa_ok($ex, 'Exception::Class::Base');
    can_ok($ex, 'state');
    is($ex->state, 'foo', 'state is foo');

    $rasterize->{state} = 'qux';
    is_deeply([$rasterize->_split_path_data('M')], [1], 'in_error is 1');
}

sub test_ie {
    my $rasterize;
    my $svg;
    my $ex;

    $rasterize = SVG::Rasterize->new;

    $svg = SVG->new(width => 100, height => 100);
    $svg->rect(width => 10, height => -10);
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Negative rectangle height \-10\./,
	      'negative rectangle height');
    $ex = $@;
    isa_ok($ex, 'SVG::Rasterize::Exception::InError');
    isa_ok($ex, 'SVG::Rasterize::Exception::Base');
    isa_ok($ex, 'Exception::Class::Base');
    can_ok($ex, 'state');
    isa_ok($ex->state, 'SVG::Rasterize::State', 'state isa State');
    is($ex->state->node_name, 'rect', 'node name is rect');

    $svg = SVG->new(width => 100, height => 100);
    $svg->rect(width => -10, height => 1);
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Negative rectangle width \-10\./,
	      'undefined rectangle width');

    $svg = SVG->new(width => 100, height => 100);
    $svg->rect(width => 1, height => 1, rx => -1);
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Negative rectangle corner radius \-1\./,
	      'undefined rectangle rx');

    $svg = SVG->new(width => 100, height => 100);
    $svg->rect(width => 1, height => 1, ry => -1);
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Negative rectangle corner radius \-1\./,
	      'undefined rectangle ry');
}

sub test_pv {
    my $ex;
    my $rasterize;

    $rasterize = SVG::Rasterize->new;
    throws_ok(sub { $rasterize->px_per_in('foo') },
	      qr/foo/,
	      'ex_pv message');
    $ex = $@;
    isa_ok($ex, 'SVG::Rasterize::Exception::ParamsValidate');
    isa_ok($ex, 'SVG::Rasterize::Exception::Base');
    isa_ok($ex, 'Exception::Class::Base');
    can_ok($ex, 'state');
    ok(!defined($ex->state), 'state is undefined');

    $rasterize = SVG::Rasterize->new;
    $rasterize->{state} = 'bar';
    throws_ok(sub { $rasterize->px_per_in('foo') },
	      qr/foo/,
	      'ex_pv message');
    $ex = $@;
    is($ex->state, 'bar', 'state is bar');
}

sub test_ie_pv {
    my $rasterize;
    my $svg;
    my $ex;

    $rasterize = SVG::Rasterize->new;

    $svg = SVG->new(width => 100, height => 100);
    $svg->rect(width => 10, height => 10, style => 'stroke-width:foo');
    throws_ok(sub { $rasterize->rasterize(svg => $svg) },
	      qr/Property stroke-width failed validation:/,
	      'stroke-width foo');
    $ex = $@;
    isa_ok($ex, 'SVG::Rasterize::Exception::InError');
    isa_ok($ex, 'SVG::Rasterize::Exception::Base');
    isa_ok($ex, 'Exception::Class::Base');
    can_ok($ex, 'state');
    isa_ok($ex->state, 'SVG::Rasterize::State', 'state isa State');
    is($ex->state->node_name, 'rect', 'node name is rect');
}

sub readonly {
    my $rasterize;
    my $svg;
    my $state;

    $rasterize = SVG::Rasterize->new;
    throws_ok(sub { $rasterize->engine('foo') },
	      qr/Attribute SVG::Rasterize->engine is readonly/,
	      'readonly attribute');

    $rasterize = SVG::Rasterize->new;
    $svg       = SVG->new(width => 10, height => 10)->firstChild;
    $state     = SVG::Rasterize::State->new
	(rasterize       => $rasterize,
	 node            => $svg,
	 node_name       => $svg->getNodeName,
	 node_attributes => {$svg->getAttributes},
	 cdata           => undef,
	 child_nodes     => undef);
    throws_ok(sub { $state->parent('foo') },
	      qr/Attribute SVG::Rasterize::State->parent is readonly/,
	      'readonly attribute');
}

test_ex_pa;
test_ie;
test_pv;
test_ie_pv;
readonly;
