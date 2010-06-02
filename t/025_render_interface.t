#!perl -T
use strict;
use warnings;

use Test::More tests => 16;
use Test::Exception;

use SVG::Rasterize;
use SVG;

sub svg_rasterize {
    my $rasterize;
    my $svg;

    $rasterize = SVG::Rasterize->new;
    throws_ok(sub { $rasterize->rasterize }, qr/svg/, 'blank rasterize');
    throws_ok(sub { $rasterize->rasterize(svg => undef) },
	      qr/svg.*SVG\:\:Rasterize|SVG\:\:Rasterize.*svg/,
	      'invalid svg');
    # more input validation

    $rasterize = SVG::Rasterize->new(width => 400, height => 300);
    is($rasterize->width, 400, 'width before rasterize');
    is($rasterize->height, 300, 'height before rasterize');
    $svg       = SVG->new;
    $rasterize->rasterize(svg => $svg);
    is($rasterize->width, 400, 'width after rasterize');
    is($rasterize->height, 300, 'height after rasterize');
    $rasterize->rasterize(svg => $svg, width => 600, height => 100);
    is($rasterize->width, 400, 'width after rasterize');
    is($rasterize->height, 300, 'height after rasterize');

    is($rasterize->engine->width, 600, 'engine width');
    is($rasterize->engine->height, 100, 'engine height');
}

sub engine_args {
    my $rasterize;
    my $svg;

    $rasterize = SVG::Rasterize->new(width => 10, height => 20);
    $svg       = SVG->new;
    $rasterize->rasterize(svg => $svg);
    is($rasterize->engine->width, 10, 'engine width from attribute');
    is($rasterize->engine->height, 20, 'engine height from attribute');

    $rasterize->rasterize(svg => $svg, width => 11, height => 21);
    is($rasterize->engine->width, 11, 'engine width from parameter');
    is($rasterize->engine->height, 21, 'engine height from parameter');

    $rasterize->rasterize(svg => $svg, width => 11, height => 21,
			  engine_args => {width => 12, height => 22});
    is($rasterize->engine->width, 12, 'engine width from engine_args');
    is($rasterize->engine->height, 22, 'engine height from engine_args');
}

svg_rasterize;
engine_args;
