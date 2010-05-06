#!perl -T
use strict;
use warnings;

use Test::More tests => 10;
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

svg_rasterize;
