#!perl -T
use strict;
use warnings;

use Test::More tests => 2;
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
    $svg = SVG->new;
#    $rasterize->rasterize(svg => $svg);
}

svg_rasterize;
