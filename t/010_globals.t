#!perl -T
use strict;
use warnings;

use Test::More tests => 52;

use SVG;
use Test::Exception;
use SVG::Rasterize;

sub regexp {
    ok('0'        =~ /^$SVG::Rasterize::INTEGER$/, '0 integer');
    ok('+0'       =~ /^$SVG::Rasterize::INTEGER$/, '+0 integer');
    ok('-0'       =~ /^$SVG::Rasterize::INTEGER$/, '-0 integer');
    ok('5'        =~ /^$SVG::Rasterize::INTEGER$/, '5 integer');
    ok('-1234'    =~ /^$SVG::Rasterize::INTEGER$/, '-1234 integer');
    ok('001'      =~ /^$SVG::Rasterize::INTEGER$/, '001 integer');
    ok('-'        !~ /^$SVG::Rasterize::INTEGER$/, '- no integer');
    ok('30'       !~ /^$SVG::Rasterize::FRACTION$/, '30 no fraction');
    ok('0.1'      =~ /^$SVG::Rasterize::FRACTION$/, '0.1 fraction');
    ok('30.'      =~ /^$SVG::Rasterize::FRACTION$/, '30. fraction');
    ok('-30.'     =~ /^$SVG::Rasterize::FRACTION$/, '-30. fraction');
    ok('-.1'      =~ /^$SVG::Rasterize::FRACTION$/, '-.1 fraction');
    ok('+.3'      =~ /^$SVG::Rasterize::FRACTION$/, '+.3 fraction');
    ok('+00.3'    =~ /^$SVG::Rasterize::FRACTION$/, '+00.3 fraction');
    ok('e+03'     =~ /^$SVG::Rasterize::EXPONENT$/, 'e+03 exponent');
    ok('E4'       =~ /^$SVG::Rasterize::EXPONENT$/, 'E4 exponent');
    ok('E.4'      !~ /^$SVG::Rasterize::EXPONENT$/, 'E.4 no exponent');
    ok('30.'      =~ /^$SVG::Rasterize::FLOAT$/, '30. floating point');
    ok('-30.'     =~ /^$SVG::Rasterize::FLOAT$/, '-30. floating point');
    ok('-.1'      =~ /^$SVG::Rasterize::FLOAT$/, '-.1 floating point');
    ok('+.3'      =~ /^$SVG::Rasterize::FLOAT$/, '+.3 floating point');
    ok('+00.3'    =~ /^$SVG::Rasterize::FLOAT$/, '+00.3 floating point');
    ok('30.E-01'  =~ /^$SVG::Rasterize::FLOAT$/, '30.E-01 floating point');
    ok('-30.E-01' =~ /^$SVG::Rasterize::FLOAT$/, '-30.E-01 floating point');
    ok('-.1E-01'  =~ /^$SVG::Rasterize::FLOAT$/, '-.1E-01 floating point');
    ok('+.3E-01'  =~ /^$SVG::Rasterize::FLOAT$/, '+.3E-01 floating point');
    ok('+00.3E-01'=~ /^$SVG::Rasterize::FLOAT$/, '+00.3E-01 floating point');
    ok('+00.3E-01'=~ /^$SVG::Rasterize::FLOAT$/, '+00.3E-01 floating point');
    ok('123E5'    =~ /^$SVG::Rasterize::FLOAT$/, '123E5 floating point');
    ok('123E5'    !~ /^$SVG::Rasterize::INTEGER$/, '123E5 no integer');
    ok('123E5'    =~ /^$SVG::Rasterize::A_NUMBER$/, '123E5 number');
    ok('12345'    =~ /^$SVG::Rasterize::A_NUMBER$/, '12345 number');
    ok('+.1E-7'   =~ /^$SVG::Rasterize::A_NUMBER$/, '+.1E-7 number');
    ok('--1'      !~ /^$SVG::Rasterize::A_NUMBER$/, '--1 not number');
}

sub units {
    my $rasterize;

    $rasterize = SVG::Rasterize->new;
    cmp_ok($rasterize->px_per_in, '==', 90, 'default value');
    cmp_ok(abs($rasterize->px_per_in(81.3e-1) - 8.13), '<', 1e-10,
	   'mutator return');
    cmp_ok(abs($rasterize->px_per_in - 8.13), '<', 1e-10,
	   'new value');
    throws_ok(sub { $rasterize->px_per_in('--1') },
	      qr/px_per_in/, 'px_per_in check');

    # map_abs_length
    $rasterize = SVG::Rasterize->new;
    throws_ok(sub { $rasterize->map_abs_length }, qr/map_abs_length/,
	      'map_abs_length no argument');
    throws_ok(sub { $rasterize->map_abs_length('-13xy') },
	      qr/map_abs_length/,
	      'map_abs_length invalid argument');
    lives_ok(sub { $rasterize->map_abs_length('-13px') },
	     'valid argument');
    throws_ok(sub { $rasterize->map_abs_length('-13 px') },
	      qr/map_abs_length/,
	      'map_abs_length invalid argument (space)');
    cmp_ok($rasterize->map_abs_length(-3), '==', -3, 'no unit');
    cmp_ok($rasterize->map_abs_length('-3px'), '==', -3, 'px');
    throws_ok(sub { $rasterize->map_abs_length(' -25px ') },
	      qr/to SVG::Rasterize::map_abs_length did not pass regex/,
	      'whitespace in length');
    cmp_ok($rasterize->map_abs_length('1.5in'), '==', 135, 'in');
    $rasterize->dpi(120);
    cmp_ok($rasterize->map_abs_length('1.5in'), '==', 180,
	   'in, custom dpi');
    cmp_ok(abs($rasterize->map_abs_length('5.08cm') - 240), '<', 1e-10,
	   'cm');
}

sub typeglobs {
    $SVG::Rasterize::DPI = 30;
    is($SVG::Rasterize::PX_PER_IN, 30, 'setting DPI');

    my $rasterize = SVG::Rasterize->new;
    is($rasterize->px_per_in, 30, 'getting object variable');
    $rasterize->dpi(120);
    is($rasterize->dpi, 120, 'setting object variable dpi');
    is($rasterize->px_per_in, 120, 'setting object variable dpi');
}

regexp;
units;
typeglobs;
