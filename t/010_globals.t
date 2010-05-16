#!perl -T
use strict;
use warnings;

use Test::More tests => 67;

use SVG;
use Test::Exception;
use SVG::Rasterize;
use SVG::Rasterize::Regexes qw(:all);

sub transform_validation {
    ok('0'        =~ /^$RE_NUMBER{INTEGER}$/,
       '0 integer');
    ok('+0'       =~ /^$RE_NUMBER{INTEGER}$/,
       '+0 integer');
    ok('-0'       =~ /^$RE_NUMBER{INTEGER}$/,
       '-0 integer');
    ok('5'        =~ /^$RE_NUMBER{INTEGER}$/,
       '5 integer');
    ok('-1234'    =~ /^$RE_NUMBER{INTEGER}$/,
       '-1234 integer');
    ok('001'      =~ /^$RE_NUMBER{INTEGER}$/,
       '001 integer');
    ok('-'        !~ /^$RE_NUMBER{INTEGER}$/,
       '- no integer');
    ok('30'       !~ /^$RE_NUMBER{FRACTION}$/,
       '30 no fraction');
    ok('0.1'      =~ /^$RE_NUMBER{FRACTION}$/,
       '0.1 fraction');
    ok('30.'      =~ /^$RE_NUMBER{FRACTION}$/,
       '30. fraction');
    ok('-30.'     =~ /^$RE_NUMBER{FRACTION}$/,
       '-30. fraction');
    ok('-.1'      =~ /^$RE_NUMBER{FRACTION}$/,
       '-.1 fraction');
    ok('+.3'      =~ /^$RE_NUMBER{FRACTION}$/,
       '+.3 fraction');
    ok('+00.3'    =~ /^$RE_NUMBER{FRACTION}$/,
       '+00.3 fraction');
    ok('e+03'     =~ /^$RE_NUMBER{EXPONENT}$/,
       'e+03 exponent');
    ok('E4'       =~ /^$RE_NUMBER{EXPONENT}$/,
       'E4 exponent');
    ok('E.4'      !~ /^$RE_NUMBER{EXPONENT}$/,
       'E.4 no exponent');
    ok('30.'      =~ /^$RE_NUMBER{FLOAT}$/,
       '30. floating point');
    ok('-30.'     =~ /^$RE_NUMBER{FLOAT}$/,
       '-30. floating point');
    ok('-.1'      =~ /^$RE_NUMBER{FLOAT}$/,
       '-.1 floating point');
    ok('+.3'      =~ /^$RE_NUMBER{FLOAT}$/,
       '+.3 floating point');
    ok('+00.3'    =~ /^$RE_NUMBER{FLOAT}$/,
       '+00.3 floating point');
    ok('30.E-01'  =~ /^$RE_NUMBER{FLOAT}$/,
       '30.E-01 floating point');
    ok('-30.E-01' =~ /^$RE_NUMBER{FLOAT}$/,
       '-30.E-01 floating point');
    ok('-.1E-01'  =~ /^$RE_NUMBER{FLOAT}$/,
       '-.1E-01 floating point');
    ok('+.3E-01'  =~ /^$RE_NUMBER{FLOAT}$/,
       '+.3E-01 floating point');
    ok('+00.3E-01'=~ /^$RE_NUMBER{FLOAT}$/,
       '+00.3E-01 floating point');
    ok('+00.3E-01'=~ /^$RE_NUMBER{FLOAT}$/,
       '+00.3E-01 floating point');
    ok('123E5'    =~ /^$RE_NUMBER{FLOAT}$/,
       '123E5 floating point');
    ok('123E5'    !~ /^$RE_NUMBER{INTEGER}$/,
       '123E5 no integer');
    ok('123E5'    =~ /^$RE_NUMBER{A_NUMBER}$/,
       '123E5 number');
    ok('12345'    =~ /^$RE_NUMBER{A_NUMBER}$/,
       '12345 number');
    ok('+.1E-7'   =~ /^$RE_NUMBER{A_NUMBER}$/,
       '+.1E-7 number');
    ok('--1'      !~ /^$RE_NUMBER{A_NUMBER}$/,
       '--1 not number');
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

sub pAR_validation {
    ok('none' =~ $RE_VIEW_BOX{ALIGN}, q{ALIGN 'none'});
    ok('xMidYMax' =~ $RE_VIEW_BOX{ALIGN}, q{ALIGN 'xMidYMax'});
    ok('defer xMidYMax' =~ $RE_VIEW_BOX{PAR},
       q{PAR 'defer xMidYMax'});
    ok('defer xMidYMax meet' =~ $RE_VIEW_BOX{PAR},
       q{PAR 'defer xMidYMax meet'});
}

sub path_data_validation {
    ok('M1 2' =~ $RE_PATH{p_PATH_LIST},
       'just move');
    ok('M1.2' =~ $RE_PATH{p_PATH_LIST},
       'non intuitive move');
    ok('M1.2 0' =~ $RE_PATH{p_PATH_LIST},
       'more intuitive move');
    ok('M1.2 0.3' =~ $RE_PATH{p_PATH_LIST},
       'ambiguous move');
    ok('M1.2 0.3C1.3,5,10,1,5-4' =~ $RE_PATH{p_PATH_LIST},
       'move curve-to');
}

sub path_data_splitting {
    my $rasterize = SVG::Rasterize->new;

    is_deeply([$rasterize->_split_path_data('M3 -4')],
	      [['M', 3, -4]],
	      q{path data 'M3 -4'});
    is_deeply([$rasterize->_split_path_data('M3 -4 12.3')],
	      [['M', 3, -4], ['M', '12.', 3]],
	      q{path data 'M3 -4 12.3'});
    is_deeply([$rasterize->_split_path_data('M3 -4 12.23')],
	      [['M', 3, -4], ['M', 12.2, 3]],
	      q{path data 'M3 -4 12.23'});
    is_deeply([$rasterize->_split_path_data('M-13')],
	      [['M', -1, 3]],
	      q{path data 'M-13'});
    is_deeply([$rasterize->_split_path_data('M-13.4')],
	      [['M', '-13.', 4]],
	      q{path data 'M-13.4'});
    is_deeply([$rasterize->_split_path_data('M3-1l100Z')],
	      [['M', 3, -1], ['l', 10, 0], ['Z']],
	      q{path data 'M3-1l100Z'});
}

transform_validation;
units;
typeglobs;
pAR_validation;
path_data_validation;
path_data_splitting;
