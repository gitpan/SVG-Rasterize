#!perl -T
use strict;
use warnings;

use Test::More tests => 177;

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
    my $d;

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
    ok('M4 3L10 1' =~ $RE_PATH{p_PATH_LIST},
       'move line');
    ok('L4 3L10 1' !~ $RE_PATH{p_PATH_LIST},
       'line line invalid');
    ok('M4 3H10' =~ $RE_PATH{p_PATH_LIST},
       'move horizontal line');
    $d = 'M 125 , 75 L 100 50 20 30 H 1  C 1 2 3 4 5 6 a 1 2 3 0 1 4 5';
    ok('M 125 , 75 L 100 50 20 30' =~ $RE_PATH{p_PATH_LIST},
       'M 125 , 75 L 100 50 20 30');
    ok('M 125 , 75 L 100 50 20 30 H1' =~ $RE_PATH{p_PATH_LIST},
       'M 125 , 75 L 100 50 20 30 H1');
    ok('M 125 , 75 L 100 50 20 30 H 1' =~ $RE_PATH{p_PATH_LIST},
       'M 125 , 75 L 100 50 20 30 H 1');
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81 A 107,107 0 0,1 600,295 ';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81 A 107,107 0 0,1 600,295 A 107,107 0 0,1 600,81 z';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81 A 107,107 0 0,1 600,295 A 107,107 0 0,1 600,81 z '.
	 'M 600,139';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81 A 107,107 0 0,1 600,295 A 107,107 0 0,1 600,81 z'.
	 'M 600,139';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    $d = 'M 600,81 A 107,107 0 0,1 600,295 '.
	 'A 107,107 0 0,1 600,81 z'.
	 'M 600,139 A 49,49 0 0,1 600,237 '.
	 'A 49,49 0 0,1 600,139 z';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
}

sub path_data_splitting {
    my $rasterize = SVG::Rasterize->new;
    my $d;

    is_deeply([$rasterize->_split_path_data('M3 -4')],
	      [['M', 3, -4]],
	      q{path data 'M3 -4'});
    is_deeply([$rasterize->_split_path_data('M3 -4 12.3')],
	      [['M', 3, -4], ['L', '12.', 3]],
	      q{path data 'M3 -4 12.3'});
    is_deeply([$rasterize->_split_path_data('M3 -4 M12.3')],
	      [['M', 3, -4], ['M', '12.', 3]],
	      q{path data 'M3 -4 M12.3'});
    is_deeply([$rasterize->_split_path_data('M3 -4 12.23')],
	      [['M', 3, -4], ['L', 12.2, 3]],
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
    is_deeply([$rasterize->_split_path_data('M-1-2H.4 3')],
	      [['M', -1, -2], ['H', '.4'], ['H', 3]],
	      q{path data 'M-1-2H.4 3'});
    is_deeply([$rasterize->_split_path_data('C1 2 3 4 5 6 1 2 3 4 5 6')],
	      [['C', 1, 2, 3, 4, 5, 6], ['C', 1, 2, 3, 4, 5, 6]],
	      q{path data 'C1 2 3 4 5 6 1 2 3 4 5 6'});
    is_deeply([$rasterize->_split_path_data('S1 2 3 4 5 6 1 2')],
	      [['S', 1, 2, 3, 4], ['S', 5, 6, 1, 2]],
	      q{path data 'S1 2 3 4 5 6 1 2'});
    is_deeply([$rasterize->_split_path_data('Q1 2 3 4t56')],
	      [['Q', 1, 2, 3, 4], ['t', 5, 6]],
	      q{path data 'Q1 2 3 4t56'});
    is_deeply([$rasterize->_split_path_data('A1 2 -3 0 0 1 4')],
	      [['A', 1, 2, -3, 0, 0, 1, 4]],
	      q{path data 'A1 2 -3 0 0 1 4'});
    $d = 'M 125,75 a100,50 0 0,0 100,50';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    is_deeply([$rasterize->_split_path_data($d)],
	      [['M', 125, 75], ['a', 100, 50, 0, 0, 0, 100, 50]],
	      qq{path data '$d'});
    $d = 'M 125 , 75 L 100 50 20 30 H 1  C 1 2 3 4 5 6 a 1 2 3 0 1 4 5';
    ok($d =~ $RE_PATH{p_PATH_LIST}, $d);
    is_deeply([$rasterize->_split_path_data($d)],
	      [['M', 125, 75], ['L', 100, 50], ['L', 20, 30], ['H', 1],
	       ['C', 1, 2, 3, 4, 5, 6], ['a', 1, 2, 3, 0, 1, 4, 5]],
	      qq{path data '$d'});
}

sub angle {
    my $pi = atan2(0, -1);
    my $diff;
    my $angle;
    foreach my $glob ($pi/4, 3*$pi/4, 5*$pi/4, 7*$pi/4) {
	$diff  = $pi / 4;
	$angle = SVG::Rasterize::_angle(cos($glob),
					sin($glob),
					cos($glob + $diff),
					sin($glob + $diff));
	ok($angle > 0, '1. quadrant > 0');
	ok($angle < $pi / 2, '1. quadrant < pi / 2');
	$diff  = 3 * $pi / 4;
	$angle = SVG::Rasterize::_angle(cos($glob),
					sin($glob),
					cos($glob + $diff),
					sin($glob + $diff));
	ok($angle > $pi / 2, '2. quadrant > pi / 2');
	ok($angle < $pi, '2. quadrant < pi');
	$diff  = 5 * $pi / 4;
	$angle = SVG::Rasterize::_angle(cos($glob),
					sin($glob),
					cos($glob + $diff),
					sin($glob + $diff));
	ok($angle > -$pi, '3. quadrant < -pi');
	ok($angle < -$pi / 2, '3. quadrant < -pi / 2');
	$diff  = 7 * $pi / 4;
	$angle = SVG::Rasterize::_angle(cos($glob),
					sin($glob),
					cos($glob + $diff),
					sin($glob + $diff));
	ok($angle > -$pi / 2, '4. quadrant > -pi / 2');
	ok($angle < 0, '4. quadrant < 0');
    }
}

sub adjust_arc_radii {
    my @result;

    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 0, 1, 0, -1, 0);
    is(scalar(@result), 2, 'rx 0');
    is_deeply(\@result, [0, 1], 'radii untouched');
    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 0, -1, 0, -1, 0);
    is(scalar(@result), 2, 'rx 0');
    is_deeply(\@result, [0, 1], 'radii positive');
    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 1, 0, 0, -1, 0);
    is(scalar(@result), 2, 'ry 0');
    is_deeply(\@result, [1, 0], 'radii untouched');
    @result = SVG::Rasterize::adjust_arc_radii(1, 0, -1, 0, 0, -1, 0);
    is(scalar(@result), 2, 'ry 0');
    is_deeply(\@result, [1, 0], 'radii positive');

    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 1, 1, 0, 1, 0);
    is(scalar(@result), 6, 'end = start');
    is($result[0], 1, 'rx');
    is($result[1], 1, 'ry');
    cmp_ok(abs($result[2] - 0), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - 1), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4] - 0), '<', 1e-12, 'x1_p');
    cmp_ok(abs($result[5] - 0), '<', 1e-12, 'y1_p');

    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 1, 1, 0, -1, 0);
    is(scalar(@result), 7, 'end = start');
    is($result[0], 1, 'rx');
    is($result[1], 1, 'ry');
    cmp_ok(abs($result[2] - 0), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - 1), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4] - 1), '<', 1e-12, 'x1_p');
    cmp_ok(abs($result[5] - 0), '<', 1e-12, 'y1_p');
    cmp_ok(abs($result[6] - 0), '<', 1e-12, 'radicand');

    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 1, 1, 0, -1.2, 0);
    is(scalar(@result), 7, 'end = start');
    cmp_ok(abs($result[0] - 1.1), '<', 1e-12, 'rx');
    cmp_ok(abs($result[1] - 1.1), '<', 1e-12, 'ry');
    cmp_ok(abs($result[2] - 0), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - 1), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4] - 1.1), '<', 1e-12, 'x1_p');
    cmp_ok(abs($result[5] - 0), '<', 1e-12, 'y1_p');
    cmp_ok(abs($result[6] - 0), '<', 1e-12, 'radicand');

    @result = SVG::Rasterize::adjust_arc_radii(6, 4, 1, 1, 0, 3.8, 4);
    is(scalar(@result), 7, 'end = start');
    cmp_ok(abs($result[0] - 1.1), '<', 1e-12, 'rx');
    cmp_ok(abs($result[1] - 1.1), '<', 1e-12, 'ry');
    cmp_ok(abs($result[2] - 0), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - 1), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4] - 1.1), '<', 1e-12, 'x1_p');
    cmp_ok(abs($result[5] - 0), '<', 1e-12, 'y1_p');
    cmp_ok(abs($result[6] - 0), '<', 1e-12, 'radicand');

    @result = SVG::Rasterize::adjust_arc_radii(1, 0, 2, 2, 0, -1, 0);
    is(scalar(@result), 7, 'end = start');
    cmp_ok(abs($result[0] - 2), '<', 1e-12, 'rx');
    cmp_ok(abs($result[1] - 2), '<', 1e-12, 'ry');
    cmp_ok(abs($result[2] - 0), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - 1), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4] - 1), '<', 1e-12, 'x1_p');
    cmp_ok(abs($result[5] - 0), '<', 1e-12, 'y1_p');
    cmp_ok(abs($result[6] - 3), '<', 1e-12, 'radicand');

    @result = SVG::Rasterize::adjust_arc_radii(5, 4, 2, 2, 0.3, 5, 2);
    is(scalar(@result), 7, 'end = start');
    cmp_ok(abs($result[0] - 2), '<', 1e-12, 'rx');
    cmp_ok(abs($result[1] - 2), '<', 1e-12, 'ry');
    cmp_ok(abs($result[2] - sin(0.3)), '<', 1e-12, 'sin(phi)');
    cmp_ok(abs($result[3] - cos(0.3)), '<', 1e-12, 'cos(phi)');
    cmp_ok(abs($result[4]**2 + $result[5]**2 - 1), '<', 1e-12, 'd_p');
    cmp_ok(abs($result[6] - 3), '<', 1e-12, 'radicand');
}

sub dasharray_validation {
    ok('1.2, 1.8pt,3em ,  7%' =~ $RE_DASHARRAY{p_DASHARRAY},
       '1.2, 1.8pt,3em ,  7%');
}

transform_validation;
units;
typeglobs;
pAR_validation;
path_data_validation;
path_data_splitting;
angle;
adjust_arc_radii;
dasharray_validation;
