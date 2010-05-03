#!perl -T

use Test::More tests => 3;

BEGIN {
    use_ok( 'SVG::Rasterize' )        || print "Bail out!\n";
    use_ok( 'SVG::Rasterize::State' ) || print "Bail out!\n";
    use_ok( 'SVG::Rasterize::Cairo' ) || print "Bail out!\n";
}

diag( "Testing SVG::Rasterize $SVG::Rasterize::VERSION, Perl $], $^X" );
