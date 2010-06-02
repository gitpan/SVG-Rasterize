#!perl -T

use Test::More tests => 8;

BEGIN {
    use_ok('SVG::Rasterize::Regexes')       || print "Bail out!\n";
    use_ok('SVG::Rasterize')                || print "Bail out!\n";
    use_ok('SVG::Rasterize::Cairo')         || print "Bail out!\n";
    use_ok('SVG::Rasterize::Specification') || print "Bail out!\n";
    use_ok('SVG::Rasterize::Properties')    || print "Bail out!\n";
    use_ok('SVG::Rasterize::Colors')        || print "Bail out!\n";
    use_ok('SVG::Rasterize::State')         || print "Bail out!\n";
    use_ok('SVG::Rasterize::Exception')     || print "Bail out!\n";
}

diag( "Testing SVG::Rasterize $SVG::Rasterize::VERSION, Perl $], $^X" );
