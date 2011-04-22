#!perl -T
use strict;
use warnings;

use Test::More tests => 87;
use Test::Warn;

use SVG;
use Test::Exception;
use SVG::Rasterize::Exception;

foreach(@SVG::Rasterize::Exception::EXPORT,
	@SVG::Rasterize::Exception::EXPORT_OK)
{
    warning_is { eval "&SVG::Rasterize::Exception::$_" } undef,
        "no warning in $_ without arguments";
    ok(defined($@), 'exception has been thrown');
    isa_ok($@, 'SVG::Rasterize::Exception::Base');
}

