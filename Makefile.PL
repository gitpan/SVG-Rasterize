use inc::Module::Install;

name     'SVG-Rasterize';
all_from 'lib/SVG/Rasterize.pm';
author   q{Lutz Gehlen <perl@lutzgehlen.de>};
license  'perl';

no_index directory => 'examples';

requires 'Class::Accessor'       => '0.30';
requires 'SVG'                   => '2.37';
requires 'Cairo'                 => '1.061';
#requires 'Pango'                 => '1.220';
requires 'Params::Validate'      => '0.91';
requires 'Scalar::Util'          => '1.19';
requires 'Exception::Class'      => '1.29';

build_requires 'Test::More'      => '0.80';
build_requires 'Test::Exception' => '0.27';

auto_install;

WriteAll;

