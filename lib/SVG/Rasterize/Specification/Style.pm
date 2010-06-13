package SVG::Rasterize::Specification::Style;
use strict;
use warnings;

use Params::Validate qw(:types);

use SVG::Rasterize::Regexes qw(:attributes);

# $Id$

=head1 NAME

C<SVG::Rasterize::Specification::Style> - specification for class Style

=head1 VERSION

Version 0.003003

=cut

our $VERSION = '0.003003';

our %CHILDREN = ('style' => {});

our %ATTR_VAL = ('style' => {'id'        => {'optional' => 1,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'media'     => {'optional' => 1,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'title'     => {'optional' => 1,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'type'      => {'optional' => 0,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'xml:base'  => {'optional' => 1,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'xml:lang'  => {'optional' => 1,
                                             'type'     => SCALAR,
                                             'regex'    => qr//},
                             'xml:space' => {'default'  => 'preserve',
                                             'type'     => SCALAR,
                                             'regex'    => qr/^(?:preserve)$/}});

our %ATTR_HINTS = ('style' => {});
1;


__END__

=pod

=head1 DESCRIPTION

This file was automatically generated using the SVG DTD available
under
L<http://www.w3.org/Graphics/SVG/1.1/DTD/svg11-flat-20030114.dtd>.

See L<SVG::Rasterize::Specification|SVG::Rasterize::Specification>
for more details.


=head1 AUTHOR

Lutz Gehlen, C<< <perl at lutzgehlen.de> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2010 Lutz Gehlen.

This program is free software; you can redistribute it and/or modify
it under the terms of either: the GNU General Public License as
published by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
