package SVG::Rasterize::Specification::View;
use strict;
use warnings;

use Params::Validate qw(:types);

use SVG::Rasterize::Regexes qw(:attributes);

# $Id$

=head1 NAME

C<SVG::Rasterize::Specification::View> - specification for class View

=head1 VERSION

Version 0.003003

=cut

our $VERSION = '0.003003';

our %CHILDREN = ('view' => {'desc'     => 1,
                            'metadata' => 1,
                            'title'    => 1});

our %ATTR_VAL = ('view' => {'externalResourcesRequired' => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:false|true)$/},
                            'id'                        => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                            'preserveAspectRatio'       => {'default'  => 'xMidYMid meet',
                                                            'type'     => SCALAR,
                                                            'regex'    => $RE_VIEW_BOX{PAR}},
                            'viewBox'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => $RE_VIEW_BOX{p_VIEW_BOX}},
                            'viewTarget'                => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                            'xml:base'                  => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                            'xml:lang'                  => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                            'xml:space'                 => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:default|preserve)$/},
                            'zoomAndPan'                => {'default'  => 'magnify',
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:disable|magnify)$/}});

our %ATTR_HINTS = ('view' => {});
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
