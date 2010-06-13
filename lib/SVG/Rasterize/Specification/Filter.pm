package SVG::Rasterize::Specification::Filter;
use strict;
use warnings;

use Params::Validate qw(:types);

use SVG::Rasterize::Regexes qw(:attributes);

# $Id$

=head1 NAME

C<SVG::Rasterize::Specification::Filter> - specification for class Filter

=head1 VERSION

Version 0.003003

=cut

our $VERSION = '0.003003';

our %CHILDREN = ('filter' => {'animate'             => 1,
                              'desc'                => 1,
                              'feBlend'             => 1,
                              'feColorMatrix'       => 1,
                              'feComponentTransfer' => 1,
                              'feComposite'         => 1,
                              'feConvolveMatrix'    => 1,
                              'feDiffuseLighting'   => 1,
                              'feDisplacementMap'   => 1,
                              'feFlood'             => 1,
                              'feGaussianBlur'      => 1,
                              'feImage'             => 1,
                              'feMerge'             => 1,
                              'feMorphology'        => 1,
                              'feOffset'            => 1,
                              'feSpecularLighting'  => 1,
                              'feTile'              => 1,
                              'feTurbulence'        => 1,
                              'metadata'            => 1,
                              'set'                 => 1,
                              'title'               => 1});

our %ATTR_VAL = ('filter' => {'alignment-baseline'           => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|baseline|before\-edge|text\-before\-edge|middle|central|after\-edge|text\-after\-edge|ideographic|alphabetic|hanging|mathematical|inherit)$/},
                              'baseline-shift'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'class'                        => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'clip'                         => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'clip-path'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'clip-rule'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:nonzero|evenodd|inherit)$/},
                              'color'                        => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'color-interpolation'          => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|sRGB|linearRGB|inherit)$/},
                              'color-interpolation-filters'  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|sRGB|linearRGB|inherit)$/},
                              'color-profile'                => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'color-rendering'              => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|optimizeSpeed|optimizeQuality|inherit)$/},
                              'cursor'                       => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'direction'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:ltr|rtl|inherit)$/},
                              'display'                      => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:inline|block|list\-item|run\-in|compact|marker|table|inline\-table|table\-row\-group|table\-header\-group|table\-footer\-group|table\-row|table\-column\-group|table\-column|table\-cell|table\-caption|none|inherit)$/},
                              'dominant-baseline'            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|use\-script|no\-change|reset\-size|ideographic|alphabetic|hanging|mathematical|central|middle|text\-after\-edge|text\-before\-edge|inherit)$/},
                              'enable-background'            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'externalResourcesRequired'    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:false|true)$/},
                              'fill'                         => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'fill-opacity'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                              'fill-rule'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:nonzero|evenodd|inherit)$/},
                              'filter'                       => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'filterRes'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'filterUnits'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:userSpaceOnUse|objectBoundingBox)$/},
                              'flood-color'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'flood-opacity'                => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                              'font-family'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'font-size'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'font-size-adjust'             => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'font-stretch'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:normal|wider|narrower|ultra\-condensed|extra\-condensed|condensed|semi\-condensed|semi\-expanded|expanded|extra\-expanded|ultra\-expanded|inherit)$/},
                              'font-style'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:normal|italic|oblique|inherit)$/},
                              'font-variant'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:normal|small\-caps|inherit)$/},
                              'font-weight'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:normal|bold|bolder|lighter|100|200|300|400|500|600|700|800|900|inherit)$/},
                              'glyph-orientation-horizontal' => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'glyph-orientation-vertical'   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'height'                       => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => $RE_LENGTH{p_A_LENGTH}},
                              'id'                           => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'image-rendering'              => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|optimizeSpeed|optimizeQuality|inherit)$/},
                              'kerning'                      => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'letter-spacing'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'lighting-color'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'marker-end'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'marker-mid'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'marker-start'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'mask'                         => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'opacity'                      => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                              'overflow'                     => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:visible|hidden|scroll|auto|inherit)$/},
                              'pointer-events'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:visiblePainted|visibleFill|visibleStroke|visible|painted|fill|stroke|all|none|inherit)$/},
                              'primitiveUnits'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:userSpaceOnUse|objectBoundingBox)$/},
                              'shape-rendering'              => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|optimizeSpeed|crispEdges|geometricPrecision|inherit)$/},
                              'stop-color'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'stop-opacity'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                              'stroke'                       => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'stroke-dasharray'             => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_DASHARRAY{p_DASHARRAY}|^inherit$|^none$/},
                              'stroke-dashoffset'            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_LENGTH{p_A_LENGTH}|^inherit$/},
                              'stroke-linecap'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:butt|round|square|inherit)$/},
                              'stroke-linejoin'              => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:miter|round|bevel|inherit)$/},
                              'stroke-miterlimit'            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NNNUMBER}|^inherit$/},
                              'stroke-opacity'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                              'stroke-width'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/$RE_LENGTH{p_A_LENGTH}|^inherit$/},
                              'style'                        => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'text-anchor'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:start|middle|end|inherit)$/},
                              'text-decoration'              => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'text-rendering'               => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:auto|optimizeSpeed|optimizeLegibility|geometricPrecision|inherit)$/},
                              'unicode-bidi'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:normal|embed|bidi\-override|inherit)$/},
                              'visibility'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:visible|hidden|inherit)$/},
                              'width'                        => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => $RE_LENGTH{p_A_LENGTH}},
                              'word-spacing'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'writing-mode'                 => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:lr\-tb|rl\-tb|tb\-rl|lr|rl|tb|inherit)$/},
                              'x'                            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => $RE_LENGTH{p_A_LENGTH}},
                              'xlink:actuate'                => {'default'  => 'onLoad',
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:onLoad)$/},
                              'xlink:arcrole'                => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xlink:href'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xlink:role'                   => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xlink:show'                   => {'default'  => 'other',
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:other)$/},
                              'xlink:title'                  => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xlink:type'                   => {'default'  => 'simple',
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:simple)$/},
                              'xml:base'                     => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xml:lang'                     => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'xml:space'                    => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr/^(?:default|preserve)$/},
                              'xmlns:xlink'                  => {'default'  => 'http://www.w3.org/1999/xlink',
                                                                 'type'     => SCALAR,
                                                                 'regex'    => qr//},
                              'y'                            => {'optional' => 1,
                                                                 'type'     => SCALAR,
                                                                 'regex'    => $RE_LENGTH{p_A_LENGTH}}});

our %ATTR_HINTS = ('filter' => {'color'        => {'color'  => 1},
                                'fill'         => {'color'  => 1},
                                'height'       => {'length' => 1},
                                'stroke'       => {'color'  => 1},
                                'stroke-width' => {'length' => 1},
                                'width'        => {'length' => 1},
                                'x'            => {'length' => 1},
                                'y'            => {'length' => 1}});
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
