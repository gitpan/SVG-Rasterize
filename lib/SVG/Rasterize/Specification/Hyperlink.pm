package SVG::Rasterize::Specification::Hyperlink;
use strict;
use warnings;

use Params::Validate qw(:types);

use SVG::Rasterize::Regexes qw(:attributes);

# $Id: Hyperlink.pm 6211 2010-06-15 04:21:13Z mullet $

=head1 NAME

C<SVG::Rasterize::Specification::Hyperlink> - specification for class Hyperlink

=head1 VERSION

Version 0.003004

=cut

our $VERSION = '0.003004';

our %CHILDREN = ('a' => {'a'                => 1,
                         'altGlyphDef'      => 1,
                         'animate'          => 1,
                         'animateColor'     => 1,
                         'animateMotion'    => 1,
                         'animateTransform' => 1,
                         'circle'           => 1,
                         'clipPath'         => 1,
                         'color-profile'    => 1,
                         'cursor'           => 1,
                         'defs'             => 1,
                         'desc'             => 1,
                         'ellipse'          => 1,
                         'filter'           => 1,
                         'font'             => 1,
                         'font-face'        => 1,
                         'g'                => 1,
                         'image'            => 1,
                         'line'             => 1,
                         'linearGradient'   => 1,
                         'marker'           => 1,
                         'mask'             => 1,
                         'metadata'         => 1,
                         'path'             => 1,
                         'pattern'          => 1,
                         'polygon'          => 1,
                         'polyline'         => 1,
                         'radialGradient'   => 1,
                         'rect'             => 1,
                         'script'           => 1,
                         'set'              => 1,
                         'style'            => 1,
                         'svg'              => 1,
                         'switch'           => 1,
                         'symbol'           => 1,
                         'text'             => 1,
                         'title'            => 1,
                         'use'              => 1,
                         'view'             => 1});

our %ATTR_VAL = ('a' => {'alignment-baseline'           => {'optional' => 1,
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
                                                            'regex'    => $RE_PAINT{p_COLOR}},
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
                                                            'regex'    => $RE_PAINT{p_PAINT}},
                         'fill-opacity'                 => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/$RE_NUMBER{p_A_NUMBER}|^inherit$/},
                         'fill-rule'                    => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:nonzero|evenodd|inherit)$/},
                         'filter'                       => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
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
                         'onactivate'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onclick'                      => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onfocusin'                    => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onfocusout'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onload'                       => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onmousedown'                  => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onmousemove'                  => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onmouseout'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onmouseover'                  => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'onmouseup'                    => {'optional' => 1,
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
                         'requiredExtensions'           => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'requiredFeatures'             => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
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
                                                            'regex'    => $RE_PAINT{p_PAINT}},
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
                         'systemLanguage'               => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'target'                       => {'optional' => 1,
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
                         'transform'                    => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => $RE_TRANSFORM{p_TRANSFORM_LIST}},
                         'unicode-bidi'                 => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:normal|embed|bidi\-override|inherit)$/},
                         'visibility'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:visible|hidden|inherit)$/},
                         'word-spacing'                 => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'writing-mode'                 => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:lr\-tb|rl\-tb|tb\-rl|lr|rl|tb|inherit)$/},
                         'xlink:actuate'                => {'default'  => 'onRequest',
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:onRequest)$/},
                         'xlink:arcrole'                => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'xlink:href'                   => {'optional' => 0,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'xlink:role'                   => {'optional' => 1,
                                                            'type'     => SCALAR,
                                                            'regex'    => qr//},
                         'xlink:show'                   => {'default'  => 'replace',
                                                            'type'     => SCALAR,
                                                            'regex'    => qr/^(?:new|replace)$/},
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
                                                            'regex'    => qr//}});

our %ATTR_HINTS = ('a' => {'color'        => {'color'  => 1},
                           'fill'         => {'color'  => 1},
                           'stroke'       => {'color'  => 1},
                           'stroke-width' => {'length' => 1}});

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
