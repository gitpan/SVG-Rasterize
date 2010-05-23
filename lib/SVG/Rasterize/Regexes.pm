package SVG::Rasterize::Regexes;
use strict;
use warnings;

use Exporter 'import';

# $Id$

=head1 NAME

C<SVG::Rasterize::Regexes> - Commonly used regular expressions

=head1 VERSION

Version 0.001005

=cut

our $VERSION = '0.001005';

our @EXPORT    = qw();
our @EXPORT_OK = qw($WSP
		    $CWSP
                    %RE_PACKAGE
                    %RE_NUMBER
                    %RE_LENGTH
                    %RE_COLOR
                    %RE_TRANSFORM
                    %RE_VIEW_BOX
                    %RE_PATH
                    %RE_DASHARRAY);

our %EXPORT_TAGS = (all           => [@EXPORT, @EXPORT_OK],
		    whitespace    => [qw($WSP $CWSP)],
                    attributes    => [qw(%RE_NUMBER
                                         %RE_LENGTH
                                         %RE_COLOR
                                         %RE_TRANSFORM
                                         %RE_VIEW_BOX
                                         %RE_PATH
                                         %RE_DASHARRAY)]);

our $WSP  = qr/[\x{20}\x{9}\x{D}\x{A}]/;
our $CWSP = qr/(?:$WSP+\,?$WSP*|\,$WSP*)/;

our %RE_PACKAGE = ();
{
    my $package_part = qr/[a-zA-Z][a-zA-Z0-9\_]*/;
    
    $RE_PACKAGE{p_PACKAGE_NAME} =
	qr/^$package_part(?:\:\:$package_part)*$/;
}

# numbers and lengths
our %RE_NUMBER = ();
our %RE_LENGTH = ();
{
    $RE_NUMBER{NNINTEGER}    = qr/\d+/;
    $RE_NUMBER{INTEGER}      = qr/[\+\-]?$RE_NUMBER{NNINTEGER}/;
    $RE_NUMBER{p_INTEGER}    = qr/^$RE_NUMBER{INTEGER}$/;
    $RE_NUMBER{w_INTEGER}    = qr/^$WSP*$RE_NUMBER{INTEGER}$WSP*$/;

    $RE_NUMBER{NNFRACTION}   = qr/(?:\d*\.\d+|\d+\.)/;
    $RE_NUMBER{FRACTION}     = qr/[\+\-]?$RE_NUMBER{NNFRACTION}/;
    $RE_NUMBER{p_FRACTION}   = qr/^$RE_NUMBER{FRACTION}$/;
    $RE_NUMBER{w_FRACTION}   = qr/^$WSP*$RE_NUMBER{FRACTION}$WSP*$/;
    $RE_NUMBER{EXPONENT}     = qr/[eE][\+\-]?\d+/;
    $RE_NUMBER{NNFLOAT}      = qr/(?:$RE_NUMBER{NNFRACTION}
                                    $RE_NUMBER{EXPONENT}?
                                   |$RE_NUMBER{NNINTEGER}
                                    $RE_NUMBER{EXPONENT})/x;
    $RE_NUMBER{FLOAT}        = qr/[\+\-]?$RE_NUMBER{NNFLOAT}/;
    $RE_NUMBER{p_FLOAT}      = qr/^$RE_NUMBER{FLOAT}$/;
    $RE_NUMBER{w_FLOAT}      = qr/^$WSP*$RE_NUMBER{FLOAT}$WSP*$/;
    $RE_NUMBER{P_NNNUMBER}   = qr/(?:$RE_NUMBER{NNFRACTION}
                                   |$RE_NUMBER{NNINTEGER})/x;
    $RE_NUMBER{p_P_NNNUMBER} = qr/^$RE_NUMBER{P_NNNUMBER}$/;
    $RE_NUMBER{w_P_NNNUMBER} = qr/^$WSP*$RE_NUMBER{P_NNNUMBER}$WSP*$/;
    $RE_NUMBER{P_NUMBER}     = qr/[\+\-]?$RE_NUMBER{P_NNNUMBER}/;
    $RE_NUMBER{p_P_NUMBER}   = qr/^$RE_NUMBER{P_NUMBER}$/;
    $RE_NUMBER{w_P_NUMBER}   = qr/^$WSP*$RE_NUMBER{P_NUMBER}$WSP*$/;
    $RE_NUMBER{A_NNNUMBER}   = qr/(?:$RE_NUMBER{NNFLOAT}
                                 |$RE_NUMBER{NNINTEGER})/x;
    $RE_NUMBER{p_A_NNNUMBER} = qr/^$RE_NUMBER{A_NNNUMBER}$/;
    $RE_NUMBER{w_A_NNNUMBER} = qr/^$WSP*$RE_NUMBER{A_NNNUMBER}$WSP*$/;
    $RE_NUMBER{A_NUMBER}     = qr/[\+\-]?$RE_NUMBER{A_NNNUMBER}/;
    $RE_NUMBER{p_A_NUMBER}   = qr/^$RE_NUMBER{A_NUMBER}$/;
    $RE_NUMBER{w_A_NUMBER}   = qr/^$WSP*$RE_NUMBER{A_NUMBER}$WSP*$/;

    $RE_LENGTH{UNIT}         = qr/(?:em|ex|px|pt|pc|cm|mm|in|\%)/;
    $RE_LENGTH{P_LENGTH}     = qr/$RE_NUMBER{P_NUMBER}$RE_LENGTH{UNIT}?/;
    $RE_LENGTH{p_P_LENGTH}   = qr/^$RE_LENGTH{P_LENGTH}$/;
    $RE_LENGTH{w_P_LENGTH}   = qr/^$WSP*$RE_LENGTH{P_LENGTH}$WSP*$/;
    $RE_LENGTH{A_LENGTH}     = qr/$RE_NUMBER{A_NUMBER}$RE_LENGTH{UNIT}?/;
    $RE_LENGTH{p_A_LENGTH}   = qr/^$RE_LENGTH{A_LENGTH}$/;
    $RE_LENGTH{w_A_LENGTH}   = qr/^$WSP*$RE_LENGTH{A_LENGTH}$WSP*$/;
}

# rgb colors
our %RE_COLOR = ();
{
    my $rgbe = qr/[\+\-]?\d{1,3}\%?/;

    $RE_COLOR{p_RGB} = qr/^rgb\($WSP*($rgbe)$WSP*\,
                               $WSP*($rgbe)$WSP*\,
                               $WSP*($rgbe)$WSP*\)$/x;
}

# attribute stuff

# transform
# The following regular expressions are basically a one-to-one
# translation of the Backus Naur form given in the SVG
# specification on
# http://www.w3.org/TR/SVG11/coords.html#TransformAttribute
# There is the following identifier correspondence:
# transform-list          - $TRANSFORM_LIST
# transforms              - $TRANSFORM_SPLIT
# transform               - $tf
# matrix                  - $ma
# translate               - $tr
# scale                   - $sc
# rotate                  - $ro
# skewX                   - $sx
# skewY                   - $sy
# number                  - $A_NUMBER
# comma-wsp               - $CWSP
# wsp                     - $WSP
# integer-constant        - $INTEGER,  implicit
# floating-point-constant - $FLOAT,    implicit
# fractional-constant     - $FRACTION, implicit
# exponent                - $EXPONENT, implicit
# sign                    - optimized away
#
# digit, digit sequence, and comma are used directly.
# The definition allows some "weird" numbers like 001 or 00.1,
# but this is what the specification says.
# If any of these REs are changed, 010_geometry.t should be
# changed accordingly.

our %RE_TRANSFORM = ();
{
    my  $nu           = $RE_NUMBER{A_NUMBER};
    my  $wnu          = qr/$WSP*$nu/;
    my  $ma           = qr/matrix$WSP*\($WSP*(?:$nu$CWSP){5}$nu$WSP*\)/;
    my  $tr           = qr/translate$WSP*\($wnu(?:$CWSP$nu)?$WSP*\)/;
    my  $sc           = qr/scale$WSP*\($wnu(?:$CWSP$nu)?$WSP*\)/;
    my  $ro           = qr/rotate$WSP*\($wnu(?:(?:$CWSP$nu){2})?$WSP*\)/;
    my  $sx           = qr/skewX$WSP*\($wnu$WSP*\)/;
    my  $sy           = qr/skewY$WSP*\($wnu$WSP*\)/;
    my  $tf           = qr/(?:$ma|$tr|$sc|$ro|$sx|$sy)/;
    my  $tfm          = qr/$tf(?:$CWSP$tf)*/;
    my  $tfn          = qr/(?:matrix|translate|scale|rotate|skewX|skewY)/;

    %RE_TRANSFORM =
	(TRANSFORM_SPLIT   => qr/($tf)(?:$CWSP($tfm))?/,
	 p_TRANSFORM_LIST  => qr/^$WSP*($tfm)?$WSP*$/,
	 TRANSFORM_CAPTURE => qr/($tfn)$WSP*
                                 \($WSP*($nu(?:$CWSP$nu)*)$WSP*\)/x);
}

# viewBox and pAR
our %RE_VIEW_BOX = ();
{
    my $nu = $RE_NUMBER{A_NUMBER};

    $RE_VIEW_BOX{p_VIEW_BOX} = qr/^($nu)$CWSP($nu)$CWSP($nu)$CWSP($nu)$/;
    $RE_VIEW_BOX{ALIGN}      = qr/(none
                                   |x(?:Min|Mid|Max)Y(?:Min|Mid|Max))/x;
    $RE_VIEW_BOX{MOS}        = qr/(meet|slice)/;
    $RE_VIEW_BOX{PAR}        = qr/^(?:defer\ +)?
                                  (?:$RE_VIEW_BOX{ALIGN}
                                   \ +$RE_VIEW_BOX{MOS}
                                   |$RE_VIEW_BOX{ALIGN})$/x;
}

# path data
# The following regular expressions are basically a one-to-one
# translation of the Backus Naur form given in the SVG
# specification on
# http://www.w3.org/TR/SVG11/paths.html#PathDataBNF
# There is the following identifier correspondence:
# svg-path                                   - $p_PATH_LIST
# moveto-drawto-command-groups               - $pcgm
# moveto-drawto-command-group                - $pcg
# drawto-commands                            - $dtm
# drawto-command                             - $dt
# moveto                                     - $mt
# moveto-argument-sequence                   - $mas
# closepath                                  - $cl
# lineto                                     - $lt
# lineto-argument-sequence                   - $las
# horizontal-lineto                          - $hlt
# horizontal-lineto-argument-sequence        - $hlas
# vertical-lineto                            - $vlt
# vertical-lineto-argument-sequence          - $vlas
# curveto                                    - $ct
# curveto-argument-sequence                  - $cas
# curveto-argument                           - $ca
# smooth-curveto                             - $sct
# smooth-curveto-argument-sequence           - $scas
# smooth-curveto-argument                    - $sca
# quadratic-bezier-curveto                   - $qb
# quadratic-bezier-curveto-argument-sequence - $qbas
# smooth-quadratic-bezier-curveto            - $sqb
# elliptical-arc                             - $ea
# elliptical-arc-argument-sequence           - $eaas
# elliptical-arc-argument                    - $eaa
# coordinate-pair                            - $cp

our %RE_PATH = ();
{
    my  $fl      = qr/[01]/;
    my  $cp      = qr/$RE_NUMBER{A_NUMBER}$CWSP?$RE_NUMBER{A_NUMBER}/;
    my  $mas     = qr/$cp(?:$CWSP?$cp)*/;
    my  $mt      = qr/(?:M|m)$WSP*$mas/;
    my  $cl      = qr/(?:Z|z)/;
#    my  $las     = $mas;  # if changed check carefully all below
    my  $lt      = qr/(?:L|l)$WSP*$mas/;
    my  $hlas    = qr/$RE_NUMBER{A_NUMBER}(?:$CWSP?$RE_NUMBER{A_NUMBER})*/;
    my  $hlt     = qr/(?:H|h)$WSP*$hlas/;
#    my  $vlas    = $hlas;  # if changed check carefully all below
    my  $vlt     = qr/(?:V|v)$WSP*$hlas/;
    my  $ca      = qr/$cp$CWSP?$cp$CWSP?$cp/;
    my  $cas     = qr/$ca(?:$CWSP?$ca)*/;
    my  $ct      = qr/(?:C|c)$WSP*$cas/;
    my  $sca     = qr/$cp$CWSP?$cp/;
    my  $scas    = qr/$sca(?:$CWSP?$sca)*/;
    my  $sct     = qr/(?:S|s)$WSP*$scas/;
#    my  $qbas    = $scas;  # if changed check carefully all below
    my  $qb      = qr/(?:Q|q)$WSP*$scas/;
#    my  $sqbas   = $mas;  # if changed check carefully all below
    my  $sqb     = qr/(?:T|t)$WSP*$mas/;
    my  $eaa     = qr/$RE_NUMBER{A_NNNUMBER}$CWSP?
                      $RE_NUMBER{A_NNNUMBER}$CWSP?
                      $RE_NUMBER{A_NUMBER}$CWSP
                      $fl$CWSP$fl$CWSP
                      $cp/x;
    my  $eaas    = qr/$eaa(?:$CWSP?$eaa)*/;
    my  $ea      = qr/(?:A|a)$WSP*$eaas/;
    my  $dt      = qr/(?:$cl|$lt|$hlt|$vlt|$ct|$sct|$qb|$sqb|$ea)/;
    my  $dtm     = qr/$dt(?:$WSP*$dt)*/;        # draw to multiple
    my  $pcg     = qr/$mt(?:$WSP*$dtm)?/;       # path command group
    my  $pcgm    = qr/$pcg(?:$WSP*$pcg)*/;      # pcg multiple

    $RE_PATH{p_PATH_LIST} = qr/^$WSP*$pcgm$WSP*$/;
    $RE_PATH{MAS_SPLIT}   = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($mas?)$/x;
    $RE_PATH{LAS_SPLIT}   = $RE_PATH{MAS_SPLIT};
    $RE_PATH{HLAS_SPLIT}  = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($hlas?)$/x;
    $RE_PATH{VLAS_SPLIT}  = $RE_PATH{HLAS_SPLIT};
    $RE_PATH{CAS_SPLIT}   = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($cas?)$/x;
    $RE_PATH{SCAS_SPLIT}  = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($scas?)$/x;
    $RE_PATH{QBAS_SPLIT}  = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($scas?)$/x;
    $RE_PATH{SQBAS_SPLIT} = qr/^($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($mas?)$/x;
    $RE_PATH{EAAS_SPLIT}  = qr/^($RE_NUMBER{A_NNNUMBER})$CWSP?
                                ($RE_NUMBER{A_NNNUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP
                                ($fl)$CWSP($fl)$CWSP
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($RE_NUMBER{A_NUMBER})$CWSP?
                                ($eaas?)$/x;
}

# stroke-dasharray
our %RE_DASHARRAY = ();
$RE_DASHARRAY{p_DASHARRAY} = qr/^$RE_LENGTH{A_LENGTH}
                                 (?:$WSP*\,$WSP*$RE_LENGTH{A_LENGTH})*$/x;
$RE_DASHARRAY{SPLIT}       = qr/$WSP*\,$WSP*/;

1;


__END__

=pod

=head1 DESCRIPTION

The following regular expressions are used at different locations of
the code to validate or extract user input. It is not part of the
interface where exactly they are used. They are documented for
inspection only. They are compiled into other expressions so
changing them will probably not achieve what you might expect. The
exception to this rule is the C<PACKAGE_NAME> variable. The other
items are more or less a direct translation of parts of the Backus
Naur form given by the C<SVG> specification for the C<transform>
attribute
(L<http://www.w3.org/TR/SVG11/coords.html#TransformAttribute>).

=over 4

=item * PACKAGE_PART

  qr/[a-zA-Z][a-zA-Z0-9\_]*/

=item * PACKAGE_NAME

  qr/^$PACKAGE_PART(?:\:\:PACKAGE_PART)*$/

Package names given to methods in this distribution, namely the
C<engine_class> parameters have to match this regular expression. I
am not sure which package names exactly are allowed. If you know
where in the Perl manpages or the Camel book this is described,
please point me to it. If this pattern is too strict for your
favourite package name, you can change this variable.

=item * INTEGER

  qr/[\+\-]?\d+/;

Note that this allows leading zeroes like '00030'. This is for
compliance with the C<SVG> specification.

=item * FRACTION

  qr/[\+\-]?(?:\d*\.\d+|\d+\.)/;

Floating point number in non-scientific notation.
Note that this allows leading zeroes like '000.123'. This is for
compliance with the C<SVG> specification.

=item * EXPONENT

  qr/[eE][\+\-]?\d+/;

=item * FLOAT

  qr/$FRACTION$EXPONENT?|$INTEGER$EXPONENT/;

Floating point number in decimal or scientific notation.

=item * P_NUMBER

  qr/$INTEGER|$FRACTION/;

Number allowed in C<CSS> compliant style properties: integer
or float in decimal notation.

=item * A_NUMBER

  qr/$INTEGER|$FLOAT/;

Number allowed in C<XML> attributes. Integer or float in either
decimal or scientific notation.

=item * UNIT

  qr/em|ex|px|pt|pc|cm|mm|in|\%/;

=item * P_LENGTH

  qr/$P_NUMBER$UNIT?/;

=item * A_LENGTH

  qr/$A_NUMBER$UNIT?/;

=back

=head1 SEE ALSO

=over 4

=item * L<SVG::Rasterize|SVG::Rasterize>

=back


=head1 AUTHOR

Lutz Gehlen, C<< <perl at lutzgehlen.de> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2010 Lutz Gehlen.

This program is free software; you can redistribute it and/or modify
it under the terms of either: the GNU General Public License as
published by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
