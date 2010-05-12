package SVG::Rasterize::Regexes;
use strict;
use warnings;

use Exporter 'import';

# $Id$

=head1 NAME

C<SVG::Rasterize::Regexes> - Commonly used regular expressions

=head1 VERSION

Version 0.000009

=cut

our @EXPORT    = qw();
our @EXPORT_OK = qw($p_PACKAGE_NAME
		    $WSP         
		    $CWSP        
		    $INTEGER     
		    $p_INTEGER   
		    $w_INTEGER   
		    $FRACTION    
		    $p_FRACTION  
		    $w_FRACTION  
		    $EXPONENT    
		    $FLOAT       
		    $p_FLOAT     
		    $w_FLOAT     
		    $P_NUMBER    
		    $p_P_NUMBER  
		    $w_P_NUMBER  
		    $A_NUMBER    
		    $p_A_NUMBER  
		    $w_A_NUMBER  
		    $UNIT        
		    $P_LENGTH    
		    $p_P_LENGTH  
		    $w_P_LENGTH  
		    $A_LENGTH    
		    $p_A_LENGTH  
                    $w_A_LENGTH);

our %EXPORT_TAGS = (all        => [@EXPORT, @EXPORT_OK],
		    whitespace => [qw($WSP $CWSP)],
		    numbers    => [qw($INTEGER
		    	    	      $p_INTEGER
		    	    	      $w_INTEGER
		    	    	      $FRACTION
		    	    	      $p_FRACTION
		    	    	      $w_FRACTION
		    	    	      $EXPONENT
		    	    	      $FLOAT
		    	    	      $p_FLOAT
		    	    	      $w_FLOAT
		    	    	      $P_NUMBER
		    	    	      $p_P_NUMBER
		    	    	      $w_P_NUMBER
		    	    	      $A_NUMBER
		    	    	      $p_A_NUMBER
		    	    	      $w_A_NUMBER)],
		    lengths    => [qw($UNIT
		                      $P_LENGTH
		                      $p_P_LENGTH
		                      $w_P_LENGTH
		                      $A_LENGTH
		                      $p_A_LENGTH
                                      $w_A_LENGTH)]);

our $PACKAGE_PART   = qr/[a-zA-Z][a-zA-Z0-9\_]*/;
our $p_PACKAGE_NAME = qr/^$PACKAGE_PART(?:\:\:$PACKAGE_PART)*$/;
our $WSP            = qr/[\x{20}\x{9}\x{D}\x{A}]/;
our $CWSP           = qr/$WSP+\,?$WSP*|\,$WSP*/;
our $INTEGER        = qr/[\+\-]?\d+/;
our $p_INTEGER      = qr/^$INTEGER$/;
our $w_INTEGER      = qr/^$WSP*$INTEGER$WSP*$/;
our $FRACTION       = qr/[\+\-]?(?:\d*\.\d+|\d+\.)/;
our $p_FRACTION     = qr/^$FRACTION$/;
our $w_FRACTION     = qr/^$WSP*$FRACTION$WSP*$/;
our $EXPONENT       = qr/[eE][\+\-]?\d+/;
our $FLOAT          = qr/$FRACTION$EXPONENT?|$INTEGER$EXPONENT/;
our $p_FLOAT        = qr/^$FLOAT$/;
our $w_FLOAT        = qr/^$WSP*$FLOAT$WSP*$/;
our $P_NUMBER       = qr/$INTEGER|$FRACTION/;
our $p_P_NUMBER     = qr/^$P_NUMBER$/;
our $w_P_NUMBER     = qr/^$WSP*$P_NUMBER$WSP*$/;
our $A_NUMBER       = qr/$INTEGER|$FLOAT/;
our $p_A_NUMBER     = qr/^$A_NUMBER$/;
our $w_A_NUMBER     = qr/^$WSP*$A_NUMBER$WSP*$/;
our $UNIT           = qr/em|ex|px|pt|pc|cm|mm|in|\%/;
our $P_LENGTH       = qr/$P_NUMBER$UNIT?/;
our $p_P_LENGTH     = qr/^$P_LENGTH$/;
our $w_P_LENGTH     = qr/^$WSP*$P_LENGTH$WSP*$/;
our $A_LENGTH       = qr/$A_NUMBER$UNIT?/;
our $p_A_LENGTH     = qr/^$A_LENGTH$/;
our $w_A_LENGTH     = qr/^$WSP*$A_LENGTH$WSP*$/;

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
