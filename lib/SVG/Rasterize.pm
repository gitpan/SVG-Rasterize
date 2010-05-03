package SVG::Rasterize;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Carp;
use Params::Validate qw(validate validate_pos :types);

use SVG::Rasterize::State;

# $Id: Rasterize.pm 5460 2010-05-03 01:55:35Z mullet $

=head1 NAME

C<SVG::Rasterize> - rasterize SVG content to pixel graphics

=head1 INHERITANCE

  SVG::Rasterize is a
    L<Class::Accessor|Class::Accessor>

=head1 VERSION

Version 0.000007

=cut

our $VERSION = '0.000007';


__PACKAGE__->mk_accessors(qw(normalize_attributes));

__PACKAGE__->mk_ro_accessors(qw(engine
                                width
                                height));

###########################################################################
#                                                                         #
#                      Class Variables and Methods                        # 
#                                                                         #
###########################################################################

our $PX_PER_IN = 90;
our $DPI;  *DPI = \$PX_PER_IN;
our $IN_PER_CM = 1 / 2.54;
our $IN_PER_MM = 1 / 25.4;
our $IN_PER_PT = 1 / 72;
our $IN_PER_PC = 1 / 6;

our $WSP        = qr/[\x{20}\x{9}\x{D}\x{A}]/;
our $CWSP       = qr/$WSP+\,?$WSP*|\,$WSP*/;
our $INTEGER    = qr/[\+\-]?\d+/;
our $p_INTEGER  = qr/^$INTEGER$/;
our $w_INTEGER  = qr/^$WSP*$INTEGER$WSP*$/;
our $FRACTION   = qr/[\+\-]?(?:\d*\.\d+|\d+\.)/;
our $p_FRACTION = qr/^$FRACTION$/;
our $w_FRACTION = qr/^$WSP*$FRACTION$WSP*$/;
our $EXPONENT   = qr/[eE][\+\-]?\d+/;
our $FLOAT      = qr/$FRACTION$EXPONENT?|$INTEGER$EXPONENT/;
our $p_FLOAT    = qr/^$FLOAT$/;
our $w_FLOAT    = qr/^$WSP*$FLOAT$WSP*$/;
our $P_NUMBER   = qr/$INTEGER|$FRACTION/;
our $p_P_NUMBER = qr/^$P_NUMBER$/;
our $w_P_NUMBER = qr/^$WSP*$P_NUMBER$WSP*$/;
our $A_NUMBER   = qr/$INTEGER|$FLOAT/;
our $p_A_NUMBER = qr/^$A_NUMBER$/;
our $w_A_NUMBER = qr/^$WSP*$A_NUMBER$WSP*$/;
our $UNIT       = qr/em|ex|px|pt|pc|cm|mm|in|\%/;
our $P_LENGTH   = qr/$P_NUMBER$UNIT?/;
our $p_P_LENGTH = qr/^$P_LENGTH$/;
our $w_P_LENGTH = qr/^$WSP*$P_LENGTH$WSP*$/;
our $A_LENGTH   = qr/$A_NUMBER$UNIT?/;
our $p_A_LENGTH = qr/^$A_LENGTH$/;
our $w_A_LENGTH = qr/^$WSP*$A_LENGTH$WSP*$/;

sub multiply_matrices {
    my $n = pop(@_);
    my $m = pop(@_);

    return [$m->[0] * $n->[0] + $m->[2] * $n->[1],
	    $m->[1] * $n->[0] + $m->[3] * $n->[1],
	    $m->[0] * $n->[2] + $m->[2] * $n->[3],
	    $m->[1] * $n->[2] + $m->[3] * $n->[3],
	    $m->[0] * $n->[4] + $m->[2] * $n->[5] + $m->[4],
	    $m->[1] * $n->[4] + $m->[3] * $n->[5] + $m->[5]];
}

###########################################################################
#                                                                         #
#                             Init Process                                #
#                                                                         #
###########################################################################

sub new {
    my ($class, @args) = @_;

    my $self = bless {}, $class;
    $self->init(@args);
    return $self;
}

sub init {
    my ($self, %args) = @_;

    $args{normalize_attributes} = 1
	if(!defined($args{normalize_attributes}));
    foreach(keys %args) {
	my $meth = $_;
	if($self->can($meth)) { $self->$meth($args{$meth}) }
	else { carp "Unrecognized init parameter $meth.\n" }
    }
}

sub _process_initial_viewport_length {
    my ($self, $length, $description) = @_;
    my $eff_length;

    if($length) {
	# a length of 100% is as good as none here
	return undef if($length =~ /^$WSP*100\%$WSP*$/);

	$eff_length = eval { $self->map_abs_length($length) };
	if($@) {
	    # should be impossible because checked in rasterize
	    croak("Invalid $description ($length) specified:\n$@\n");
	}
	$eff_length = int($eff_length + 0.5);
	if($eff_length < 0) {
	    croak("Negative $description ($length) specified.\n");
	}
    }

    return $eff_length;
}

sub _initial_viewport {
    my ($self, $node_attributes, $args_ptr) = @_;
    my $matrix                              = [1, 0, 0, 1, 0, 0];
    my @width                               = ();
    my @height                              = ();

    # collecting information
    # width set by user in rasterize
    $width[0]  = $self->_process_initial_viewport_length
	($args_ptr->{width}, 'external width');
    $width[1]  = $self->_process_initial_viewport_length
	($node_attributes->{width}, 'width attribute');
    $height[0] = $self->_process_initial_viewport_length
	($args_ptr->{height}, 'external height');
    $height[1] = $self->_process_initial_viewport_length
	($node_attributes->{height}, 'height attribute');

    # width mapping
    if($width[0]) {
	if($width[1]) { $matrix->[0] = $width[0] / $width[1]  }
	else          { $node_attributes->{width} = $width[0] }
    }
    elsif($width[1]) { $width[0] = $width[1] }
    else { 
	croak("Failed to determine the width of the ".
	      "initial viewport.\n");
    }
    # same for height
    if($height[0]) {
	if($height[1]) { $matrix->[3] = $height[0] / $height[1]  }
	else           { $node_attributes->{height} = $height[0] }
    }
    elsif($height[1]) { $height[0] = $height[1] }
    else { 
	croak("Failed to determine the height of the ".
	      "initial viewport.\n");
    }

    $self->{width}      = $width[0];
    $self->{height}     = $height[0];

    $args_ptr->{matrix} = $matrix;
}

# Assumes that $self->{width}, $self->{height} are set to their final
# values, i.e. _initial_viewport has been executed.
sub _create_engine {
    my ($self, $args_ptr) = @_;
    my $default           = 'SVG::Rasterize::Cairo';
    my %engine_args       = (width  => $self->{width},
			     height => $self->{height});

    # TODO: class name testing
    $args_ptr->{engine_class} ||= $default;
    my $load_success = eval "require $args_ptr->{engine_class}";
    if(!$load_success and $args_ptr->{engine_class} ne $default) {
	carp("Unable to load $args_ptr->{engine_class}: $!. ".
	     "Falling back to $default.\n");
	$args_ptr->{engine_class} = $default;
	$load_success = eval "require $args_ptr->{engine_class}";
    }
    if(!$load_success) {
	croak("Unable to load $args_ptr->{engine_class}: $!. ".
	      "Bailing out.\n");
    }

    $self->{engine} = $args_ptr->{engine_class}->new(%engine_args);
}

###########################################################################
#                                                                         #
#                               Accessors                                 # 
#                                                                         #
###########################################################################

sub px_per_in {
    my ($self, @args) = @_;

    validate_pos(@args, { regex => qr/^$A_NUMBER$/, optional => 1 });
    $self->{px_per_in} = $args[0] if(@args);
    return defined($self->{px_per_in}) ? $self->{px_per_in} : $PX_PER_IN;
}

*dpi = \&px_per_in;  &dpi if(0);

sub in_per_cm {
    my ($self, @args) = @_;

    validate_pos(@args, { regex => qr/^$A_NUMBER$/, optional => 1 });
    $self->{in_per_cm} = $args[0] if(@args);
    return defined($self->{in_per_cm}) ? $self->{in_per_cm} : $IN_PER_CM;
}

sub in_per_mm {
    my ($self, @args) = @_;

    validate_pos(@args, { regex => qr/^$A_NUMBER$/, optional => 1 });
    $self->{in_per_mm} = $args[0] if(@args);
    return defined($self->{in_per_mm}) ? $self->{in_per_mm} : $IN_PER_MM;
}

sub in_per_pt {
    my ($self, @args) = @_;

    validate_pos(@args, { regex => qr/^$A_NUMBER$/, optional => 1 });
    $self->{in_per_pt} = $args[0] if(@args);
    return defined($self->{in_per_pt}) ? $self->{in_per_pt} : $IN_PER_PT;
}

sub in_per_pc {
    my ($self, @args) = @_;

    validate_pos(@args, { regex => qr/^$A_NUMBER$/, optional => 1 });
    $self->{in_per_pc} = $args[0] if(@args);
    return defined($self->{in_per_pc}) ? $self->{in_per_pc} : $IN_PER_PC;
}

sub map_abs_length {
    my ($self, @args) = @_;

    # To me it is unclear if leading/trailing white space is allowed
    # in a length attribute. I allow it.
    my ($number, $unit);
    if(@args < 2) {
	validate_pos(@args, {regex => $p_A_LENGTH});
	($number, $unit) = $args[0] =~ /^($A_NUMBER)($UNIT?)$/;
    }
    else { ($number, $unit) = @args }  # bypasses validation!

    my $dpi = $self->px_per_in;
    if(!$unit)           { return $number }
    elsif($unit eq 'em') { croak "Unit em in map_abs_length.\n" }
    elsif($unit eq 'ex') { croak "Unit ex in map_abs_length.\n" }
    elsif($unit eq 'px') { return $number }
    elsif($unit eq 'pt') { return $number * $self->in_per_pt * $dpi }
    elsif($unit eq 'pc') { return $number * $self->in_per_pc * $dpi }
    elsif($unit eq 'cm') { return $number * $self->in_per_cm * $dpi }
    elsif($unit eq 'mm') { return $number * $self->in_per_mm * $dpi }
    elsif($unit eq 'in') { return $number * $dpi }
    elsif($unit eq '%')  { croak "Lenght in % in map_abs_length.\n" }
}

sub before_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, {type => CODEREF});
	$self->{before_node_hook} = $args[0];
    }
    return $self->{before_node_hook} || sub {};
}

sub start_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, {type => CODEREF});
	$self->{start_node_hook} = $args[0];
    }
    return $self->{start_node_hook} || sub {};
}

sub end_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, {type => CODEREF});
	$self->{end_node_hook} = $args[0];
    }
    return $self->{end_node_hook} || sub {};
}

###########################################################################
#                                                                         #
#                                Drawing                                  #
#                                                                         #
###########################################################################

sub _draw_line {
    my ($self, $state) = @_;
    my $node = $state->node;
    my $x1   = $self->map_abs_length($node->getAttribute('x1') || 0);
    my $y1   = $self->map_abs_length($node->getAttribute('y1') || 0);
    my $x2   = $self->map_abs_length($node->getAttribute('x2') || 0);
    my $y2   = $self->map_abs_length($node->getAttribute('y2') || 0);

    $self->{engine}->draw_line($state->properties,
			       $state->transform($x1, $y1),
			       $state->transform($x2, $y2));
}

###########################################################################
#                                                                         #
#                             Tree Traversal                              #
#                                                                         #
###########################################################################

sub _in_error {
    my ($self, $message) = @_;

    croak $message;
}

sub _process_normalize_attributes {
    my ($self, @args) = @_;

    $args[0] ||= {};

    if($self->{normalize_attributes}) {
	foreach(keys %{$args[0]}) {
	    $args[0]->{$_} =~ s/^$WSP*//;
	    $args[0]->{$_} =~ s/$WSP*$//;
	    $args[0]->{$_} =~ s/$WSP+/ /g;
	}
    }

    return $args[0];
}

sub rasterize {
    my ($self, @args) = @_;

    my %args = validate
	(@args,
	 {svg          => { isa => 'SVG::Element' },
	  width        => { optional => 1, regex => qr/^$A_LENGTH$/ },
	  height       => { optional => 1, regex => qr/^$A_LENGTH$/ },
	  engine_class => 0});

    # $engine stays constant. In contrast, $node and $state
    # save the current svg element and the current state object,
    # respectively.
    my $node            = $args{svg}->getNodeName eq 'document'
	? $args{svg}->firstChild : $args{svg};
    my $node_name       = $node->getNodeName;
    my $node_attributes = $self->_process_normalize_attributes
	(scalar($node->getAttributes));

    $self->before_node_hook->($self,
			      $node,
			      $node_name,
			      $node_attributes);
    $self->_initial_viewport($node_attributes, \%args);

    my $state           = SVG::Rasterize::State->new
	(rasterize       => $self,
	 node            => $node,
	 node_name       => $node_name,
	 node_attributes => $node_attributes,
	 matrix          => $args{matrix});

    $self->start_node_hook->($self, $state);

    my $engine = $self->_create_engine(\%args);

    my @stack = ();
    while($state) {
	if($state->hasChildren) {
	    $node = $state->nextChild;
	    if($node) {
		push(@stack, $state);
		$node_name       = $node->getNodeName;
		$node_attributes = $self->_process_normalize_attributes
		    (scalar($node->getAttributes));
		$self->before_node_hook->($self,
					  $node,
					  $node_name,
					  $node_attributes);
		$state           = SVG::Rasterize::State->new
		    (rasterize       => $self,
		     parent          => $state,
		     node            => $node,
		     node_name       => $node_name,
		     node_attributes => $node_attributes);
		$self->start_node_hook->($self, $state);
	    }
	    else {
		$self->end_node_hook->($self, $state);
		$state = pop @stack;
	    }
	}
	else {
	    # do something
	    $self->_draw_line($state) if($state->node_name eq 'line');

	    $self->end_node_hook->($self, $state);
	    $state = pop @stack;
	}
    }
}

sub write { return shift(@_)->{engine}->write(@_) }

1;


__END__

=pod

=head1 SYNOPSIS

    use SVG;
    use SVG::Rasterize;

    my $svg = SVG->new(width => 300, height => 200);
    $svg->line(x1 => 10, y1 => 20, x2 => 220, y2 => 150,
               style => {stroke => 'black', stroke-width => '2pt' });

    # add more svg content
    # .
    # .
    # .

    my $rasterize = SVG::Rasterize->new();
    $rasterize->rasterize(svg => $svg);
    $rasterize->write(type => 'png', file_name => 'out.png');


=head1 DESCRIPTION

C<SVG::Rasterize> can be used to rasterize L<SVG|SVG> objects to
pixel graphics (currently png only) building on the L<Cairo|Cairo>
library (by default, other underlying rasterization engines could be
added). The direct rasterization of C<SVG> B<files> might be
implemented in the future, right now you should have a look at
L<SVG::Parser|SVG::Parser> which can generate an L<SVG|SVG> object
from an C<svg> file.

=head2 Motivation

In the past, I have used several programs to rasterize C<SVG>
graphics including L<Inkscape|http://www.inkscape.org/>,
L<Konqueror|http://www.konqueror.org/>, L<Adobe
Illustrator|http://www.adobe.com/products/illustrator/>, and
L<rsvg|librsvg.sourceforge.net/>. While
L<Inkscape|http://www.inkscape.org/> was my favourite none of them
made me entirely happy. There were always parts of the standard that
I would have liked to use, but were unsupported.

So finally, I set out to write my own rasterization engine. The
ultimate goal is complete compliance with the requirements for a
C<Conforming Static SVG Viewer> as described in the SVG
specification:
L<http://www.w3.org/TR/SVG11/conform.html#ConformingSVGViewers>.
Obviously, this is a long way to go. I do not know if any support
for the dynamic features of C<SVG> will ever be added. Anyway, the
priority for C<SVG::Rasterize> is accuracy, not speed.

=head2 Status

This release is a proof of concept. It mainly shows the successful
deployment of L<Cairo|Cairo>. The support of transform attributes
and the establishment of the initial viewport are fully implemented.
However, the only things you can draw at the moment are lines
with specified color and stroke width.

I hope that the interface described here will be largely stable.
However, this is not guaranteed. Some features are documented as
likely to change, but everything is subject to change at this early
stage.

=head1 INTERFACE

=head2 Constructors

=head3 new

  Usage   : SVG::Rasterize->new(%args)
  Function: creates a new SVG::Rasterize object
  Returns : a SVG::Rasterize object
  Args    : initial attribute values as named parameters

Creates a new C<SVG::Rasterize> object and calls C<init(%args)>. If
you subclass C<SVG::Rasterize> overload L<init|/init>, not C<new>.

For each given argument, L<init|/init> calls the accessor with the
same name to initialize the attribute. If such an accessor (or in
fact, any method of that name) does not exist a warning is printed
and the argument is ignored.

=head2 Public Attributes

There are some attributes that influence unit conversions,
white space handling, and the choice of the underlying rasterization
engine. See L<ADVANCED TOPICS|/ADVANCED TOPICS>.

=head2 Methods for Users

=head3 rasterize

  $rasterize->rasterize(%args)

Traverses through the given C<SVG> content and renders the
output. Does not return anything.

B<Examples:>

  $rasterize->rasterize(svg => $svg);
  $rasterize->rasterize(svg => $svg, width => 640, height => 480);
  $rasterize->rasterize(svg => $svg, engine_class => 'My::Class');

Supported parameters:

=over 4

=item * svg (mandatory): L<SVG|SVG> object to rasterize

=item * width (optional): width of the target image in pixels

=item * height (optional): height of the target image in pixels

=item * engine_class (optional): alternative engine class to
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. See there for
details on the interface.

=back

If C<width> (the same applies to C<height>) is 0 it is treated as
not set. If you encounter any scenario where you would wish an
explicit size of 0 to be treated in some other way let me know.

If C<width> and/or C<height> are not specified they have to have
absolute values in the root C<SVG> element. If both the root C<SVG>
element and the C<rasterize> method have width and/or height
settings then the C<rasterize> parameters determine the size of the
output image and the specified C<SVG> viewport is mapped to this
image taking the C<viewBox> and C<preserveAspectRatio> attributes
into account if they are present. See
L<http://www.w3.org/TR/SVG11/coords.html#ViewportSpace> for details.

The user can influence the rasterization process via hooks. See the
L<Hooks|/Hooks> section below.

=head3 write

  $rasterize->write(%args)

Writes the rendered image to a file.

B<Example:>

  $rasterize->write(type => 'png', file_name => 'foo.png');

The supported parameters depend on the rasterization backend. The
C<write> method hands all parameters over to the backend. See
L<write|SVG::Rasterize::Cairo/write> in C<SVG::Rasterize::Cairo> for
an example.

=head2 Methods for Subclass Developers

=head3 init

  Usage   : only called by new
  Function: initializes attributes
  Returns : nothing
  Args    : initial attribute values as named parameters

If you overload C<init>, your method should also call this one.

For each given argument, C<init> calls the accessor with the same
name to initialize the attribute. If such an accessor (or in fact,
any method of that name) does not exist a warning is printed and the
argument is ignored. Readonly attributes that are allowed to be set
at initialization time are set separately at the beginning.

=head2 Class Methods

=head3 multiply_matrices

2D affine transformation can be represented by 3 x 3 matrices
of the form:

  ( a  c  e )
  ( b  d  f )
  ( 0  0  1 )

In this case, the concatenation of such transformations is
represented by canonical matrix multiplication. This method takes
two ARRAY references of the form C<[a, b, c, d, e, f]> whose entries
correspond to the matrix entries above and returns an ARRAY
reference with 6 entries representing the product matrix.

The method can be called either as subroutine or as class
method or as object method:

  multiply_matrices($m, $n);
  SVG::Rasterize->multiply_matrices($m, $n);
  $rasterize->multiply_matrices($m, $n);

Note that C<multiply_matrices> does not perform any input check. It
expects that you provide (at least) two ARRAY references with (at
least) 6 numbers each. If you pass more parameters then the last two
are used. If they contain more than 6 entries then the first 6 are
used.

=head1 ADVANCED TOPICS

=head2 Units

C<SVG> supports the absolute units C<px>, C<pt>, C<pc>, C<cm>,
C<mm>, C<in>, and the relative units C<em>, C<ex>, and C<%>.
Lengths can also be given as numbers without unit which is then
interpreted as C<px>. See
L<http://www.w3.org/TR/SVG11/coords.html#Units>.

C<SVG::Rasterize> stores default values for unit conversion rates as
class variables. You can either change these values or the
corresponding object variables. If you have only one
C<SVG::Rasterize> object both approaches have the same effect.

The default values are listed below. Except C<px_per_in>, they
are taken from the C<CSS> specification. See
L<http://www.w3.org/TR/2008/REC-CSS2-20080411/syndata.html#length-units>.
The default for C<px_per_in> is arbitrarily set to 90.

Currently, the relative units listed above are not supported by
C<SVG::Rasterize>.

Unit conversions:

=over 4

=item * px_per_in

Pixels per inch. Defaults to 90.

=item * dpi

Alias for L<px_per_in|/px_per_in>. This is realized via a typeglob
copy:

  *dpi = \&px_per_in

=item * in_per_cm

Inches per centimeter. Defaults to 1/2.54. This is the
internationally defined value. I do not see why I should prohibit
a change, but it would hardly make sense.

=item * in_per_mm

Inches per millimeter. Defaults to 1/25.4. This is the
internationally defined value. I do not see why I should prohibit
a change, but it would hardly make sense.

=item * in_per_pt

Inches per point. Defaults to 1/72. According to L<[1]|/[1]>, this
default was introduced by the Postscript language. There are other
definitions. However, the C<CSS> specification is quite firm about
it.

=item * in_per_pc

Inches per pica. Defaults to 1/6. According to the C<CSS>
specification, 12pc equal 1pt.

=item * map_abs_length

  $rasterize->map_abs_length($length)
  $rasterize->map_abs_length($number, $unit)

This method takes a length and returns the corresponding value
in C<px> according to the conversion rates above. Surrounding
white space is not allowed.

B<Examples:>

  $x = $rasterize->map_abs_length('5.08cm');  # returns 180
  $x = $rasterize->map_abs_length(10);        # returns 10
  $x = $rasterize->map_abs_length(10, 'pt')   # returns 12.5
  $x = $rasterize->map_abs_length('  1in ');  # croaks
  $x = $rasterize->map_abs_length('50%')      # croaks

The unit has to be absolute, C<em>, C<ex>, and C<%> trigger an
exception. See L<map_length|SVG::Rasterize::State/map_length>
in C<SVG::Rasterize::State>.

There are two different interfaces. You can either pass one string
or the number and unit separately. NB: In the second case, the input
is not validated. This interface is meant for situations where the
length string has already been parsed (namely in
L<map_length|SVG::Rasterize::State/map_length> in
C<SVG::Rasterize::State>) to avoid duplicate validation. The number
is expected to match L<A_NUMBER|/A_NUMBER> and the unit to match
L<UNIT|/UNIT> (see below). However, it is still checked if the unit
is absolute.

=back

The corresponding class attributes are:

=over 4

=item * PX_PER_IN

Defaults to 90.

=item * DPI

Alias for C<PX_PER_IN>. This is realized via a typeglob copy

  *DPI = \$PX_PER_IN

=item * IN_PER_CM

Defaults to 1/2.54.

=item * IN_PER_MM

Defaults to 1/25.4.

=item * IN_PER_PT

Defaults to 1/72.

=item * IN_PER_PC

Defaults to 1/6.

=back

=head2 Lengths versus Numbers

The C<SVG> specification determines which values (e.g. in
attributes) are lengths (numbers possibly with a unit) and which are
numbers (without a unit). Some attributes have to be numbers
although a length would make sense (e.g. in the C<viewBox> attribute
or a C<translate> in a C<transform> attribute). C<SVG::Rasterize>
aims for strict compliance with the specification. However, in the
future there might be a C<relax> attribute which when turned to 1 or
higher allows a more and more relaxed interpretation.

=head2 White Space Handling

The C<XML> specification
(L<http://www.w3.org/TR/2006/REC-xml11-20060816/#AVNormalize>)
states that an attribute value unless it is of the type CDATA shall
be normalized such that leading and trailing white space is removed
and internal white space is flattened to single space characters.
C<XML> entities can complicate this normalization, see the
specification for details.

If the C<SVG> tree to be rasterized by C<SVG::Rasterize> comes out
of an parsed C<XML> document then the parser should have performed
this normalization already. However, the tree might also be
constructed directly using the L<SVG|SVG> module. In order to
prevent C<SVG::Rasterization> from choking on an attribute like
C<stroke-width="2pt "> it performs by default an additional
normalization run:

  $value =~ s/^$WSP*//;
  $value =~ s/$WSP*$//;
  $value =~ s/$WSP+/ /g;

where

  $WSP = qr/[\x{20}\x{9}\x{D}\x{A}]/;  # space, tab, CR, LF

To prevent this normalization, you can set the
C<normalize_attributes> attribute to a false value.

=head2 Hooks

The L<rasterize|/rasterize> method traverses through the C<SVG> tree
and creates an L<SVG::Rasterize::State|/SVG::Rasterize::State>
object for each node (at least each node of relevance). Hooks allow
you to execute your own subroutines at given steps of this
traversal. However, the whole hook business is experimental at the
moment and likely to change. Right now, to set your own hooks you
can set one of the following attributes to a code reference of your
choice.

Currently, there are three hooks:

=over 4

=item * before_node_hook

Executed at encounter of a new node right before the new
L<SVG::Rasterize::State|SVG::Rasterize::State> object is created.
It is called as an object method and receives the C<SVG::Rasterize>
object, the node object (during C<DOM> tree traversal, otherwise
undef), the node name, and the the node attributes (as HASH
reference). The attribute values have already been normalized at
this stage (see L<White Space Handling|/White Space Handling>
above).

=item * start_node_hook

Executed right after creation of the
L<SVG::Rasterize::State|SVG::Rasterize::State> object. The
attributes have been parsed,
L<properties|SVG::Rasterize::State/properties> and
L<matrix|SVG::Rasterize::State/matrix> have been set etc. The
method receives the C<SVG::Rasterize> object and the
L<SVG::Rasterize::State|SVG::Rasterize::State> object.

=item * end_node_hook

Executed right before the
L<SVG::Rasterize::State|SVG::Rasterize::State> runs out of
scope because the current node is done with. The
method receives the C<SVG::Rasterize> object and the
L<SVG::Rasterize::State|SVG::Rasterize::State> object.

=back

B<Examples:>

  $rasterize->start_node_hook(sub { ... })

=head2 Rasterization Backend

C<SVG::Rasterize> does not render pixel graphics itself. By default,
it uses the L<cairo|http://www.cairographics.org/> library through
its L<Perl bindings|Cairo>. The interface is provided by
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. However, this
interface could also be implemented by other backends. In the
future, it will be documented in
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. Currently, it has to
be consisered unstable, though, and the documentation is sparse.

=head3 engine

  $rasterize->engine

This attribute holds the interface object to the rasterization
backend, by default a L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>
object. The object is created by the L<rasterize|/rasterize>
method.

The attribute is readonly, but, of course, you are able to
manipulate the object directly via its methods. However, this is
not part of the normal workflow and you do this on your own risk
;-).

=head1 DIAGNOSTICS

=head2 Exceptions

Not documented, yet. Sorry.

=head2 Warnings

Not documented, yet. Sorry.

=head2 Error processing

The C<SVG> documentation specifies how C<SVG> interpreters
should react to certain incidents. The relevant section can be
found here:
L<http://www.w3.org/TR/SVG11/implnote.html#ErrorProcessing>.

This section will describe how some of these instructions are
implemented by C<SVG::Rasterize> and how it reacts in some other
situations in which the specification does not give instructions.
However, at the moment, C<SVG::Rasterize> usually croaks whenever
it is unhappy.

=head3 Skew of 90°

=head1 DEPENDENCIES

=over 4

=item * L<Class::Accessor|Class::Accessor>, version 0.30 or higher

=item * L<SVG|SVG>, version 2.37 or higher

=item * L<Cairo|Cairo>, version 1.061 or higher

With respect to the actual code, the dependency on L<Cairo|Cairo> is
not strict. The code only requires L<Cairo|Cairo> in case no other
rasterization engine is specified. However, if you do not provide a
different rasterization backend, which would probably at least
require a wrapper written by you, then you cannot do anything
without L<Cairo|Cairo>. Therefore I have included it as a strict
dependency. Feel free take that out of the Makefile.PL if you know
what you are doing.

=item * L<Params::Validate|Params::Validate>, version 0.91 or higher

=item * L<Test::More|Test::More>, version 0.86 or higher

=item * L<Test::Exception|Test::Exception>, version 0.27 or higher

=item * L<Scalar::Util|Scalar::Util>, version 1.19 or higher

=back

=head1 BUGS AND LIMITATIONS

=head2 Bugs

No bugs have been reported. Please report any bugs or feature
requests to C<bug-svg-rasterize at rt.cpan.org>, or through the web
interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=SVG-Rasterize>. I
will be notified, and then you will automatically be notified of
progress on your bug as I make changes.

=head2 Caveats

=head3 C<eval BLOCK> and C<$SIG{__DIE__}>

Several methods in this distribution use C<eval BLOCK> statements
without setting a local C<$SIG{__DIE__}>. Therefore, a
C<$SIG{__DIE__}> installed somewhere else can be triggered by
these statements. See C<die> and C<eval> in C<perlfunc> and
C<$^S> in C<perlvar>.

=head3 Thread safety

I do not know much about threads and how to make a module thread
safe. No specific measures have been taken to achieve thread
safety of this distribution.

=head3 Parameter Checking

C<SVG::Rasterize> uses L<Params::Validate|Params::Validate> for
parameter validation. However, it currently does not do that as
thoroughly as one would wish for. Do not rely on that wrong
stuff will not pass unnoticed.

=head3 Test Suite

The test suite covers essential features, but is far from
exhaustive.

=head1 INTERNALS

=head2 Regular Expressions

The following regular expressions are used at different locations of
the code to validate or extract user input. They are listed in the
INTERNALS section because it is not part of the interface where
exactly they are used. They are documented for inspection only. They
are compiled into other expressions so changing them will probably
not achieve what you might expect. The following items are global
variables in the C<SVG::Rasterize> package. They are more or less a
direct translation of parts of the Backus Naur form given by the
C<SVG> specification for the C<transform> attribute
(L<http://www.w3.org/TR/SVG11/coords.html#TransformAttribute>).

=over 4

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

=head2 Internal Methods

=over 4

=item * _in_error

=item * _create_engine

=item * _process_initial_viewport_length

=item * _initial_viewport

=item * _draw_line

=back

=head1 FOOTNOTES

=over 4

=item * [1] Yannis Haralambous: Fonts & Encodings. O'Reilly, 2007.

Tons of information about what the author calls the "digital space
for writing".

=back

=head1 SEE ALSO

=over 4

=item * L<SVG|SVG>

=item * L<SVG::Parser|SVG::Parser>

=item * L<Cairo|Cairo>

=item * L<http://www.w3.org/TR/SVG11/>.

=back


=head1 ACKNOWLEDGEMENTS

This distribution builds heavily on the
L<cairo|http://www.cairographics.org/> library and its
L<Perl bindings|Cairo>.


=head1 AUTHOR

Lutz Gehlen, C<< <perl at lutzgehlen.de> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2010 Lutz Gehlen.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
