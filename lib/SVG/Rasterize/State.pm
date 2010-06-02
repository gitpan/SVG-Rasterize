package SVG::Rasterize::State;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Params::Validate qw(:all);
use Scalar::Util qw(blessed looks_like_number);
use List::Util qw(min max);

use SVG::Rasterize::Regexes qw(:whitespace
                               :attributes);
use SVG::Rasterize::Specification;
use SVG::Rasterize::Properties;
use SVG::Rasterize::Colors;
use SVG::Rasterize::Exception qw(:all);

# $Id: State.pm 5878 2010-06-01 08:41:47Z mullet $

=head1 NAME

C<SVG::Rasterize::State> - state of settings during traversal

=head1 VERSION

Version 0.002002

=cut

our $VERSION = '0.002002';


__PACKAGE__->mk_accessors(qw());

__PACKAGE__->mk_ro_accessors(qw(rasterize
                                hasChildren
                                node_name
                                node
                                matrix
                                properties));

###########################################################################
#                                                                         #
#                      Class Variables and Methods                        # 
#                                                                         #
###########################################################################

use constant PI => 3.14159265358979;

*multiply_matrices = \&SVG::Rasterize::multiply_matrices;

###########################################################################
#                                                                         #
#                             Init Process                                #
#                                                                         #
###########################################################################

sub new {
    my ($class, @args) = @_;

    my $self = bless {}, $class;
    return $self->init(@args);
}

sub init {
    my ($self, @args) = @_;
    my %args          = validate_with
	(params => \@args,
	 spec   =>
	     {rasterize       => {isa      => 'SVG::Rasterize'},
	      parent          => {isa      => 'SVG::Rasterize::State',
				  optional => 1},
	      node_name       => {type     => SCALAR},
	      node_attributes => {type     => HASHREF},
	      node            => {can      => ['getChildren'],
				  optional => 1},
	      matrix          => {type     => ARRAYREF,
				  optional => 1}},
	on_fail => sub { SVG::Rasterize->ex_pv($_[0]) });

    # read only and private arguments
    $self->{_parent}         = $args{parent} if(exists($args{parent}));
    $self->{rasterize}       = $args{rasterize};
    $self->{node_name}       = $args{node_name};
    $self->{node_attributes} = $args{node_attributes};
    $self->{node}            = $args{node}   if(exists($args{node}));
    $self->{matrix}          = $args{matrix} if(exists($args{matrix}));

    if($args{node}) {
	my $children = eval { $args{node}->getChildren };
	$self->{children}    = $children ? [@$children] : [];
	$self->{hasChildren} = defined($children) ? 1 : 0;
    }

    $self->_process_node;

    return $self;
}

sub _process_transform_attribute {
    my ($self)    = @_;
    my $transform = $self->{node_attributes}->{transform};

    return if(!$transform);

    # dissect string into single transformation strings
    my @atoms = ();
    my $str   = $transform;
    while($str) {
	if($str =~ $RE_TRANSFORM{TRANSFORM_SPLIT}) {
	    push(@atoms, $1);
	    $str = $2;
	}
	else { $self->ex_pa('transform', $transform) }
    }
    
    # process the single transformations
    my $sm = $self->{matrix};
    foreach(@atoms) {
	my ($type, $param_str) = $_ =~ $RE_TRANSFORM{TRANSFORM_CAPTURE};
	my @params             = split(/$CWSP/, $param_str);

	my $cm;  # current matrix
	if   ($type eq 'matrix') { $cm = [@params] }
	elsif($type eq 'translate') {
	    $cm = [1, 0, 0, 1, $params[0], $params[1] || 0];
	}
	elsif($type eq 'scale') {
	    $cm = [$params[0], 0,
		   0, defined($params[1]) ? $params[1] : $params[0],
		   0, 0];
	}
	elsif($type eq 'rotate') {
	    my $cos = cos($params[0] / 180 * PI);
	    my $sin = sin($params[0] / 180 * PI);
	    my $tx  = $params[1] || 0;
	    my $ty  = $params[2] || 0;
	    $cm = [$cos, $sin, -$sin, $cos,
		   ($cos - 1) * $tx + $sin * $ty,
		   $sin * $tx + ($cos - 1) * $ty];
	}
	elsif($type eq 'skewX') {
	    my $cos = cos($params[0] / 180 * PI);
	    if($cos == 0) {
		warn("tan($params[0]) is undefined, cannot skew with ".
		     "this angle. Skipping this transform\n");
		$cm = [1, 0, 0, 1, 0, 0];
	    }
	    else {
		$cm = [1, 0, sin($params[0] / 180 * PI) / $cos, 1, 0, 0];
	    }
	}
	elsif($type eq 'skewY') {
	    my $cos = cos($params[0] / 180 * PI);
	    if($cos == 0) {
		warn("tan($params[0]) is undefined, cannot skew with ".
		     "this angle. Skipping this transform\n");
		$cm = [1, 0, 0, 1, 0, 0];
	    }
	    else {
		$cm = [1, sin($params[0] / 180 * PI) / $cos, 0, 1, 0, 0];
	    }
	}
	else { $self->ex_pa('transform', $transform) }

	$sm = multiply_matrices($sm, $cm);
    }

    $self->{matrix} = $sm;
    return;
}

sub _process_viewBox_attribute {
    my ($self)     = @_;
    my $name       = $self->{node_name};
    my $attributes = $self->{node_attributes};
    my $viewBox    = $attributes->{viewBox};

    return if(!$viewBox);

    my $width  = $attributes->{width};
    my $height = $attributes->{height};
    if(!$width or !$height) {
	$self->ex_us_si("Element with viewBox, but without both ".
			"width and height");
    }

    # viewBox
    my ($min_x, $min_y, $vB_width, $vB_height) =
	$viewBox =~ $RE_VIEW_BOX{p_VIEW_BOX};
    if($vB_width < 0) {
	$self->ie_at_vb_nw($vB_width);
    }
    if($vB_width == 0) {
	$self->ex_us_is("A viewBox width of 0");
    }
    if($vB_height < 0) {
	$self->ie_at_vb_nw($vB_height);
    }
    if($vB_height == 0) {
	$self->ex_us_is("A viewBox height of 0");
    }

    # preserveAspectRatio
    my ($defer, $align, $meetOrSlice);
    my $pAR = $attributes->{preserveAspectRatio};
    if($pAR) {
	if($pAR =~ /^defer +$RE_VIEW_BOX{ALIGN} +$RE_VIEW_BOX{MOS}$/) {
	    ($defer, $align, $meetOrSlice) = ('defer', $1, $2);
	}
	elsif($pAR =~ /^$RE_VIEW_BOX{ALIGN} +$RE_VIEW_BOX{MOS}$/) {
	    ($defer, $align, $meetOrSlice) = ('', $1, $2);
	}
	elsif($pAR =~ /^defer +$RE_VIEW_BOX{ALIGN}$/) {
	    ($defer, $align, $meetOrSlice) = ('defer', $1, 'meet');
	}
	elsif($pAR =~ /^$RE_VIEW_BOX{ALIGN}$/) {
	    ($defer, $align, $meetOrSlice) = ('', $1, 'meet');
	}
	else {
	    $self->ex_pa('preserveAspectRatio', $pAR);
	}
    }

    my $sc_x = $width / $vB_width;
    my $sc_y = $height / $vB_height;
    my $matrix;
    if($align and $align ne 'none') {
	if($name eq 'image' and $defer) {
	    # TODO: handle defer; deferring should result in a return
	}

	$matrix = [1, 0, 0, 1, -$min_x, -$min_y];
	my $sc = $meetOrSlice eq 'meet'
	    ? min($sc_x, $sc_y) : max($sc_x, $sc_y);
	$matrix = multiply_matrices([$sc, 0, 0, $sc, 0, 0], $matrix);
	my ($x_str, $y_str) = (substr($align, 0, 4), substr($align, 4, 4));
	my ($x_tr, $y_tr);
	if($x_str eq 'xMin')    { $x_tr = 0 }
	elsif($x_str eq 'xMid') { $x_tr = ($width - $sc * $vB_width) / 2 }
	elsif($x_str eq 'xMax') { $x_tr = $width - $sc * $vB_width }
	else { $self->ex_pa('preserveAspectRatio', $pAR) }

	if($y_str eq 'YMin')    { $y_tr = 0 }
	elsif($y_str eq 'YMid') { $y_tr = ($height - $sc * $vB_height) /2 }
	elsif($y_str eq 'YMax') { $y_tr = $height - $sc * $vB_height }	
	else { $self->ex_pa('preserveAspectRatio', $pAR) }

	$matrix = multiply_matrices([1, 0, 0, 1, $x_tr, $y_tr], $matrix);
    }
    else {
	my @f = ($width / $vB_width, $height / $vB_height);
	$matrix = [$sc_x, 0, 0, $sc_y,-$min_x * $sc_x, -$min_y * $sc_y];
    }

    $self->{matrix} = multiply_matrices($self->{matrix}, $matrix);
    return;
}

sub _process_styling_properties {
    my ($self)      = @_;
    my $name        = $self->{node_name};
    my $attributes  = $self->{node_attributes};
    my @prop_names  = grep { $ATTR_VAL{$name}->{$_} } keys %PROPERTIES;
    my $parent_prop = $self->{_parent} ? $self->{_parent}->properties : {};
    my $css         = {};
    my $properties  = {};

    # parse style attribute
    if($attributes->{style}) {
	foreach(split(/$WSP*\;$WSP*/, $attributes->{style})) {
	    my $prop_pattern = qr/([^\:]+)\:(.+)/;
	    if(my ($prop_name, $prop_value) = $_ =~ $prop_pattern) {
		# deal with whitespace
		$css->{lc($prop_name)} = $prop_value;
	    }
	    else { ex_pa('css property', $_) }
	}
    }

    foreach(@prop_names) {
	my $spec  = $PROPERTIES{$_};
	my $hints = $ATTR_HINTS{$name}->{$_};
	if(defined($css->{$_}) or defined($attributes->{$_})) {
	    my $val = {%{$ATTR_VAL{$name}->{$_}}};
	    $val->{optional} = 1;
	    delete $val->{default};
	    
	    if(defined($css->{$_})) {
		validate_with
		    (params      => $css,
		     spec        => {$_ => $val},
		     allow_extra => 1,
		     on_fail     =>
		         sub { $self->ie_pr_pv($_, $_[0]) });
		$properties->{$_} = $css->{$_}
		    unless($css->{$_} eq 'inherit');
	    }
	    else {
		# the following validation is redundant as long as
		# the property validation uses the attribute
		# validation framework
		validate_with
		    (params      => $attributes,
		     spec        => {$_ => $val},
		     allow_extra => 1,
		     on_fail     =>
		         sub { $self->ie_pr_pv($_, $_[0]) });
		$properties->{$_} = $attributes->{$_}
		    unless($attributes->{$_} eq 'inherit');
	    }
	}

	if(!defined($properties->{$_})) {
	    if(defined($parent_prop->{$_}) and $spec->{inherited}) {
		$properties->{$_} = $parent_prop->{$_};
	    }
	    else {
		$properties->{$_} = $spec->{default};
	    }
	}

	# parse color specs
	if($hints->{color} and defined($properties->{$_})) {
	    if(ref($properties->{$_}) eq 'ARRAY') {}
	    elsif($properties->{$_} =~ $RE_COLOR{p_RGB}) {
		$properties->{$_} = [$1, $2, $3];
		foreach my $rgb_entry (@{$properties->{$_}}) {
		    if((my $i = index($rgb_entry, '%')) >= 0) {
			$rgb_entry = int(substr($rgb_entry, 0, $i)
					 * 2.55 + 0.5);
		    }
		}
	    }
	    elsif(exists($COLORS{$properties->{$_}})) {
		$properties->{$_} = $COLORS{$properties->{$_}};
	    }
	    else { $self->ie_pr_co_iv($properties->{$_}) }
	}

	# lengths
	if($hints->{length} and defined($properties->{$_})) {
	    $properties->{$_} = $self->map_length($properties->{$_});
	}

	# specific attribute processing
	# stroke-miterlimit
	if($_ eq 'stroke-miterlimit') {
	    if($properties->{$_} < 1) {
		$self->ie_pr_st_nm($properties->{$_});
	    }
	}

	# stroke-dasharray
	if($_ eq 'stroke-dasharray') {
	    if(defined($properties->{$_})) {
		if($properties->{$_} eq 'none') {
		    $properties->{$_} = undef;
		}
		else {
		    $properties->{$_} =
			[map { $self->map_length($_) }
			 split($RE_DASHARRAY{SPLIT}, $properties->{$_})];
		    foreach my $dash (@{$properties->{$_}}) {
			if($dash < 0) {
			    $self->ie_pr_st_nd($dash);
			}
		    }
		    if(@{$properties->{$_}} % 2) {
			$properties->{$_} = [@{$properties->{$_}},
					     @{$properties->{$_}}];
		    }
		}
	    }
	}
    }

    $self->{properties} = $properties;
    return $properties;  # just a return value that makes sense
}

sub _process_node {
    my ($self)     = @_;
    my $name       = $self->{node_name};
    my $attributes = $self->{node_attributes};

    if($self->{_parent}) {
	# allowed child element?
	my $p_node_name = $self->{_parent}->node_name;
	if(!$CHILDREN{$p_node_name}->{$name}) {
	    $self->ie_el($name, $p_node_name);
	}
    }
    else {
	# valid element itself
	$self->ie_el($name) if(!exists($CHILDREN{$name}));
    }

    # attribute validation
    my @attr_buffer = %$attributes;
    validate_with(params  => \@attr_buffer,
		  spec    => $ATTR_VAL{$name},
		  on_fail => sub { $self->ie_at_pv($_[0]) });

    # apply transformations
    $self->{matrix} ||= [1, 0, 0, 1, 0, 0];
    if($self->{_parent} and $self->{_parent}->{matrix}) {
	$self->{matrix} = multiply_matrices($self->{_parent}->{matrix},
					    $self->{matrix});
    }
    $self->_process_transform_attribute if($attributes->{transform});
    $self->_process_viewBox_attribute   if($attributes->{viewBox});
    $self->_process_styling_properties;

    return;
}

###########################################################################
#                                                                         #
#                               Accessors                                 # 
#                                                                         #
###########################################################################

sub node_attributes {
    my ($self) = @_;

    $self->{node_attributes} ||= {};
    return $self->{node_attributes};
}

###########################################################################
#                                                                         #
#                             Retrieve Data                               #
#                                                                         #
###########################################################################

sub map_length {
    my ($self, @args) = @_;

    validate_with(params  => \@args,
		  spec    => [{regex => $RE_LENGTH{p_A_LENGTH}}],
		  on_fail => sub { $self->ie_pv($_[0]) });

    my ($number, $unit) =
	$args[0] =~ /^($RE_NUMBER{A_NUMBER})($RE_LENGTH{UNIT}?)$/;
    
    if(!$unit)           { return $number }
    elsif($unit eq 'em') { $self->ex_us_is("Unit em") }
    elsif($unit eq 'ex') { $self->ex_us_is("Unit ex") }
    elsif($unit eq '%')  { $self->ex_us_is("Unit %")  }

    return $self->{rasterize}->map_abs_length($number, $unit);
}

sub transform {
    my ($self, $x, $y) = @_;
    my $matrix         = $self->{matrix};

    # validation of $x and $y is done in map_length;
    my ($x_user, $y_user) = ($self->map_length($x),
			     $self->map_length($y));

    return($matrix->[0] * $x_user + $matrix->[2] * $y_user + $matrix->[4],
	   $matrix->[1] * $x_user + $matrix->[3] * $y_user + $matrix->[5]);
}

###########################################################################
#                                                                         #
#                              Change Data                                # 
#                                                                         #
###########################################################################

sub nextChild {
    my ($self) = @_;

    return shift(@{$self->{children}});
}

1;


__END__

=pod

=head1 DESCRIPTION

An instance of this class saves one state during the traversal
through an C<SVG> tree. At encounter of a new child element the old
state is pushed to a stack and retrieved later. A state saves the
current transformation matrix, style settings and so on. Part of
this functionality overlaps with the ability of L<Cairo|Cairo> to
push its state onto a stack, but I do not want to entirely rely on
that because I am not sure if everything can be handled in that way
and also because future alternative backends might not have this
feature.

This class is instanced only by L<SVG::Rasterize|SVG::Rasterize>.
The information of this document will mainly be interesting for
maintainers of L<SVG::Rasterize|SVG::Rasterize> and possibly for
advanced users who want to write L<hooks|SVG::Rasterize/Hooks>.

=head1 INTERFACE

=head2 Constructors

=head3 new

  $state = SVG::Rasterize::State->new(%args)

Creates a new C<SVG::Rasterize::State> object and calls
C<init(%args)>. If you subclass C<SVG::Rasterize::State> overload
L<init|/init>, not C<new>.

Supported arguments:

=over 4

=item * rasterize (mandatory): L<SVG::Rasterize|SVG::Rasterize>
object

=item * parent (optional): the parent state object, always
expected except for the root

=item * node_name (mandatory): name of the current node

=item * node_attributes (mandatory): HASH reference

=item * node (optional): must offer a C<getChildren> method if
provided; unused except for children, but available for hooks

=item * matrix (optional): must be an ARRAY reference if provided

=back

=head2 Public Attributes

=head3 rasterize

Can only be set at construction time. Stores a weakened reference to
the L<SVG::Rasterize|SVG::Rasterize> object.

=head3 node

Can only be set at construction time. If the C<SVG> data to
rasterize are provided as an L<SVG|SVG> object (or, in fact, some
C<DOM> object in general) this attribute stores the node object for
which this state object was created. All processing (except
children, see L<nextChild|/nextChild>) uses the
L<node_name|/node_name> and L<node_attributes|/node_attributes>
attributes which are always present. It is also recommended that you
use these instead of C<node> wherever possible. For example,
C<< $node->getAttributes >> might be undefined or not normalized
(see L<White Space Handling|SVG::Rasterize/White Space Handling> in
C<SVG::Rasterize>).

=head3 node_name

Can only be set at construction time. Stores the name of the current
node even if L<node|/node> above is C<undef>. If it differs from
C<< $node->getNodeName >> (usage not recommended), C<node_name> is
used.

=head3 node_attributes

Can only be set at construction time (any arguments to the accessor
are silently ignored). Stores the attributes of the current node as
a HASH reference even if L<node|/node> above is C<undef>. The
accessor does not create a copy of the hash, so changes will affect
the hash stored in the object. This is on purpose to give you full
control e.g. inside a L<hook|SVG::Rasterize/Hooks>. In case the node
has no attributes an empty HASH reference is returned. If the
content differs from C<< $node->getAttributes >> (usage not
recommended), C<node_attributes> is used.

=head3 matrix

Readonly attribute (you can change the contents, of course, but this
is considered a hack bypassing the interface). Stores an ARRAY
reference with 6 numbers C<[a, b, c, d, e, f]> such that the matrix

  ( a  c  e )
  ( b  d  f )
  ( 0  0  1 )

represents the map from coordinates in the current user coordinate
system to the output pixel grid. See
L<multiply_matrices|SVG::Rasterize/multiply_matrices> in
C<SVG::Rasterize> for more background.

Before you use the matrix directly have a look at
L<transform|/transform> below.

=head2 Methods for Users

The distinction between users and developers is a bit arbitrary
because these methods are only interesting for users who write
hooks which makes them almost a developer.

=head3 map_length

  $state->map_length($length)

This method takes a length and returns the corresponding value in
C<px> according to the conversion rates described in the L<ADVANCED
TOPICS|SVG::Rasterize/Units> section of
C<SVG::Rasterize>. Surrounding white space is not allowed.

B<Examples:>

  $x = $rasterize->map_length('5.08cm');  # returns 180
  $x = $rasterize->map_length(10);        # returns 10
  $x = $rasterize->map_length('  1in ');  # error
  $x = $rasterize->map_length('50%')      # depends on context

Note that there is no C<< $state->map_length($number, $unit) >>
interface like in
L<map_abs_length|SVG::Rasterize/map_abs_length> in
C<SVG::Rasterize>. It can be added on request.

Currently, relative units are not supported.

=head3 transform

  ($x_abs, $y_abs) = $state->transform($x, $y)

Takes an C<x> and a C<y> coordinate and maps them from the current
user space to the output pixel coordinate system using the current
value of L<matrix|/matrix>. C<$x> and C<$y> can be numbers or
lengths (see L<Lengths versus Numbers|SVG::Rasterize/Lengths versus
Numbers> in C<SVG::Rasterize>).

=head2 Methods for Subclass Developers

=head3 init

See new for a description of the interface. If you overload C<init>,
your method should also call this one.

=head3 nextChild

  $node = $state->nextChild

This method only works when traversing through a C<DOM> tree.
When the state object is instantiated it saves references to all
children in a list. This method shifts from this list and returns
the result. If the list is exhausted (or has never been filled
because there was no node object) then it returns undef.

=head3 multiply_matrices

Alias to L<multiply_matrices|SVG::Rasterize/multiply_matrices> in
C<SVG::Rasterize>. The alias is established via the typeglob:

  *multiply_matrices = \&SVG::Rasterize::multiply_matrices;

=head1 DIAGNOSTICS

=head2 Exceptions

Not documented, yet. Sorry.

=head2 Warnings

Not documented, yet. Sorry.


=head1 BUGS AND LIMITATIONS

No bugs have been reported. Please report any bugs or feature
requests to C<bug-svg-rasterize at rt.cpan.org>, or through the web
interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=SVG-Rasterize>. I
will be notified, and then you will automatically be notified of
progress on your bug as I make changes.

=head1 INTERNALS

=head2 Private Attributes

=over 4

=item * _parent

Stores a weakened reference to the parent state object. This
attribute should maybe become public readonly for use in
hooks.

=back

=head2 Internal Methods

These methods are just documented for myself. You can read on to
satisfy your voyeuristic desires, but be aware of that they might
change or vanish without notice in a future version.

=over 4

=item * _process_node

Called after creation of the state object. Checks for relevant
attributes and processes them.

Does not take any arguments and does not return anything.

=item * _process_transform_attribute

Parses the string given in a C<transform> attribute and sets the
L<matrix|/matrix> attribute accordingly.

Does not take any parameters and does not return anything. Expects
that C<< $self->{matrix} >> is set properly (which it is in
L<_process_node|/_process_node>).

=item * _process_viewBox_attribute

Parses the C<viewBox> and C<preserveAspectRatio> attributes (if
present) of the current node and modifies the current transformation
matrix accordingly.

Does not take any parameters and does not return anything. Expects
that C<< $self->{matrix} >> is set properly (which it is in
L<_process_node|/_process_node>).

=item * _process_style_properties

Creates a hash with current style properties which are taken from
(in order of decreasing preference) the C<style> attribute or the
respective property attribute or (if inheritable) from the parent
state or from the hash of default values in
L<SVG::Rasterize::Properties|SVG::Rasterize::Properties>.

Does not take any arguments. Returns the properties HASH reference.

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
