package SVG::Rasterize;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Carp;
use Params::Validate qw(validate validate_pos :types);

use SVG::Rasterize::Regexes qw(:whitespace
                               %RE_PACKAGE
                               %RE_NUMBER
                               %RE_LENGTH
                               %RE_PATH);
use SVG::Rasterize::State;

# $Id: Rasterize.pm 5720 2010-05-23 09:37:42Z mullet $

=head1 NAME

C<SVG::Rasterize> - rasterize SVG content to pixel graphics

=head1 INHERITANCE

  SVG::Rasterize is a
    L<Class::Accessor|Class::Accessor>

=head1 VERSION

Version 0.001005

=cut

our $VERSION = '0.001005';


__PACKAGE__->mk_accessors(qw(normalize_attributes
                             svg
                             width
                             height
                             engine_class));

__PACKAGE__->mk_ro_accessors(qw(engine
                                width
                                height));

###########################################################################
#                                                                         #
#                      Class Variables and Methods                        # 
#                                                                         #
###########################################################################

use constant TWO_PI => 6.28318530717959;

our $PX_PER_IN = 90;
our $DPI;  *DPI = \$PX_PER_IN;
our $IN_PER_CM = 1 / 2.54;
our $IN_PER_MM = 1 / 25.4;
our $IN_PER_PT = 1 / 72;
our $IN_PER_PC = 1 / 6;

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

sub _angle {
    shift(@_) if(@_ % 2);
    my ($x1, $y1, $x2, $y2) = @_;
    my $l_prod     = sqrt(($x1**2 + $y1**2) * ($x2**2 + $y2**2));

    return undef if($l_prod == 0);

    my $scalar     = $x1 * $x2 + $y1 * $y2;
    my $cos        = $scalar / $l_prod;
    my $sign       = $x1 * $y2 - $y1 * $x2 > 0 ? 1 : -1;
    my $sin        = $sign * sqrt($l_prod**2 - $scalar**2) / $l_prod;

    $cos = 1  if($cos > 1);
    $cos = -1 if($cos < -1);
    $sin = 1  if($sin > 1);
    $sin = -1 if($sin < -1);

    return atan2($sin, $cos);
}

sub adjust_arc_radii {
    shift(@_) if(@_ % 2 == 0);
    my ($x1, $y1, $rx, $ry, $phi, $x2, $y2) = @_;

    $rx = abs($rx);
    $ry = abs($ry);
    return($rx, $ry) if($rx == 0 or $ry == 0);
    
    my $sin_phi = sin($phi);
    my $cos_phi = cos($phi);
    my $x1_h    = ($x1 - $x2) / 2;
    my $y1_h    = ($y1 - $y2) / 2;
    my $x1_p    =  $cos_phi * $x1_h + $sin_phi * $y1_h;
    my $y1_p    = -$sin_phi * $x1_h + $cos_phi * $y1_h;
    my $lambda  = ($x1_p / $rx) ** 2 + ($y1_p / $ry) ** 2;

    return($rx, $ry, $sin_phi, $cos_phi, $x1_p, $y1_p) if($lambda <= 0);

    my $radicand = 1 / $lambda - 1;
    if($radicand < 0) {
	my $sqrt_lambda = sqrt($lambda);
	$rx       *= $sqrt_lambda;
	$ry       *= $sqrt_lambda;
	$radicand  = 0;
    }

    return($rx, $ry, $sin_phi, $cos_phi, $x1_p, $y1_p, $radicand);
}

sub endpoint_to_center {
    shift(@_) if(@_ % 2 == 0);
    my ($x1, $y1, $rx, $ry, $phi, $fa, $fs, $x2, $y2) = @_;
    my ($sin_phi, $cos_phi, $x1_p, $y1_p, $radicand);

    ($rx, $ry, $sin_phi, $cos_phi, $x1_p, $y1_p, $radicand) =
	adjust_arc_radii($x1, $y1, $rx, $ry, $phi, $x2, $y2);

    return if(!defined($radicand));

    my ($cx_p, $cy_p);
    my $factor;
    if($radicand > 0) {
	$factor = ($fa == $fs ? -1 : 1) * sqrt($radicand);
	$cx_p   =      $factor * $rx / $ry * $y1_p;
	$cy_p   = -1 * $factor * $ry / $rx * $x1_p;
    }
    else {
	$cx_p = 0;
	$cy_p = 0;
    }

    my $x1_c = ($x1 + $x2) / 2;
    my $y1_c = ($y1 + $y2) / 2;
    my $cx   = $cos_phi * $cx_p - $sin_phi * $cy_p + $x1_c;
    my $cy   = $sin_phi * $cx_p + $cos_phi * $cy_p + $y1_c;

    # 0 <= $th1 < 2PI
    my $th1  = _angle(1, 0, ($x1_p - $cx_p) / $rx, ($y1_p - $cy_p) / $ry);
    $th1 += TWO_PI while($th1 < 0);

    # sign of $dth
    my $dth  = _angle(($x1_p - $cx_p) / $rx, ($y1_p - $cy_p) / $ry,
		      (-$x1_p - $cx_p) / $rx, (-$y1_p - $cy_p) / $ry);
    if($fs == 0) { $dth -= TWO_PI while($dth > 0) }
    else         { $dth += TWO_PI while($dth < 0) }
    
    return($cx, $cy, $rx, $ry, $th1, $dth);
}

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
    my ($self, %args) = @_;

    foreach(keys %args) {
	my $meth = $_;
	if($self->can($meth)) { $self->$meth($args{$meth}) }
	else { carp "Unrecognized init parameter $meth.\n" }
    }

    return $self;
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

    $args_ptr->{width}  = $width[0];
    $args_ptr->{height} = $height[0];
    $args_ptr->{matrix} = $matrix;
}

sub _create_engine {
    my ($self, $args_ptr) = @_;
    my $default           = 'SVG::Rasterize::Cairo';
    my %engine_args       = (width  => $args_ptr->{width},
			     height => $args_ptr->{height});

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

    if(@args) {
	validate_pos(@args, { regex    => $RE_NUMBER{p_A_NUMBER},
			      optional => 1 });
	$self->{px_per_in} = $args[0];
    }

    return defined($self->{px_per_in}) ? $self->{px_per_in} : $PX_PER_IN;
}

*dpi = \&px_per_in;  &dpi if(0);

sub in_per_cm {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, { regex    => $RE_NUMBER{p_A_NUMBER},
			      optional => 1 });
	$self->{in_per_cm} = $args[0];
    }

    return defined($self->{in_per_cm}) ? $self->{in_per_cm} : $IN_PER_CM;
}

sub in_per_mm {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, { regex    => $RE_NUMBER{p_A_NUMBER},
			      optional => 1 });
	$self->{in_per_mm} = $args[0];
    }

    return defined($self->{in_per_mm}) ? $self->{in_per_mm} : $IN_PER_MM;
}

sub in_per_pt {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, { regex    => $RE_NUMBER{p_A_NUMBER},
			      optional => 1 });
	$self->{in_per_pt} = $args[0];
    }

    return defined($self->{in_per_pt}) ? $self->{in_per_pt} : $IN_PER_PT;
}

sub in_per_pc {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, { regex    => $RE_NUMBER{p_A_NUMBER},
			      optional => 1 });
	$self->{in_per_pc} = $args[0];
    }

    return defined($self->{in_per_pc}) ? $self->{in_per_pc} : $IN_PER_PC;
}

sub map_abs_length {
    my ($self, @args) = @_;

    # To me it is unclear if leading/trailing white space is allowed
    # in a length attribute. I allow it.
    my ($number, $unit);
    if(@args < 2) {
	validate_pos(@args, {regex => $RE_LENGTH{p_A_LENGTH}});
	($number, $unit) =
	    $args[0] =~ /^($RE_NUMBER{A_NUMBER})($RE_LENGTH{UNIT}?)$/;
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
	validate_pos(@args, {type => CODEREF|UNDEF});
	$self->{before_node_hook} = $args[0];
    }
    return $self->{before_node_hook} || sub {};
}

sub start_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, {type => CODEREF|UNDEF});
	$self->{start_node_hook} = $args[0];
    }
    return $self->{start_node_hook} || sub {};
}

sub end_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_pos(@args, {type => CODEREF|UNDEF});
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
    my $attributes     = $state->node_attributes;
    my $x1             = $state->map_length($attributes->{x1} || 0);
    my $y1             = $state->map_length($attributes->{y1} || 0);
    my $x2             = $state->map_length($attributes->{x2} || 0);
    my $y2             = $state->map_length($attributes->{y2} || 0);

    return $self->{engine}->draw_line($state, $x1, $y1, $x2, $y2);
}

sub _split_path_data {
    my ($self, $d)    = @_;
    my @sub_path_data = grep { /\S/ } split(qr/\s*([a-zA-Z])\s*/, $d);
    my @instructions  = ();
    my $parse_error   =
	"Failed to process the path data string %s correctly. Please ".
	"report this as a bug and include the string into the bug ".
	"report.\n";

    my $arg_sequence;
    while(@sub_path_data) {
	my $key = shift(@sub_path_data);

	if($key eq 'M' or $key eq 'm') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{MAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2]);
		    $arg_sequence = $3;

		    # subsequent moveto cmds are turned into lineto
		    $key = 'L' if($key eq 'M');
		    $key = 'l' if($key eq 'm');
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'Z' or $key eq 'z') {
	    push(@instructions, [$key]);
	    next;
	}
	if($key eq 'L' or $key eq 'l') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{LAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2]);
		    $arg_sequence = $3;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'H' or $key eq 'h') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{HLAS_SPLIT}) {
		    push(@instructions, [$key, $1]);
		    $arg_sequence = $2;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'V' or $key eq 'v') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{VLAS_SPLIT}) {
		    push(@instructions, [$key, $1]);
		    $arg_sequence = $2;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'C' or $key eq 'c') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{CAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4, $5, $6]);
		    $arg_sequence = $7;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'S' or $key eq 's') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{SCAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4]);
		    $arg_sequence = $5;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'Q' or $key eq 'q') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{QBAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4]);
		    $arg_sequence = $5;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'T' or $key eq 't') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{SQBAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2]);
		    $arg_sequence = $3;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}
	if($key eq 'A' or $key eq 'a') {
	    $self->_in_error(sprintf($parse_error, $d))
		if(!@sub_path_data);
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{EAAS_SPLIT}) {
		    if($1 == 0 or $2 == 0) {
			# 0 radius: turn into lineto
			push(@instructions,
			     [($key eq 'A' ? 'L' : 'l'), $6, $7]);
		    }
		    else {
			push(@instructions,
			     [$key, $1, $2, $3, $4, $5, $6, $7]);
		    }
		    $arg_sequence = $8;
		}
		else {
		    $self->_in_error(sprintf($parse_error, $d));
		}
	    }
	    next;
	}

	# If we arrive here we are in trouble.
	$self->_in_error(sprintf($parse_error, $d));
    }

    return @instructions;
}

sub _draw_path {
    my ($self, $state) = @_;
    my $path_data      = $state->node_attributes->{d} || '';

    return if(!$path_data);

    return $self->{engine}->draw_path
	($state, $self->_split_path_data($path_data));
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
    my ($self, $normalize, @args) = @_;

    $args[0] ||= {};

    if($normalize) {
	foreach(keys %{$args[0]}) {
	    $args[0]->{$_} =~ s/^$WSP*//;
	    $args[0]->{$_} =~ s/$WSP*$//;
	    $args[0]->{$_} =~ s/$WSP+/ /g;
	}
    }

    return $args[0];
}

sub rasterize {
    my ($self, %args) = @_;

    # validate args and process object defaults
    $args{normalize_attributes} = $self->{normalize_attributes}
        if(!exists($args{normalize_attributes})
	   and exists($self->{normalize_attributes}));
    $args{svg}                  = $self->{svg}
        if(!exists($args{svg}) and exists($self->{svg}));
    $args{width}                = $self->{width}
        if(!exists($args{width}) and exists($self->{width}));
    $args{height}               = $self->{height}
        if(!exists($args{height}) and exists($self->{height}));
    $args{engine_class}         = $self->{engine_class}
        if(!exists($args{engine_class}) and exists($self->{engine_class}));

    my @args = %args;
    %args = validate
	(@args,
	 {normalize_attributes => {default  => 1,
				   type     => BOOLEAN},
	  svg                  => {can      => ['getNodeName',
						'getAttributes']},
	  width                => {optional => 1,
				   type     => SCALAR,
				   regex    => $RE_LENGTH{p_A_LENGTH}},
	  height               => {optional => 1,
				   type     => SCALAR,
				   regex    => $RE_LENGTH{p_A_LENGTH}},
	  engine_class         => {default  => 'SVG::Rasterize::Cairo',
				   type     => SCALAR,
				   regex    =>
				       $RE_PACKAGE{p_PACKAGE_NAME}}});

    # process initial node and establish initial viewport
    my $node            = $args{svg}->getNodeName eq 'document'
	? $args{svg}->firstChild : $args{svg};
    my $node_name       = $node->getNodeName;
    my $node_attributes = $self->_process_normalize_attributes
	($args{normalize_attributes}, scalar($node->getAttributes));

    $self->_initial_viewport($node_attributes, \%args);
    $self->_create_engine(\%args);

    $self->before_node_hook->($self,
			      $node,
			      $node_name,
			      $node_attributes);
    my $state           = SVG::Rasterize::State->new
	(rasterize       => $self,
	 node            => $node,
	 node_name       => $node_name,
	 node_attributes => $node_attributes,
	 matrix          => $args{matrix});
    $self->start_node_hook->($self, $state);

    my @stack = ();
    while($state) {
	if($state->hasChildren) {
	    $node = $state->nextChild;
	    if($node) {
		push(@stack, $state);
		$node_name       = $node->getNodeName;
		$node_attributes = $self->_process_normalize_attributes
		    ($args{normalize_attributes},
		     scalar($node->getAttributes));
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
	    $self->_draw_path($state) if($state->node_name eq 'path');

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
L<rsvg|http://library.gnome.org/devel/rsvg/stable/>. While
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

The support of transform attributes and the establishment of the
initial viewport are fully implemented. The following elements are
drawn at the moment:

=over 4

=item * line

=item * path.

=back

The following attributes are at least partly interpreted:

=over

=item * transform

=item * viewBox

=item * preserveAspectRatio

=item * style

=item * fill and all associated properties

=item * stroke and all associated properties

=back

I hope that the interface described here will be largely stable.
However, this is not guaranteed. Some features are documented as
likely to change, but everything is subject to change at this early
stage.

Here is my current view of the next part of the roadmap:

=over 4

=item Version 0.002

=over 4

=item paths

=item basic shapes

=item solid filling

=item opacity

=back

=item Version 0.003

=over 4

=item error processing

=item parameter validation

=back

=item Version 0.004

=over 4

=item text basics

=back

=item Version 0.005

=over 4

=item gradients and patterns

=item clipping paths

=item masks

=back

=back


=head1 INTERFACE

=head2 Constructors

=head3 new

  $rasterize = SVG::Rasterize->new(%args)

Creates a new C<SVG::Rasterize> object and calls
L<init(%args)|/init>. If you subclass C<SVG::Rasterize> overload
L<init|/init>, not C<new>.

L<init|/init> goes through the arguments given to new. If a method
of the same name exists it is called with the respective value as
argument. This can be used for attribute initialization. Some of the
values can be also given to L<rasterize|/rasterize> to temporarily
override the attribute values. The values of these overridable
attributes are only validated once they are used by
L<rasterize|/rasterize>.

Supported arguments include:

=over 4

=item * svg (optional): A C<DOM> object to render.

=item * width (optional): width (in pixels) of the generated output
image

=item * height (optional): height (in pixels) of the generated
output image

=back

Any additional arguments are silently ignored. Note that the
attribute values are only validated once their are used by
rasterize.

=head2 Public Attributes

=head3 svg

Holds the C<DOM> object to render. It does not have to be a
L<SVG|SVG> object, but it has to offer a C<getNodeName>, a
C<getAttributes>, and a C<getChildren> method analogous to those
defined by the <SVG|SVG> class. If a different object is given to
L<rasterize|/rasterize> then the latter one overrules this value
temporarily (i.e. without overwriting it).

=head3 width

The width of the generated output in pixels.

=head3 height

The height of the generated output in pixels.

There are other attributes that influence unit conversions,
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

=item * svg (optional): C<DOM> object to rasterize. If not specified
the value of the L<svg|/svg> attribute is used. One of them has to
be set and has to provide the C<getNodeName>, C<getAttributes>, and
C<getChildren> C<DOM> methods.

=item * width (optional): width of the target image in pixels,
temporarily overrides the L<width|/width> attribute.

=item * height (optional): height of the target image in pixels,
temporarily overrides the L<height|/height> attribute.

=item * engine_class (optional): alternative engine class to
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>, temporarily
overrides the L<engine_class|/engine_class> attribute. See
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo> for details on the
interface. The value has to match the regular expression saved in
L<PACKAGE_NAME|/PACKAGE_NAME>.

=item * normalize_attributes (optional): Influences L<White Space
Handling|/White Space Handling>, temporarily overrides the
L<normalize_attributes|/normalize_attributes> attribute. Defaults to
1.

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

  $rasterize->init(%args)

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

  $product = multiply_matrices($m, $n);
  $product = SVG::Rasterize->multiply_matrices($m, $n);
  $product = $rasterize->multiply_matrices($m, $n);

Note that C<multiply_matrices> does not perform any input check. It
expects that you provide (at least) two ARRAY references with (at
least) 6 numbers each. If you pass more parameters then the last two
are used. If they contain more than 6 entries then the first 6 are
used.

=head3 endpoint_to_center

  @result = endpoint_to_center(@input)
  @result = SVG::Rasterize->endpoint_to_center(@input)
  @result = $rasterize->endpoint_to_center(@input)

Rasterization engines like
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo> might use center
parameterization instead of endpoint parametrization of an
elliptical arc (see
L<http://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes>).
This method calculates the center parameters from the endpoint
parameters given in a C<SVG> path data string. As indicated above,
it can be called as a subroutine or a class method or an object
method. The required parameters are:

=over 4

=item * x coordinate of the starting point

=item * y coordinate of the starting point

=item * radius in x direction

=item * radius in y direction

=item * angle by which the ellipse is rotated with respect to the
positive x axis (in radiant, not degrees)

=item * large arc flag

=item * sweep flag

=item * x coordinate of the end point

=item * y coordinate of the end point.

=back

If the reparameterization cannot be computed an empty list is
returned. This can have to possible reasons. Either one of the radii
is equal (with respect to machine precision) to 0 or the start and
end point of the arc are equal (with respect to machine
precision). The first case should have been checked before (note
that no rounding problems can occur here because no arithmetics is
done with the passed values) because in this case the arc should be
turned into a line. In the second case, the arc should just not be
drawn.

Note that the input values are not validated (e.g. if the values are
numbers, if the flags are either 0 or 1 and so on). It is assumed
that this has been checked before.  Furthermore, it is not checked
if the radii are very close to 0 or start and end point are nearly
equal.

A list of the following parameters is returned (unless an empty
list is returned due to the reasons mentioned above):

=over 4

=item * x coordinate of the center

=item * y coordinate of the center

=item * radius in x direction

This value might have been increased to make the ellipse big enough
to connect start and end point. If it was negative the absolute
value has been used (so the return value is always positive).

=item * radius in y direction

This value might have been increased to make the ellipse big enough
to connect start and end point. If it was negative the absolute
value has been used (so the return value is always positive).

=item * start angle in radiant

=item * sweep angle (positive or negative) in radiant.

=back


=head3 adjust_arc_radii

  @result = adjust_arc_radii(@input)
  @result = SVG::Rasterize->adjust_arc_radii(@input)
  @result = $rasterize->adjust_arc_radii(@input)

The C<SVG> specification requires that the radii of an elliptic arc
are increased automatically if the given values are too small to
connect the given endpoints (see
L<http://www.w3.org/TR/SVG11/implnote.html#ArcImplementationNotes>).
This situation can arise from rounding errors, but also for example
during an animation. Moreover, if given radii are negative then the
absolute values are to be taken. This method takes care of these
adjustments and returns the new values plus some intermediate values
that might be useful for callers, namely
L<endpoint_to_center|/endpoint_to_center>.

In detail, it requires the following parameters:

=over 4

=item * x coordinate of the starting point

=item * y coordinate of the starting point

=item * radius in x direction

=item * radius in y direction

=item * angle phi by which the ellipse is rotated with respect to the
positive x axis (in radiant)

=item * x coordinate of the end point

=item * y coordinate of the end point.

=back

Note that the input values are not validated (e.g. if the values are
numbers etc.). It is assumed that this has been checked before.
Furthermore, it is not checked if the radii are very close to 0 or
start and end point are nearly equal.

The following values are guaranteed to be returned:

=over 4

=item * adjusted absolute value of the radius in x direction

=item * adjusted absolute value of the radius in y direction

=back

This is all if one of the radii is equal to 0. Otherwise, the
following additional values are returned:

=over 4

=item * sin(phi)

=item * cos(phi)

=item * x_1' (see C<SVG> specification link above)

=item * y_1' (see C<SVG> specification link above)

=item * 1 / Lambda - 1

This value is only returned if Lambda is greater than 0 which is
equivalent (assuming exact arithmetics) to the end point of the arc
being different to the starting point. Lambda is the value
calculated in equation (F.6.6.2) of the specification (see link
above). 1 / Lambda - 1 is equal to the radicand in equation
(F.6.5.2).

=back

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

  $number = $rasterize->map_abs_length($length)
  $number = $rasterize->map_abs_length($number, $unit)

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

=head3 Invalid and numerically unstable values

There are situations where certain values cannot be dealt with,
e.g. denominators of 0 or negative radicands. Examples are skews of
90 degrees or elliptical arcs where one radius is 0. In these
situations, C<SVG::Rasterize> checks for these cases and acts
accordingly. Great care is taken to check directly those values
which are used as denominator, radicand etc. and not some
mathematically equivalent expression which might evaluate to a
slightly different value due to rounding errors. However, it is
not checked if such an expression is very close to a critical
value which might render the processing numerically unstable. I
do not want to impose a certain notion of "too close" on C<SVG>
authors. Instead it is left to them to check for these border
cases. However, the underlying rasterization engine might still
impose boundaries.

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

Some regular expressions are used at different locations of the code
to validate or extract user input. They are listed in the INTERNALS
section because it is not part of the interface where exactly they
are used. They are documented for inspection only. They are compiled
into other expressions so changing them will probably not achieve
what you might expect. The exception to this rule is the
C<PACKAGE_NAME> variable. See
L<SVG::Rasterize::Regexes|SVG::Rasterize::Regexes> for a full list.

=over 4

=item * $package_part (internal)

  qr/[a-zA-Z][a-zA-Z0-9\_]*/

=item * $SVG::Rasterize::Regexes::RE_PACKAGE{p_PACKAGE_NAME}

  qr/^$package_part(?:\:\:$package_part)*$/

Package names given to methods in this class, namely the
C<engine_class> parameters have to match this regular expression. I
am not sure which package names exactly are allowed. If you know
where in the Perl manpages or the Camel book this is described,
please point me to it. If this pattern is too strict for your
favourite package name, you can change this variable.

=back

=head2 Internal Methods

=over 4

=item * _in_error

=item * _create_engine

=item * _process_initial_viewport_length

=item * _initial_viewport

=item * _process_normalize_attributes

=item * _angle

=item * _draw_line

=item * _split_path_data

=item * _draw_path

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
