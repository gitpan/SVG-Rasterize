package SVG::Rasterize;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Params::Validate qw(:all);

use SVG::Rasterize::Regexes qw(:whitespace
                               %RE_PACKAGE
                               %RE_NUMBER
                               %RE_LENGTH
                               %RE_PATH
                               %RE_POLY);
use SVG::Rasterize::Exception qw(:all);
use SVG::Rasterize::State;

# $Id: Rasterize.pm 5899 2010-06-02 08:40:19Z mullet $

=head1 NAME

C<SVG::Rasterize> - rasterize SVG content to pixel graphics

=head1 VERSION

Version 0.003000

=cut

our $VERSION = '0.003000';


__PACKAGE__->mk_accessors(qw(normalize_attributes
                             svg
                             width
                             height
                             engine_class
                             engine_args));

__PACKAGE__->mk_ro_accessors(qw(engine
                                width
                                height
                                state));

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
    my $l_prod              = sqrt(($x1**2 + $y1**2) * ($x2**2 + $y2**2));

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
	else { warn "Unrecognized init parameter $meth.\n" }
    }

    $self->{in_error_hook} ||= sub {
	my ($self, $state) = @_;
	my $engine         = $self->engine or return;
	my $width          = $engine->width;
	my $height         = $engine->height;
	my $min            = $width < $height ? $width : $height;
	my $edge           = $min / 8;
	my $properties     = $state->properties;

	$properties->{'fill'}         = [45, 45, 45];
	$properties->{'fill-opacity'} = 0.6;
	
	my $x = 0;
	while($x < $width) {
	    my $y = 0;
	    while($y < $height) {
		$engine->draw_path($state,
				   ['M', $x, $y],
				   ['h', $edge],
				   ['v', $edge],
				   ['h', -$edge],
				   ['z']);
		$y += 2 * $edge;
	    }
	    $x += 2 * $edge;
	}

	return;
    };

    return $self;
}

sub _initial_viewport {
    my ($self, $node_attributes, $args_ptr) = @_;
    my $matrix                              = [1, 0, 0, 1, 0, 0];
    my @width                               = ();
    my @height                              = ();

    # NB: $node_attributes are not validated and can have any value
    # at this point. However, the values of $args_ptr have been
    # validated.

    # collecting information
    # width
    $width[0] = defined($args_ptr->{width})
	? int($self->map_abs_length($args_ptr->{width}) + 0.5) : undef;
    $self->ex_su_iw($width[0]) if(($width[0] || 0) < 0);

    $width[1] = defined($node_attributes->{width})
	? $node_attributes->{width} : undef;
    if($width[1]) {
	$self->ex_su_iw($width[1])
	    if($width[1] !~ $RE_LENGTH{p_A_LENGTH});
	$width[1] = undef if($width[1] eq '100%');
    }
    if($width[1]) {
	if($width[1] !~ $RE_LENGTH{p_ABS_A_LENGTH}) {
	    $self->ex_us_si("Relative length (different from 100%) for ".
			    "width of root element ($width[1])");
	}
	$width[1] = int($self->map_abs_length($width[1]) + 0.5);
	$self->ex_su_iw($width[1]) if($width[1] < 0);
    }
    
    # height
    $height[0] = defined($args_ptr->{height})
	? int($self->map_abs_length($args_ptr->{height}) + 0.5) : undef;
    $self->ex_su_iw($height[0]) if(($height[0] || 0) < 0);

    $height[1] = defined($node_attributes->{height})
	? $node_attributes->{height} : undef;
    if($height[1]) {
	$self->ex_su_iw($height[1])
	    if($height[1] !~ $RE_LENGTH{p_A_LENGTH});
	$height[1] = undef if($height[1] eq '100%');
    }
    if($height[1]) {
	if($height[1] !~ $RE_LENGTH{p_ABS_A_LENGTH}) {
	    $self->ex_us_si("Relative length (different from 100%) for ".
			    "height of root element ($height[1])");
	}
	$height[1] = int($self->map_abs_length($height[1]) + 0.5);
	$self->ex_su_iw($height[1]) if($height[1] < 0);
    }
    
    # width mapping
    if($width[0]) {
	if($width[1]) { $matrix->[0] = $width[0] / $width[1]  }
	else          { $node_attributes->{width} = $width[0] }
    }
    elsif($width[1]) { $width[0] = $width[1] }
    else             { $width[0] = 0         }

    # same for height
    if($height[0]) {
	if($height[1]) { $matrix->[3] = $height[0] / $height[1]  }
	else           { $node_attributes->{height} = $height[0] }
    }
    elsif($height[1]) { $height[0] = $height[1] }
    else              { $height[0] = 0          }

    $args_ptr->{width}  = $width[0];
    $args_ptr->{height} = $height[0];
    $args_ptr->{matrix} = $matrix;

    return;
}

sub _create_engine {
    my ($self, $args_ptr) = @_;

    # The values of $args_ptr have been validated, but
    # $args_ptr->{engine_args} is only validated to be a HASH
    # reference (if it exists at all).

    my $default           = 'SVG::Rasterize::Cairo';
    my %engine_args       = (width  => $args_ptr->{width},
			     height => $args_ptr->{height},
			     %{$args_ptr->{engine_args} || {}});

    $args_ptr->{engine_class} ||= $default;
    my $load_success = eval "require $args_ptr->{engine_class}";
    if(!$load_success and $args_ptr->{engine_class} ne $default) {
	warn("Unable to load $args_ptr->{engine_class}: $!. ".
	     "Falling back to $default.\n");
	$args_ptr->{engine_class} = $default;
	$load_success = eval "require $args_ptr->{engine_class}";
    }
    if(!$load_success) { ex_se_en_lo($args_ptr->{engine_class}, $!) }

    $self->{engine} = $args_ptr->{engine_class}->new(%engine_args);

    return $self->{engine};
}

###########################################################################
#                                                                         #
#                               Accessors                                 # 
#                                                                         #
###########################################################################

sub px_per_in {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_NUMBER{p_A_NUMBER}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{px_per_in} = $args[0];
    }

    return defined($self->{px_per_in}) ? $self->{px_per_in} : $PX_PER_IN;
}

*dpi = \&px_per_in;  &dpi if(0);

sub in_per_cm {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_NUMBER{p_A_NUMBER}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{in_per_cm} = $args[0];
    }

    return defined($self->{in_per_cm}) ? $self->{in_per_cm} : $IN_PER_CM;
}

sub in_per_mm {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_NUMBER{p_A_NUMBER}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{in_per_mm} = $args[0];
    }

    return defined($self->{in_per_mm}) ? $self->{in_per_mm} : $IN_PER_MM;
}

sub in_per_pt {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_NUMBER{p_A_NUMBER}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{in_per_pt} = $args[0];
    }

    return defined($self->{in_per_pt}) ? $self->{in_per_pt} : $IN_PER_PT;
}

sub in_per_pc {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_NUMBER{p_A_NUMBER}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{in_per_pc} = $args[0];
    }

    return defined($self->{in_per_pc}) ? $self->{in_per_pc} : $IN_PER_PC;
}

sub map_abs_length {
    my ($self, @args) = @_;

    my ($number, $unit);
    if(@args < 2) {
	validate_with(params  => \@args,
		      spec    => [{regex => $RE_LENGTH{p_A_LENGTH}}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	($number, $unit) =
	    $args[0] =~ /^($RE_NUMBER{A_NUMBER})($RE_LENGTH{UNIT}?)$/;
    }
    else { ($number, $unit) = @args }  # bypasses validation!

    my $dpi = $self->px_per_in;
    if(!$unit)           { return $number }
    elsif($unit eq 'em') { $self->ex_pm_rl($number.$unit) }
    elsif($unit eq 'ex') { $self->ex_pm_rl($number.$unit) }
    elsif($unit eq 'px') { return $number }
    elsif($unit eq 'pt') { return $number * $self->in_per_pt * $dpi }
    elsif($unit eq 'pc') { return $number * $self->in_per_pc * $dpi }
    elsif($unit eq 'cm') { return $number * $self->in_per_cm * $dpi }
    elsif($unit eq 'mm') { return $number * $self->in_per_mm * $dpi }
    elsif($unit eq 'in') { return $number * $dpi }
    elsif($unit eq '%')  { $self->ex_pm_rl($number.$unit) }
}

sub before_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{type => CODEREF|UNDEF}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{before_node_hook} = $args[0];
    }

    return $self->{before_node_hook} || sub {};
}

sub start_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{type => CODEREF|UNDEF}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{start_node_hook} = $args[0];
    }

    return $self->{start_node_hook} || sub {};
}

sub end_node_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{type => CODEREF|UNDEF}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{end_node_hook} = $args[0];
    }

    return $self->{end_node_hook} || sub {};
}

sub in_error_hook {
    my ($self, @args) = @_;

    if(@args) {
	validate_with(params  => \@args,
		      spec    => [{type => CODEREF|UNDEF}],
		      on_fail => sub { $self->ex_pv($_[0]) });
	$self->{in_error_hook} = $args[0];
    }

    return $self->{in_error_hook} || sub {};
}

###########################################################################
#                                                                         #
#                                Drawing                                  #
#                                                                         #
###########################################################################

################################## Paths ##################################

sub _split_path_data {
    my ($self, $d)    = @_;
    my @sub_path_data = grep { /\S/ } split(qr/$WSP*([a-zA-Z])$WSP*/, $d);
    my @instructions  = ();
    my $in_error      = 0;

    my $arg_sequence;
  INSTR_SEQ:
    while(@sub_path_data) {
	my $key = shift(@sub_path_data);

	if($key eq 'M' or $key eq 'm') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
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
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'Z' or $key eq 'z') {
	    push(@instructions, [$key]);
	    next;
	}
	if($key eq 'L' or $key eq 'l') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{LAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2]);
		    $arg_sequence = $3;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'H' or $key eq 'h') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{HLAS_SPLIT}) {
		    push(@instructions, [$key, $1]);
		    $arg_sequence = $2;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'V' or $key eq 'v') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{VLAS_SPLIT}) {
		    push(@instructions, [$key, $1]);
		    $arg_sequence = $2;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'C' or $key eq 'c') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{CAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4, $5, $6]);
		    $arg_sequence = $7;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'S' or $key eq 's') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{SCAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4]);
		    $arg_sequence = $5;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'Q' or $key eq 'q') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{QBAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2, $3, $4]);
		    $arg_sequence = $5;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'T' or $key eq 't') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
	    $arg_sequence = shift(@sub_path_data);

	    while($arg_sequence) {
		if($arg_sequence =~ $RE_PATH{SQBAS_SPLIT}) {
		    push(@instructions, [$key, $1, $2]);
		    $arg_sequence = $3;
		}
		else {
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}
	if($key eq 'A' or $key eq 'a') {
	    if(!@sub_path_data) {
		$in_error = 1;
		last INSTR_SEQ;
	    }
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
		    $in_error = 1;
		    last INSTR_SEQ;
		}
	    }
	    next;
	}

	# If we arrive here we are in trouble.
	$in_error = 1;
	last INSTR_SEQ;
    }

    return($in_error, @instructions);
}

sub _draw_path {
    my ($self)    = @_;
    my $state     = $self->{state};
    my $path_data = $state->node_attributes->{d};

    return if(!$path_data);

    my ($in_error, @instructions) = $self->_split_path_data($path_data);
    my $result = $self->{engine}->draw_path($state, @instructions);
    
    if($in_error) { $self->ie_at_pd($path_data) }
    else          { return $result              }
}

############################### Basic Shapes ##############################

sub _draw_rect {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $attributes = $state->node_attributes;
    my $x          = $state->map_length($attributes->{x} || 0);
    my $y          = $state->map_length($attributes->{y} || 0);
    my $w          = $attributes->{width};
    my $h          = $attributes->{height};
    my $rx         = $attributes->{rx};
    my $ry         = $attributes->{ry};

    $w = $state->map_length($w);
    $h = $state->map_length($h);
    $self->ie_at_re_nw($w) if($w < 0);
    $self->ie_at_re_nh($h) if($h < 0);
    return if(!$w or !$h);

    if(defined($rx)) {
	$rx = $state->map_length($rx);
	$self->ie_at_re_nr($rx) if($rx < 0);
	$ry = $rx if(!defined($ry));
    }
    if(defined($ry)) {
	$ry = $state->map_length($ry);
	$self->ie_at_re_nr($ry) if($ry < 0);
	$rx = $ry if(!defined($rx));
    }

    $rx = $ry = 0 if(!$rx or !$ry);
    $rx = $w / 2 if($rx > $w / 2);
    $ry = $h / 2 if($ry > $h / 2);

    my $engine = $self->{engine};
    if($engine->can('draw_rect')) {
	return $engine->draw_rect($state, $x, $y, $w, $h, $rx, $ry);
    }
    else {
	if($rx) {
	    return $engine->draw_path
		($state,
		 ['M', $x, $y + $ry],
		 ['a', $rx, $ry, 0, 0, 1, $rx, -$ry],
		 ['h', $w - 2*$rx],
		 ['a', $rx, $ry, 0, 0, 1, $rx, $ry],
		 ['v', $h - 2*$ry],
		 ['a', $rx, $ry, 0, 0, 1, -$rx, $ry],
		 ['h', -($w - 2*$rx)],
		 ['a', $rx, $ry, 0, 0, 1, -$rx, -$ry],
		 ['Z']);
	}
	else {
	    return $engine->draw_path
		($state,
		 ['M', $x, $y],
		 ['h', $w],
		 ['v', $h],
		 ['h', -$w],
		 ['Z']);
	}
    }
}

sub _draw_circle {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $attributes = $state->node_attributes;
    my $cx         = $state->map_length($attributes->{cx} || 0);
    my $cy         = $state->map_length($attributes->{cy} || 0);
    my $r          = $attributes->{r};

    $r = $state->map_length($r);
    $self->ie_at_ci_nr($r) if($r < 0);
    return if(!$r);

    my $engine = $self->{engine};
    if($engine->can('draw_circle')) {
	return $engine->draw_circle($state, $cx, $cy, $r);
    }
    else {
	return $engine->draw_path
	    ($state,
	     ['M', $cx + $r, $cy],
	     ['A', $r, $r, 0, 1, 1, $cx, $cy - $r],
	     ['A', $r, $r, 0, 0, 1, $cx + $r, $cy],
	     ['Z']);
    }
}

sub _draw_ellipse {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $attributes = $state->node_attributes;
    my $cx         = $state->map_length($attributes->{cx} || 0);
    my $cy         = $state->map_length($attributes->{cy} || 0);
    my $rx         = $attributes->{rx};
    my $ry         = $attributes->{ry};

    $rx = $state->map_length($rx);
    $ry = $state->map_length($ry);
    $self->ie_at_el_nr($rx) if($rx < 0);
    $self->ie_at_el_nr($ry) if($ry < 0);
    return if(!$rx or !$ry);

    my $engine = $self->{engine};
    if($engine->can('draw_ellipse')) {
	return $engine->draw_ellipse($state, $cx, $cy, $rx, $ry);
    }
    else {
	return $engine->draw_path
	    ($state,
	     ['M', $cx + $rx, $cy],
	     ['A', $rx, $ry, 0, 1, 1, $cx, $cy - $ry],
	     ['A', $rx, $ry, 0, 0, 1, $cx + $rx, $cy],
	     ['Z']);
    }
}

sub _draw_line {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $attributes = $state->node_attributes;
    my $x1         = $state->map_length($attributes->{x1} || 0);
    my $y1         = $state->map_length($attributes->{y1} || 0);
    my $x2         = $state->map_length($attributes->{x2} || 0);
    my $y2         = $state->map_length($attributes->{y2} || 0);

    my $engine = $self->{engine};
    if($engine->can('draw_line')) {
	return $engine->draw_line($state, $x1, $y1, $x2, $y2);
    }
    else {
	return $engine->draw_path
	    ($state, ['M', $x1, $y1], ['L', $x2, $y2]);
    }
}

sub _draw_polyline {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $points_str = $state->node_attributes->{points};

    return if(!$points_str);

    my @points;
    while($points_str) {
	if($points_str =~ $RE_POLY{POINTS_SPLIT}) {
	    push(@points, [$1, $2]);
	    $points_str = $3;
	}
	else { last }
    }

    my $engine = $self->{engine};
    my $result;
    if($engine->can('draw_polyline')) {
	$result = $engine->draw_polyline($state, @points);
    }
    else {
	$result = $engine->draw_path
	    ($state,
	     ['M', @{shift(@points)}],
	     map { ['L', @$_] } @points);
    }

    if($points_str) { $self->ie_at_po($state->node_attributes->{points}) }
    else            { return $result }
}

sub _draw_polygon {
    my ($self)     = @_;
    my $state      = $self->{state};
    my $points_str = $state->node_attributes->{points};

    return if(!$points_str);

    my @points;
    while($points_str) {
	if($points_str =~ $RE_POLY{POINTS_SPLIT}) {
	    push(@points, [$1, $2]);
	    $points_str = $3;
	}
	else { last }
    }

    my $engine = $self->{engine};
    my $result;
    if($engine->can('draw_polygon')) {
	$result = $engine->draw_polygon($state, @points);
    }
    else {
	$result = $engine->draw_path
	    ($state,
	     ['M', @{shift(@points)}],
	     (map { ['L', @$_] } @points),
	     ['Z']);
    }

    if($points_str) { $self->ie_at_po($state->node_attributes->{points}) }
    else            { return $result }
}

###########################################################################
#                                                                         #
#                             Tree Traversal                              #
#                                                                         #
###########################################################################

sub in_error {
    my ($self, $exception) = @_;
    my $state              = SVG::Rasterize::State->new
	(rasterize       => $self,
	 node_name       => 'g',
	 node_attributes => {});

    $self->in_error_hook->($self, $state);

    die $exception;
}

sub _process_normalize_attributes {
    my ($self, $normalize, $attr) = @_;

    my %attributes = %{$attr || {}};

    if($normalize) {
	foreach(keys %attributes) {
	    $attributes{$_} =~ s/^$WSP*//;
	    $attributes{$_} =~ s/$WSP*$//;
	    $attributes{$_} =~ s/$WSP+/ /g;
	}
    }

    return \%attributes;
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
    $args{engine_args}          = $self->{engine_args}
        if(!exists($args{engine_args}) and exists($self->{engine_args}));

    my @args = %args;
    %args = validate_with
	(params => \@args,
	 spec   =>
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
					   $RE_PACKAGE{p_PACKAGE_NAME}},
	      engine_args          => {optional => 1,
				       type     => HASHREF}},
	on_fail => sub { $self->ex_pv($_[0]) });

    # process initial node and establish initial viewport
    my $node            = $args{svg}->getNodeName eq 'document'
	? $args{svg}->firstChild : $args{svg};
    my $node_name       = $node->getNodeName;
    my $node_attributes = $self->_process_normalize_attributes
	($args{normalize_attributes}, scalar($node->getAttributes));

    $self->_initial_viewport($node_attributes, \%args);
    if(!$args{width}) {
	warn "Surface width is 0, nothing to do.\n";
	return;
    }
    if(!$args{height}) {
	warn "Surface height is 0, nothing to do.\n";
	return;
    }

    $self->_create_engine(\%args);

    $self->before_node_hook->($self,
			      $node,
			      $node_name,
			      $node_attributes);
    $self->{state} = SVG::Rasterize::State->new
	(rasterize       => $self,
	 node            => $node,
	 node_name       => $node_name,
	 node_attributes => $node_attributes,
	 matrix          => $args{matrix});
    $self->start_node_hook->($self, $self->{state});

    my @stack = ();
    while($self->{state}) {
	if($self->{state}->hasChildren) {
	    $node = $self->{state}->nextChild;
	    if($node) {
		push(@stack, $self->{state});
		$node_name       = $node->getNodeName;
		$node_attributes = $self->_process_normalize_attributes
		    ($args{normalize_attributes},
		     scalar($node->getAttributes));
		$self->before_node_hook->($self,
					  $node,
					  $node_name,
					  $node_attributes);
		$self->{state} = SVG::Rasterize::State->new
		    (rasterize       => $self,
		     parent          => $self->{state},
		     node            => $node,
		     node_name       => $node_name,
		     node_attributes => $node_attributes);
		$self->start_node_hook->($self, $self->{state});
	    }
	    else {
		$self->end_node_hook->($self, $self->{state});
		$self->{state} = pop @stack;
	    }
	}
	else {
	    # do something
	    my $this_node_name = $self->{state}->node_name;
	    $self->_draw_path     if($this_node_name eq 'path');
	    $self->_draw_rect     if($this_node_name eq 'rect');
	    $self->_draw_circle	  if($this_node_name eq 'circle');
	    $self->_draw_ellipse  if($this_node_name eq 'ellipse');
	    $self->_draw_line     if($this_node_name eq 'line');
	    $self->_draw_polyline if($this_node_name eq 'polyline');
	    $self->_draw_polygon  if($this_node_name eq 'polygon');

	    $self->end_node_hook->($self, $self->{state});
	    $self->{state} = pop @stack;
	}
    }

    return;
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

The following elements are drawn at the moment:

=over 4

=item * C<path>

=item * all basic shapes: C<rect>, C<circle>, C<ellipse>, C<line>,
C<polyline>, C<polygon>.

=back

The inheritance of styling properties is implemented. The following
attributes are at least partly interpreted:

=over

=item * C<transform>

=item * C<viewBox>

=item * C<preserveAspectRatio>

=item * C<style>

=item * C<fill> and all associated properties

=item * C<stroke> and all associated properties

=back

I hope that the interface described here will be largely stable.
However, this is not guaranteed. Some features are documented as
likely to change, but everything is subject to change at this early
stage.

Here is my current view of the next part of the roadmap:

=over 4

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

L<init|/init> goes through the arguments given to C<new>. If a
method of the same name exists it is called with the respective
value as argument. This can be used for attribute
initialization. Some of the values can be also given to
L<rasterize|/rasterize> to temporarily override the attribute
values. The values of these overridable attributes are only
validated once they are used by L<rasterize|/rasterize>.

The most commonly used arguments are:

=over 4

=item * svg (optional): A C<DOM> object to render.

=item * width (optional): width (in pixels) of the generated output
image

=item * height (optional): height (in pixels) of the generated
output image.

=back


=head2 Public Attributes

=head3 svg

Holds the C<DOM> object to render. It does not have to be a
L<SVG|SVG> object, but it has to offer a C<getNodeName>, a
C<getAttributes>, and a C<getChildren> method analogous to those
defined by the L<SVG|SVG> class. If a different object is given to
L<rasterize|/rasterize> then the latter one overrules this value
temporarily (i.e. without overwriting it).

=head3 width

The width of the generated output in pixels.

=head3 height

The height of the generated output in pixels.

=head3 state

Readonly attribute. Holds the current
L<SVG::Rasterize::State|SVG::Rasterize::State> object during tree
traversal.

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

The element can be any valid C<SVG> element, e.g. C<< <svg> >>, C<<
<g> >>, or even just a basic shape element or so.

=item * width (optional): width of the target image in pixels,
temporarily overrides the L<width|/width> attribute.

=item * height (optional): height of the target image in pixels,
temporarily overrides the L<height|/height> attribute.

=item * engine_class (optional): alternative engine class to
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>, temporarily
overrides the C<engine_class> attribute. See
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo> for details on the
interface. The value has to match the regular
L<p_PACKAGE_NAME|SVG::Rasterize::Regexes/%RE_PACKAGE>.

=item * engine_args (optional): arguments for the constructor of the
rasterization engine, temporarily overriding the C<engine_args>
attribute (NB: in the future, this behaviour might be changed such
that the two hashes are merged and only the values given here
override the values in the attribute C<engine_args>; however, at the
moment, the whole hash is temporarily replaced if the parameter
exists). The width and height of the output image can be set in
several ways. The following values for the width are used with
decreasing precedence (the same hierarchy applies to the height):

=over 4

=item 1. C<< engine_args->{width} >>, given to C<rasterize>

=item 2. C<< $rasterize->engine_args->{width} >>

=item 3. C<width>, given to C<rasterize>

=item 4. C<< $rasterize->width >>

=item 5. the width attribute of the root C<SVG> object.

=back

=item * normalize_attributes (optional): Influences L<White Space
Handling|/White Space Handling>, temporarily overrides the
C<normalize_attributes> attribute. Defaults to 1.

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

=head2 Methods for Developers

=head3 init

  $rasterize->init(%args)

If you overload C<init>, your method should also call this one.

For each given argument, C<init> calls the accessor with the same
name to initialize the attribute. If such an accessor (or in fact,
any method of that name) does not exist a warning is printed and the
argument is ignored. Readonly attributes that are allowed to be set
at initialization time are set separately at the beginning.

=head3 in_error

Expects an exception object or error message. Creates a fresh
L<SVG::Rasterize::State|SVG::Rasterize::State> object (without any
transfrom etc.) and calls L<in_error_hook|/in_error_hook> (which by
default draws a translucent checkerboard across the image). After
that, it dies with the given message.

Before you call C<in_error> directly, check out
L<SVG::Rasterize::Exception|SVG::Rasterize::Exception>.

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

  $product = multiply_matrices($m, $n)
  $product = SVG::Rasterize->multiply_matrices($m, $n)
  $product = $rasterize->multiply_matrices($m, $n)

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
drawn. Be aware that this latter case includes a full ellipse. This
means that a full ellipse cannot be drawn as one arc. The C<SVG>
specification is very clear on that point. However, an ellipse can
be drawn as two arcs.

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
during an animation. Moreover, if a given radius is negative then
the absolute value is to be used. This method takes care of these
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
Furthermore, it is not checked if the radii are very close to C<0>
or start and end point are nearly equal.

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

Inches per point. Defaults to 1/72. According to [1], this default
was introduced by the C<Postscript> language. There are other
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
  $x = $rasterize->map_abs_length('  1in ');  # error
  $x = $rasterize->map_abs_length('50%')      # error

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

The corresponding class attributes are listed below. Note that these
values are not validated. Take care that you only set them to
numbers.

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
C<normalize_attributes> attribute (as object attribute or as
parameter to L<rasterize|/rasterize> to a false value.

=head2 Hooks

The L<rasterize|/rasterize> method traverses through the C<SVG> tree
and creates an L<SVG::Rasterize::State|/SVG::Rasterize::State>
object for each node (at least each node of relevance). Hooks allow
you to execute your own subroutines at given steps of this
traversal. However, the whole hook business is experimental at the
moment and likely to change. Right now, to set your own hooks you
can set one of the following attributes to a code reference of your
choice.

Currently, there are for hooks:

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
L<matrix|SVG::Rasterize::State/matrix> have been set etc. The method
receives the C<SVG::Rasterize> object and the
L<SVG::Rasterize::State|SVG::Rasterize::State> object as parameters.

=item * end_node_hook

Executed right before the
L<SVG::Rasterize::State|SVG::Rasterize::State> runs out of scope
because the current node is done with. The method receives the
C<SVG::Rasterize> object and the
L<SVG::Rasterize::State|SVG::Rasterize::State> object as parameters.

=item * in_error_hook

Executed right before C<die> when the document is in error (see L<In
error|/In error> below. Receives the C<SVG::Rasterize> object and a
newly created L<SVG::Rasterize::State|SVG::Rasterize::State> object
as parameters.

=back

B<Examples:>

  $rasterize->start_node_hook(sub { ... })

=head2 Rasterization Backend

C<SVG::Rasterize> does not render pixel graphics itself. By default,
it uses the L<cairo|http://www.cairographics.org/> library through
its L<Perl bindings|Cairo>. The interface is documented in
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. However, this
interface could also be implemented by other backends. In the
future, it will be documented in
L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. Currently, it has to
be considered unstable, though, and the documentation is sparse.

=head3 engine_class

This attribute defaults to C<SVG::Rasterize::Cairo>. It can be set
as an object attribute or temporarily as a parameter to the
L<rasterize|/rasterize> method.

=head3 engine_args

This attribute can hold a HASH reference. The corresponding hash is
given to the constructor of the rasterization engine when it is
called by L<rasterize|/rasterize>. C<engine_args> can be set as an
object attribute or temporarily as a parameter to the
L<rasterize|/rasterize> method.

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

=head2 C<SVG> Validation

C<SVG::Rasterize> is not an C<SVG> validator. It does check a lot of
things including the validity of the element hierarchy, the required
presence and absence of attributes and the values of all attributes
in interpretes plus some that it does not interprete. However, it
does not (and probably will never) claim to detect all errors in an
C<SVG> document.


=head1 EXAMPLES

There are a few example scripts in the C<examples> directory of the
tar ball. However, they rather illustrate the currently supported
C<SVG> subset than options of C<SVG::Rasterize>.


=head1 DIAGNOSTICS

=head2 Error processing

The C<SVG> documentation specifies how C<SVG> interpreters
should react to certain incidents. The relevant section can be
found here:
L<http://www.w3.org/TR/SVG11/implnote.html#ErrorProcessing>.

This section describes how some of these instructions are
implemented by C<SVG::Rasterize> and how it reacts in some other
situations in which the specification does not give instructions.

=head3 In error

According to the C<SVG> specification (see
L<http://www.w3.org/TR/SVG11/implnote.html#ErrorProcessing>, a
document is "in error" if:

=over 4

=item * "the content does not conform to the C<XML 1.0>
specification, such as the use of incorrect C<XML> syntax"

C<SVG::Rasterize> currently does not parse C<SVG> files and will
therefore not detect such an error.

=item * "an element or attribute is encountered in the document
which is not part of the C<SVG DTD> and which is not properly
identified as being part of another namespace"

Currently, C<SVG::Rasterize> will also reject elements that B<are>
properly identified as being part of another namespace.

=item * "an element has an attribute or property value which is not
permissible according to this specification"

This is checked for those attributes and properties that are
currently supported by C<SVG::Rasterize>. Values that are currently
ignored may or may not be checked.

=item * "Other situations that are described as being I<in error> in
this specification"

=back

In these cases, the rendering is supposed to stop before the
incriminated element. Exceptions are C<path>, C<polyline>, and
C<polygon> elements which are supposed to be partially rendered up
to the point where the error occurs.

Furthermore, a "highly perceivable indication of error shall
occur. For visual rendering situations, an example of an indication
of error would be to render a translucent colored pattern such as a
checkerboard on top of the area where the SVG content is rendered."

In C<SVG::Rasterize> this is done by the
L<in_error_hook|/in_error_hook>. By default, it indeed draws a
translucent (C<rgb(45, 45, 45)> with opacity C<0.6>) checkerboard
with 8 fields along the width or height (whichever is shorter). This
behaviour can be changed by setting the
L<in_error_hook|/in_error_hook>. Setting the hook to C<undef> or
C<sub {}> will disable the process.

=head3 C<SVG::Rasterize> exceptions

When C<SVG::Rasterize> encounters a problem it usually throws an
exception. The cases where only a warning is issued a rare. This
behaviour has several reasons:

=over 4

=item * If the document is in error (see section above) the C<SVG>
specification requires that the rendering stops.

=item * In case of failed parameter validation,
L<Params::Validate|Params::Validate> expects the code execution to
stop. One could work around this in an onerous and fragile way, but
I will not do this.

=item * Often there is no good fallback without knowing what the
user inteded to do. In these cases, it is better to just bail out
and let the user fix the problem himself.

=item * A too forgiving user agent deludes the user into bad
behaviour. I think that if the rules are clear, a program should
enforce them rather strictly.

=back

The exceptions are thrown in form of objects. See
L<Exception::Class|Exception::Class> for a detailed description. See
L<below|/Exceptions> for a description of the classes used in this
distribution. All error messages are described in
L<SVG::Rasterize::Exception|SVG::Rasterize::Exception>.

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

=head2 Exceptions

C<SVG::Rasterize> currently uses the following exception
classes. This framework is experimental and might change
considerably in future versions. See
L<Exception::Class|Exception::Class> on how you can make use of this
framework. See
L<SVG::Rasterize::Exception|SVG::Rasterize::Exception> for a
detailed list of error messages.

=over 4

=item * C<SVG::Rasterize::Exception::Base>

Base class for the others. Defines the state attribute which holds
the current L<SVG::Rasterize::State> object at the time the
exception is thrown.

=item * C<SVG::Rasterize::Exception::InError>

The processing encountered an error in the C<SVG> content.

=item * C<SVG::Rasterize::Exception::Setting>

The exception was triggered by an error during the general
preparation of the processing, e.g. an error during initialization
of the rasterization backend.

=item * C<SVG::Rasterize::Exception::Parse>

An error occured during parsing (usually of an attribute value). An
exception of this class always indicates an inconsistency between
validation and parsing of this value and should be reported as a
bug.

=item * C<SVG::Rasterize::Exception::Unsupported>

The document (or user) tried to use a feature that is currently
unsupported.

=item * C<SVG::Rasterize::Exception::ParamsValidate>

A method parameter did not pass a
L<Params::Validate|Params::Validate> check.

=item * C<SVG::Rasterize::Exception::Param>

A method parameter passed the L<Params::Validate|Params::Validate>
check, but is still invalid (an example is that the
L<Params::Validate|Params::Validate> check only included that the
value must be a number, but it also has to be in a certain range
which is checked individually later).

=back

=head2 Warnings

=over 4

=item * "Unrecognized init parameter %s."

You have given a parameter to the L<new|/new> method which does not
have a corresponding method. The parameter is ignored in that case.

=item * "Unable to load %s: %s. Falling back to
SVG::Rasterize::Cairo."

The C<engine_class> you were trying to use for rasterization could
not be loaded. C<SVG::Rasterize> then tries to use its default
backend L<SVG::Rasterize::Cairo|SVG::Rasterize::Cairo>. If that also
fails, it gives up.

=item * "Surface width is 0, nothing to do."

The width of output image evaluates to C<0>. This value is rounded
to an integer number of pixels, therefore this warning does not mean
that you have provided an explicit number of C<0> (it could also
have been e.g. C<0.005in> at a resolution of C<90dpi>). In this
case, nothing is drawn.

=item * "Surface height is 0, nothing to do."

Like above.

=back


=head1 DEPENDENCIES

=over 4

=item * L<Class::Accessor|Class::Accessor>, version 0.30 or higher

=item * L<SVG|SVG>, version 2.37 or higher

=item * L<Cairo|Cairo>, version 1.061 or higher

With respect to the module code, the dependency on L<Cairo|Cairo> is
not strict.  The code only requires L<Cairo|Cairo> in case no other
rasterization engine is specified (see documentation for
details). However, if you do not provide a different backend, which
would probably at least require a wrapper written by you, then you
cannot do anything without L<Cairo|Cairo>. Therefore I have included
it as a strict dependency. You could take it out of the Makefile.PL
if you know what you are doing. However, the distribution will not
pass the test suite without L<Cairo|Cairo>.

=item * L<Params::Validate|Params::Validate>, version 0.91 or higher

=item * L<Scalar::Util|Scalar::Util>, version 1.19 or higher

=item * L<Exception::Class>, version 1.29 or higher

=item * L<Test::More|Test::More>, version 0.86 or higher

=item * L<Test::Exception|Test::Exception>, version 0.27 or higher

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

These methods are just documented for myself. You can read on to
satisfy your voyeuristic desires, but be aware of that they might
change or vanish without notice in a future version.

=over 4

=item * _create_engine

Expects a HASH reference as parameter. No validation is
performed. The entries C<width>, C<height>, and C<engine_class> are
used and expected to be valid if present.

=item * _process_normalize_attributes

Expects a flag (to indicate if normalization is to be performed) and
a HASH reference. The second parameter can be false, but if it is
true it is expected (without validation) to be a HASH
reference. Makes a copy of the hash and returns it after removing
(if the flag is true) enclosing white space from each value.

=item * _initial_viewport

Expects two HASH references. The first one is expected to be defined
and a HASH reference, but the content can be arbitrary. The second
is expected to be validated. Does not return anything.

=item * _angle

  $angle = _angle($x1, $y1, $x2, $y2)
  $angle = SVG::Rasterize->_angle($x1, $y1, $x2, $y2)
  $angle = $rasterize->_angle($x1, $y1, $x2, $y2)

Expects two vectors and returns the angle between them in C<rad>. No
parameter validation is performed. If one of the vectors is C<0>
(and only if it is exactly C<0>), C<undef> is returned.

=item * _split_path_data

Expects a path data string. This is expected (without validation) to
be defined. Everything else is checked within the method.

Returns a list. The first entry is either C<1> or C<0> indicating if
an error has occured (i.e. if the string is not fully valid). The
rest is a list of ARRAY references containing the instructions to
draw the path.

=item * _draw_path

Does not take any parameters (i.e. ignores them). Expects the
following:

=over 4

=item * C<< $self->state >> is defined and valid.

=back

The rest is handed over to the rasterization backend (which has -
nota bene - its expectations.

=item * _draw_rect

Does not take any parameters (i.e. ignores them). Expects the
following:

=over 4

=item * C<< $self->state >> is defined and valid.

=item * C<< $state->node_attributes >> have been validated.

=back

The rest is handed over to the rasterization backend (which has -
nota bene - its expectations.

=item * _draw_circle

Same es L<_draw_rect|/_draw_rect>.

=item * _draw_ellipse

Same es L<_draw_rect|/_draw_rect>.

=item * _draw_line

Same es L<_draw_rect|/_draw_rect>.

=item * _draw_polyline

Same es L<_draw_path|/_draw_path>.

=item * _draw_polygon

Same es L<_draw_path|/_draw_path>.

=back

=head1 FOOTNOTES

=over 4

=item * [1] Yannis Haralambous: Fonts & Encodings. O'Reilly, 2007.

Tons of information about what the author calls the "digital space
for writing".

=back

=head1 SEE ALSO

=over 4

=item * L<http://www.w3.org/TR/SVG11/>

=item * L<SVG|SVG>

=item * L<SVG::Parser|SVG::Parser>

=item * L<Cairo|Cairo>

=item * L<Exception::Class|Exception::Class>

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
