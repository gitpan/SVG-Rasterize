package SVG::Rasterize::Cairo;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Carp;
use Cairo;

# $Id: Cairo.pm 5716 2010-05-23 09:25:38Z mullet $

=head1 NAME

C<SVG::Rasterize::Cairo> - rasterize output using Cairo

=head1 INHERITANCE

  SVG::Rasterize::Cairo is a
    L<Class::Accessor|Class::Accessor>

=head1 VERSION

Version 0.001005

=cut

our $VERSION = '0.001005';


__PACKAGE__->mk_accessors(qw(width height));

__PACKAGE__->mk_ro_accessors(qw(context));

###########################################################################
#                                                                         #
#                      Class Variables and Methods                        # 
#                                                                         #
###########################################################################

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
    my ($self, @args) = @_;

    # parameter checking, width, height must be there
    my %args = @args;

    foreach(keys %args) {
	my $meth = $_;
	if($self->can($meth)) { $self->$meth($args{$meth}) }
	else { carp "Unrecognized init parameter $meth.\n" }
    }

    my $surface = ($args{surface_class} || 'Cairo::ImageSurface')->create
	('argb32', $args{width}, $args{height});
    $self->{context} = Cairo::Context->create($surface);
}

###########################################################################
#                                                                         #
#                               Accessors                                 # 
#                                                                         #
###########################################################################

###########################################################################
#                                                                         #
#                                Drawing                                  #
#                                                                         #
###########################################################################

sub _prepare_fill {
    my ($self, $properties) = @_;
    my $context             = $self->{context};

    if(my $fill = $properties->{'fill'}) {
	$context->set_source_rgba($fill->[0] / 256,  # values are clamped
				  $fill->[1] / 256,  # by cairo
				  $fill->[2] / 256,
				  $properties->{'fill-opacity'});

	if($properties->{'fill-rule'} eq 'nonzero') {
	    $context->set_fill_rule('winding');
	}
	elsif($properties->{'fill-rule'} eq 'evenodd') {
	    $context->set_fill_rule('even-odd');
	}
    }
}

sub _prepare_stroke {
    my ($self, $properties) = @_;
    my $context             = $self->{context};

    if(my $stroke = $properties->{'stroke'}) {
	$context->set_source_rgba($stroke->[0] / 256,  # values are
				  $stroke->[1] / 256,  # clamped by cairo
				  $stroke->[2] / 256,
				  $properties->{'stroke-opacity'});

	$context->set_line_width($properties->{'stroke-width'});

	# could be simplified, but I keep it for homogeinity
	if($properties->{'stroke-linecap'} eq 'butt') {
	    $context->set_line_cap('butt');
	}
	elsif($properties->{'stroke-linecap'} eq 'round') {
	    $context->set_line_cap('round');
	}
	if($properties->{'stroke-linecap'} eq 'square') {
	    $context->set_line_cap('square');
	}

	# could be simplified, but I keep it for homogeinity
	if($properties->{'stroke-linejoin'} eq 'miter') {
	    $context->set_line_join('miter');
	}
	elsif($properties->{'stroke-linejoin'} eq 'round') {
	    $context->set_line_join('round');
	}
	if($properties->{'stroke-linejoin'} eq 'bevel') {
	    $context->set_line_join('bevel');
	}

	$context->set_miter_limit($properties->{'stroke-miterlimit'});

	my @dash = ($properties->{'stroke-dashoffset'});
	if($properties->{'stroke-dasharray'}) {
	    push(@dash, @{$properties->{'stroke-dasharray'}});
	}
	$context->set_dash(@dash);
    }
}

sub _fill {
    my ($self, $properties) = @_;

    if($properties->{'fill'}) {
	$self->_prepare_fill($properties);
	$self->{context}->fill;
    }
    else {
	$self->{context}->new_path;
    }
}

sub _stroke {
    my ($self, $properties) = @_;

    if($properties->{'stroke'}) {
	$self->_prepare_stroke($properties);
	$self->{context}->stroke;
    }
    else {
	$self->{context}->new_path;
    }
}

sub _fill_and_stroke {
    my ($self, $properties) = @_;
    my $context             = $self->{context};

    $context->push_group;

    if($properties->{'fill'}) {
	$self->_prepare_fill($properties);
	$context->fill_preserve;
    }

    if($properties->{'stroke'}) {
	$self->_prepare_stroke($properties);
	my $op = $properties->{'stroke-opacity'};
	if(defined($op) and $op < 1) {
	    $context->save;
	    $context->set_operator('clear');
	    $context->stroke_preserve;
	    $context->restore;
	}
	$context->stroke;
    }
    else {
	$context->new_path;
    }

    $context->pop_group_to_source;
    $context->paint;
}

sub draw_line {
    my ($self, $state, $x1, $y1, $x2, $y2) = @_;
    my $context                            = $self->{context};
    
    $context->save;

    $context->set_matrix(Cairo::Matrix->init(@{$state->matrix}));

    $context->move_to($x1, $y1);
    $context->line_to($x2, $y2);

    $self->_stroke($state->properties);
    $context->restore;
    return;
}

sub draw_path {
    my ($self, $state, @instructions) = @_;
    my $context                       = $self->{context};
    
    $context->save;
    $context->set_matrix(Cairo::Matrix->init(@{$state->matrix}));
    
    my @last_control = ();
    foreach(@instructions) {
	if($_->[0] eq 'M') {
	    @last_control = ();
	    $context->move_to($_->[1], $_->[2]);
	    next;
	}
	if($_->[0] eq 'm') {
	    @last_control = ();
	    if($context->has_current_point) {
		$context->rel_move_to($_->[1], $_->[2]);
	    }
	    else {
		$context->move_to($_->[1], $_->[2]);
	    }
	    next;
	}
	if($_->[0] eq 'Z' or $_->[0] eq 'z') {
	    @last_control = ();
	    $context->close_path;
	    next;
	}
	if($_->[0] eq 'L') {
	    @last_control = ();
	    $context->line_to($_->[1], $_->[2]);
	    next;
	}
	if($_->[0] eq 'l') {
	    @last_control = ();
	    $context->rel_line_to($_->[1], $_->[2]);
	    next;
	}
	if($_->[0] eq 'H') {
	    @last_control = ();
	    my @curr = $context->get_current_point;
	    $context->line_to($_->[1], $curr[1]);
	    next;
	}
	if($_->[0] eq 'h') {
	    @last_control = ();
	    $context->rel_line_to($_->[1], 0);
	    next;
	}
	if($_->[0] eq 'V') {
	    @last_control = ();
	    my @curr = $context->get_current_point;
	    $context->line_to($curr[0], $_->[2]);
	    next;
	}
	if($_->[0] eq 'v') {
	    @last_control = ();
	    $context->rel_line_to(0, $_->[2]);
	    next;
	}
	if($_->[0] eq 'C') {
	    $context->curve_to(@$_[1..6]);
	    @last_control = ($_->[3], $_->[4]);
	    next;
	}
	if($_->[0] eq 'c') {
	    my @curr = $context->get_current_point;
	    $context->rel_curve_to(@$_[1..6]);
	    @last_control = ($curr[0] + $_->[3], $curr[1] + $_->[4]);
	    next;
	}
	if($_->[0] eq 'S') {
	    my @coords = (undef, undef, @$_[1..4]);
	    my @curr   = $context->get_current_point;
	    if(@last_control) {
		$coords[0] = 2 * $curr[0] - $last_control[0];
		$coords[1] = 2 * $curr[1] - $last_control[1];
	    }
	    else { ($coords[0], $coords[1]) = @curr }

	    $context->curve_to(@coords);
	    @last_control = ($_->[1], $_->[2]);
	    next;
	}
	if($_->[0] eq 's') {
	    my @coords = (undef, undef, @$_[1..4]);
	    my @curr   = $context->get_current_point;
	    if(@last_control) {
		$coords[0] = $curr[0] - $last_control[0];
		$coords[1] = $curr[1] - $last_control[1];
	    }
	    else { ($coords[0], $coords[1]) = (0, 0) }

	    $context->rel_curve_to(@coords);
	    @last_control = ($curr[0] + $_->[1], $curr[1] + $_->[2]);
	    next;
	}
	if($_->[0] eq 'Q') {
	    my @curr   = $context->get_current_point;
	    my @coords = (1/3 * $curr[0] + 2/3 * $_->[1],
			  1/3 * $curr[1] + 2/3 * $_->[2],
			  2/3 * $_->[1]  + 1/3 * $_->[3],
			  2/3 * $_->[2]  + 1/3 * $_->[4],
			  $_->[3],
			  $_->[4]);

	    $context->curve_to(@coords);
	    @last_control = ($_->[1], $_->[2]);
	    next;
	}
	if($_->[0] eq 'q') {
	    my @curr   = $context->get_current_point;
	    my @coords = ($curr[0] + 2/3 * $_->[1],
			  $curr[1] + 2/3 * $_->[2],
			  $curr[0] + 2/3 * $_->[1] + 1/3 * $_->[3],
			  $curr[1] + 2/3 * $_->[2] + 1/3 * $_->[4],
			  $curr[0] + $_->[3],
			  $curr[1] + $_->[4]);

	    $context->curve_to(@coords);
	    @last_control = ($curr[0] + $_->[1], $curr[1] + $_->[2]);
	    next;
	}
	if($_->[0] eq 'T') {
	    my @curr    = $context->get_current_point;
	    my @control = @curr;
	    if(@last_control) {
		$control[0] = 2 * $curr[0] - $last_control[0];
		$control[1] = 2 * $curr[1] - $last_control[1];
	    }
	    my @coords = (1/3 * $curr[0]    + 2/3 * $control[0],
			  1/3 * $curr[1]    + 2/3 * $control[1],
			  2/3 * $control[0] + 1/3 * $_->[1],
			  2/3 * $control[1] + 1/3 * $_->[2],
			  $_->[1],
			  $_->[2]);

	    $context->curve_to(@coords);
	    @last_control = ($control[0], $control[1]);
	    next;
	}
	if($_->[0] eq 't') {
	    my @curr    = $context->get_current_point;
	    my @control = @curr;
	    if(@last_control) {
		$control[0] = 2 * $curr[0] - $last_control[0];
		$control[1] = 2 * $curr[1] - $last_control[1];
	    }
	    my @coords = (1/3 * $curr[0]    + 2/3 * $control[0],
			  1/3 * $curr[1]    + 2/3 * $control[1],
			  2/3 * $control[0] + 1/3 * ($curr[0] + $_->[1]),
			  2/3 * $control[1] + 1/3 * ($curr[1] + $_->[2]),
			  $curr[0] + $_->[1],
			  $curr[1] + $_->[2]);

	    $context->curve_to(@coords);
	    @last_control = ($control[0], $control[1]);
	    next;
	}
	if($_->[0] eq 'A') {
	    @last_control = ();
	    my @curr = $context->get_current_point;
	    my ($cx, $cy, $rx, $ry, $th1, $dth) =
		SVG::Rasterize::endpoint_to_center(@curr, (@$_)[1..7]);

	    # If nothing comes back it means that a radius is 0
	    # (but then we would not be here because the command
	    # has been switched to a lineto) or start and end are
	    # equal. In this case we do nothing.
	    next if(!defined($cx));

	    my $sin_phi = sin($_->[3]);
	    my $cos_phi = cos($_->[3]);
	    my $matrix  = SVG::Rasterize::multiply_matrices
		($state->{matrix}, [$rx * $cos_phi, -$ry * $sin_phi,
				    $rx * $sin_phi,  $ry * $cos_phi,
				    $cx, $cy]);
	    $context->save;
	    $context->set_matrix(Cairo::Matrix->init(@$matrix));
	    if($dth > 0) {
		$context->arc(0, 0, 1, $th1, $th1 + $dth);
	    }
	    else {
		$context->arc_negative(0, 0, 1, $th1, $th1 + $dth);
	    }
	    $context->restore;
	    next;
	}
	if($_->[0] eq 'a') {
	    @last_control = ();
	    my @curr = $context->get_current_point;
	    my @end  = ($curr[0] + $_->[6], $curr[1] + $_->[7]);  
	    my ($cx, $cy, $rx, $ry, $th1, $dth) =
		SVG::Rasterize::endpoint_to_center
		    (@curr, (@$_)[1..5], @end);

	    # see 'A'
	    next if(!defined($cx));

	    my $sin_phi = sin($_->[3]);
	    my $cos_phi = cos($_->[3]);
	    my $matrix  = SVG::Rasterize::multiply_matrices
		($state->{matrix}, [$rx * $cos_phi, -$ry * $sin_phi,
				    $rx * $sin_phi,  $ry * $cos_phi,
				    $cx, $cy]);
	    $context->save;
	    $context->set_matrix(Cairo::Matrix->init(@$matrix));
	    if($dth > 0) {
		$context->arc(0, 0, 1, $th1, $th1 + $dth);
	    }
	    else {
		$context->arc_negative(0, 0, 1, $th1, $th1 + $dth);
	    }
	    $context->restore;
	    next;
	}
    }

    $self->_fill_and_stroke($state->properties);
    $context->restore;
    return;
}

sub write {
    my ($self, @args) = @_;

    # TODO: check args
    my %args = @args;
    if(!$args{type}) {
	carp "No output type specified.\n";
    }
    elsif(lc($args{type}) eq 'png') {
	if(!$args{file_name}) {
	    carp("Unable to write png file. No file name specified.\n");
	    return undef;
	}

	$self->{context}->show_page;
	$self->{context}->get_target->write_to_png($args{file_name});
    }
    else {
	carp "Unknown output type ($args{type}).\n";
    }
}

1;

__END__

=head1 SYNOPSIS


=head1 DESCRIPTION

The interface of this class has to be considered unstable. Therefore
it is also only very sparsely documented.

=head1 INTERFACE

=head2 Constructors

=head3 new

  SVG::Rasterize::Cairo->new(%args)

Creates a new C<SVG::Rasterize::Cairo> object and calls C<init(%args)>.
If you subclass C<SVG::Rasterize::Cairo> overload C<init>, not C<new>.

=head3 init

  $cairo->init(%args)

If you overload C<init>, your method should also call this one.
It provides the following functions:

=over 4

=item * For each given argument (which has not been deleted by
the previous actions) it calls the accessor with the same name to
initialize the attribute. If such an accessor does not exist a
warning is printed and the argument is ignored.

=back

=head2 Public Attributes

These are the attributes which alternative rasterization engines
have to implement.

=head3 width

=head3 height

=head2 Methods for Developers

These are the methods which alternative rasterization engines have
to implement.

=head3 draw_line

=head3 draw_path

=head3 write

  $engine->write(%args)

Writes the rendered image to a file.

B<Example:>

  $engine->write(type => 'png', file_name => 'foo.png');

C<type> and C<file_name> are the only supported parameters at the
moment and the only supported type is "png".

=head1 DIAGNOSTICS

=head2 Exceptions

=head2 Warnings


=head1 BUGS AND LIMITATIONS

No bugs have been reported.
Please report any bugs or feature requests to
C<bug-svg-rasterize at rt.cpan.org>, or through
the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=SVG-Rasterize>. 
I will be notified, and then you will automatically be notified
of progress on your bug as I make changes.


=head1 AUTHOR

Lutz Gehlen, C<< <perl at lutzgehlen.de> >>


=head1 LICENSE AND COPYRIGHT

Copyright 2010 Lutz Gehlen.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut
