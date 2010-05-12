package SVG::Rasterize::Cairo;
use base Class::Accessor;

use warnings;
use strict;

use 5.008009;

use Carp;
use Cairo;

# $Id: Cairo.pm 5539 2010-05-12 04:47:49Z mullet $

=head1 NAME

C<SVG::Rasterize::Cairo> - rasterize output using Cairo

=head1 INHERITANCE

  SVG::Rasterize::Cairo is a
    L<Class::Accessor|Class::Accessor>

=head1 VERSION

Version 0.000009

=cut

our $VERSION = '0.000009';


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

sub draw_line {
    my ($self, $properties, $x1, $y1, $x2, $y2) = @_;
    my $context                                 = $self->{context};
    
    return if(!$properties->{'stroke'});

    $context->save;

    $context->set_source_rgb(@{$properties->{'stroke'}});
    $context->set_line_width($properties->{'stroke-width'});
    
    $context->move_to($x1, $y1);
    $context->line_to($x2, $y2);

    $context->stroke;

    $context->restore;
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
