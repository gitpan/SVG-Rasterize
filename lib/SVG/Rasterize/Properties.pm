package SVG::Rasterize::Properties;
use Exporter 'import';

# $Id: Properties.pm 5460 2010-05-03 01:55:35Z mullet $

=head1 NAME

C<SVG::Rasterize::Properties> - SVG styling properties

=head1 VERSION

Version 0.000007

=cut

our $VERSION = '0.000007';

our @EXPORT = qw(%property_specification %relevant_properties);

our %property_specification =
    ('clip-path'           => 1,
     'color'               => 1,
     'color-interpolation' => 1,
     'color-rendering'     => 1,
     'cursor'              => 1,
     'display'             => 1,
     'fill'                => 1,
     'fill-opacity'        => 1,
     'fill-rule'           => 1,
     'filter'              => 1,
     'marker'              => 1,
     'marker-end'          => 1,
     'marker-mid'          => 1,
     'marker-start'        => 1,
     'mask'                => 1,
     'opacity'             => 1,
     'pointer-events'      => 1,
     'shape-rendering'     => 1,
     'stroke'              => 
         {inherited  => 1,
          color      => 1},
     'stroke-dasharray'    => 1,
     'stroke-dashoffset'   => 1,
     'stroke-linecap'      => 1,
     'stroke-linejoin'     => 'miter',
     'stroke-miterlimit'   => 1,
     'stroke-opacity'      => 1,
     'stroke-width'        =>
         {validation => {xsl => {regexp => $p_A_LENGTH},
			 css => {regexp => $p_P_LENGTH}},
	  inherited  => 1,
	  length     => 1},
     'visibility'          => 1);

our %relevant_properties =
    ('line'  => {'clip-path'           => 1,
		 'color'               => 1,
		 'color-interpolation' => 1,
		 'color-rendering'     => 1,
		 'cursor'              => 1,
		 'display'             => 1,
		 'fill'                => 1,
		 'fill-opacity'        => 1,
		 'fill-rule'           => 1,
		 'filter'              => 1,
		 'marker'              => 1,
		 'marker-end'          => 1,
		 'marker-mid'          => 1,
		 'marker-start'        => 1,
		 'mask'                => 1,
		 'opacity'             => 1,
		 'pointer-events'      => 1,
		 'shape-rendering'     => 1,
		 'stroke'              => 'none',
		 'stroke-dasharray'    => 1,
		 'stroke-dashoffset'   => 1,
		 'stroke-linecap'      => 1,
		 'stroke-linejoin'     => 'miter',
		 'stroke-miterlimit'   => 1,
		 'stroke-opacity'      => 1,
		 'stroke-width'        => 1,
		 'visibility'          => 1},
     'svg'   => {'stroke-width'        => 1,
		 'stroke'              => 'none'},
     'g'     => {'stroke-width'        => 1,
		 'stroke'              => 'none'});

1;


__END__

=pod

Exports the following variables:

=over 4

=item * C<%property_specification>

=item * C<%relevant_properties>

=back

=cut
