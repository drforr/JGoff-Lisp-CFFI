package JGoff::Lisp::CFFI::ForeignCStruct;

use Moose;
use Function::Parameters qw( :strict );

has documentation => (
  is => 'ro',
  default => '',
);
has size => (
  is => 'ro',
  default => -1
);
has keys => (
  is => 'ro',
  isa => 'ArrayRef'
);

=head1 NAME

JGoff::Lisp::CFFI::ForeignCStruct - Foreign CStruct internal class

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

Quick summary of what the module does.

Perhaps a little code snippet.

    use JGoff::Lisp::CFFI;

    # load libcurl in a platform independent fashion
    # call curl_global_init(0) from perl

    $cffi = JGoff::Lisp::CFFI->new;

    $libcurl = $cffi->define_foreign_library(
        [ ':unix' => [ ':or' => 'libcurl.so.3', 'libcurl.so' ] ],
        [ __default__ => [ ':default' => 'libcurl' ] ] ); 
    $cffi->use_foreign_library( $libcurl );

    $curl_code = $cffi->defctype( ':int' );
    $cffi->defcfun( curl_global_init => $curl_code,
      [ flags => ':long' ] );

    curl_global_init( 0 );

=head1 METHODS

=head1 AUTHOR

Jeffrey Goff, C<< <jgoff at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-jgoff-lisp-cffi at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JGoff-Lisp-CFFI>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JGoff::Lisp::CFFI::ForeignAddress

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=JGoff-Lisp-CFFI>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/JGoff-Lisp-CFFI>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/JGoff-Lisp-CFFI>

=item * Search CPAN

L<http://search.cpan.org/dist/JGoff-Lisp-CFFI/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Jeffrey Goff.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.


=cut

1; # End of JGoff::Lisp::CFFI
