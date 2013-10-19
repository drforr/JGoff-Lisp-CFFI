#!perl -T

use strict;
use warnings;
use Data::Dumper qw( Dumper );

use Test::More tests => 2;
BEGIN {
  use_ok( 'JGoff::Lisp::CFFI' ) || print "Bail out!\n";
}

sub deftest {
  my ( $name, $func, $result ) = @_;
  my ( $package, $filename, $line ) = caller();
  my $test = $func->();
  if ( ref $test ) {
    is_deeply( $test, $result, $name ) or
      diag( "  at test file $filename line $line\n" . Dumper( $test ) );
  }
  else {
    is( $test, $result, $name ) or
      diag( "  at test file $filename line $line" );
  }
}

my $cffi = JGoff::Lisp::CFFI->new;

SKIP:{
  my $count = 1;
  my $str = "$count tests not ready yet";
  diag $str; skip $str, $count;

# (defbitfield open-flags
#   (:rdonly #x0000)
#   :wronly               ;#x0001
#   :rdwr                 ;...
#   :nonblock
#   :append
#   (:creat  #x0200))
#   ;; etc...

my $open_flags;
$cffi->defbitfield( \$open_flags,
  [ ':rdonly' => 0x0000 ],
  ':wronly',
  ':rdwr',
  ':nonblock',
  ':append',
  [ ':creat' => 0x0200 ] );
  # etc...

# (defbitfield flags
#   (flag-a 1)
#   (flag-b 2)
#   (flag-c 4))

my $flags;
$cffi->defbitfield( \$flags,
  [ 'flag-a' => 1 ],
  [ 'flag-b' => 2 ],
  [ 'flag-c' => 4 ] );

# Regression test: defbitfield was misbehaving when the first value
# was provided.
# (deftest bitfield.1
#     (eval '(defbitfield bf1
#              (:foo 0)))
#   bf1)

my $bf1; # Scope?
deftest 'bitfield.1' => sub {
  return $cffi->defbitfield( \$bf1, [ ':foo' => 0 ] );
}, $bf1;

# (defbitfield bf2
#   one
#   two
#   four
#   eight
#   sixteen
#   thirty-two
#   sixty-four)

my $bf2;
$cffi->defbitfield( \$bf2,
  'one',
  'two',
  'four',
  'eight',
  'sixteen',
  'thirty-two',
  'sixty-four' );

# (deftest bitfield.2
#     (mapcar (lambda (symbol)
#              (foreign-bitfield-value 'bf2 (list symbol)))
#             '(one two four eight sixteen thirty-two sixty-four))
#   (1 2 4 8 16 32 64))

deftest 'bitfield.2' => sub {
  map { $cffi->foreign_bitfield_value( $bf2, [ $_ ] ) }
      [ 'one', 'two', 'four', 'eight', 'sixteen', 'thirty-two', 'sixty-four' ];
}, [ 1, 2, 4, 8, 16, 32, 64 ];

# (defbitfield bf3
#   (three 3)
#   one
#   (seven 7)
#   two
#   (eight 8)
#   sixteen)

my $bf3;
$cffi->defbitfield( \$bf3,
  [ three => 3 ],
  'one',
  [ seven => 7 ],
  'two',
  [ eight => 8 ],
  'sixteen' );

# Non-single-bit numbers must not influence the progression of
# implicit values.  Single bits larger than any before *must*
# influence said progression.
# (deftest bitfield.3
#     (mapcar (lambda (symbol)
#               (foreign-bitfield-value 'bf3 (list symbol)))
#             '(one two sixteen))
#   (1 2 16))

deftest 'bitfield.3' => sub {
  map { $cffi->foreign_bitfield_value( $bf3, [ $_ ] ) }
      ( 'one', 'two', 'sixteen' );
}, [ 1, 2, 16 ];

# (defbitfield bf4
#   (zero 0)
#   one)

my $bf4;
$cffi->defbitfield( \$bf4,
  [ zero => 0 ],
  'one' );

# Yet another edge case with the 0...
# (deftest bitfield.4
#     (foreign-bitfield-value 'bf4 '(one))
#   1)

deftest 'bitfield.4' => sub {
  $cffi->foreign_bitfield_value( $bf4, [ 'one' ] );
}, 1;
};
