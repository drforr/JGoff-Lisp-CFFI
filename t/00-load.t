#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'JGoff::Lisp::CFFI' ) || print "Bail out!\n";
}

diag( "Testing JGoff::Lisp::CFFI $JGoff::Lisp::CFFI::VERSION, Perl $], $^X" );
