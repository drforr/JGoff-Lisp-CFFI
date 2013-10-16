#!perl -T

use strict;
use warnings;

use Test::More tests => 28;
BEGIN {
  use_ok( 'JGoff::Lisp::CFFI' ) || print "Bail out!\n";
}

my $cffi = JGoff::Lisp::CFFI->new;

# {{{ convert_to_foreign returns correct value in scalar context
{ my $foreign =
    $cffi->convert_to_foreign( "a boat", $JGoff::Lisp::CFFI::string );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
}
# }}}

# {{{ convert_to_foreign returns correct values in array context
{ my ( $params, $foreign ) =
    $cffi->convert_to_foreign( "a boat", $JGoff::Lisp::CFFI::string );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
  is( $params, 2 );
}
# }}}

# {{{ convert_from_foreign
{ my $foreign = $cffi->convert_to_foreign(
                  "a boat", $JGoff::Lisp::CFFI::string );
  my $object = $cffi->convert_from_foreign(
                 $foreign, $JGoff::Lisp::CFFI::string );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
}
# }}}

# {{{ defbitfield and friends with just names
{ my $flags;
  $cffi->defbitfield( \$flags,
    'flag-a',
    'flag-b',
    'flag-c'
  );
  isa_ok( $flags, 'JGoff::Lisp::CFFI::ForeignBitfield' );

  my $test_bitfield_symbols = [
    $cffi->foreign_bitfield_symbols( $flags, 0b101 )
  ];
  is_deeply(
    $test_bitfield_symbols,
    [ 'FLAG-A', 'FLAG-C' ]
  );

  is(
    $cffi->foreign_bitfield_value( $flags, [ 'flag-a', 'flag-c' ] ),
    0b101
  );
}
# }}}

# {{{ defbitfield and friends with name and one offset
{ my $flags;
  $cffi->defbitfield( \$flags,
    [ 'flag-a' => 2 ],
    'flag-b',
    'flag-c'
  );
  isa_ok( $flags, 'JGoff::Lisp::CFFI::ForeignBitfield' );

  my $test_bitfield_symbols = [
    $cffi->foreign_bitfield_symbols( $flags, 0b1010 )
  ];
  is_deeply(
    $test_bitfield_symbols,
    [ 'FLAG-A', 'FLAG-C' ]
  );

  is(
    $cffi->foreign_bitfield_value( $flags, [ 'flag-a', 'flag-c' ] ),
    0b1010
  );
}
# }}}

# {{{ defbitfield and friends with name and two offsets
{ my $flags;
  $cffi->defbitfield( \$flags,
    [ 'flag-a' => 2 ],
    'flag-b',
    [ 'flag-c' => 16 ]
  );
  isa_ok( $flags, 'JGoff::Lisp::CFFI::ForeignBitfield' );

  my $test_bitfield_symbols = [
    $cffi->foreign_bitfield_symbols( $flags, 0b10010 )
  ];
  is_deeply(
    $test_bitfield_symbols,
    [ 'FLAG-A', 'FLAG-C' ]
  );

  is(
    $cffi->foreign_bitfield_value( $flags, [ 'flag-a', 'flag-c' ] ),
    0b10010
  );
}
# }}}

# {{{ defbitfield with an all-ones flag
{ my $flags;
  $cffi->defbitfield( \$flags,
    [ 'flag-all' => 0 ],
    'flag-a',
    'flag-b',
    'flag-c'
  );
  isa_ok( $flags, 'JGoff::Lisp::CFFI::ForeignBitfield' );

  my $test_bitfield_symbols = [
    $cffi->foreign_bitfield_symbols( $flags, 0b101 )
  ];
  is_deeply(
    $test_bitfield_symbols,
    [ 'FLAG-ALL', 'FLAG-A', 'FLAG-C' ]
  );
}
# }}}

# {{{ defcenum and friends
{ my $enum;
  $cffi->defcenum( \$enum,
    ':no',
    ':yes'
  );
  isa_ok( $enum, 'JGoff::Lisp::CFFI::ForeignEnum' );

  is( $cffi->foreign_enum_keyword( $enum, 0 ), ':NO' );
  is( $cffi->foreign_enum_keyword( $enum, 1 ), ':YES' );

  is( $cffi->foreign_enum_value( $enum, ':no' ), 0 );
  is( $cffi->foreign_enum_value( $enum, ':yes' ), 1 );
}
# }}}

# {{{ defcstruct and friends
{ my $rect_struct;
  $cffi->defcstruct( \$rect_struct,
    [ 'x'      => $JGoff::Lisp::CFFI::int ],
    [ 'y'      => $JGoff::Lisp::CFFI::int ],
    [ 'width'  => $JGoff::Lisp::CFFI::int ],
    [ 'height' => $JGoff::Lisp::CFFI::int ],
  );
  isa_ok( $rect_struct, 'JGoff::Lisp::CFFI::ForeignCStruct' );

  is_deeply(
    [ $cffi->foreign_slot_names( $rect_struct ) ],
    [ 'X', 'Y', 'WIDTH', 'HEIGHT' ]
  );

  is( $cffi->foreign_slot_offset( $rect_struct, 'x'      ), 0 );
  is( $cffi->foreign_slot_offset( $rect_struct, 'y'      ), 2 );
  is( $cffi->foreign_slot_offset( $rect_struct, 'width'  ), 4 );
  is( $cffi->foreign_slot_offset( $rect_struct, 'height' ), 6 );

  my $window_rect;
# $cffi->with_foreign_object( $window_rect, $rect_struct, sub {
#   $cffi->foreign_slot_value( $window_rect $rect_struct, 'x' ) = 42;
#   $cffi->foreign_slot_value( $window_rect $rect_struct, 'y' ) = 41;
#   $cffi->foreign_slot_value( $window_rect $rect_struct, 'width' ) = 40;
#   $cffi->foreign_slot_value( $window_rect $rect_struct, 'height' ) = 39;
# } );

  $window_rect = $cffi->foreign_alloc( $rect_struct );

  #
  # XXX It'd be nice to have an lvalue here for this slot mechanism.
  # XXX Feels a little bit overengineered to me, but whatever works.
  #
  $cffi->foreign_slot_value( $window_rect, $rect_struct, 'x', 42 );
  $cffi->foreign_slot_value( $window_rect, $rect_struct, 'y', 41 );
  $cffi->foreign_slot_value( $window_rect, $rect_struct, 'width', 40 );
  $cffi->foreign_slot_value( $window_rect, $rect_struct, 'height', 39 );

  $cffi->with_foreign_slots(
           [ 'x', 'y', 'width', 'height' ],
           $window_rect,
           $rect_struct,
           sub {
             is_deeply(
               [ $_{x}, $_{y}, $_{width}, $_{height} ],
               [ 42, 41, 40, 39 ]
             );
           }
  );
}
# }}}

#  my @collection;
#  my $array;
#  $cffi->with_foreign_object( [ $array, $JGoff::Lisp::CFFI::int => 10 ], sub {
#    for my $i ( 1 .. 10 ) {
#      $cffi->mem_aref( $%, $JGoff::Lisp::CFFI::int => $i, $i );
#    }
#    for my $i ( 1 .. 10 ) {
#      push @collection, $cffi->mem_aref( $%, $JGoff::Lisp::CFFI::int => $i );
#    }
#  } );
#  is_deeply( [ @collection ],
#             [ 1 .. 10 ] );

=pod

# {{{ bindings.lisp

;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; libtest.lisp --- Setup CFFI bindings for libtest.
;;;
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira(@)common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

#(define-foreign-library (libtest :type :test)
#  (:darwin (:or "libtest.dylib" "libtest32.dylib"))
#  (:unix (:or "libtest.so" "libtest32.so"))
#  (:windows "libtest.dll")
#  (t (:default "libtest")))

$cffi->define_foreign_library( [ \$libtest, ':type', ':test' ],
  [ ':darwin' => [ ':or' => [ 'libtest.dylib', 'libtest32.dylib' ] ] ],
  [ ':unix' => [ ':or' => [ 'libtest.so', 'libtest32.so' ] ] ],
  [ ':windows' => 'libtest.dll' ],
  [ t => [ ':default' => 'libtest' ] ] );

#(define-foreign-library (libtest2 :type :test)
#  (:darwin (:or "libtest2.dylib" "libtest2_32.dylib"))
#  (:unix (:or "libtest2.so" "libtest2_32.so"))
#  (t (:default "libtest2")))

$cffi->define_foreign_library( [ \$libtest2, ':type', ':test' ],
  [ ':darwin' => [ ':or' => [ 'libtest2.dylib', 'libtest2_32.dylib' ] ] ],
  [ ':unix' => [ ':or' => [ 'libtest2.so', 'libtest2_32.so' ] ] ],
  [ t => [ ':default' => 'libtest2' ] ] );

#(define-foreign-library (libfsbv :type :test)
#  (:darwin (:or "libfsbv.dylib" "libfsbv32.dylib"))
#  (:unix (:or "libfsbv.so" "libfsbv_32.so"))
#  (:windows "libfsbv.dll")
#  (t (:default "libfsbv")))

$cffi->define_foreign_library( [ \$libfsbv, ':type', ':test' ],
  [ ':darwin' => [ ':or' => [ 'libfsbv.dylib', 'libfsbv32.dylib' ] ] ],
  [ ':unix' => [ ':or' => [ 'libfsbv.so', 'libfsbv32.so' ] ] ],
  [ ':windows' => 'libfsbv.dll' ],
  [ t => [ ':default' => 'libfsbv' ] ] );

#(define-foreign-library libc
#  (:windows "msvcrt.dll"))

$cffi->define_foreign_library( \$libc,
  [ ':windows' => 'msvcrt.dll' ] );

#(define-foreign-library libm
#  (:darwin "/usr/lib/libm.dylib")
#  (t (:default "libm")))

$cffi->define_foreign_library( \$libm,
  [ ':darwin' => '/usr/lib/libm.dylib' ],
  [ t => [ ':default' => 'libm' ] ] );

(defun call-within-new-thread (fn &rest args)
  (let (result
        error
        (cv (bordeaux-threads:make-condition-variable))
        (lock (bordeaux-threads:make-lock)))
    (bordeaux-threads:with-lock-held (lock)
      (bordeaux-threads:make-thread
       (lambda ()
         (multiple-value-setq (result error)
           (ignore-errors (apply fn args)))
         (bordeaux-threads:with-lock-held (lock)
           (bordeaux-threads:condition-notify cv))))
      (bordeaux-threads:condition-wait cv lock)
      (values result error))))

;;; As of OSX 10.6.6, loading CoreFoundation on something other than
;;; the initial thread results in a crash.
(deftest load-core-foundation
    (progn
      (call-within-new-thread 'load-foreign-library
                              '(:framework "CoreFoundation"))
      t)
  t)

;;; Return the directory containing the source when compiling or
;;; loading this file.  We don't use *LOAD-TRUENAME* because the fasl
;;; file may be in a different directory than the source with certain
;;; ASDF extensions loaded.
(defun load-directory ()
  (let ((here #.(or *compile-file-truename* *load-truename*)))
    (make-pathname :name nil :type nil :version nil
                   :defaults here)))

(defun load-test-libraries ()
  (let ((*foreign-library-directories* (list (load-directory))))
    (load-foreign-library 'libtest)
    (load-foreign-library 'libtest2)
    (load-foreign-library 'libfsbv)
    (load-foreign-library 'libc)
    (load-foreign-library 'libm)))

(load-test-libraries)

#+(:and :ecl (:not :dffi))
(ffi:load-foreign-library
 #.(make-pathname :name "libtest" :type "so"
                  :defaults (or *compile-file-truename* *load-truename*)))

#;;; check libtest version
#(defparameter *required-dll-version* "20120107")

$required_dll_version = "20120107";

#(defcvar "dll_version" :string)

my $dll_version;
$cffi->defcvar( \$dll_version => $JGoff::Lisp::CFFI::string );

#(unless (string= *dll-version* *required-dll-version*)
#  (error "version check failed: expected ~s but libtest reports ~s"
#         *required-dll-version*
#         *dll-version*))

is( $dll_version, $required_dll_version,
    "version check failed, expected $required_dll_version but libtest reports $dll_version" );

;;; The maximum and minimum values for single and double precision C
;;; floating point values, which may be quite different from the
;;; corresponding Lisp versions.
#(defcvar "float_max" :float)
#(defcvar "float_min" :float)
#(defcvar "double_max" :double)
#(defcvar "double_min" :double)

my ( $float_max, $float_min, $double_max, $double_min );
$cffi->defcvar( \$float_max, $JGoff::Lisp::CFFI::float );
$cffi->defcvar( \$float_min, $JGoff::Lisp::CFFI::float );
$cffi->defcvar( \$double_max, $JGoff::Lisp::CFFI::double );
$cffi->defcvar( \$double_min, $JGoff::Lisp::CFFI::double );

(defun run-cffi-tests (&key (compiled nil))
  (let ((regression-test::*compile-tests* compiled)
        (*package* (find-package '#:cffi-tests)))
    (format t "~&;;; running tests (~Acompiled)" (if compiled "" "un"))
    (do-tests)))

(defmacro expecting-error (&body body)
  `(handler-case (progn ,@body :no-error)
     (error () :error)))

# }}}

# {{{ callbacks.lisp

#(defcfun "expect_char_sum"           :int (f :pointer))
#(defcfun "expect_unsigned_char_sum"  :int (f :pointer))
#(defcfun "expect_short_sum"          :int (f :pointer))
#(defcfun "expect_unsigned_short_sum" :int (f :pointer))
#(defcfun "expect_int_sum"            :int (f :pointer))
#(defcfun "expect_unsigned_int_sum"   :int (f :pointer))
#(defcfun "expect_long_sum"           :int (f :pointer))
#(defcfun "expect_unsigned_long_sum"  :int (f :pointer))
#(defcfun "expect_float_sum"          :int (f :pointer))
#(defcfun "expect_double_sum"         :int (f :pointer))
#(defcfun "expect_pointer_sum"        :int (f :pointer))
#(defcfun "expect_strcat"             :int (f :pointer))

$cffi->defcfun( expect_char_sum => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_unsigned_char_sum => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_short_sum          => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_unsigned_short_sum => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_int_sum            => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_unsigned_int_sum   => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_long_sum           => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_unsigned_long_sum  => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_float_sum          => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_double_sum         => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_pointer_sum        => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );
$cffi->defcfun( expect_strcat             => $JGoff::Lisp::CFFI::int,
                [ f => $JGoff::Lisp::CFFI::pointer ] );

##-cffi-sys::no-long-long
#(progn
#  (defcfun "expect_long_long_sum"          :int (f :pointer))
#  (defcfun "expect_unsigned_long_long_sum" :int (f :pointer)))

unless ( $JGoff::Lisp::CFFI::no_long_long ) {
  $cffi->defcfun( expect_long_long_sum => $JGoff::Lisp::CFFI::int,
    [ f => $JGoff::Lisp::CFFI::pointer ] );
  $cffi->defcfun( expect_unsigned_long_long_sum => $JGoff::Lisp::CFFI::int,
    [ f => $JGoff::Lisp::CFFI::pointer ] );
}

##+long-float
#(defcfun "expect_long_double_sum"    :int (f :pointer))

if ( $JGoff::Lisp::CFFI::long_float ) {
  $cffi->defcfun( expect_long_double_sum => $JGoff::Lisp::CFFI::int, [ f => $JGoff::Lisp::CFFI::pointer ] );
}

#(defcallback sum-char :char ((a :char) (b :char))
#  "Test if the named block is present and the docstring too."
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (return-from sum-char (+ a b)))

$cffi->defcallback( sum_char => $JGoff::Lisp::CFFI::char,
                    [ [ a => $JGoff::Lisp::CFFI::char ],
                      [ b => $JGoff::Lisp::CFFI::char ] ],
  "Test if the named block is present and the docstring too.",
  sub {
    # ...
  }
)

#(defcallback sum-unsigned-char :unsigned-char
#    ((a :unsigned-char) (b :unsigned-char))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

$cffi->defcallback( sum_unsigned_char => $JGoff::Lisp::CFFI::unsigned_char,
  [ [ a => $JGoff::Lisp::CFFI::unsigned_char ],
    [ b => $JGoff::Lisp::CFFI::unsigned_char ] ],
  sub {
    $_{a} + $_{b}
  }
);

#(defcallback sum-short :short ((a :short) (b :short))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

$cffi->defcallback( sum_short => $JGoff::Lisp::CFFI::short,
  [ [ a => $JGoff::Lisp::CFFI::short ],
    [ b => $JGoff::Lisp::CFFI::short ] ],
  sub {
    $_{a} + $_{b}
  }
);

#(defcallback sum-unsigned-short :unsigned-short
#    ((a :unsigned-short) (b :unsigned-short))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

$cffi->defcallback( sum_unsigned_short => $JGoff::Lisp::CFFI::unsigned_short,
  [ [ a => $JGoff::Lisp::CFFI::unsigned_short ],
    [ b => $JGoff::Lisp::CFFI::unsigned_short ] ],
  sub {
    $_{a} + $_{b}
  }
);

#(defcallback sum-int :int ((a :int) (b :int))
#  (+ a b))

$cffi->defcallback( sum_int => $JGoff::Lisp::CFFI::int,
  [ [ a => $JGoff::Lisp::CFFI::int ],
    [ b => $JGoff::Lisp::CFFI::int ] ],
  sub {
    return $_{a} + $_{b}
  }
);

#(defcallback sum-unsigned-int :unsigned-int
#    ((a :unsigned-int) (b :unsigned-int))
#  (+ a b))

$cffi->defcallback( sum_unsigned_int => $JGoff::Lisp::CFFI::unsigned_int,
  [ [ a => $JGoff::Lisp::CFFI::unsigned_int ],
    [ b => $JGoff::Lisp::CFFI::unsigned_int ] ],
  sub {
    return $_{a} + $_{b}
  }
);

#(defcallback sum-long :long ((a :long) (b :long))
#  (+ a b))

$cffi->defcallback( sum_short => $JGoff::Lisp::CFFI::short,
  [ [ a => $JGoff::Lisp::CFFI::short ],
    [ b => $JGoff::Lisp::CFFI::short ] ],
  sub {
    return $_{a} + $_{b}
  }
);

#(defcallback sum-unsigned-long :unsigned-long
#    ((a :unsigned-long) (b :unsigned-long))
#  (+ a b))

$cffi->defcallback( sum_unsigned_long => $JGoff::Lisp::CFFI::unsigned_long,
  [ [ a => $JGoff::Lisp::CFFI::unsigned_long ],
    [ b => $JGoff::Lisp::CFFI::unsigned_long ] ],
  sub {
    return $_{a} + $_{b}
  }
);

##-cffi-sys::no-long-long
#(progn
#  (defcallback sum-long-long :long-long
#      ((a :long-long) (b :long-long))
#    (+ a b))
#
#  (defcallback sum-unsigned-long-long :unsigned-long-long
#      ((a :unsigned-long-long) (b :unsigned-long-long))
#    (+ a b)))

unless ( $JGoff::Lisp::CFFI::no_long_long ) {
  $cffi->defcallback(
    sum_unsigned_long_long => $JGoff::Lisp::CFFI::unsigned_long,
    [ [ a => $JGoff::Lisp::CFFI::unsigned_long_long ],
      [ b => $JGoff::Lisp::CFFI::unsigned_long_long ] ],
    sub {
      return $_{a} + $_{b}
    }
  );
  
  $cffi->defcallback(
    sum_long_long => $JGoff::Lisp::CFFI::unsigned_long,
    [ [ a => $JGoff::Lisp::CFFI::long_long ],
      [ b => $JGoff::Lisp::CFFI::long_long ] ],
    sub {
      return $_{a} + $_{b}
    }
  );
}

#(defcallback sum-float :float ((a :float) (b :float))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

$cffi->defcallback( sum_float => $JGoff::Lisp::CFFI::float,
  [ [ a => $JGoff::Lisp::CFFI::float ],
    [ b => $JGoff::Lisp::CFFI::float ] ],
  sub {
    $_{a} + $_{b}
  }
);

#(defcallback sum-double :double ((a :double) (b :double))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

$cffi->defcallback( sum_double => $JGoff::Lisp::CFFI::float,
  [ [ a => $JGoff::Lisp::CFFI::double ],
    [ b => $JGoff::Lisp::CFFI::double ] ],
  sub {
    $_{a} + $_{b}
  }
);

##+long-float
#(defcallback sum-long-double :long-double ((a :long-double) (b :long-double))
#  ;(format t "~%}}} a: ~A, b: ~A {{{~%" a b)
#  (+ a b))

if ( $JGoff::Lisp::CFFI::long_float ) {
  $cffi->defcallback( sum_long_double => $JGoff::Lisp::CFFI::float,
    [ [ a => $JGoff::Lisp::CFFI::long_double ],
      [ b => $JGoff::Lisp::CFFI::long_double ] ],
    sub {
      $_{a} + $_{b}
    }
  );
}

#(defcallback sum-pointer :pointer ((ptr :pointer) (offset :int))
#  (inc-pointer ptr offset))

$cffi->defcallback( sum_pointer => $JGoff::Lisp::CFFI::pointer,
  [ [ ptr => $JGoff::Lisp::CFFI::pointer ],
    [ offset => $JGoff::Lisp::CFFI::int ] ],
  sub {
    $cffi->inc_ponter( $_{ptr}, $_{offset} ); # XXX $cffi...
  }
);

#(defcallback lisp-strcat :string ((a :string) (b :string))
#  (concatenate 'string a b))

$cffi->defcallback( lisp_strcat => $JGoff::Lisp::CFFI::string,
  [ [ a => $JGoff::Lisp::CFFI::string ],
    [ b => $JGoff::Lisp::CFFI::string ] ],
  sub {
    $_{a} . $_{b}
  }
);

(deftest callbacks.char
    (expect-char-sum (get-callback 'sum-char))
  1)

(deftest callbacks.unsigned-char
    (expect-unsigned-char-sum (get-callback 'sum-unsigned-char))
  1)

(deftest callbacks.short
    (expect-short-sum (callback sum-short))
  1)

(deftest callbacks.unsigned-short
    (expect-unsigned-short-sum (callback sum-unsigned-short))
  1)

(deftest callbacks.int
    (expect-int-sum (callback sum-int))
  1)

(deftest callbacks.unsigned-int
    (expect-unsigned-int-sum (callback sum-unsigned-int))
  1)

(deftest callbacks.long
    (expect-long-sum (callback sum-long))
  1)

(deftest callbacks.unsigned-long
    (expect-unsigned-long-sum (callback sum-unsigned-long))
  1)

#-cffi-sys::no-long-long
(progn
  (deftest callbacks.long-long
      (expect-long-long-sum (callback sum-long-long))
    1)

  (deftest callbacks.unsigned-long-long
      (expect-unsigned-long-long-sum (callback sum-unsigned-long-long))
    1))

(deftest callbacks.float
    (expect-float-sum (callback sum-float))
  1)

(deftest callbacks.double
    (expect-double-sum (callback sum-double))
  1)

#+long-float
(deftest callbacks.long-double
    (expect-long-double-sum (callback sum-long-double))
  1)

(deftest callbacks.pointer
    (expect-pointer-sum (callback sum-pointer))
  1)

(deftest callbacks.string
    (expect-strcat (callback lisp-strcat))
  1)

#-cffi-sys::no-foreign-funcall
(defcallback return-a-string-not-nil :string ()
  "abc")

#-cffi-sys::no-foreign-funcall
(deftest callbacks.string-not-docstring
    (foreign-funcall-pointer (callback return-a-string-not-nil) () :string)
  "abc")

(defcallback check-for-nil :boolean ((pointer :pointer))
  (null pointer))

#-cffi-sys::no-foreign-funcall
(deftest callbacks.nil-for-null
    (foreign-funcall-pointer (callback check-for-nil) nil
                             :pointer (null-pointer) :boolean)
  nil)

;;; This one tests mem-aref too.
#(defcfun "qsort" :void
#  (base :pointer)
#  (nmemb :int)
#  (size :int)
#  (fun-compar :pointer))

$cffi->defcfun( qsort => ':void',
  [ base => $JGoff::Lisp::CFFI::pointer ],
  [ nmemb => $JGoff::Lisp::CFFI::int ],
  [ size => $JGoff::Lisp::CFFI::int ],
  [ fun_compar => $JGoff::Lisp::CFFI::pointer ] );

#
# Woo, maybe overload here?
#
(defcallback < :int ((a :pointer) (b :pointer))
  (let ((x (mem-ref a :int))
        (y (mem-ref b :int)))
    (cond ((> x y) 1)
          ((< x y) -1)
          (t 0))))

(deftest callbacks.qsort
    (with-foreign-object (array :int 10)
      ;; Initialize array.
      (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
            do (setf (mem-aref array :int i) n))
      ;; Sort it.
      (qsort array 10 (foreign-type-size :int) (callback <))
      ;; Return it as a list.
      (loop for i from 0 below 10
            collect (mem-aref array :int i)))
  (1 2 3 4 5 6 7 8 9 10))

#;;; void callback
#(defparameter *int* -1)

$int = -1;

#(defcfun "pass_int_ref" :void (f :pointer))

$cffi->defcfun( pass_int_ref => ':void', [ f => $JGoff::Lisp::CFFI::pointer ] );

#;;; CMUCL chokes on this one for some reason.
#(defcallback read-int-from-pointer :void ((a :pointer))
#  (setq *int* (mem-ref a :int)))

$cffi->defcallback( read_int_from_pointer => ':void',
  [ [ a => $JGoff::Lisp::CFFI::pointer ] ],
  sub {
    $int = $cffi->mem_ref( $_{a} => ':int' )
  }
);

(deftest callbacks.void
    (progn
      (pass-int-ref (callback read-int-from-pointer))
      *int*)
  1984)

;;; test funcalling of a callback and also declarations inside
;;; callbacks.

##-cffi-sys::no-foreign-funcall
#(progn
if ( $JGoff::Lisp::CFFI::no_foreign_funcall ) {
  #(defcallback sum-2 :int ((a :int) (b :int) (c :int))
  #  (declare (ignore c))
  #  (+ a b))
  $cffi->defcallback( sum_2 => ':int',
    [ [ a => ':int' ], [ b => ':int' ], [ c => ':int' ] ],
    sub {
      $_{a} + $_{b}
    }
  );

  (deftest callbacks.funcall.1
      (foreign-funcall-pointer (callback sum-2) () :int 2 :int 3 :int 1 :int)
    5)

  #(defctype foo-float :float)

  my $foo_float;
  $cffi->defctype( \$foo_float => ':float' );

  #(defcallback sum-2f foo-float
  #    ((a foo-float) (b foo-float) (c foo-float) (d foo-float) (e foo-float))
  #  "This one ignores the middle 3 arguments."
  #  (declare (ignore b c))
  #  (declare (ignore d))
  #  (+ a e))

  $cffi->defcallback( sum_2f => $foo_float,
    [ [ a => $foo_float ], [ b => $foo_float ], [ c => $foo_float ], [ d => $foo_float ], [ e => $foo_float ] ],
    sub {
      $_{a} + $_{e}
    }
  );

  (deftest callbacks.funcall.2
      (foreign-funcall-pointer (callback sum-2f) () foo-float 1.0 foo-float 2.0
                               foo-float 3.0 foo-float 4.0 foo-float 5.0
                               foo-float)
    6.0))
}

;;; (cb-test :no-long-long t)

#(defcfun "call_sum_127_no_ll" :long (cb :pointer))

$cffi->defcfun( call_sum_127_no_ll => ':long', [ cb => $JGoff::Lisp::CFFI::pointer ] );

;;; CMUCL, ECL and CCL choke on this one.
#.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:or) '(:and)))
(defcallback sum-127-no-ll :long
    ((a1 :unsigned-long) (a2 :pointer) (a3 :long) (a4 :double)
     (a5 :unsigned-long) (a6 :float) (a7 :float) (a8 :int) (a9 :unsigned-int)
     (a10 :double) (a11 :double) (a12 :double) (a13 :pointer)
     (a14 :unsigned-short) (a15 :unsigned-short) (a16 :pointer) (a17 :long)
     (a18 :long) (a19 :int) (a20 :short) (a21 :unsigned-short)
     (a22 :unsigned-short) (a23 :char) (a24 :long) (a25 :pointer) (a26 :pointer)
     (a27 :char) (a28 :unsigned-char) (a29 :unsigned-long) (a30 :short)
     (a31 :int) (a32 :int) (a33 :unsigned-char) (a34 :short) (a35 :long)
     (a36 :long) (a37 :pointer) (a38 :unsigned-short) (a39 :char) (a40 :double)
     (a41 :unsigned-short) (a42 :pointer) (a43 :short) (a44 :unsigned-long)
     (a45 :unsigned-short) (a46 :float) (a47 :unsigned-char) (a48 :short)
     (a49 :float) (a50 :short) (a51 :char) (a52 :unsigned-long)
     (a53 :unsigned-long) (a54 :char) (a55 :float) (a56 :long) (a57 :pointer)
     (a58 :short) (a59 :float) (a60 :unsigned-int) (a61 :float)
     (a62 :unsigned-int) (a63 :double) (a64 :unsigned-int) (a65 :unsigned-char)
     (a66 :int) (a67 :long) (a68 :char) (a69 :short) (a70 :double) (a71 :int)
     (a72 :pointer) (a73 :char) (a74 :unsigned-short) (a75 :pointer)
     (a76 :unsigned-short) (a77 :pointer) (a78 :unsigned-long) (a79 :double)
     (a80 :pointer) (a81 :long) (a82 :float) (a83 :unsigned-short)
     (a84 :unsigned-short) (a85 :pointer) (a86 :float) (a87 :int)
     (a88 :unsigned-int) (a89 :double) (a90 :float) (a91 :long) (a92 :pointer)
     (a93 :unsigned-short) (a94 :float) (a95 :unsigned-char) (a96 :unsigned-char)
     (a97 :float) (a98 :unsigned-int) (a99 :float) (a100 :unsigned-short)
     (a101 :double) (a102 :unsigned-short) (a103 :unsigned-long)
     (a104 :unsigned-int) (a105 :unsigned-long) (a106 :pointer)
     (a107 :unsigned-char) (a108 :char) (a109 :char) (a110 :unsigned-short)
     (a111 :unsigned-long) (a112 :float) (a113 :short) (a114 :pointer)
     (a115 :long) (a116 :unsigned-short) (a117 :short) (a118 :double)
     (a119 :short) (a120 :int) (a121 :char) (a122 :unsigned-long) (a123 :long)
     (a124 :int) (a125 :pointer) (a126 :double) (a127 :unsigned-char))
  (let ((args (list a1 (pointer-address a2) a3 (floor a4) a5 (floor a6)
                    (floor a7) a8 a9 (floor a10) (floor a11) (floor a12)
                    (pointer-address a13) a14 a15 (pointer-address a16) a17 a18
                    a19 a20 a21 a22 a23 a24 (pointer-address a25)
                    (pointer-address a26) a27 a28 a29 a30 a31 a32 a33 a34 a35
                    a36 (pointer-address a37) a38 a39 (floor a40) a41
                    (pointer-address a42) a43 a44 a45 (floor a46) a47 a48
                    (floor a49) a50 a51 a52 a53 a54 (floor a55) a56
                    (pointer-address a57) a58 (floor a59) a60 (floor a61) a62
                    (floor a63) a64 a65 a66 a67 a68 a69 (floor a70) a71
                    (pointer-address a72) a73 a74 (pointer-address a75) a76
                    (pointer-address a77) a78 (floor a79) (pointer-address a80)
                    a81 (floor a82) a83 a84 (pointer-address a85) (floor a86)
                    a87 a88 (floor a89) (floor a90) a91 (pointer-address a92)
                    a93 (floor a94) a95 a96 (floor a97) a98 (floor a99) a100
                    (floor a101) a102 a103 a104 a105 (pointer-address a106) a107
                    a108 a109 a110 a111 (floor a112) a113 (pointer-address a114)
                    a115 a116 a117 (floor a118) a119 a120 a121 a122 a123 a124
                    (pointer-address a125) (floor a126) a127)))
    (loop for i from 1 and arg in args do
          (format t "a~A: ~A~%" i arg))
    (reduce #'+ args)))

#+#.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:and) '(:or))
(deftest callbacks.bff.1
    (call-sum-127-no-ll (callback sum-127-no-ll))
  2008547941)

;;; (cb-test)

#-cffi-sys::no-long-long
      #.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(or) '(and))
(progn
  (defcfun "call_sum_127" :long-long (cb :pointer))

  ;;; CMUCL, ECL and CCL choke on this one.
  (defcallback sum-127 :long-long
      ((a1 :short) (a2 :char) (a3 :pointer) (a4 :float) (a5 :long) (a6 :double)
       (a7 :unsigned-long-long) (a8 :unsigned-short) (a9 :unsigned-char)
       (a10 :char) (a11 :char) (a12 :unsigned-short) (a13 :unsigned-long-long)
       (a14 :unsigned-short) (a15 :long-long) (a16 :unsigned-short)
       (a17 :unsigned-long-long) (a18 :unsigned-char) (a19 :unsigned-char)
       (a20 :unsigned-long-long) (a21 :long-long) (a22 :char) (a23 :float)
       (a24 :unsigned-int) (a25 :float) (a26 :float) (a27 :unsigned-int)
       (a28 :float) (a29 :char) (a30 :unsigned-char) (a31 :long) (a32 :long-long)
       (a33 :unsigned-char) (a34 :double) (a35 :long) (a36 :double)
       (a37 :unsigned-int) (a38 :unsigned-short) (a39 :long-long)
       (a40 :unsigned-int) (a41 :int) (a42 :unsigned-long-long) (a43 :long)
       (a44 :short) (a45 :unsigned-int) (a46 :unsigned-int)
       (a47 :unsigned-long-long) (a48 :unsigned-int) (a49 :long) (a50 :pointer)
       (a51 :unsigned-char) (a52 :char) (a53 :long-long) (a54 :unsigned-short)
       (a55 :unsigned-int) (a56 :float) (a57 :unsigned-char) (a58 :unsigned-long)
       (a59 :long-long) (a60 :float) (a61 :long) (a62 :float) (a63 :int)
       (a64 :float) (a65 :unsigned-short) (a66 :unsigned-long-long) (a67 :short)
       (a68 :unsigned-long) (a69 :long) (a70 :char) (a71 :unsigned-short)
       (a72 :long-long) (a73 :short) (a74 :double) (a75 :pointer)
       (a76 :unsigned-int) (a77 :char) (a78 :unsigned-int) (a79 :pointer)
       (a80 :pointer) (a81 :unsigned-char) (a82 :pointer) (a83 :unsigned-short)
       (a84 :unsigned-char) (a85 :long) (a86 :pointer) (a87 :char) (a88 :long)
       (a89 :unsigned-short) (a90 :unsigned-char) (a91 :double)
       (a92 :unsigned-long-long) (a93 :unsigned-short) (a94 :unsigned-short)
       (a95 :unsigned-int) (a96 :long) (a97 :char) (a98 :long) (a99 :char)
       (a100 :short) (a101 :unsigned-short) (a102 :unsigned-long)
       (a103 :unsigned-long) (a104 :short) (a105 :long-long) (a106 :long-long)
       (a107 :long-long) (a108 :double) (a109 :unsigned-short)
       (a110 :unsigned-char) (a111 :short) (a112 :unsigned-char) (a113 :long)
       (a114 :long-long) (a115 :unsigned-long-long) (a116 :unsigned-int)
       (a117 :unsigned-long) (a118 :unsigned-char) (a119 :long-long)
       (a120 :unsigned-char) (a121 :unsigned-long-long) (a122 :double)
       (a123 :unsigned-char) (a124 :long-long) (a125 :unsigned-char)
       (a126 :char) (a127 :long-long))
    (+ a1 a2 (pointer-address a3) (values (floor a4)) a5 (values (floor a6))
       a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 a21 a22
       (values (floor a23)) a24 (values (floor a25)) (values (floor a26))
       a27 (values (floor a28)) a29 a30 a31 a32 a33 (values (floor a34))
       a35 (values (floor a36)) a37 a38 a39 a40 a41 a42 a43 a44 a45 a46 a47
       a48 a49 (pointer-address a50) a51 a52 a53 a54 a55 (values (floor a56))
       a57 a58 a59 (values (floor a60)) a61 (values (floor a62)) a63
       (values (floor a64)) a65 a66 a67 a68 a69 a70 a71 a72 a73
       (values (floor a74)) (pointer-address a75) a76 a77 a78
       (pointer-address a79) (pointer-address a80) a81 (pointer-address a82)
       a83 a84 a85 (pointer-address a86) a87 a88 a89 a90 (values (floor a91))
       a92 a93 a94 a95 a96 a97 a98 a99 a100 a101 a102 a103 a104 a105 a106 a107
       (values (floor a108)) a109 a110 a111 a112 a113 a114 a115 a116 a117 a118
       a119 a120 a121 (values (floor a122)) a123 a124 a125 a126 a127))

  (deftest callbacks.bff.2
      (call-sum-127 (callback sum-127))
    8166570665645582011))

;;; regression test: (callback non-existant-callback) should throw an error
(deftest callbacks.non-existant
    (not (null (nth-value 1 (ignore-errors (callback doesnt-exist)))))
  t)

;;; Handling many arguments of type double. Many lisps (used to) fail
;;; this one on darwin/ppc. This test might be bogus due to floating
;;; point arithmetic rounding errors.
;;;
;;; CMUCL chokes on this one.
(defcallback double26 :double
    ((a1 :double) (a2 :double) (a3 :double) (a4 :double) (a5 :double)
     (a6 :double) (a7 :double) (a8 :double) (a9 :double) (a10 :double)
     (a11 :double) (a12 :double) (a13 :double) (a14 :double) (a15 :double)
     (a16 :double) (a17 :double) (a18 :double) (a19 :double) (a20 :double)
     (a21 :double) (a22 :double) (a23 :double) (a24 :double) (a25 :double)
     (a26 :double))
  (let ((args (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
                    a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26)))
    (loop for i from 1 and arg in args do
          (format t "a~A: ~A~%" i arg))
    (reduce #'+ args)))

(defcfun "call_double26" :double (f :pointer))

(deftest callbacks.double26
    (call-double26 (callback double26))
  81.64d0)

#-cffi-sys::no-foreign-funcall
(deftest callbacks.double26.funcall
    (foreign-funcall-pointer
     (callback double26) () :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double 3.14d0 :double 3.14d0 :double 3.14d0 :double 3.14d0
     :double)
  81.64d0)

;;; Same as above, for floats.
(defcallback float26 :float
    ((a1 :float) (a2 :float) (a3 :float) (a4 :float) (a5 :float)
     (a6 :float) (a7 :float) (a8 :float) (a9 :float) (a10 :float)
     (a11 :float) (a12 :float) (a13 :float) (a14 :float) (a15 :float)
     (a16 :float) (a17 :float) (a18 :float) (a19 :float) (a20 :float)
     (a21 :float) (a22 :float) (a23 :float) (a24 :float) (a25 :float)
     (a26 :float))
  (let ((args (list a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15
                    a16 a17 a18 a19 a20 a21 a22 a23 a24 a25 a26)))
    (loop for i from 1 and arg in args do
          (format t "a~A: ~A~%" i arg))
    (reduce #'+ args)))

(defcfun "call_float26" :float (f :pointer))

(deftest callbacks.float26
    (call-float26 (callback float26))
  130.0)

#-cffi-sys::no-foreign-funcall
(deftest callbacks.float26.funcall
    (foreign-funcall-pointer
     (callback float26) () :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float 5.0 :float 5.0 :float 5.0 :float 5.0
     :float)
  130.0)

;;; Defining a callback as a non-toplevel form. Not portable. Doesn't
;;; work for CMUCL or Allegro.
(let ((n 42))
  (defcallback non-toplevel-cb :int ()
    n))

(deftest callbacks.non-toplevel
    (foreign-funcall (callback non-toplevel-cb) :int)
  42)

;;;# Stdcall

#+(and x86 (not cffi-sys::no-stdcall))
(progn
  (defcallback (stdcall-cb :convention :stdcall) :int
      ((a :int) (b :int) (c :int))
    (+ a b c))

  (defcfun "call_stdcall_fun" :int
    (f :pointer))

  (deftest callbacks.stdcall.1
      (call-stdcall-fun (callback stdcall-cb))
    42))

;;; RT: many of the %DEFCALLBACK implementations wouldn't handle
;;;     uninterned symbols.
(deftest callbacks.uninterned
    (values (defcallback #1=#:foo :void ())
            (pointerp (callback #1#)))
  #1# t)

# }}}

# {{{ defcfun.lisp

(deftest defcfun.parse-name-and-options.1
    (multiple-value-bind (lisp-name foreign-name)
        (let ((*package* (find-package '#:cffi-tests)))
          (cffi::parse-name-and-options "foo_bar"))
      (list lisp-name foreign-name))
  (foo-bar "foo_bar"))

(deftest defcfun.parse-name-and-options.2
    (multiple-value-bind (lisp-name foreign-name)
        (let ((*package* (find-package '#:cffi-tests)))
          (cffi::parse-name-and-options "foo_bar" t))
      (list lisp-name foreign-name))
  (*foo-bar* "foo_bar"))

(deftest defcfun.parse-name-and-options.3
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options 'foo-bar)
      (list lisp-name foreign-name))
  (foo-bar "foo_bar"))

(deftest defcfun.parse-name-and-options.4
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '*foo-bar* t)
      (list lisp-name foreign-name))
  (*foo-bar* "foo_bar"))

(deftest defcfun.parse-name-and-options.5
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '("foo_bar" foo-baz))
      (list lisp-name foreign-name))
  (foo-baz "foo_bar"))

(deftest defcfun.parse-name-and-options.6
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '("foo_bar" *foo-baz*) t)
      (list lisp-name foreign-name))
  (*foo-baz* "foo_bar"))

(deftest defcfun.parse-name-and-options.7
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '(foo-baz "foo_bar"))
      (list lisp-name foreign-name))
  (foo-baz "foo_bar"))

(deftest defcfun.parse-name-and-options.8
    (multiple-value-bind (lisp-name foreign-name)
        (cffi::parse-name-and-options '(*foo-baz* "foo_bar") t)
      (list lisp-name foreign-name))
  (*foo-baz* "foo_bar"))

;;;# Name translation

(deftest translate-underscore-separated-name.to-symbol
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-underscore-separated-name "some_name_with_underscores"))
  some-name-with-underscores)

(deftest translate-underscore-separated-name.to-string
    (translate-underscore-separated-name 'some-name-with-underscores)
  "some_name_with_underscores")

(deftest translate-camelcase-name.to-symbol
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-camelcase-name "someXmlFunction"))
  some-xml-function)

(deftest translate-camelcase-name.to-string
    (translate-camelcase-name 'some-xml-function)
  "someXmlFunction")

(deftest translate-camelcase-name.to-string-upper
    (translate-camelcase-name 'some-xml-function :upper-initial-p t)
  "SomeXmlFunction")

(deftest translate-camelcase-name.to-symbol-special
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-camelcase-name "someXMLFunction" :special-words '("XML")))
  some-xml-function)

(deftest translate-camelcase-name.to-string-special
    (translate-camelcase-name 'some-xml-function :special-words '("XML"))
  "someXMLFunction")

(deftest translate-name-from-foreign.function
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-name-from-foreign "some_xml_name" *package*))
  some-xml-name)

(deftest translate-name-from-foreign.var
    (let ((*package* (find-package '#:cffi-tests)))
      (translate-name-from-foreign "some_xml_name" *package* t))
  *some-xml-name*)

(deftest translate-name-to-foreign.function
    (translate-name-to-foreign 'some-xml-name *package*)
  "some_xml_name")

(deftest translate-name-to-foreign.var
    (translate-name-to-foreign '*some-xml-name* *package* t)
  "some_xml_name")

;;;# Calling with built-in c types
;;;
;;; Tests calling standard C library functions both passing
;;; and returning each built-in type. (adapted from funcall.lisp)

(defcfun "toupper" :char
  "toupper docstring"
  (char :char))

(deftest defcfun.char
    (toupper (char-code #\a))
  #.(char-code #\A))

(deftest defcfun.docstring
    (documentation 'toupper 'function)
  "toupper docstring")


(defcfun ("abs" c-abs) :int
  (n :int))

(deftest defcfun.int
    (c-abs -100)
  100)


(defcfun "labs" :long
  (n :long))

(deftest defcfun.long
    (labs -131072)
  131072)


#-cffi-features:no-long-long
(progn
  (defcfun "my_llabs" :long-long
    (n :long-long))

  (deftest defcfun.long-long
      (my-llabs -9223372036854775807)
    9223372036854775807)

  (defcfun "ullong" :unsigned-long-long
    (n :unsigned-long-long))

  (deftest defcfun.unsigned-long-long
      (let ((ullong-max (1- (expt 2 (* 8 (foreign-type-size :unsigned-long-long))))))
        (eql ullong-max (ullong ullong-max)))
    t))


(defcfun "my_sqrtf" :float
  (n :float))

(deftest defcfun.float
    (my-sqrtf 16.0)
  4.0)


(defcfun ("sqrt" c-sqrt) :double
  (n :double))

(deftest defcfun.double
    (c-sqrt 36.0d0)
  6.0d0)


#+long-float
(defcfun ("sqrtl" c-sqrtl) :long-double
  (n :long-double))

#+long-float
(deftest defcfun.long-double
    (c-sqrtl 36.0l0)
  6.0l0)


(defcfun "strlen" :int
  (n :string))

(deftest defcfun.string.1
    (strlen "Hello")
  5)


(defcfun "strcpy" (:pointer :char)
  (dest (:pointer :char))
  (src :string))

(defcfun "strcat" (:pointer :char)
  (dest (:pointer :char))
  (src :string))

(deftest defcfun.string.2
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (strcpy s "Hello")
      (strcat s ", world!"))
  "Hello, world!")

(defcfun "strerror" :string
  (n :int))

(deftest defcfun.string.3
    (typep (strerror 1) 'string)
  t)


;;; Regression test. Allegro would warn on direct calls to
;;; functions with no arguments.
;;;
;;; Also, let's check if void functions will return NIL.
;;;
;;; Check if a docstring without arguments doesn't cause problems.

(defcfun "noargs" :int
  "docstring")

(deftest defcfun.noargs
    (noargs)
  42)

(defcfun "noop" :void)

(deftest defcfun.noop
    (noop)
  #|no values|#)

;;;# Calling varargs functions

(defcfun "sprintf" :int
  "sprintf docstring"
  (str (:pointer :char))
  (control :string)
  &rest)

(deftest defcfun.varargs.docstrings
    (documentation 'sprintf 'function)
  "sprintf docstring")

(deftest defcfun.varargs.char
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%c" :char 65))
  "A")

(deftest defcfun.varargs.short
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%d" :short 42))
  "42")

(deftest defcfun.varargs.int
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%d" :int 1000))
  "1000")

(deftest defcfun.varargs.long
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%ld" :long 131072))
  "131072")

(deftest defcfun.varargs.float
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%.2f" :float (float pi)))
  "3.14")

(deftest defcfun.varargs.double
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%.2f" :double (float pi 1.0d0)))
  "3.14")

#+long-float
(deftest defcfun.varargs.long-double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (sprintf s "%.2Lf" :long-double pi))
  "3.14")

(deftest defcfun.varargs.string
    (with-foreign-pointer-as-string (s 100)
      (sprintf s "%s, %s!" :string "Hello" :string "world"))
  "Hello, world!")

;;; (let ((rettype (find-type :long))
;;;       (arg-types (n-random-types-no-ll 127)))
;;;   (c-function rettype arg-types)
;;;   (gen-function-test rettype arg-types))

#+(and #.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:and) '(:or)))
(progn
  (defcfun "sum_127_no_ll" :long
    (a1 :long) (a2 :unsigned-long) (a3 :short) (a4 :unsigned-short) (a5 :float)
    (a6 :double) (a7 :unsigned-long) (a8 :float) (a9 :unsigned-char)
    (a10 :unsigned-short) (a11 :short) (a12 :unsigned-long) (a13 :double)
    (a14 :long) (a15 :unsigned-int) (a16 :pointer) (a17 :unsigned-int)
    (a18 :unsigned-short) (a19 :long) (a20 :float) (a21 :pointer) (a22 :float)
    (a23 :int) (a24 :int) (a25 :unsigned-short) (a26 :long) (a27 :long)
    (a28 :double) (a29 :unsigned-char) (a30 :unsigned-int) (a31 :unsigned-int)
    (a32 :int) (a33 :unsigned-short) (a34 :unsigned-int) (a35 :pointer)
    (a36 :double) (a37 :double) (a38 :long) (a39 :short) (a40 :unsigned-short)
    (a41 :long) (a42 :char) (a43 :long) (a44 :unsigned-short) (a45 :pointer)
    (a46 :int) (a47 :unsigned-int) (a48 :double) (a49 :unsigned-char)
    (a50 :unsigned-char) (a51 :float) (a52 :int) (a53 :unsigned-short)
    (a54 :double) (a55 :short) (a56 :unsigned-char) (a57 :unsigned-long)
    (a58 :float) (a59 :float) (a60 :float) (a61 :pointer) (a62 :pointer)
    (a63 :unsigned-int) (a64 :unsigned-long) (a65 :char) (a66 :short)
    (a67 :unsigned-short) (a68 :unsigned-long) (a69 :pointer) (a70 :float)
    (a71 :double) (a72 :long) (a73 :unsigned-long) (a74 :short)
    (a75 :unsigned-int) (a76 :unsigned-short) (a77 :int) (a78 :unsigned-short)
    (a79 :char) (a80 :double) (a81 :short) (a82 :unsigned-char) (a83 :float)
    (a84 :char) (a85 :int) (a86 :double) (a87 :unsigned-char) (a88 :int)
    (a89 :unsigned-long) (a90 :double) (a91 :short) (a92 :short)
    (a93 :unsigned-int) (a94 :unsigned-char) (a95 :float) (a96 :long)
    (a97 :float) (a98 :long) (a99 :long) (a100 :int) (a101 :int)
    (a102 :unsigned-int) (a103 :char) (a104 :char) (a105 :unsigned-short)
    (a106 :unsigned-int) (a107 :unsigned-short) (a108 :unsigned-short)
    (a109 :int) (a110 :long) (a111 :char) (a112 :double) (a113 :unsigned-int)
    (a114 :char) (a115 :short) (a116 :unsigned-long) (a117 :unsigned-int)
    (a118 :short) (a119 :unsigned-char) (a120 :float) (a121 :pointer)
    (a122 :double) (a123 :int) (a124 :long) (a125 :char) (a126 :unsigned-short)
    (a127 :float))

  (deftest defcfun.bff.1
      (sum-127-no-ll
       1442906394 520035521 -4715 50335 -13557.0 -30892.0d0 24061483 -23737.0
       22 2348 4986 104895680 8073.0d0 -571698147 102484400
       (make-pointer 507907275) 12733353 7824 -1275845284 13602.0
       (make-pointer 286958390) -8042.0 -773681663 -1289932452 31199 -154985357
       -170994216 16845.0d0 177 218969221 2794350893 6068863 26327 127699339
       (make-pointer 184352771) 18512.0d0 -12345.0d0 -179853040 -19981 37268
       -792845398 116 -1084653028 50494 (make-pointer 2105239646) -1710519651
       1557813312 2839.0d0 90 180 30580.0 -532698978 8623 9537.0d0 -10882 54
       184357206 14929.0 -8190.0 -25615.0 (make-pointer 235310526)
       (make-pointer 220476977) 7476055 1576685 -117 -11781 31479 23282640
       (make-pointer 8627281) -17834.0 10391.0d0 -1904504370 114393659 -17062
       637873619 16078 -891210259 8107 0 760.0d0 -21268 104 14133.0 10
       588598141 310.0d0 20 1351785456 16159552 -10121.0d0 -25866 24821
       68232851 60 -24132.0 -1660411658 13387.0 -786516668 -499825680
       -1128144619 111849719 2746091587 -2 95 14488 326328135 64781 18204
       150716680 -703859275 103 16809.0d0 852235610 -43 21088 242356110
       324325428 -22380 23 24814.0 (make-pointer 40362014) -14322.0d0
       -1864262539 523684371 -21 49995 -29175.0)
    796447501))

;;; (let ((rettype (find-type :long-long))
;;;       (arg-types (n-random-types 127)))
;;;   (c-function rettype arg-types)
;;;   (gen-function-test rettype arg-types))

#-cffi-sys::no-long-long
      #.(cl:if (cl:>= cl:lambda-parameters-limit 127) '(:or) '(:and))
(progn
  (defcfun "sum_127" :long-long
    (a1 :pointer) (a2 :pointer) (a3 :float) (a4 :unsigned-long) (a5 :pointer)
    (a6 :long-long) (a7 :double) (a8 :double) (a9 :unsigned-short) (a10 :int)
    (a11 :long-long) (a12 :long) (a13 :short) (a14 :unsigned-int) (a15 :long)
    (a16 :unsigned-char) (a17 :int) (a18 :double) (a19 :short) (a20 :short)
    (a21 :long-long) (a22 :unsigned-int) (a23 :unsigned-short) (a24 :short)
    (a25 :pointer) (a26 :short) (a27 :unsigned-short) (a28 :unsigned-short)
    (a29 :int) (a30 :long-long) (a31 :pointer) (a32 :int) (a33 :unsigned-long)
    (a34 :unsigned-long) (a35 :pointer) (a36 :unsigned-long-long) (a37 :float)
    (a38 :int) (a39 :short) (a40 :pointer) (a41 :unsigned-long-long)
    (a42 :long-long) (a43 :unsigned-long) (a44 :unsigned-long)
    (a45 :unsigned-long-long) (a46 :unsigned-long) (a47 :char) (a48 :double)
    (a49 :long) (a50 :unsigned-int) (a51 :int) (a52 :short) (a53 :pointer)
    (a54 :long) (a55 :unsigned-long-long) (a56 :int) (a57 :unsigned-short)
    (a58 :unsigned-long-long) (a59 :float) (a60 :pointer) (a61 :float)
    (a62 :unsigned-short) (a63 :unsigned-long) (a64 :float) (a65 :unsigned-int)
    (a66 :unsigned-long-long) (a67 :pointer) (a68 :double)
    (a69 :unsigned-long-long) (a70 :double) (a71 :double) (a72 :long-long)
    (a73 :pointer) (a74 :unsigned-short) (a75 :long) (a76 :pointer) (a77 :short)
    (a78 :double) (a79 :long) (a80 :unsigned-char) (a81 :pointer)
    (a82 :unsigned-char) (a83 :long) (a84 :double) (a85 :pointer) (a86 :int)
    (a87 :double) (a88 :unsigned-char) (a89 :double) (a90 :short) (a91 :long)
    (a92 :int) (a93 :long) (a94 :double) (a95 :unsigned-short)
    (a96 :unsigned-int) (a97 :int) (a98 :char) (a99 :long-long) (a100 :double)
    (a101 :float) (a102 :unsigned-long) (a103 :short) (a104 :pointer)
    (a105 :float) (a106 :long-long) (a107 :int) (a108 :long-long)
    (a109 :long-long) (a110 :double) (a111 :unsigned-long-long) (a112 :double)
    (a113 :unsigned-long) (a114 :char) (a115 :char) (a116 :unsigned-long)
    (a117 :short) (a118 :unsigned-char) (a119 :unsigned-char) (a120 :int)
    (a121 :int) (a122 :float) (a123 :unsigned-char) (a124 :unsigned-char)
    (a125 :double) (a126 :unsigned-long-long) (a127 :char))

  (deftest defcfun.bff.2
      (sum-127
       (make-pointer 2746181372) (make-pointer 177623060) -32334.0 3158055028
       (make-pointer 242315091) 4288001754991016425 -21047.0d0 287.0d0 18722
       243379286 -8677366518541007140 581399424 -13872 4240394881 1353358999
       226 969197676 -26207.0d0 6484 11150 1241680089902988480 106068320 61865
       2253 (make-pointer 866809333) -31613 35616 11715 1393601698
       8940888681199591845 (make-pointer 1524606024) 805638893 3315410736
       3432596795 (make-pointer 1490355706) 696175657106383698 -25438.0
       1294381547 26724 (make-pointer 3196569545) 2506913373410783697
       -4405955718732597856 4075932032 3224670123 2183829215657835866
       1318320964 -22 -3786.0d0 -2017024146 1579225515 -626617701 -1456
       (make-pointer 3561444187) 395687791 1968033632506257320 -1847773261
       48853 142937735275669133 -17974.0 (make-pointer 2791749948) -14140.0
       2707 3691328585 3306.0 1132012981 303633191773289330
       (make-pointer 981183954) 9114.0d0 8664374572369470 -19013.0d0
       -10288.0d0 -3679345119891954339 (make-pointer 3538786709) 23761
       -154264605 (make-pointer 2694396308) 7023 997.0d0 1009561368 241
       (make-pointer 2612292671) 48 1431872408 -32675.0d0
       (make-pointer 1587599336) 958916472 -9857.0d0 111 -14370.0d0 -7308
       -967514912 488790941 2146978095 -24111.0d0 13711 86681861 717987770
       111 1013402998690933877 17234.0d0 -8772.0 3959216275 -8711
       (make-pointer 3142780851) 9480.0 -3820453146461186120 1616574376
       -3336232268263990050 -1906114671562979758 -27925.0d0 9695970875869913114
       27033.0d0 1096518219 -12 104 3392025403 -27911 60 89 509297051
       -533066551 29158.0 110 54 -9802.0d0 593950442165910888 -79)
    7758614658402721936))

;;; regression test: defining an undefined foreign function should only
;;; throw some sort of warning, not signal an error.

(deftest defcfun.undefined
    (progn
      (eval '(defcfun ("undefined_foreign_function" undefined-foreign-function) :void))
      (compile 'undefined-foreign-function)
      t)
  t)

;;; Test whether all doubles are passed correctly. On some platforms, eg.
;;; darwin/ppc, some are passed on registers others on the stack.
(defcfun "sum_double26" :double
  (a1 :double) (a2 :double) (a3 :double) (a4 :double) (a5 :double)
  (a6 :double) (a7 :double) (a8 :double) (a9 :double) (a10 :double)
  (a11 :double) (a12 :double) (a13 :double) (a14 :double) (a15 :double)
  (a16 :double) (a17 :double) (a18 :double) (a19 :double) (a20 :double)
  (a21 :double) (a22 :double) (a23 :double) (a24 :double) (a25 :double)
  (a26 :double))

(deftest defcfun.double26
    (sum-double26 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0 3.14d0
                  3.14d0 3.14d0 3.14d0 3.14d0 3.14d0)
  81.64d0)

;;; Same as above for floats.
(defcfun "sum_float26" :float
  (a1 :float) (a2 :float) (a3 :float) (a4 :float) (a5 :float)
  (a6 :float) (a7 :float) (a8 :float) (a9 :float) (a10 :float)
  (a11 :float) (a12 :float) (a13 :float) (a14 :float) (a15 :float)
  (a16 :float) (a17 :float) (a18 :float) (a19 :float) (a20 :float)
  (a21 :float) (a22 :float) (a23 :float) (a24 :float) (a25 :float)
  (a26 :float))

(deftest defcfun.float26
    (sum-float26 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0
                 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0 5.0)
  130.0)

;;;# Namespaces

#-cffi-sys::flat-namespace
(progn
  (defcfun ("ns_function" ns-fun1 :library libtest) :boolean)
  (defcfun ("ns_function" ns-fun2 :library libtest2) :boolean)

  (deftest defcfun.namespace.1
      (values (ns-fun1) (ns-fun2))
    t nil))

;;;# stdcall

#+(and x86 windows (not cffi-sys::no-stdcall))
(progn
  (defcfun ("stdcall_fun@12" stdcall-fun :convention :stdcall) :int
    (a :int)
    (b :int)
    (c :int))

  (deftest defcfun.stdcall.1
      (loop repeat 100 do (stdcall-fun 1 2 3)
            finally (return (stdcall-fun 1 2 3)))
    6))

# }}}

# {{{ enum.lisp

#(defcenum numeros
#  (:one 1)
#  :two
#  :three
#  :four
#  (:forty-one 41)
#  :forty-two)

my $numeros;
$cffi-defcenum( \$numeros,
  [ ':one' => 1 ],
  ':two',
  ':three',
  ':four',
  [ ':forty-one' => 41 ],
  ':forty-two' );

#(defcfun "check_enums" :int
#  (one numeros)
#  (two numeros)
#  (three numeros)
#  (four numeros)
#  (forty-one numeros)
#  (forty-two numeros))

$cffi->defcfun( check_enums => ':int',
  [ one => $numeros ],
  [ two => $numeros ],
  [ three => $numeros ],
  [ four => $numeros ],
  [ forty-one => $numeros ],
  [ forty-two => $numeros ] );

(deftest enum.1
    (check-enums :one :two :three 4 :forty-one :forty-two)
  1)

#(defcenum another-boolean :false :true)
#(defcfun "return_enum" another-boolean (x :int))

my $another_boolean
$cffi->defcenum( \$another_boolean, ':false', ':true' );
$cffi->defcfun( return_enum => $another_boolean, [ x => ':int' ] );

(deftest enum.2
    (and (eq :false (return-enum 0))
         (eq :true (return-enum 1)))
  t)

#(defctype yet-another-boolean another-boolean)
#(defcfun ("return_enum" return-enum2) yet-another-boolean
#  (x yet-another-boolean))

my $yet_another_boolean;
$cffi->defctype( \$yet_another_boolean, $another_boolean );
$cffi->defcfun( [ return_enum => $return_enum2 ] => $yet_another_boolean,
  [ x => $yet_another_boolean ] );

(deftest enum.3
    (and (eq :false (return-enum2 :false))
         (eq :true (return-enum2 :true)))
  t)

;;;# Bitfield tests

;;; Regression test: defbitfield was misbehaving when the first value
;;; was provided.
(deftest bitfield.1
    (eval '(defbitfield bf1
             (:foo 0)))
  bf1)

#(defbitfield bf2
#  one
#  two
#  four
#  eight
#  sixteen
#  thirty-two
#  sixty-four)

my $bf2;
$cffi->defbitfield( \$bf2,
  'one',
  'two',
  'four',
  'eight',
  'sixteen',
  'thirty-two',
  'sixty-four' );

(deftest bitfield.2
    (mapcar (lambda (symbol)
              (foreign-bitfield-value 'bf2 (list symbol)))
            '(one two four eight sixteen thirty-two sixty-four))
  (1 2 4 8 16 32 64))

#(defbitfield bf3
#  (three 3)
#  one
#  (seven 7)
#  two
#  (eight 8)
#  sixteen)

my $bf3;
$cffi->defbitfield( \$bf3,
  [ three => 3 ],
  'one',
  [ seven => 7 ],
  'two',
  [ eight => 8 ],
  'sixteen' );

;;; Non-single-bit numbers must not influence the progression of
;;; implicit values.  Single bits larger than any before *must*
;;; influence said progression.
(deftest bitfield.3
    (mapcar (lambda (symbol)
              (foreign-bitfield-value 'bf3 (list symbol)))
            '(one two sixteen))
  (1 2 16))

#(defbitfield bf4
#  (zero 0)
#  one)

my $bf4;
$cffi->defbitfield( \$bf4,
  [ zero => 0 ],
  'one' );

;;; Yet another edge case with the 0...
(deftest bitfield.4
    (foreign-bitfield-value 'bf4 '(one))
  1)

# }}}

# {{{ foreign-globals.lisp

#(defcvar ("var_char" *char-var*)  :char)
#(defcvar "var_unsigned_char"      :unsigned-char)
#(defcvar "var_short"              :short)
#(defcvar "var_unsigned_short"     :unsigned-short)
#(defcvar "var_int"                :int)
#(defcvar "var_unsigned_int"       :unsigned-int)
#(defcvar "var_long"               :long)
#(defcvar "var_unsigned_long"      :unsigned-long)
#(defcvar "var_float"              :float)
#(defcvar "var_double"             :double)
#(defcvar "var_pointer"            :pointer)
#(defcvar "var_string"             :string)
#(defcvar "var_long_long"          :long-long)
#(defcvar "var_unsigned_long_long" :unsigned-long-long)

my ( $var_char, $var_unsigned_char, $var_short, $var_unsigned_short,
     $var_int, $var_unsigned_int, $var_long, $var_unsigned_long,
     $var_float, $var_double, $var_pointer, $var_string,
     $var_long_long, $var_unsigned_long );
$cffi->defcvar( [ var_char => $char_var ],  ':char' );
$cffi->defcvar( var_unsigned_char =>      ':unsigned-char' );
$cffi->defcvar( var_short =>              ':short' );
$cffi->defcvar( var_unsigned_short =>     ':unsigned-short' );
$cffi->defcvar( var_int =>                ':int' );
$cffi->defcvar( var_unsigned_int =>       ':unsigned-int' );
$cffi->defcvar( var_long =>               ':long' );
$cffi->defcvar( var_unsigned_long =>      ':unsigned-long' );
$cffi->defcvar( var_float =>              ':float' );
$cffi->defcvar( var_double =>             ':double' );
$cffi->defcvar( var_pointer =>            $JGoff::Lisp::CFFI::pointer );
$cffi->defcvar( var_string =>             ':string' );
$cffi->defcvar( var_long_long =>          ':long-long' );
$cffi->defcvar( var_unsigned_long_long => ':unsigned-long-long' );

;;; The expected failures marked below result from this odd behaviour:
;;;
;;;   (foreign-symbol-pointer "var_char") => NIL
;;;
;;;   (foreign-symbol-pointer "var_char" :library 'libtest)
;;;     => #<Pointer to type :VOID = #xF7F50740>
;;;
(deftest foreign-globals.ref.char
    *char-var*
  -127)

(deftest foreign-globals.ref.unsigned-char
    *var-unsigned-char*
  255)

(deftest foreign-globals.ref.short
    *var-short*
  -32767)

(deftest foreign-globals.ref.unsigned-short
    *var-unsigned-short*
  65535)

(deftest foreign-globals.ref.int
    *var-int*
  -32767)

(deftest foreign-globals.ref.unsigned-int
    *var-unsigned-int*
  65535)

(deftest foreign-globals.ref.long
    *var-long*
  -2147483647)

(deftest foreign-globals.ref.unsigned-long
    *var-unsigned-long*
  4294967295)

(deftest foreign-globals.ref.float
    *var-float*
  42.0)

(deftest foreign-globals.ref.double
    *var-double*
  42.0d0)

(deftest foreign-globals.ref.pointer
    (null-pointer-p *var-pointer*)
  t)

(deftest foreign-globals.ref.string
    *var-string*
  "Hello, foreign world!")

(deftest foreign-globals.ref.long-long
    *var-long-long*
  -9223372036854775807)

(deftest foreign-globals.ref.unsigned-long-long
    *var-unsigned-long-long*
  18446744073709551615)

;; The *.set.* tests restore the old values so that the *.ref.*
;; don't fail when re-run.
(defmacro with-old-value-restored ((place) &body body)
  (let ((old (gensym)))
    `(let ((,old ,place))
       (prog1
           (progn ,@body)
         (setq ,place ,old)))))

(deftest foreign-globals.set.int
    (with-old-value-restored (*var-int*)
      (setq *var-int* 42)
      *var-int*)
  42)

(deftest foreign-globals.set.string
    (with-old-value-restored (*var-string*)
      (setq *var-string* "Ehxosxangxo")
      (prog1
          *var-string*
        ;; free the string we just allocated
        (foreign-free (mem-ref (get-var-pointer '*var-string*) :pointer))))
  "Ehxosxangxo")

(deftest foreign-globals.set.long-long
    (with-old-value-restored (*var-long-long*)
      (setq *var-long-long* -9223000000000005808)
      *var-long-long*)
  -9223000000000005808)

(deftest foreign-globals.get-var-pointer.1
    (pointerp (get-var-pointer '*char-var*))
  t)

(deftest foreign-globals.get-var-pointer.2
    (mem-ref (get-var-pointer '*char-var*) :char)
  -127)

;;; Symbol case.

#(defcvar "UPPERCASEINT1"     :int)
#(defcvar "UPPER_CASE_INT1"   :int)
#(defcvar "MiXeDCaSeInT1"     :int)
#(defcvar "MiXeD_CaSe_InT1"   :int)

$cffi->defcvar( \$

(deftest foreign-globals.ref.uppercaseint1
    *uppercaseint1*
  12345)

(deftest foreign-globals.ref.upper-case-int1
    *upper-case-int1*
  23456)

(deftest foreign-globals.ref.mixedcaseint1
    *mixedcaseint1*
  34567)

(deftest foreign-globals.ref.mixed-case-int1
    *mixed-case-int1*
  45678)

(when (string= (symbol-name 'nil) "NIL")
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (eval (read-from-string "(defcvar \"UPPERCASEINT2\"     :int)"))
    (eval (read-from-string "(defcvar \"UPPER_CASE_INT2\"   :int)"))
    (eval (read-from-string "(defcvar \"MiXeDCaSeInT2\"     :int)"))
    (eval (read-from-string "(defcvar \"MiXeD_CaSe_InT2\"   :int)"))
    (setf (readtable-case *readtable*) :preserve)
    (eval (read-from-string "(DEFCVAR \"UPPERCASEINT3\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"UPPER_CASE_INT3\"   :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeDCaSeInT3\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeD_CaSe_InT3\"   :INT)"))))


;;; EVAL gets rid of SBCL's unreachable code warnings.
(when (string= (symbol-name (eval nil)) "nil")
  (let ((*readtable* (copy-readtable)))
    (setf (readtable-case *readtable*) :invert)
    (eval (read-from-string "(DEFCVAR \"UPPERCASEINT2\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"UPPER_CASE_INT2\"   :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeDCaSeInT2\"     :INT)"))
    (eval (read-from-string "(DEFCVAR \"MiXeD_CaSe_InT2\"   :INT)"))
    (setf (readtable-case *readtable*) :downcase)
    (eval (read-from-string "(defcvar \"UPPERCASEINT3\"     :int)"))
    (eval (read-from-string "(defcvar \"UPPER_CASE_INT3\"   :int)"))
    (eval (read-from-string "(defcvar \"MiXeDCaSeInT3\"     :int)"))
    (eval (read-from-string "(defcvar \"MiXeD_CaSe_InT3\"   :int)"))))

(deftest foreign-globals.ref.uppercaseint2
    *uppercaseint2*
  12345)

(deftest foreign-globals.ref.upper-case-int2
    *upper-case-int2*
  23456)

(deftest foreign-globals.ref.mixedcaseint2
    *mixedcaseint2*
  34567)

(deftest foreign-globals.ref.mixed-case-int2
    *mixed-case-int2*
  45678)

(deftest foreign-globals.ref.uppercaseint3
    *uppercaseint3*
  12345)

(deftest foreign-globals.ref.upper-case-int3
    *upper-case-int3*
  23456)

(deftest foreign-globals.ref.mixedcaseint3
    *mixedcaseint3*
  34567)

(deftest foreign-globals.ref.mixed-case-int3
    *mixed-case-int3*
  45678)

;;; regression test:
;;; gracefully accept symbols in defcvar

(defcvar *var-char* :char)
(defcvar var-char :char)

(deftest foreign-globals.symbol-name
    (values *var-char* var-char)
  -127 -127)

;;;# Namespace

#-cffi-sys::flat-namespace
(progn
  (deftest foreign-globals.namespace.1
      (values
       (mem-ref (foreign-symbol-pointer "var_char" :library 'libtest) :char)
       (foreign-symbol-pointer "var_char" :library 'libtest2))
    -127 nil)

  (deftest foreign-globals.namespace.2
      (values
       (mem-ref (foreign-symbol-pointer "ns_var" :library 'libtest) :boolean)
       (mem-ref (foreign-symbol-pointer "ns_var" :library 'libtest2) :boolean))
    t nil)

  ;; For its "default" module, Lispworks seems to cache lookups from
  ;; the newest module tried.  If a lookup happens to have failed
  ;; subsequent lookups will fail even the symbol exists in other
  ;; modules.  So this test fails.
  (deftest foreign-globals.namespace.3
      (values
       (foreign-symbol-pointer "var_char" :library 'libtest2)
       (mem-ref (foreign-symbol-pointer "var_char") :char))
    nil -127)

  (defcvar ("ns_var" *ns-var1* :library libtest) :boolean)
  (defcvar ("ns_var" *ns-var2* :library libtest2) :boolean)

  (deftest foreign-globals.namespace.4
      (values *ns-var1* *ns-var2*)
    t nil))

;;;# Read-only

(defcvar ("var_char" *var-char-ro* :read-only t) :char
  "docstring")

(deftest foreign-globals.read-only.1
    (values *var-char-ro*
            (ignore-errors (setf *var-char-ro* 12)))
  -127 nil)

(deftest defcvar.docstring
    (documentation '*var-char-ro* 'variable)
  "docstring")

;;;# Other tests

;;; RT: FOREIGN-SYMBOL-POINTER shouldn't signal an error when passed
;;; an undefined variable.
(deftest foreign-globals.undefined.1
    (foreign-symbol-pointer "surely-undefined?")
  nil)

(deftest foreign-globals.error.1
    (handler-case (foreign-symbol-pointer 'not-a-string)
      (type-error () t))
  t)

# }}}

# {{{ fsbv.lisp

;; Requires struct.lisp

(defcfun "sumpair" :int
  (p (:struct struct-pair)))

(defcfun "doublepair" (:struct struct-pair)
  (p (:struct struct-pair)))

(defcfun "prodsumpair" :double
  (p (:struct struct-pair+double)))

(defcfun "doublepairdouble" (:struct struct-pair+double)
  (p (:struct struct-pair+double)))

;;; Call struct by value
(deftest fsbv.1
    (sumpair '(1 . 2))
  3)

;;; Call and return struct by value
(deftest fsbv.2
    (doublepair '(1 . 2))
  (2 . 4))

;;; Call recursive structure by value
(deftest fsbv.3
    (prodsumpair '(pr (a 4 b 5) dbl 2.5d0))
  22.5d0)

;;; Call and return recursive structure by value
(deftest fsbv.4
    (let ((ans (doublepairdouble '(pr (a 4 b 5) dbl 2.5d0))))
      (values (getf (getf ans 'pr) 'a)
	      (getf (getf ans 'pr) 'b)
	      (getf ans 'dbl)))
  8
  10
  5.0d0)

;;; Typedef fsbv test

(defcfun ("sumpair" sumpair2) :int
  (p struct-pair-typedef1))

(deftest fsbv.5
    (sumpair2 '(1 . 2))
  3)

;;; Test ulonglong on no-long-long implementations.

(defcfun "ullsum" :unsigned-long-long
  (a :unsigned-long-long) (b :unsigned-long-long))

(deftest fsbv.6
    (ullsum #x10DEADBEEF #x2300000000)
  #x33DEADBEEF)

# }}}

# {{{ funcall

;;;# Calling with Built-In C Types
;;;
;;; Tests calling standard C library functions both passing and
;;; returning each built-in type.

;;; Don't run these tests if the implementation does not support
;;; foreign-funcall.
#-cffi-sys::no-foreign-funcall
(progn

(deftest funcall.char
    (foreign-funcall "toupper" :char (char-code #\a) :char)
  #.(char-code #\A))

(deftest funcall.int.1
    (foreign-funcall "abs" :int -100 :int)
  100)

(defun funcall-abs (n)
  (foreign-funcall "abs" :int n :int))

;;; regression test: lispworks's %foreign-funcall based on creating
;;; and caching foreign-funcallables at macro-expansion time.
(deftest funcall.int.2
    (funcall-abs -42)
  42)

(deftest funcall.long
    (foreign-funcall "labs" :long -131072 :long)
  131072)

#-cffi-sys::no-long-long
(deftest funcall.long-long
    (foreign-funcall "my_llabs" :long-long -9223372036854775807 :long-long)
  9223372036854775807)

#-cffi-sys::no-long-long
(deftest funcall.unsigned-long-long
    (let ((ullong-max (1- (expt 2 (* 8 (foreign-type-size :unsigned-long-long))))))
      (eql ullong-max
           (foreign-funcall "ullong" :unsigned-long-long ullong-max
                                     :unsigned-long-long)))
  t)

(deftest funcall.float
    (foreign-funcall "my_sqrtf" :float 16.0 :float)
  4.0)

(deftest funcall.double
    (foreign-funcall "sqrt" :double 36.0d0 :double)
  6.0d0)

#+long-float
(deftest funcall.long-double
    (foreign-funcall "sqrtl" :long-double 36.0l0 :long-double)
  6.0l0)

(deftest funcall.string.1
    (foreign-funcall "strlen" :string "Hello" :int)
  5)

(deftest funcall.string.2
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "strcpy" :pointer s :string "Hello" :pointer)
      (foreign-funcall "strcat" :pointer s :string ", world!" :pointer))
  "Hello, world!")

(deftest funcall.string.3
    (with-foreign-pointer (ptr 100)
      (lisp-string-to-foreign "Hello, " ptr 8)
      (foreign-funcall "strcat" :pointer ptr :string "world!" :string))
  "Hello, world!")

;;;# Calling Varargs Functions

;; The CHAR argument must be passed as :INT because chars are promoted
;; to ints when passed as variable arguments.
(deftest funcall.varargs.char
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%c" :int 65 :int))
  "A")

(deftest funcall.varargs.int
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%d" :int 1000 :int))
  "1000")

(deftest funcall.varargs.long
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%ld" :long 131072 :int))
  "131072")

;;; There is no FUNCALL.VARARGS.FLOAT as floats are promoted to double
;;; when passed as variable arguments.  Currently this fails in SBCL
;;; and CMU CL on Darwin/ppc.
(deftest funcall.varargs.double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%.2f"
                       :double (coerce pi 'double-float) :int))
  "3.14")

#+long-float
(deftest funcall.varargs.long-double
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%.2Lf"
                       :long-double pi :int))
  "3.14")

(deftest funcall.varargs.string
    (with-foreign-pointer-as-string (s 100)
      (setf (mem-ref s :char) 0)
      (foreign-funcall "sprintf" :pointer s :string "%s, %s!"
                       :string "Hello" :string "world" :int))
  "Hello, world!")

;;; See DEFCFUN.DOUBLE26.
(deftest funcall.double26
    (foreign-funcall "sum_double26"
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double 3.14d0
                     :double 3.14d0 :double 3.14d0 :double)
  81.64d0)

;;; See DEFCFUN.FLOAT26.
(deftest funcall.float26
    (foreign-funcall "sum_float26"
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float 5.0 :float 5.0 :float 5.0 :float 5.0
                     :float 5.0 :float)
  130.0)

;;; Funcalling a pointer.
(deftest funcall.f-s-p.1
    (foreign-funcall-pointer (foreign-symbol-pointer "abs") nil :int -42 :int)
  42)

;;;# Namespaces

#-cffi-sys::flat-namespace
(deftest funcall.namespace.1
    (values (foreign-funcall ("ns_function" :library libtest) :boolean)
            (foreign-funcall ("ns_function" :library libtest2) :boolean))
  t nil)

;;;# stdcall

#+(and x86 windows (not cffi-sys::no-stdcall))
(deftest funcall.stdcall.1
    (flet ((fun ()
             (foreign-funcall ("stdcall_fun@12" :convention :stdcall)
                              :int 1 :int 2 :int 3 :int)))
      (loop repeat 100 do (fun)
            finally (return (fun))))
  6)

;;; RT: NIL arguments are skipped

(defvar *nil-skipped*)

(define-foreign-type check-nil-skip-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser check-nil-skip-type))

(defmethod expand-to-foreign (val (type check-nil-skip-type))
  (declare (ignore val))
  (setf *nil-skipped* nil)
  (null-pointer))

(deftest funcall.nil-skip
    (let ((*nil-skipped* t))
      (compile nil '(lambda ()
                     (foreign-funcall "abs" check-nil-skip-type nil)))
      *nil-skipped*)
  nil)

;;; RT: CLISP returns NIL instead of a null-pointer

(deftest funcall.pointer-not-nil
    (not (null (foreign-funcall "strchr" :string "" :int 1 :pointer)))
  t)

) ;; #-cffi-sys::no-foreign-funcall

# }}}

# {{{ memory

(deftest deref.char
    (with-foreign-object (p :char)
      (setf (mem-ref p :char) -127)
      (mem-ref p :char))
  -127)

(deftest deref.unsigned-char
    (with-foreign-object (p :unsigned-char)
      (setf (mem-ref p :unsigned-char) 255)
      (mem-ref p :unsigned-char))
  255)

(deftest deref.short
    (with-foreign-object (p :short)
      (setf (mem-ref p :short) -32767)
      (mem-ref p :short))
  -32767)

(deftest deref.unsigned-short
    (with-foreign-object (p :unsigned-short)
      (setf (mem-ref p :unsigned-short) 65535)
      (mem-ref p :unsigned-short))
  65535)

(deftest deref.int
    (with-foreign-object (p :int)
      (setf (mem-ref p :int) -131072)
      (mem-ref p :int))
  -131072)

(deftest deref.unsigned-int
    (with-foreign-object (p :unsigned-int)
      (setf (mem-ref p :unsigned-int) 262144)
      (mem-ref p :unsigned-int))
  262144)

(deftest deref.long
    (with-foreign-object (p :long)
      (setf (mem-ref p :long) -536870911)
      (mem-ref p :long))
  -536870911)

(deftest deref.unsigned-long
    (with-foreign-object (p :unsigned-long)
      (setf (mem-ref p :unsigned-long) 536870912)
      (mem-ref p :unsigned-long))
  536870912)

(deftest deref.long-long
    (with-foreign-object (p :long-long)
      (setf (mem-ref p :long-long) -9223372036854775807)
      (mem-ref p :long-long))
  -9223372036854775807)

(deftest deref.unsigned-long-long
    (with-foreign-object (p :unsigned-long-long)
      (setf (mem-ref p :unsigned-long-long) 18446744073709551615)
      (mem-ref p :unsigned-long-long))
  18446744073709551615)

(deftest deref.float.1
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) 0.0)
      (mem-ref p :float))
  0.0)

(deftest deref.float.2
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) *float-max*)
      (mem-ref p :float))
  #.*float-max*)

(deftest deref.float.3
    (with-foreign-object (p :float)
      (setf (mem-ref p :float) *float-min*)
      (mem-ref p :float))
  #.*float-min*)

(deftest deref.double.1
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) 0.0d0)
      (mem-ref p :double))
  0.0d0)

(deftest deref.double.2
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) *double-max*)
      (mem-ref p :double))
  #.*double-max*)

(deftest deref.double.3
    (with-foreign-object (p :double)
      (setf (mem-ref p :double) *double-min*)
      (mem-ref p :double))
  #.*double-min*)

;;; TODO: use something like *DOUBLE-MIN/MAX* above once we actually
;;; have an available lisp that supports long double.
#+long-double
(progn
  (deftest deref.long-double.1
      (with-foreign-object (p :long-double)
        (setf (mem-ref p :long-double) 0.0l0)
        (mem-ref p :long-double))
    0.0l0)

  (deftest deref.long-double.2
      (with-foreign-object (p :long-double)
        (setf (mem-ref p :long-double) most-positive-long-float)
        (mem-ref p :long-double))
    #.most-positive-long-float)

  (deftest deref.long-double.3
      (with-foreign-object (p :long-double)
        (setf (mem-ref p :long-double) least-positive-long-float)
        (mem-ref p :long-double))
    #.least-positive-long-float))

;;; make sure the lisp doesn't convert NULL to NIL
(deftest deref.pointer.null
    (with-foreign-object (p :pointer)
      (setf (mem-ref p :pointer) (null-pointer))
      (null-pointer-p (mem-ref p :pointer)))
  t)

;;; regression test. lisp-string-to-foreign should handle empty strings
(deftest lisp-string-to-foreign.empty
    (with-foreign-pointer (str 2)
      (setf (mem-ref str :unsigned-char) 42)
      (lisp-string-to-foreign "" str 1)
      (mem-ref str :unsigned-char))
  0)

;;; regression test. with-foreign-pointer shouldn't evaluate
;;; the size argument twice.
(deftest with-foreign-pointer.evalx2
    (let ((count 0))
      (with-foreign-pointer (x (incf count) size-var)
        (values count size-var)))
  1 1)

(defconstant +two+ 2)

;;; regression test. cffi-allegro's with-foreign-pointer wasn't
;;; handling constants properly.
(deftest with-foreign-pointer.constant-size
    (with-foreign-pointer (p +two+ size)
      size)
  2)

(deftest mem-ref.left-to-right
    (let ((i 0))
      (with-foreign-object (p :char 3)
        (setf (mem-ref p :char 0) 66 (mem-ref p :char 1) 92)
        (setf (mem-ref p :char (incf i)) (incf i))
        (values (mem-ref p :char 0) (mem-ref p :char 1) i)))
  66 2 2)

;;; This needs to be in a real function for at least Allegro CL or the
;;; compiler macro on %MEM-REF is not expanded and the test doesn't
;;; actually test anything!
(defun %mem-ref-left-to-right ()
  (let ((result nil))
    (with-foreign-object (p :char)
      (%mem-set 42 p :char)
      (%mem-ref (progn (push 1 result) p) :char (progn (push 2 result) 0))
      (nreverse result))))

;;; Test left-to-right evaluation of the arguments to %MEM-REF when
;;; optimized by the compiler macro.
(deftest %mem-ref.left-to-right
    (%mem-ref-left-to-right)
  (1 2))

;;; This needs to be in a top-level function for at least Allegro CL
;;; or the compiler macro on %MEM-SET is not expanded and the test
;;; doesn't actually test anything!
(defun %mem-set-left-to-right ()
  (let ((result nil))
    (with-foreign-object (p :char)
      (%mem-set (progn (push 1 result) 0)
                (progn (push 2 result) p)
                :char
                (progn (push 3 result) 0))
      (nreverse result))))

;;; Test left-to-right evaluation of the arguments to %MEM-SET when
;;; optimized by the compiler macro.
(deftest %mem-set.left-to-right
    (%mem-set-left-to-right)
  (1 2 3))

;; regression test. mem-aref's setf expansion evaluated its type argument twice.
(deftest mem-aref.eval-type-x2
    (let ((count 0))
      (with-foreign-pointer (p 1)
        (setf (mem-aref p (progn (incf count) :char) 0) 127))
      count)
  1)

(deftest mem-aref.left-to-right
    (let ((count -1))
      (with-foreign-pointer (p 2)
        (values
         (setf (mem-aref p (progn (incf count) :char) (incf count)) (incf count))
         (setq count -1)
         (mem-aref (progn (incf count) p) :char (incf count))
         count)))
  2 -1 2 1)

;; regression tests. nested mem-ref's and mem-aref's had bogus getters
(deftest mem-ref.nested
    (with-foreign-object (p :pointer)
      (with-foreign-object (i :int)
        (setf (mem-ref p :pointer) i)
        (setf (mem-ref i :int) 42)
        (setf (mem-ref (mem-ref p :pointer) :int) 1984)
        (mem-ref i :int)))
  1984)

(deftest mem-aref.nested
    (with-foreign-object (p :pointer)
      (with-foreign-object (i :int 2)
        (setf (mem-aref p :pointer 0) i)
        (setf (mem-aref i :int 1) 42)
        (setf (mem-aref (mem-ref p :pointer 0) :int 1) 1984)
        (mem-aref i :int 1)))
  1984)

(cffi:defcstruct mem-aref.bare-struct
  (a :uint8))

$cffi->defcstruct( \$mem_aref_bare_struct, [ a => ':uint8' ] );

;;; regression test: although mem-aref was dealing with bare struct
;;; types as though they were pointers, it wasn't calculating the
;;; proper offsets. The offsets for bare structs types should be
;;; calculated as aggregate types.
(deftest mem-aref.bare-struct
    (with-foreign-object (a 'mem-aref.bare-struct 2)
      (eql (- (pointer-address (cffi:mem-aref a 'mem-aref.bare-struct 1))
              (pointer-address (cffi:mem-aref a 'mem-aref.bare-struct 0)))
           (foreign-type-size '(:struct mem-aref.bare-struct))))
  t)

;;; regression tests. dereferencing an aggregate type. dereferencing a
;;; struct should return a pointer to the struct itself, not return the
;;; first 4 bytes (or whatever the size of :pointer is) as a pointer.
;;;
;;; This important for accessing an array of structs, which is
;;; what the deref.array-of-aggregates test does.
(defcstruct some-struct (x :int))

(deftest deref.aggregate
    (with-foreign-object (s 'some-struct)
      (pointer-eq s (mem-ref s 'some-struct)))
  t)

(deftest deref.array-of-aggregates
    (with-foreign-object (arr 'some-struct 3)
      (loop for i below 3
            do (setf (foreign-slot-value (mem-aref arr 'some-struct i)
                                         'some-struct 'x)
                     112))
      (loop for i below 3
            collect (foreign-slot-value (mem-aref arr 'some-struct i)
                                        'some-struct 'x)))
  (112 112 112))

;;; pointer operations
(deftest pointer.1
    (pointer-address (make-pointer 42))
  42)

;;; I suppose this test is not very good. --luis
(deftest pointer.2
    (pointer-address (null-pointer))
  0)

(deftest pointer.null
    (nth-value 0 (ignore-errors (null-pointer-p nil)))
  nil)

(deftest foreign-pointer-type.nil
    (typep nil 'foreign-pointer)
  nil)

;;; Ensure that a pointer to the highest possible address can be
;;; created using MAKE-POINTER.  Regression test for CLISP/X86-64.
(deftest make-pointer.high
    (let* ((pointer-length (foreign-type-size :pointer))
           (high-address (1- (expt 2 (* pointer-length 8))))
           (pointer (make-pointer high-address)))
      (- high-address (pointer-address pointer)))
  0)

;;; Ensure that incrementing a pointer by zero bytes returns an
;;; equivalent pointer.
(deftest inc-pointer.zero
    (with-foreign-object (x :int)
      (pointer-eq x (inc-pointer x 0)))
  t)

;;; Test the INITIAL-ELEMENT keyword argument to FOREIGN-ALLOC.
(deftest foreign-alloc.1
    (let ((ptr (foreign-alloc :int :initial-element 42)))
      (unwind-protect
           (mem-ref ptr :int)
        (foreign-free ptr)))
  42)

;;; Test the INITIAL-ELEMENT and COUNT arguments to FOREIGN-ALLOC.
(deftest foreign-alloc.2
    (let ((ptr (foreign-alloc :int :count 4 :initial-element 100)))
      (unwind-protect
           (loop for i from 0 below 4
                 collect (mem-aref ptr :int i))
        (foreign-free ptr)))
  (100 100 100 100))

;;; Test the INITIAL-CONTENTS and COUNT arguments to FOREIGN-ALLOC,
;;; passing a list of initial values.
(deftest foreign-alloc.3
    (let ((ptr (foreign-alloc :int :count 4 :initial-contents '(4 3 2 1))))
      (unwind-protect
           (loop for i from 0 below 4
                 collect (mem-aref ptr :int i))
        (foreign-free ptr)))
  (4 3 2 1))

;;; Test INITIAL-CONTENTS and COUNT with FOREIGN-ALLOC passing a
;;; vector of initial values.
(deftest foreign-alloc.4
    (let ((ptr (foreign-alloc :int :count 4 :initial-contents #(10 20 30 40))))
      (unwind-protect
           (loop for i from 0 below 4
                 collect (mem-aref ptr :int i))
        (foreign-free ptr)))
  (10 20 30 40))

;;; Ensure calling FOREIGN-ALLOC with both INITIAL-ELEMENT and
;;; INITIAL-CONTENTS signals an error.
(deftest foreign-alloc.5
    (values
     (ignore-errors
       (let ((ptr (foreign-alloc :int :initial-element 1
                                 :initial-contents '(1))))
         (foreign-free ptr))
       t))
  nil)

;;; Regression test: FOREIGN-ALLOC shouldn't actually perform translation
;;; on initial-element/initial-contents since MEM-AREF will do that already.
(define-foreign-type not-an-int ()
  ()
  (:actual-type :int)
  (:simple-parser not-an-int))

(defmethod translate-to-foreign (value (type not-an-int))
  (assert (not (integerp value)))
  0)

(deftest foreign-alloc.6
    (let ((ptr (foreign-alloc 'not-an-int :initial-element 'foooo)))
      (foreign-free ptr)
      t)
  t)

;;; Ensure calling FOREIGN-ALLOC with NULL-TERMINATED-P and a non-pointer
;;; type signals an error.
(deftest foreign-alloc.7
    (values
     (ignore-errors
       (let ((ptr (foreign-alloc :int :null-terminated-p t)))
         (foreign-free ptr))
       t))
  nil)

#;;; The opposite of the above test.
#(defctype pointer-alias :pointer)

my $pointer_alias;
$cffi->defctype( \$pointer_alias, $JGoff::Lisp::CFFI::pointer );

(deftest foreign-alloc.8
    (progn
      (foreign-free (foreign-alloc 'pointer-alias :count 0 :null-terminated-p t))
      t)
  t)

;;; Ensure calling FOREIGN-ALLOC with NULL-TERMINATED-P actually places
;;; a null pointer at the end. Not a very reliable test apparently.
(deftest foreign-alloc.9
    (let ((ptr (foreign-alloc :pointer :count 0 :null-terminated-p t)))
      (unwind-protect
           (null-pointer-p (mem-ref ptr :pointer))
        (foreign-free ptr)))
  t)

;;; RT: FOREIGN-ALLOC with :COUNT 0 on CLISP signalled an error.
(deftest foreign-alloc.10
    (foreign-free (foreign-alloc :char :count 0))
  nil)

;;; Tests for mem-ref with a non-constant type. This is a way to test
;;; the functional interface (without compiler macros).

(deftest deref.nonconst.char
    (let ((type :char))
      (with-foreign-object (p type)
        (setf (mem-ref p type) -127)
        (mem-ref p type)))
  -127)

(deftest deref.nonconst.unsigned-char
    (let ((type :unsigned-char))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 255)
        (mem-ref p type)))
  255)

(deftest deref.nonconst.short
    (let ((type :short))
      (with-foreign-object (p type)
        (setf (mem-ref p type) -32767)
        (mem-ref p type)))
  -32767)

(deftest deref.nonconst.unsigned-short
    (let ((type :unsigned-short))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 65535)
        (mem-ref p type)))
  65535)

(deftest deref.nonconst.int
    (let ((type :int))
      (with-foreign-object (p type)
        (setf (mem-ref p type) -131072)
        (mem-ref p type)))
  -131072)

(deftest deref.nonconst.unsigned-int
    (let ((type :unsigned-int))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 262144)
        (mem-ref p type)))
  262144)

(deftest deref.nonconst.long
    (let ((type :long))
      (with-foreign-object (p type)
        (setf (mem-ref p type) -536870911)
        (mem-ref p type)))
  -536870911)

(deftest deref.nonconst.unsigned-long
    (let ((type :unsigned-long))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 536870912)
        (mem-ref p type)))
  536870912)

(deftest deref.nonconst.long-long
    (let ((type :long-long))
      (with-foreign-object (p type)
        (setf (mem-ref p type) -9223372036854775807)
        (mem-ref p type)))
  -9223372036854775807)

(deftest deref.nonconst.unsigned-long-long
    (let ((type :unsigned-long-long))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 18446744073709551615)
        (mem-ref p type)))
  18446744073709551615)

(deftest deref.nonconst.float.1
    (let ((type :float))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 0.0)
        (mem-ref p type)))
  0.0)

(deftest deref.nonconst.float.2
    (let ((type :float))
      (with-foreign-object (p type)
        (setf (mem-ref p type) *float-max*)
        (mem-ref p type)))
  #.*float-max*)

(deftest deref.nonconst.float.3
    (let ((type :float))
      (with-foreign-object (p type)
        (setf (mem-ref p type) *float-min*)
        (mem-ref p type)))
  #.*float-min*)

(deftest deref.nonconst.double.1
    (let ((type :double))
      (with-foreign-object (p type)
        (setf (mem-ref p type) 0.0d0)
        (mem-ref p type)))
  0.0d0)

(deftest deref.nonconst.double.2
    (let ((type :double))
      (with-foreign-object (p type)
        (setf (mem-ref p type) *double-max*)
        (mem-ref p type)))
  #.*double-max*)

(deftest deref.nonconst.double.3
    (let ((type :double))
      (with-foreign-object (p type)
        (setf (mem-ref p type) *double-min*)
        (mem-ref p type)))
  #.*double-min*)

;;; regression tests: lispworks's %mem-ref and %mem-set compiler
;;; macros were misbehaving.

(defun mem-ref-rt-1 ()
  (with-foreign-object (a :int 2)
    (setf (mem-aref a :int 0) 123
          (mem-aref a :int 1) 456)
    (values (mem-aref a :int 0) (mem-aref a :int 1))))

(deftest mem-ref.rt.1
    (mem-ref-rt-1)
  123 456)

(defun mem-ref-rt-2 ()
  (with-foreign-object (a :double 2)
    (setf (mem-aref a :double 0) 123.0d0
          (mem-aref a :double 1) 456.0d0)
    (values (mem-aref a :double 0) (mem-aref a :double 1))))

(deftest mem-ref.rt.2
    (mem-ref-rt-2)
  123.0d0 456.0d0)

(deftest incf-pointer.1
    (let ((ptr (null-pointer)))
      (incf-pointer ptr)
      (pointer-address ptr))
  1)

(deftest incf-pointer.2
    (let ((ptr (null-pointer)))
      (incf-pointer ptr 42)
      (pointer-address ptr))
  42)

(deftest pointerp.1
    (values
     (pointerp (null-pointer))
     (null-pointer-p (null-pointer))
     (typep (null-pointer) 'foreign-pointer))
  t t t)

(deftest pointerp.2
    (let ((p (make-pointer #xFEFF)))
      (values
       (pointerp p)
       (typep p 'foreign-pointer)))
  t t)

(deftest pointerp.3
    (pointerp 'not-a-pointer)
  nil)

(deftest pointerp.4
    (pointerp 42)
  nil)

(deftest pointerp.5
    (pointerp 0)
  nil)

(deftest pointerp.6
    (pointerp nil)
  nil)

(deftest mem-ref.setf.1
    (with-foreign-object (p :char)
      (setf (mem-ref p :char) 42))
  42)

(define-foreign-type int+1 ()
  ()
  (:actual-type :int)
  (:simple-parser int+1))

(defmethod translate-to-foreign (value (type int+1))
  (1+ value))

(defmethod translate-from-foreign (value (type int+1))
  (1+ value))

(deftest mem-ref.setf.2
    (with-foreign-object (p 'int+1)
      (values (setf (mem-ref p 'int+1) 42)
              (mem-ref p 'int+1)))
  42 ; should this be 43?
  44)

(deftest pointer-eq.non-pointers.1
    (expecting-error (pointer-eq 1 2))
  :error)

(deftest pointer-eq.non-pointers.2
    (expecting-error (pointer-eq 'a 'b))
  :error)

(deftest null-pointer-p.non-pointer.1
    (expecting-error (null-pointer-p 'not-a-pointer))
  :error)

(deftest null-pointer-p.non-pointer.2
    (expecting-error (null-pointer-p 0))
  :error)

(deftest null-pointer-p.non-pointer.3
    (expecting-error (null-pointer-p nil))
  :error)

# }}}

# {{{ misc-types

(defcfun ("my_strdup" strdup) :string+ptr (str :string))

(defcfun ("my_strfree" strfree) :void (str :pointer))

(deftest misc-types.string+ptr
    (destructuring-bind (string pointer)
        (strdup "foo")
      (strfree pointer)
      string)
  "foo")

(deftest misc-types.string+ptr.ub8
    (destructuring-bind (string pointer)
        (strdup (make-array 3 :element-type '(unsigned-byte 8)
                            :initial-contents (map 'list #'char-code "foo")))
      (strfree pointer)
      string)
  "foo")

(deftest misc-types.string.ub8.1
    (let ((array (make-array 7 :element-type '(unsigned-byte 8)
                             :initial-contents '(84 117 114 97 110 103 97))))
      (with-foreign-string (foreign-string array)
        (foreign-string-to-lisp foreign-string)))
  "Turanga")

(deftest misc-types.string.ub8.2
    (let ((str (foreign-string-alloc
                (make-array 7 :element-type '(unsigned-byte 8)
                            :initial-contents '(84 117 114 97 110 103 97)))))
      (prog1 (foreign-string-to-lisp str)
        (foreign-string-free str)))
  "Turanga")

(defcfun "equalequal" :boolean
  (a (:boolean :int))
  (b (:boolean :unsigned-int)))

(defcfun "bool_and" (:boolean :char)
  (a (:boolean :unsigned-char))
  (b (:boolean :char)))

(defcfun "bool_xor" (:boolean :unsigned-long)
  (a (:boolean :long))
  (b (:boolean :unsigned-long)))

(deftest misc-types.boolean.1
    (list (equalequal nil nil)
          (equalequal t t)
          (equalequal t 23)
          (bool-and 'a 'b)
          (bool-and "foo" nil)
          (bool-xor t nil)
          (bool-xor nil nil))
  (t t t t nil t nil))


;;; Regression test: boolean type only worked with canonicalized
;;; built-in integer types. Should work for any type that canonicalizes
;;; to a built-in integer type.
#(defctype int-for-bool :int)
#(defcfun ("equalequal" equalequal2) :boolean
#  (a (:boolean int-for-bool))
#  (b (:boolean :uint)))

$cffi->defctype( \$int_for_bool => ':int' );
$cffi->defcfun( [ equalequal => $equalequal2 ] => ':boolean',
  [ a => [ ':boolean' => $int_for_bool ] ],
  [ b => [ ':boolean' => ':uint' ] ] );

(deftest misc-types.boolean.2
    (equalequal2 nil t)
  nil)

(defctype my-string :string+ptr)

(defun funkify (str)
  (concatenate 'string "MORE " (string-upcase str)))

(defun 3rd-person (value)
  (list (concatenate 'string "Strdup says: " (first value))
        (second value)))

;; (defctype funky-string
;;     (:wrapper my-string
;;               :to-c #'funkify
;;               :from-c (lambda (value)
;;                         (list
;;                          (concatenate 'string "Strdup says: "
;;                                       (first value))
;;                          (second value))))
;;   "A useful type.")

(defctype funky-string (:wrapper my-string :to-c funkify :from-c 3rd-person))

(defcfun ("my_strdup" funky-strdup) funky-string
  (str funky-string))

(deftest misc-types.wrapper
    (destructuring-bind (string ptr)
        (funky-strdup "code")
      (strfree ptr)
      string)
  "Strdup says: MORE CODE")

(deftest misc-types.sized-ints
    (mapcar #'foreign-type-size
            '(:int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64))
  (1 1 2 2 4 4 8 8))

(define-foreign-type error-error ()
  ()
  (:actual-type :int)
  (:simple-parser error-error))

(defmethod translate-to-foreign (value (type error-error))
  (declare (ignore value))
  (error "translate-to-foreign invoked."))

(defmethod translate-from-foreign (value (type error-error))
  (declare (ignore value))
  (error "translate-from-foreign invoked."))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defmethod expand-to-foreign (value (type error-error))
    value)

  (defmethod expand-from-foreign (value (type error-error))
    value))

(defcfun ("abs" expand-abs) error-error
  (n error-error))

(defcvar ("var_int" *expand-var-int*) error-error)

(defcfun ("expect_int_sum" expand-expect-int-sum) :boolean
  (cb :pointer))

(defcallback expand-int-sum error-error ((x error-error) (y error-error))
  (+ x y))

;;; Ensure that macroexpansion-time translators are called where this
;;; is guaranteed (defcfun, defcvar, foreign-funcall and defcallback)
(deftest misc-types.expand.1
    (expand-abs -1)
  1)

#-cffi-sys::no-foreign-funcall
(deftest misc-types.expand.2
    (foreign-funcall "abs" error-error -1 error-error)
  1)

(deftest misc-types.expand.3
    (let ((old (mem-ref (get-var-pointer '*expand-var-int*) :int)))
      (unwind-protect
           (progn
             (setf *expand-var-int* 42)
             *expand-var-int*)
        (setf (mem-ref (get-var-pointer '*expand-var-int*) :int) old)))
  42)

(deftest misc-types.expand.4
    (expand-expect-int-sum (callback expand-int-sum))
  t)

(define-foreign-type translate-tracker ()
  ()
  (:actual-type :int)
  (:simple-parser translate-tracker))

(declaim (special .fto-called.))

(defmethod free-translated-object (value (type translate-tracker) param)
  (declare (ignore value param))
  (setf .fto-called. t))

(define-foreign-type expand-tracker ()
  ()
  (:actual-type :int)
  (:simple-parser expand-tracker))

(defmethod free-translated-object (value (type expand-tracker) param)
  (declare (ignore value param))
  (setf .fto-called. t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod expand-to-foreign (value (type expand-tracker))
    (declare (ignore value))
    (call-next-method)))

(defcfun ("abs" ttracker-abs) :int
  (n translate-tracker))

(defcfun ("abs" etracker-abs) :int
  (n expand-tracker))

;; free-translated-object must be called when there is no etf
(deftest misc-types.expand.5
    (let ((.fto-called. nil))
      (ttracker-abs -1)
      .fto-called.)
  t)

;; free-translated-object must be called when there is an etf, but
;; they answer *runtime-translator-form*
(deftest misc-types.expand.6
    (let ((.fto-called. nil))
      (etracker-abs -1)
      .fto-called.)
  t)

(define-foreign-type misc-type.expand.7 ()
  ()
  (:actual-type :int)
  (:simple-parser misc-type.expand.7))

(defmethod translate-to-foreign (value (type misc-type.expand.7))
  (values value 'second-value))

;; Auxiliary function to test CONVERT-TO-FOREIGN's compiler macro.
(defun misc-type.expand.7-aux ()
  (convert-to-foreign "foo" 'misc-type.expand.7))

;; Checking that expand-to-foreign doesn't ignore the second value of
;; translate-to-foreign.
(deftest misc-type.expand.7
    (misc-type.expand.7-aux)
  "foo" second-value)

;; Like MISC-TYPE.EXPAND.7 but doesn't depend on compiler macros
;; kicking in.
(deftest misc-type.expand.8
    (eval (expand-to-foreign "foo" (cffi::parse-type 'misc-type.expand.7)))
  "foo" second-value)

# }}}

# {{{ misc

;;;# foreign-symbol-pointer tests

;;; This might be useful for some libraries that compare function
;;; pointers. http://thread.gmane.org/gmane.lisp.cffi.devel/694
(defcfun "compare_against_abs" :boolean (p :pointer))

(deftest foreign-symbol-pointer.1
    (compare-against-abs (foreign-symbol-pointer "abs"))
  t)

(defcfun "compare_against_xpto_fun" :boolean (p :pointer))

(deftest foreign-symbol-pointer.2
    (compare-against-xpto-fun (foreign-symbol-pointer "xpto_fun"))
  t)

;;;# Library tests
;;;
;;; Need to figure out a way to test this.  CLISP, for instance, will
;;; automatically reopen the foreign-library when we call a foreign
;;; function so we can't test CLOSE-FOREIGN-LIBRARY this way.
;;;
;;; IIRC, GCC has some extensions to have code run when a library is
;;; loaded and stuff like that.  That could work.

#||
#+dffi
(deftest library.close.2
    (unwind-protect
         (progn
           (close-foreign-library 'libtest)
           (ignore-errors (my-sqrtf 16.0)))
      (load-test-libraries))
  nil)

#+(dffi
      cffi-sys::flat-namespace
      cffi-sys::no-foreign-funcall)
(deftest library.close.2
    (unwind-protect
         (values
          (foreign-funcall ("ns_function" :library libtest) :boolean)
          (close-foreign-library 'libtest)
          (foreign-funcall "ns_function" :boolean)
          (close-foreign-library 'libtest2)
          (close-foreign-library 'libtest2)
          (ignore-errors (foreign-funcall "ns_function" :boolean)))
      (load-test-libraries))
  t t nil t nil nil)
||#

(deftest library.error.1
    (handler-case (load-foreign-library "libdoesnotexistimsure")
      (load-foreign-library-error () 'error))
  error)

(define-foreign-library pseudo-library
  (t pseudo-library-spec))

;;; RT: T clause was being handled as :T by FEATUREP.
;;;
;;; We might want to export (and clean up) the API used in this test
;;; when the need arises.
(deftest library.t-clause
    (eq (cffi::foreign-library-spec
         (cffi::get-foreign-library 'pseudo-library))
        'pseudo-library-spec)
  t)

;;;# Shareable Byte Vector Tests

(deftest shareable-vector.1
    (let ((vector (cffi-sys::make-shareable-byte-vector 5)))
      (cffi::with-pointer-to-vector-data (pointer vector)
        (strcpy pointer "xpto"))
      vector)
  #(120 112 116 111 0))

(deftest shareable-vector.2
    (block nil
      (let ((vector (cffi-sys::make-shareable-byte-vector 5)))
        (cffi::with-pointer-to-vector-data (pointer vector)
          (strcpy pointer "xpto")
          (return vector))))
  #(120 112 116 111 0))

# }}}

# {{{ random-tester

;;; This code was used to generate the C and Lisp source code for
;;; the CALLBACKS.BFF.[12] and DEFCFUN.BFF.[12] tests.
;;;
;;; The original idea was to test all combinations of argument types
;;; but obviously as soon as you do the maths that it's not quite
;;; feasable for more that 4 or 5 arguments.
;;;
;;; TODO: actually run random tests, ie compile/load/run the tests
;;; this code can generate.

(defstruct (c-type (:conc-name type-))
  keyword
  name
  abbrev
  min
  max)

(defparameter +types+
  (mapcar (lambda (type)
            (let ((keyword (first type))
                  (name (second type)))
              (multiple-value-bind (min max)
                  ;; assume we can represent an integer in the range
                  ;; [-2^16 2^16-1] in a float/double without causing
                  ;; rounding errors (probably a lame assumption)
                  (let ((type-size (if (or (eq keyword :float)
                                           (eq keyword :double))
                                       16
                                       (* 8 (foreign-type-size keyword)))))
                    (if (or (eql (char name 0) #\u) (eq keyword :pointer))
                        (values 0 (1- (expt 2 type-size)))
                        (values (- (expt 2 (1- type-size)))
                                (1- (expt 2 (1- type-size))))))
                (make-c-type :keyword keyword :name name :abbrev (third type)
                             :min min :max max))))
          '((:char "char" "c")
            (:unsigned-char "unsigned char" "uc")
            (:short "short" "s")
            (:unsigned-short "unsigned short" "us")
            (:int "int" "i")
            (:unsigned-int "unsigned int" "ui")
            (:long "long" "l")
            (:unsigned-long "unsigned long" "ul")
            (:float "float" "f")
            (:double "double" "d")
            (:pointer "void*" "p")
            (:long-long "long long" "ll")
            (:unsigned-long-long "unsigned long long" "ull"))))

(defun find-type (keyword)
  (find keyword +types+ :key #'type-keyword))

(defun n-random-types (n)
  (loop repeat n collect (nth (random (length +types+)) +types+)))

;;; same as above, without the long long types
(defun n-random-types-no-ll (n)
  (loop repeat n collect (nth (random (- (length +types+) 2)) +types+)))

(defun random-range (x y)
  (+ x (random (+ (- y x) 2))))

(defun random-sum (rettype arg-types)
  "Returns a list of integers that fit in the respective types in the
ARG-TYPES list and whose sum fits in RETTYPE."
  (loop with sum = 0
        for type in arg-types
        for x = (random-range (max (- (type-min rettype) sum) (type-min type))
                              (min (- (type-max rettype) sum) (type-max type)))
        do (incf sum x)
        collect x))

(defun combinations (n items)
  (let ((combs '()))
    (labels ((rec (n accum)
               (if (= n 0)
                   (push accum combs)
                   (loop for item in items
                         do (rec (1- n) (cons item accum))))))
      (rec n '())
      combs)))

(defun function-name (rettype arg-types)
  (format nil "sum_~A_~{_~A~}"
          (type-abbrev rettype)
          (mapcar #'type-abbrev arg-types)))

(defun c-function (rettype arg-types)
  (let ((args (loop for type in arg-types and i from 1
                    collect (list (type-name type) (format nil "a~A" i)))))
    (format t "DLLEXPORT ~A ~A(~{~{~A ~A~}~^, ~})~%~
               { return ~A(~A) ~{~A~^ + ~}~A; }"
            (type-name rettype) (function-name rettype arg-types) args
            (if (eq (type-keyword rettype) :pointer)
                "(void *)((unsigned int)("
                "")
            (type-name rettype)
            (loop for arg-pair in args collect
                  (format nil "~A~A~A"
                          (cond ((string= (first arg-pair) "void*")
                                 "(unsigned int) ")
                                ((or (string= (first arg-pair) "double")
                                     (string= (first arg-pair) "float"))
                                 "((int) ")
                                (t ""))
                          (second arg-pair)
                          (if (member (first arg-pair)
                                      '("void*" "double" "float")
                                      :test #'string=)
                              ")"
                              "")))
            (if (eq (type-keyword rettype) :pointer) "))" ""))))

(defun c-callback (rettype arg-types args)
  (format t "DLLEXPORT ~A call_~A(~A (*func)(~{~A~^, ~}~^))~%~
             { return func(~{~A~^, ~}); }"
          (type-name rettype) (function-name rettype arg-types)
          (type-name rettype) (mapcar #'type-name arg-types)
          (loop for type in arg-types and value in args collect
                (format nil "~A~A"
                        (if (eq (type-keyword type) :pointer)
                            "(void *) "
                            "")
                        value))))

;;; (output-c-code #p"generated.c" 3 5)
(defun output-c-code (file min max)
  (with-open-file (stream file :direction :output :if-exists :error)
    (let ((*standard-output* stream))
      (format t "/* automatically generated functions and callbacks */~%~%")
      (loop for n from min upto max do
            (format t "/* ~A args */" (1- n))
            (loop for comb in (combinations n +types+) do
                  (terpri) (c-function (car comb) (cdr comb))
                  (terpri) (c-callback (car comb) (cdr comb)))))))

(defmacro with-conversion (type form)
  (case type
    (:double `(float ,form 1.0d0))
    (:float `(float ,form))
    (:pointer `(make-pointer ,form))
    (t form)))

(defun integer-conversion (type form)
  (case type
    ((:double :float) `(values (floor ,form)))
    (:pointer `(pointer-address ,form))
    (t form)))

(defun gen-arg-values (rettype arg-types)
  (let ((numbers (random-sum rettype arg-types)))
    (values
     (reduce #'+ numbers)
     (loop for type in arg-types and n in numbers
           collect (case (type-keyword type)
                     (:double (float n 1.0d0))
                     (:float (float n))
                     (:pointer `(make-pointer ,n))
                     (t n))))))

(defun gen-function-test (rettype arg-types)
  (let* ((fun-name (function-name rettype arg-types))
         (fun-sym (cffi::lisp-function-name fun-name)))
    (multiple-value-bind (sum value-forms)
        (gen-arg-values rettype arg-types)
    `(progn
       (defcfun (,fun-name ,fun-sym) ,(type-keyword rettype)
         ,@(loop for type in arg-types and i from 1 collect
                 (list (symbolicate '#:a (format nil "~A" i))
                       (type-keyword type))))
       (deftest ,(symbolicate '#:defcfun. fun-sym)
           ,(integer-conversion (type-keyword rettype)
                                `(,fun-sym ,@value-forms))
         ,sum)))))

(defun gen-callback-test (rettype arg-types sum)
  (let* ((fname (function-name rettype arg-types))
         (cb-sym (cffi::lisp-function-name fname))
         (fun-name (concatenate 'string "call_" fname))
         (fun-sym (cffi::lisp-function-name fun-name))
         (arg-names (loop for i from 1 upto (length arg-types) collect
                          (symbolicate '#:a (format nil "~A" i)))))
    `(progn
       (defcfun (,fun-name ,fun-sym) ,(type-keyword rettype) (cb :pointer))
       (defcallback ,cb-sym ,(type-keyword rettype)
           ,(loop for type in arg-types and name in arg-names
                  collect (list name (type-keyword type)))
         ,(integer-conversion
           (type-keyword rettype)
           `(+ ,@(mapcar (lambda (tp n)
                           (integer-conversion (type-keyword tp) n))
                         arg-types arg-names))))
       (deftest ,(symbolicate '#:callbacks. cb-sym)
           ,(integer-conversion (type-keyword rettype)
                                `(,fun-sym (callback ,cb-sym)))
         ,sum))))

(defun cb-test (&key no-long-long)
  (let* ((rettype (find-type (if no-long-long :long :long-long)))
         (arg-types (if no-long-long
                        (n-random-types-no-ll 127)
                        (n-random-types 127)))
         (args (random-sum rettype arg-types))
         (sum (reduce #'+ args)))
    (c-callback rettype arg-types args)
    (gen-callback-test rettype arg-types sum)))

;; (defmacro define-function-and-callback-tests (min max)
;;   `(progn
;;      ,@(loop for n from min upto max appending
;;              (loop for comb in (combinations n +types+)
;;                    collect (gen-function-test (car comb) (cdr comb))
;;                    collect (gen-callback-test (car comb) (cdr comb))))))

;; (define-function-and-callback-tests 3 5)

# }}}

# {{{ strings

;;;# Foreign String Conversion Tests
;;;
;;; With the implementation of encoding support, there are a lot of
;;; things that can go wrong with foreign string conversions.  This is
;;; a start at defining tests for strings and encoding conversion, but
;;; there needs to be a lot more.

(babel:enable-sharp-backslash-syntax)

;;; *ASCII-TEST-STRING* contains the characters in the ASCII character
;;; set that we will convert to a foreign string and check against
;;; *ASCII-TEST-BYTES*.  We don't bother with control characters.
;;;
;;; FIXME: It would probably be good to move these tables into files
;;; in "tests/", especially if we ever want to get fancier and have
;;; tests for more encodings.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ascii-test-string*
    (concatenate 'string " !\"#$%&'()*+,-./0123456789:;"
                 "<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]"
                 "^_`abcdefghijklmnopqrstuvwxyz{|}~")))

;;; *ASCII-TEST-BYTES* contains the expected ASCII encoded values
;;; for each character in *ASCII-TEST-STRING*.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *ascii-test-bytes*
    (let ((vector (make-array 95 :element-type '(unsigned-byte 8))))
      (loop for i from 0
            for code from 32 below 127
            do (setf (aref vector i) code)
            finally (return vector)))))

;;; Test basic consistency converting a string to and from Lisp using
;;; the default encoding.
(deftest string.conversion.basic
    (with-foreign-string (s *ascii-test-string*)
      (foreign-string-to-lisp s))
  #.*ascii-test-string* 95)

(deftest string.conversion.basic.2
    (with-foreign-string ((ptr size) "123" :null-terminated-p nil)
      (values (foreign-string-to-lisp ptr :count 3) size))
  "123" 3)

;;; Ensure that conversion of *ASCII-TEST-STRING* to a foreign buffer
;;; and back preserves ASCII encoding.
(deftest string.encoding.ascii
    (with-foreign-string (s *ascii-test-string* :encoding :ascii)
      (let ((vector (make-array 95 :element-type '(unsigned-byte 8))))
        (loop for i from 0 below (length vector)
              do (setf (aref vector i) (mem-ref s :unsigned-char i)))
        vector))
  #.*ascii-test-bytes*)

;;; FIXME: bogus test. We need support for BOM or UTF-16{BE,LE}.
(pushnew 'string.encoding.utf-16.basic rtest::*expected-failures*)

;;; Test UTF-16 conversion of a string back and forth.  Tests proper
;;; null terminator handling for wide character strings and ensures no
;;; byte order marks are added.  (Why no BOM? --luis)
;;;
;;; FIXME: an identical test using :UTF-16 wouldn't work because on
;;; little-endian architectures, :UTF-16 defaults to little-endian
;;; when writing and big-endian on reading because the BOM is
;;; suppressed.
#-babel::8-bit-chars
(progn
  (deftest string.encoding.utf-16le.basic
      (with-foreign-string (s *ascii-test-string* :encoding :utf-16le)
        (foreign-string-to-lisp s :encoding :utf-16le))
    #.*ascii-test-string* 190)

  (deftest string.encoding.utf-16be.basic
      (with-foreign-string (s *ascii-test-string* :encoding :utf-16be)
        (foreign-string-to-lisp s :encoding :utf-16be))
    #.*ascii-test-string* 190))

;;; Ensure that writing a long string into a short buffer does not
;;; attempt to write beyond the edge of the buffer, and that the
;;; resulting string is still null terminated.
(deftest string.short-write.1
    (with-foreign-pointer (buf 6)
      (setf (mem-ref buf :unsigned-char 5) 70)
      (lisp-string-to-foreign "ABCDE" buf 5 :encoding :ascii)
      (values (mem-ref buf :unsigned-char 4)
              (mem-ref buf :unsigned-char 5)))
  0 70)

#-babel::8-bit-chars
(deftest string.encoding.utf-8.basic
    (with-foreign-pointer (buf 7 size)
      (let ((string (concatenate 'babel:unicode-string
                                 '(#\u03bb #\u00e3 #\u03bb))))
        (lisp-string-to-foreign string buf size :encoding :utf-8)
        (loop for i from 0 below size
              collect (mem-ref buf :unsigned-char i))))
  (206 187 195 163 206 187 0))

(defparameter *basic-latin-alphabet* "abcdefghijklmnopqrstuvwxyz")

(deftest string.encodings.all.basic
    (let (failed)
      ;;; FIXME: UTF-{32,16} and friends fail due to lack of BOM. See
      ;;; STRING.ENCODING.UTF-16.BASIC for more details.
      (dolist (encoding (remove-if (lambda (x)
                                     (member x '(:utf-32 :utf-16 :ucs-2)))
                                   (babel:list-character-encodings)))
        ;; (format t "Testing ~S~%" encoding)
        (with-foreign-string (ptr *basic-latin-alphabet* :encoding encoding)
          (let ((string (foreign-string-to-lisp ptr :encoding encoding)))
            ;; (format t "  got ~S~%" string)
            (unless (string= *basic-latin-alphabet* string)
              (push encoding failed)))))
      failed)
  nil)

;;; rt: make sure *default-foreign-enconding* binds to a keyword
(deftest string.encodings.default
    (keywordp *default-foreign-encoding*)
  t)

# }}}

# {{{ struct

(defcstruct timeval
  (tv-secs :long)
  (tv-usecs :long))

(defparameter *timeval-size* (* 2 (max (foreign-type-size :long)
                                       (foreign-type-alignment :long))))

;;;# Basic Structure Tests

(deftest struct.1
    (- (foreign-type-size 'timeval) *timeval-size*)
  0)

(deftest struct.2
    (with-foreign-object (tv 'timeval)
      (setf (foreign-slot-value tv 'timeval 'tv-secs) 0)
      (setf (foreign-slot-value tv 'timeval 'tv-usecs) 1)
      (values (foreign-slot-value tv 'timeval 'tv-secs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  0 1)

(deftest struct.3
    (with-foreign-object (tv 'timeval)
      (with-foreign-slots ((tv-secs tv-usecs) tv timeval)
        (setf tv-secs 100 tv-usecs 200)
        (values tv-secs tv-usecs)))
  100 200)

;; regression test: accessing a struct through a typedef

(defctype xpto (:struct timeval))

(deftest struct.4
    (with-foreign-object (tv 'xpto)
      (setf (foreign-slot-value tv 'xpto 'tv-usecs) 1)
      (values (foreign-slot-value tv 'xpto 'tv-usecs)
              (foreign-slot-value tv 'timeval 'tv-usecs)))
  1 1)

(deftest struct.names
    (sort (foreign-slot-names 'xpto) #'<
          :key (lambda (x) (foreign-slot-offset 'xpto x)))
  (tv-secs tv-usecs))

;; regression test: compiler macro not quoting the type in the
;; resulting mem-ref form. The compiler macro on foreign-slot-value
;; is not guaranteed to be expanded though.

(defctype my-int :int)
(defcstruct s5 (a my-int))

(deftest struct.5
    (with-foreign-object (s 's5)
      (setf (foreign-slot-value s 's5 'a) 42)
      (foreign-slot-value s 's5 'a))
  42)

;;;# Structs with type translators

(defcstruct struct-string
  (s :string))

(deftest struct.string.1
    (with-foreign-object (ptr 'struct-string)
      (with-foreign-slots ((s) ptr struct-string)
        (setf s "So long and thanks for all the fish!")
        s))
  "So long and thanks for all the fish!")

(deftest struct.string.2
    (with-foreign-object (ptr 'struct-string)
      (setf (foreign-slot-value ptr 'struct-string 's) "Cha")
      (foreign-slot-value ptr 'struct-string 's))
  "Cha")

;;;# Structure Alignment Tests
;;;
;;; See libtest.c and types.lisp for some comments about alignments.

(defcstruct s-ch
  (a-char :char))

(defctype s-ch (:struct s-ch))

(defcstruct s-s-ch
  (another-char :char)
  (a-s-ch s-ch))

(defctype s-s-ch (:struct s-s-ch))

(defcvar "the_s_s_ch" s-s-ch)

(deftest struct.alignment.1
    (list 'a-char (foreign-slot-value
                   (foreign-slot-pointer *the-s-s-ch* 's-s-ch 'a-s-ch)
                   's-ch 'a-char)
          'another-char (foreign-slot-value *the-s-s-ch* 's-s-ch 'another-char))
  (a-char 1 another-char 2))


(defcstruct s-short
  (a-char :char)
  (another-char :char)
  (a-short :short))

(defctype s-short (:struct s-short))

(defcstruct s-s-short
  (yet-another-char :char)
  (a-s-short s-short))

(defctype s-s-short (:struct s-s-short))

(defcvar "the_s_s_short" s-s-short)

(deftest struct.alignment.2
    (with-foreign-slots ((yet-another-char a-s-short) *the-s-s-short* s-s-short)
      (with-foreign-slots ((a-char another-char a-short) a-s-short s-short)
        (list 'a-char           a-char
              'another-char     another-char
              'a-short          a-short
              'yet-another-char yet-another-char)))
  (a-char 1 another-char 2 a-short 3 yet-another-char 4))


(defcstruct s-double
  (a-char :char)
  (a-double :double)
  (another-char :char))

(defctype s-double (:struct s-double))

(defcstruct s-s-double
  (yet-another-char :char)
  (a-s-double s-double)
  (a-short :short))

(defctype s-s-double (:struct s-s-double))

(defcvar "the_s_s_double" s-s-double)

(deftest struct.alignment.3
    (with-foreign-slots
        ((yet-another-char a-s-double a-short) *the-s-s-double* s-s-double)
      (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
        (list 'a-char            a-char
              'a-double          a-double
              'another-char      another-char
              'yet-another-char  yet-another-char
              'a-short           a-short)))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5))


(defcstruct s-s-s-double
  (another-short :short)
  (a-s-s-double s-s-double)
  (last-char :char))

(defctype s-s-s-double (:struct s-s-s-double))

(defcvar "the_s_s_s_double" s-s-s-double)

(deftest struct.alignment.4
    (with-foreign-slots
        ((another-short a-s-s-double last-char) *the-s-s-s-double* s-s-s-double)
      (with-foreign-slots
          ((yet-another-char a-s-double a-short) a-s-s-double s-s-double)
        (with-foreign-slots ((a-char a-double another-char) a-s-double s-double)
          (list 'a-char            a-char
                'a-double          a-double
                'another-char      another-char
                'yet-another-char  yet-another-char
                'a-short           a-short
                'another-short     another-short
                'last-char         last-char))))
  (a-char 1 a-double 2.0d0 another-char 3 yet-another-char 4 a-short 5
   another-short 6 last-char 7))


(defcstruct s-double2
  (a-double :double)
  (a-short  :short))

(defctype s-double2 (:struct s-double2))

(defcstruct s-s-double2
  (a-char        :char)
  (a-s-double2   s-double2)
  (another-short :short))

(defctype s-s-double2 (:struct s-s-double2))

(defcvar "the_s_s_double2" s-s-double2)

(deftest struct.alignment.5
    (with-foreign-slots
        ((a-char a-s-double2 another-short) *the-s-s-double2* s-s-double2)
      (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
        (list 'a-double       a-double
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-double 1.0d0 a-short 2 a-char 3 another-short 4))

(defcstruct s-long-long
  (a-long-long :long-long)
  (a-short     :short))

(defctype s-long-long (:struct s-long-long))

(defcstruct s-s-long-long
  (a-char        :char)
  (a-s-long-long s-long-long)
  (another-short :short))

(defctype s-s-long-long (:struct s-s-long-long))

(defcvar "the_s_s_long_long" s-s-long-long)

(deftest struct.alignment.6
    (with-foreign-slots
        ((a-char a-s-long-long another-short) *the-s-s-long-long* s-s-long-long)
      (with-foreign-slots ((a-long-long a-short) a-s-long-long s-long-long)
        (list 'a-long-long    a-long-long
              'a-short        a-short
              'a-char         a-char
              'another-short  another-short)))
  (a-long-long 1 a-short 2 a-char 3 another-short 4))

(defcstruct s-s-double3
  (a-s-double2   s-double2)
  (another-short :short))

(defctype s-s-double3 (:struct s-s-double3))

(defcstruct s-s-s-double3
  (a-s-s-double3  s-s-double3)
  (a-char         :char))

(defctype s-s-s-double3 (:struct s-s-s-double3))

(defcvar "the_s_s_s_double3" s-s-s-double3)

(deftest struct.alignment.7
    (with-foreign-slots ((a-s-s-double3 a-char) *the-s-s-s-double3* s-s-s-double3)
      (with-foreign-slots ((a-s-double2 another-short) a-s-s-double3 s-s-double3)
        (with-foreign-slots ((a-double a-short) a-s-double2 s-double2)
          (list 'a-double      a-double
                'a-short       a-short
                'another-short another-short
                'a-char        a-char))))
  (a-double 1.0d0 a-short 2 another-short 3 a-char 4))


(defcstruct empty-struct)

(defctype empty-struct (:struct empty-struct))

(defcstruct with-empty-struct
  (foo empty-struct)
  (an-int :int))

;; commented out this test because an empty struct is not valid/standard C
;; left the struct declarations anyway because they should be handled
;; gracefuly anyway.

; (defcvar "the_with_empty_struct" with-empty-struct)
;
; (deftest struct.alignment.5
;     (with-foreign-slots ((foo an-int) *the-with-empty-struct* with-empty-struct)
;       an-int)
;   42)


;; regression test, setf-ing nested foreign-slot-value forms
;; the setf expander used to return a bogus getter

(defcstruct s1
  (an-int :int))

(defctype s1 (:struct s1))

(defcstruct s2
  (an-s1 s1))

(defctype s2 (:struct s2))

(deftest struct.nested-setf
    (with-foreign-object (an-s2 's2)
      (setf (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                                's1 'an-int)
            1984)
      (foreign-slot-value (foreign-slot-value an-s2 's2 'an-s1)
                          's1 'an-int))
  1984)

;; regression test, some Lisps were returning 4 instead of 8 for
;; (foreign-type-alignment :unsigned-long-long) on darwin/ppc32

(defcstruct s-unsigned-long-long
  (an-unsigned-long-long :unsigned-long-long)
  (a-short               :short))

(defctype s-unsigned-long-long (:struct s-unsigned-long-long))

(defcstruct s-s-unsigned-long-long
  (a-char                 :char)
  (a-s-unsigned-long-long s-unsigned-long-long)
  (another-short          :short))

(defctype s-s-unsigned-long-long (:struct s-s-unsigned-long-long))

(defcvar "the_s_s_unsigned_long_long" s-s-unsigned-long-long)

(deftest struct.alignment.8
    (with-foreign-slots
        ((a-char a-s-unsigned-long-long another-short)
         *the-s-s-unsigned-long-long* s-s-unsigned-long-long)
      (with-foreign-slots ((an-unsigned-long-long a-short)
                           a-s-unsigned-long-long s-unsigned-long-long)
        (list 'an-unsigned-long-long  an-unsigned-long-long
              'a-short                a-short
              'a-char                 a-char
              'another-short          another-short)))
  (an-unsigned-long-long 1 a-short 2 a-char 3 another-short 4))

;;;# C Struct Wrappers

(define-c-struct-wrapper timeval ())

(define-c-struct-wrapper (timeval2 (:struct timeval)) ()
  (tv-secs))

(defmacro with-example-timeval (var &body body)
  `(with-foreign-object (,var 'timeval)
     (with-foreign-slots ((tv-secs tv-usecs) ,var timeval)
       (setf tv-secs 42 tv-usecs 1984)
       ,@body)))

(deftest struct-wrapper.1
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval :pointer ptr)))
        (values (timeval-tv-secs obj)
                (timeval-tv-usecs obj))))
  42 1984)

(deftest struct-wrapper.2
    (with-example-timeval ptr
      (let ((obj (make-instance 'timeval2 :pointer ptr)))
        (timeval2-tv-secs obj)))
  42)

;;;# Structures as Values

(defcstruct (struct-pair :class pair)
  (a :int)
  (b :int))

(defctype struct-pair-typedef1 (:struct struct-pair))
(defctype struct-pair-typedef2 (:pointer (:struct struct-pair)))

(deftest struct.unparse.1
    (mapcar (alexandria:compose #'cffi::unparse-type #'cffi::parse-type)
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (struct-pair
   (:struct struct-pair)
   struct-pair-typedef1
   struct-pair-typedef2))

(deftest struct.canonicalize.1
    (mapcar #'cffi::canonicalize-foreign-type
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (:pointer
   (:struct struct-pair)
   (:struct struct-pair)
   :pointer))

(deftest struct.canonicalize.2
    (mapcar #'cffi::canonicalize-foreign-type
            '(struct-pair
              (:struct struct-pair)
              struct-pair-typedef1
              struct-pair-typedef2))
  (:pointer
   (:struct struct-pair)
   (:struct struct-pair)
   :pointer))

(defmethod translate-from-foreign (pointer (type pair))
  (with-foreign-slots ((a b) pointer (:struct struct-pair))
    (cons a b)))

(defmethod translate-into-foreign-memory (object (type pair) pointer)
  (with-foreign-slots ((a b) pointer (:struct struct-pair))
    (setf a (car object)
          b (cdr object))))

(defmethod translate-to-foreign (object (type pair))
  (let ((p (foreign-alloc '(:struct struct-pair))))
    (translate-into-foreign-memory object type p)
    (values p t)))

(defmethod free-translated-object (pointer (type pair) freep)
  (when freep
    (foreign-free pointer)))

(deftest struct-values.translation.1
    (multiple-value-bind (p freep)
        (convert-to-foreign '(1 . 2) 'struct-pair)
      (assert freep)
      (unwind-protect
           (convert-from-foreign p 'struct-pair)
        (free-converted-object p 'struct-pair freep)))
  (1 . 2))

(defcfun "pair_pointer_sum" :int
  (p (:pointer (:struct struct-pair))))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.2
    (pair-pointer-sum '(1 . 2))
  3)

;;; should the return type be something along the lines of
;;; (:pointer (:struct pair) :free t)?
;;; LMH: error on ":free t" option?
(defcfun "alloc_pair" (:pointer (:struct struct-pair))
  (a :int)
  (b :int))

;; bogus: doesn't free() pointer.
#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.3
    (alloc-pair 1 2)
  (1 . 2))

(deftest struct-values.translation.mem-ref.1
    (with-foreign-object (p '(:struct struct-pair))
      (setf (mem-ref p '(:struct struct-pair)) '(1 . 2))
      (with-foreign-slots ((a b) p (:struct struct-pair))
        (values (mem-ref p '(:struct struct-pair))
                a
                b)))
  (1 . 2)
  1
  2)

(deftest struct-values.translation.mem-aref.1
    (with-foreign-object (p '(:struct struct-pair) 2)
      (setf (mem-aref p '(:struct struct-pair) 0) '(1 . 2)
            (mem-aref p '(:struct struct-pair) 1) '(3 . 4))
      (values (mem-aref p '(:struct struct-pair) 0)
              (mem-aref p '(:struct struct-pair) 1)))
  (1 . 2)
  (3 . 4))

(defcstruct (struct-pair-default-translate :class pair-default)
  (a :int)
  (b :int))

(deftest struct-values-default.translation.mem-ref.1
    (with-foreign-object (p '(:struct struct-pair-default-translate))
      (setf (mem-ref p '(:struct struct-pair-default-translate)) '(a 1 b 2))
      (with-foreign-slots ((a b) p (:struct struct-pair-default-translate))
        (let ((plist (mem-ref p '(:struct struct-pair-default-translate))))
          (values (getf plist 'a)
                  (getf plist 'b)
                  a
                  b))))
  1
  2
  1
  2)

(defcstruct (struct-pair+double :class pair+double)
  (pr (:struct struct-pair-default-translate))
  (dbl :double))

(deftest struct-values-default.translation.mem-ref.2
    (with-foreign-object (p '(:struct struct-pair+double))
      (setf (mem-ref p '(:struct struct-pair+double)) '(pr (a 4 b 5) dbl 2.5d0))
      (with-foreign-slots ((pr dbl) p (:struct struct-pair+double))
        (let ((plist (mem-ref p '(:struct struct-pair+double))))
          (values (getf (getf plist 'pr) 'a)
                  (getf (getf plist 'pr) 'b)
                  (getf plist 'dbl)))))
  4
  5
  2.5d0)

(defcstruct (struct-pair+1 :class pair+1)
  (p (:pointer (:struct struct-pair)))
  (c :int))

(defctype struct-pair+1 (:struct struct-pair+1))

(defmethod translate-from-foreign (pointer (type pair+1))
  (with-foreign-slots ((p c) pointer struct-pair+1)
    (cons p c)))

(defmethod translate-into-foreign-memory (object (type pair+1) pointer)
  (with-foreign-slots ((c) pointer struct-pair+1)
    (convert-into-foreign-memory (car object)
                                 'struct-pair
                                 (foreign-slot-pointer pointer
                                                       'struct-pair+1
                                                       'p))
    (setf c (cdr object))))

(defmethod translate-to-foreign (object (type pair+1))
  (let ((p (foreign-alloc 'struct-pair+1)))
    (translate-into-foreign-memory object type p)
    (values p t)))

(defmethod free-translated-object (pointer (type pair+1) freep)
  (when freep
    (foreign-free pointer)))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.1
    (multiple-value-bind (p freep)
        (convert-to-foreign '((1 . 2) . 3) 'struct-pair+1)
      (assert freep)
      (unwind-protect
           (convert-from-foreign p 'struct-pair+1)
        (free-converted-object p 'struct-pair+1 freep)))
  ((1 . 2) . 3))

#+#:unimplemented
(defcfun "pair_plus_one_sum" :int
  (p (:struct pair+1)))

(defcfun "pair_plus_one_pointer_sum" :int
  (p (:pointer (:struct struct-pair+1))))

#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.2
    (pair-plus-one-pointer-sum '((1 . 2) . 3))
  6)

#+#:unimplemented
(defcfun "make_pair_plus_one" (:struct pair+1)
  (a :int)
  (b :int)
  (c :int))

(defcfun "alloc_pair_plus_one" struct-pair+1
  (a :int)
  (b :int)
  (c :int))

;; bogus: doesn't free() pointer.
#+#:pointer-translation-not-yet-implemented
(deftest struct-values.translation.ppo.3
    (alloc-pair-plus-one 1 2 3)
  ((1 . 2) . 3))

#+#:unimplemented
(defcfun "pair_sum" :int
  (p (:struct pair)))

#+#:unimplemented
(defcfun "make_pair" (:struct pair)
  (a :int)
  (b :int))

#|| ; TODO: load cffi-libffi for these tests to work.
(deftest struct-values.fn.1
    (with-foreign-object (p '(:struct pair))
      (with-foreign-slots ((a b) p (:struct pair))
        (setf a -1 b 2)
        (pair-sum p)))
  1)

(deftest struct-values.fn.2
    (pair-sum '(3 . 5))
  8)

(deftest struct-values.fn.3
    (with-foreign-object (p '(:struct pair))
      (make-pair 7 11 :result-pointer p)
      (with-foreign-slots ((a b) p (:struct pair))
        (cons a b)))
  (7 . 11))

(deftest struct-values.fn.4
    (make-pair 13 17)
  (13 . 17))
||#

(defcstruct single-byte-struct
  (a :uint8))

(deftest bare-struct-types.1
    (eql (foreign-type-size 'single-byte-struct)
         (foreign-type-size '(:struct single-byte-struct)))
  t)

(defctype single-byte-struct-alias (:struct single-byte-struct))

(deftest bare-struct-types.2
    (eql (foreign-type-size 'single-byte-struct-alias)
         (foreign-type-size '(:struct single-byte-struct)))
  t)

;;; Old-style access to inner structure fields.

(defcstruct inner-struct (x :int))
(defcstruct old-style-outer (inner inner-struct))
(defcstruct new-style-outer (inner (:struct inner-struct)))

(deftest old-style-struct-access
    (with-foreign-object (s '(:struct old-style-outer))
      (let ((inner-ptr (foreign-slot-pointer s 'old-style-outer 'inner)))
        (setf (foreign-slot-value inner-ptr 'inner-struct 'x) 42))
      (assert (pointerp (foreign-slot-value s 'old-style-outer 'inner)))
      (foreign-slot-value (foreign-slot-value s 'old-style-outer 'inner)
                          'inner-struct 'x))
  42)

(deftest new-style-struct-access
    (with-foreign-object (s '(:struct new-style-outer))
      (let ((inner-ptr (foreign-slot-pointer s 'new-style-outer 'inner)))
        (setf (foreign-slot-value inner-ptr 'inner-struct 'x) 42))
      (foreign-slot-value s 'new-style-outer 'inner))
  (x 42))

# }}}

# {{{ union

(defcunion uint32-bytes
  (int-value :unsigned-int)
  (bytes :unsigned-char :count 4))

(defctype uint32-bytes (:union uint32-bytes))

(defun int-to-bytes (n)
  "Convert N to a list of bytes using a union."
  (with-foreign-object (obj 'uint32-bytes)
    (setf (foreign-slot-value obj 'uint32-bytes 'int-value) n)
    (loop for i from 0 below 4
          collect (mem-aref
                   (foreign-slot-value obj 'uint32-bytes 'bytes)
                   :unsigned-char i))))

(deftest union.1
    (let ((bytes (int-to-bytes #x12345678)))
      (cond ((equal bytes '(#x12 #x34 #x56 #x78))
             t)
            ((equal bytes '(#x78 #x56 #x34 #x12))
             t)
            (t bytes)))
  t)

# }}}

=cut

### ### 4.3 Loading foreign libraries
###
### ###   (define-foreign-library libcurl
### ###     (:unix (:or "libcurl.so.3" "libcurl.so"))
### ###     (t (:default "libcurl")))
###
### $cffi->define_foreign_library( \$libcurl,
###   [ ':unix' => [ ':or' => 'libcurl.so.3', 'libcurl.so' ] ],
###   [ __default__ => [ ':default' => 'libcurl' ] ] ); 
###
### ###   (use-foreign-library libcurl)
###
### $cffi->use_foreign_library( $libcurl );
###
### ### 4.4 Initializing libcurl
###
### ###   ;;; A CURLcode is the universal error code.  curl/curl.h says
### ###   ;;; no return code will ever be removed, and new ones will be
### ###   ;;; added to the end.
### ###   (defctype curl-code :int)
### 
### my $curl_code = $cffi->defctype( ':int' );
### 
### ###   ;;; Initialize libcurl with FLAGS.
### ###   (defcfun "curl_global_init" curl-code
### ###     (flags :long))
### 
### # JMG defcfun() exports into the current namespace.
### #
### $cffi->defcfun( curl_global_init => $curl_code,
###   [ flags => ':long' ] );
### 
### ###   cffi-user> (curl-global-init 0)
### ###   => 0
### 
### is( curl_global_init( 0 ), 0 );
### 
### ###   (defcfun "curl_easy_init" :pointer)
### 
### $cffi->defcfun( curl_easy_init => $JGoff::Lisp::CFFI::pointer );
### 
### ###   (defcfun "curl_easy_cleanup" :void
### ###     (easy-handle :pointer))
### 
### $cffi->defcfun( curl_easy_cleanup => ':void',
###   [ $easy_handle => $JGoff::Lisp::CFFI::pointer ] );
### 
### ###   cffi-user> (defparameter *easy-handle* (curl-easy-init))
### ###   => *EASY-HANDLE*
### 
### my $easy_handle = curl_easy_init();
### 
### ###   cffi-user> *easy-handle*
### ###   => #<FOREIGN-ADDRESS #x09844EE0>
### 
### isa_ok( $easy_handle, 'Foreign::Address' );
### 
### ### 4.5 Setting download options
### ### 
### ###   (defmacro define-curl-options (name type-offsets &rest enum-args)
### ###     "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:
###
### ###       (NAME TYPE NUMBER)
###
### ###   Where the arguments are as they are with the CINIT macro defined
### ###   in curl.h, except NAME is a keyword.
###
### ###   TYPE-OFFSETS is a plist of TYPEs to their integer offsets, as
### ###   defined by the CURLOPTTYPE_LONG et al constants in curl.h."
### ###     (flet ((enumerated-value (type offset)
### ###              (+ (getf type-offsets type) offset)))
### ###       `(progn
### ###          (defcenum ,name
### ###            ,@(loop for (name type number) in enum-args
### ###                 collect (list name (enumerated-value type number))))
### ###          ',name)))                ;for REPL users' sanity
###
### ###   (define-curl-options curl-option
### ###       (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
### ###     (:noprogress long 43)
### ###     (:nosignal long 99)
### ###     (:errorbuffer objectpoint 10)
### ###     (:url objectpoint 2))
###
### ###   (progn
### ###     (defcenum curl-option
### ###       (:noprogress 43)
### ###       (:nosignal 99)
### ###       (:errorbuffer 10010)
### ###       (:url 10002))
### ###     'curl-option)
###
### ###   cffi-user> (foreign-funcall "curl_easy_setopt"
### ###                  :pointer *easy-handle*
### ###                  curl-option :nosignal :long 1 curl-code)
### ###   => 0
###
### ###   cffi-user> (foreign-funcall "curl_global_init" :long 0
### ###                               curl-code)
### 
### foreign_funcall( curl_global_init => [ ':long', 0 ],
###   $curl_code );
### 
### ### 4.7 Option functions in Lisp
###
### ###   ;;; We will use this type later in a more creative way.  For
### ###   ;;; now, just consider it a marker that this isn't just any
### ###   ;;; pointer.
### ###   (defctype easy-handle :pointer)
### 
### my $easy_handle = $cffi->defctype( $JGoff::Lisp::CFFI::pointer );
### 
### ###   (defmacro curl-easy-setopt (easy-handle enumerated-name
### ###                               value-type new-value)
### ###     "Call `curl_easy_setopt' on EASY-HANDLE, using ENUMERATED-NAME
### ###   as the OPTION.  VALUE-TYPE is the CFFI foreign type of the third
### ###   argument, and NEW-VALUE is the Lisp data to be translated to the
### ###   third argument.  VALUE-TYPE is not evaluated."
### ###     `(foreign-funcall "curl_easy_setopt" easy-handle ,easy-handle
### ###                       curl-option ,enumerated-name
### ###                       ,value-type ,new-value curl-code))
###
### ###   (defun curry-curl-option-setter (function-name option-keyword)
### ###     "Wrap the function named by FUNCTION-NAME with a version that
### ###   curries the second argument as OPTION-KEYWORD.
###
### ###   This function is intended for use in DEFINE-CURL-OPTION-SETTER."
### ###     (setf (symbol-function function-name)
### ###             (let ((c-function (symbol-function function-name)))
### ###               (lambda (easy-handle new-value)
### ###                 (funcall c-function easy-handle option-keyword
### ###                          new-value)))))
###
### ###   (defmacro define-curl-option-setter (name option-type
### ###                                        option-value foreign-type)
### ###     "Define (with DEFCFUN) a function NAME that calls
### ###   curl_easy_setopt.  OPTION-TYPE and OPTION-VALUE are the CFFI
### ###   foreign type and value to be passed as the second argument to
### ###   easy_setopt, and FOREIGN-TYPE is the CFFI foreign type to be used
### ###   for the resultant function's third argument.
###
### ###   This macro is intended for use in DEFINE-CURL-OPTIONS."
### ###     `(progn
### ###        (defcfun ("curl_easy_setopt" ,name) curl-code
### ###          (easy-handle easy-handle)
### ###          (option ,option-type)
### ###          (new-value ,foreign-type))
### ###        (curry-curl-option-setter ',name ',option-value)))
###
### ###   (defmacro define-curl-options (type-name type-offsets &rest enum-args)
### ###     "As with CFFI:DEFCENUM, except each of ENUM-ARGS is as follows:
###
### ###       (NAME TYPE NUMBER)
###
### ###   Also, define functions for each option named
### ###   set-`TYPE-NAME'-`OPTION-NAME', where OPTION-NAME is the NAME from
### ###   the above destructuring."
### ###     (flet ((enumerated-value (type offset)
### ###              (+ (getf type-offsets type) offset))
### ###            ;; map PROCEDURE, destructuring each of ENUM-ARGS
### ###            (map-enum-args (procedure)
### ###              (mapcar (lambda (arg) (apply procedure arg)) enum-args))
### ###            ;; build a name like SET-CURL-OPTION-NOSIGNAL
### ###            (make-setter-name (option-name)
### ###              (intern (concatenate
### ###                       'string "SET-" (symbol-name type-name)
### ###                       "-" (symbol-name option-name)))))
### ###       `(progn
### ###          (defcenum ,type-name
### ###            ,@(map-enum-args
### ###               (lambda (name type number)
### ###                 (list name (enumerated-value type number)))))
### ###          ,@(map-enum-args
### ###             (lambda (name type number)
### ###               (declare (ignore number))
### ###               `(define-curl-option-setter ,(make-setter-name name)
### ###                  ,type-name ,name ,(ecase type
### ###                                      (long :long)
### ###                                      (objectpoint :pointer)
### ###                                      (functionpoint :pointer)
### ###                                      (off-t :long)))))
### ###          ',type-name)))
###
### ###   (progn
### ###     (defcenum curl-option
### ###       (:noprogress 43)
### ###       (:nosignal 99)
### ###       (:errorbuffer 10010)
### ###       (:url 10002))
### ###     (define-curl-option-setter set-curl-option-noprogress
### ###       curl-option :noprogress :long)
### ###     (define-curl-option-setter set-curl-option-nosignal
### ###       curl-option :nosignal :long)
### ###     (define-curl-option-setter set-curl-option-errorbuffer
### ###       curl-option :errorbuffer :pointer)
### ###     (define-curl-option-setter set-curl-option-url
### ###       curl-option :url :pointer)
### ###     'curl-option)
###
### ###   (progn
### ###     (defcfun ("curl_easy_setopt" set-curl-option-nosignal) curl-code
### ###       (easy-handle easy-handle)
### ###       (option curl-option)
### ###       (new-value :long))
### ###     (curry-curl-option-setter 'set-curl-option-nosignal ':nosignal))
###
### ### Finally, let's try this out:
###
### ###   cffi-user> (set-curl-option-nosignal *easy-handle* 1)
### ###   => 0
###
### ### 4.8 Memory management
###
### ###   (set-curl-option-url *easy-handle* "http://www.cliki.net/CFFI")
###
### ###   == (with-foreign-string (url "http://www.cliki.net/CFFI")
### ###        (foreign-funcall "curl_easy_setopt" easy-handle *easy-handle*
### ###                         curl-option :url :pointer url curl-code))
###
### ###   (let (easy-handle)
### ###     (unwind-protect
### ###       (with-foreign-string (url "http://www.cliki.net/CFFI")
### ###         (setf easy-handle (curl-easy-init))
### ###         (set-curl-option-url easy-handle url)
### ###         #|do more with the easy-handle, like actually get the URL|#)
### ###       (when easy-handle
### ###         (curl-easy-cleanup easy-handle))))
###
### ###   (defvar *easy-handle-cstrings* (make-hash-table)
### ###     "Hashtable of easy handles to lists of C strings that may be
### ###   safely freed after the handle is freed.")
###
### ###   (defun make-easy-handle ()
### ###     "Answer a new CURL easy interface handle, to which the lifetime
### ###   of C strings may be tied.  See `add-curl-handle-cstring'."
### ###     (let ((easy-handle (curl-easy-init)))
### ###       (setf (gethash easy-handle *easy-handle-cstrings*) '())
### ###       easy-handle))
###
### ###   (defun free-easy-handle (handle)
### ###     "Free CURL easy interface HANDLE and any C strings created to
### ###   be its options."
### ###     (curl-easy-cleanup handle)
### ###     (mapc #'foreign-string-free
### ###           (gethash handle *easy-handle-cstrings*))
### ###     (remhash handle *easy-handle-cstrings*))
###
### ###   (defun add-curl-handle-cstring (handle cstring)
### ###     "Add CSTRING to be freed when HANDLE is, answering CSTRING."
### ###     (car (push cstring (gethash handle *easy-handle-cstrings*))))
###
### ###   (defun curry-curl-option-setter (function-name option-keyword)
### ###     "Wrap the function named by FUNCTION-NAME with a version that
### ###   curries the second argument as OPTION-KEYWORD.
###
### ###   This function is intended for use in DEFINE-CURL-OPTION-SETTER."
### ###     (setf (symbol-function function-name)
### ###             (let ((c-function (symbol-function function-name)))
### ###               (lambda (easy-handle new-value)
### ###                 (funcall c-function easy-handle option-keyword
### ###                          (if (stringp new-value)
### ###                            (add-curl-handle-cstring
### ###                             easy-handle
### ###                             (foreign-string-alloc new-value))
### ###                            new-value))))))
###
### ###   cffi-user> (curl-easy-cleanup *easy-handle*)
### ###   => NIL
### ###   cffi-user> (setf *easy-handle* (make-easy-handle))
### ###   => #<FOREIGN-ADDRESS #x09844EE0>
### ###   cffi-user> (set-curl-option-nosignal *easy-handle* 1)
### ###   => 0
### ###   cffi-user> (set-curl-option-url *easy-handle*
### ###                                   "http://www.cliki.net/CFFI")
### ###   => 0
###
### ###   cffi-user> (foreign-string-to-lisp
### ###               (car (gethash *easy-handle* *easy-handle-cstrings*)))
### ###   => "http://www.cliki.net/CFFI"
###
### ### Looks like that worked, and libcurl now knows what URL we want to retrieve.
###
### ###   (defvar *easy-handle-errorbuffers* (make-hash-table)
### ###     "Hashtable of easy handles to C strings serving as error
### ###   writeback buffers.")
###
### ###   ;;; An extra byte is very little to pay for peace of mind.
### ###   (defparameter *curl-error-size* 257
### ###     "Minimum char[] size used by cURL to report errors.")
###
### ###   (defun make-easy-handle ()
### ###     "Answer a new CURL easy interface handle, to which the lifetime
### ###   of C strings may be tied.  See `add-curl-handle-cstring'."
### ###     (let ((easy-handle (curl-easy-init)))
### ###       (setf (gethash easy-handle *easy-handle-cstrings*) '())
### ###       (setf (gethash easy-handle *easy-handle-errorbuffers*)
### ###               (foreign-alloc :char :count *curl-error-size*
### ###                              :initial-element 0))
### ###       easy-handle))
###
### ###   (defun free-easy-handle (handle)
### ###     "Free CURL easy interface HANDLE and any C strings created to
### ###   be its options."
### ###     (curl-easy-cleanup handle)
### ###     (foreign-free (gethash handle *easy-handle-errorbuffers*))
### ###     (remhash handle *easy-handle-errorbuffers*)
### ###     (mapc #'foreign-string-free
### ###           (gethash handle *easy-handle-cstrings*))
### ###     (remhash handle *easy-handle-cstrings*))
###
### ###   (defun get-easy-handle-error (handle)
### ###     "Answer a string containing HANDLE's current error message."
### ###     (foreign-string-to-lisp
### ###      (gethash handle *easy-handle-errorbuffers*)))
### ### 
### ### 4.9 Calling Lisp from C
###
### ###   size_t
### ###   function(void *ptr, size_t size, size_t nmemb, void *stream);
###
### ###   ;;; Alias in case size_t changes.
### ###   (defctype size :unsigned-int)
### 
### my $size = $cffi->defctype( ':unsigned-int' );
### 
### ###   ;;; To be set as the CURLOPT_WRITEFUNCTION of every easy handle.
### ###   (defcallback easy-write size ((ptr :pointer) (size size)
### ###                                 (nmemb size) (stream :pointer))
### ###     (let ((data-size (* size nmemb)))
### ###       (handler-case
### ###         ;; We use the dynamically-bound *easy-write-procedure* to
### ###         ;; call a closure with useful lexical context.
### ###         (progn (funcall (symbol-value '*easy-write-procedure*)
### ###                         (foreign-string-to-lisp ptr data-size nil))
### ###                data-size)         ;indicates success
### ###         ;; The WRITEFUNCTION should return something other than the
### ###         ;; #bytes available to signal an error.
### ###         (error () (if (zerop data-size) 1 0)))))
###
### ###   (define-curl-options curl-option
### ###       (long 0 objectpoint 10000 functionpoint 20000 off-t 30000)
### ###     (:noprogress long 43)
### ###     (:nosignal long 99)
### ###     (:errorbuffer objectpoint 10)
### ###     (:url objectpoint 2)
### ###     (:writefunction functionpoint 11)) ;new item here
###
### ###   cffi-user> (set-curl-option-writefunction
### ###               *easy-handle* (callback easy-write))
### ###   => 0
###
### ### 4.10 A complete FFI?
###
### ###   (defcfun "curl_easy_perform" curl-code
### ###     (handle easy-handle))
### 
### $cffi->defcfun( curl_easy_perform => $curl_code,
###   [ handle => $asy_handle ] );
### 
### ###   cffi-user> (with-output-to-string (contents)
### ###                (let ((*easy-write-procedure*
### ###                        (lambda (string)
### ###                          (write-string string contents))))
### ###                  (declare (special *easy-write-procedure*))
### ###                  (curl-easy-perform *easy-handle*)))
### ###   => "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\"
### ###   ....
###
### ### 4.11 Defining new types
###
### ###   (define-foreign-type curl-code-type ()
### ###     ()
### ###     (:actual-type :int)
### ###     (:simple-parser curl-code))
### 
### $curl_code_type = define_foreign_type(
###   [],
###   [ ':actual-type' => ':int' ],
###   [ ':simple-parser' => $curl_code ] );
### 
### ###   (define-condition curl-code-error (error)
### ###     (($code :initarg :curl-code :reader curl-error-code))
### ###     (:report (lambda (c stream)
### ###                (format stream "libcurl function returned error ~A"
### ###                               (curl-error-code c))))
### ###     (:documentation "Signalled when a libcurl function answers
### ###   a code other than CURLE_OK."))
###
### ###   (defmethod translate-from-foreign (value (type curl-code-type))
### ###     "Raise a CURL-CODE-ERROR if VALUE, a curl-code, is non-zero."
### ###     (if (zerop value)
### ###         :curle-ok
### ###         (error 'curl-code-error :curl-code value)))
###
### ###   cffi-user> (set-curl-option-nosignal *easy-handle* 1)
### ###   => :CURLE-OK
###
###
### ###   (defclass easy-handle ()
### ###     ((pointer :initform (curl-easy-init)
### ###               :documentation "Foreign pointer from curl_easy_init")
### ###      (error-buffer
### ###       :initform (foreign-alloc :char :count *curl-error-size*
### ###                                :initial-element 0)
### ###       :documentation "C string describing last error")
### ###      (c-strings :initform '()
### ###                 :documentation "C strings set as options"))
### ###     (:documentation "I am a parameterization you may pass to
### ###   curl-easy-perform to perform a cURL network protocol request."))
###
### ###   (defmethod initialize-instance :after ((self easy-handle) &key)
### ###     (set-curl-option-errorbuffer self (slot-value self 'error-buffer)))
###
### ###   (defun add-curl-handle-cstring (handle cstring)
### ###     "Add CSTRING to be freed when HANDLE is, answering CSTRING."
### ###     (car (push cstring (slot-value handle 'c-strings))))
###
### ###   (defun get-easy-handle-error (handle)
### ###     "Answer a string containing HANDLE's current error message."
### ###     (foreign-string-to-lisp
### ###      (slot-value handle 'error-buffer)))
###
### ###   (defun free-easy-handle (handle)
### ###     "Free CURL easy interface HANDLE and any C strings created to
### ###   be its options."
### ###     (with-slots (pointer error-buffer c-strings) handle
### ###       (curl-easy-cleanup pointer)
### ###       (foreign-free error-buffer)
### ###       (mapc #'foreign-string-free c-strings)))
###
### ###   (define-foreign-type easy-handle-type ()
### ###     ()
### ###     (:actual-type :pointer)
### ###     (:simple-parser easy-handle))
### 
### $easy_handle_type = define_foreign_type(
###   [ ],
###   [ ':actual-type' => $JGoff::Lisp::CFFI::pointer ],
###   [ ':simple-parser' => $easy_handle ] );
### 
### ###   (defmethod translate-to-foreign (handle (type easy-handle-type))
### ###     "Extract the pointer from an easy-HANDLE."
### ###     (slot-value handle 'pointer))
### ### 
