package JGoff::Lisp::CFFI;

use Moose;
use Moose::Util::TypeConstraints;
use Function::Parameters qw( :strict );
use Scalar::Util qw( looks_like_number );
use Readonly;
use Carp qw( croak );

use JGoff::Lisp::CFFI::ForeignAddress;
use JGoff::Lisp::CFFI::ForeignBitfield;
use JGoff::Lisp::CFFI::ForeignCStruct;
use JGoff::Lisp::CFFI::ForeignEnum;
use JGoff::Lisp::CFFI::ForeignLibrary;
use JGoff::Lisp::CFFI::ForeignLibraryDescriptor;

enum 'ForeignTypeName' => [
  ':char',
  ':int', ':int8', ':int16',
          ':int32', ':int64',
  ':long',
  ':long-long',
  ':float',
  ':double',
  ':long-double', # Only on a few Lisps

  ':unsigned-char',
  ':unsigned-int', ':unsigned-int8', ':unsigned-int16',
                   ':unsigned-int32', ':unsigned-int64',
  ':unsigned-long',
  ':unsigned-long-long',

  ':uchar',
  ':uint', ':uint8', ':uint16',
           ':uint32', ':uint64',
  ':ulong',
  ':ulong-long',

  ':void', # void return type

  # While these aren't strictly speaking "core" types, they belong here on
  # the list of foreign types.
  ':string',
  ':string+ptr', # returns ("lisp string" #<FOREGIN-ADDRESS>)

  ':pointer', # Type is optional
  ':boolean', # base-type is optional
];
### 
### enum 'ForeignCompoundTypeName' => [
###   ':pointer',
###   ':boolean',
###   ':wrapper',
### 
### #  ':pointer &optional type', # Pointer to an object of any type
### #  ':boolean &optional (base-type :int)', # Canonicalizes to base-type
### #                                         # which is :int by default
### #  ':wrapper base-type &key to-c from-c'
### ];

=head1 NAME

JGoff::Lisp::CFFI - Port CL-CFFI to Lisp

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

# {{{ convert_from_foreign
=head2 convert_from_foreign - Outside interface to backward type translator

(Foreign Types)

Syntax
  Function: convert_from_foreign foreign_value type => value

Arguments and Values
  foreign-value
    The primitive C value as returned from a primitive foreign function or from convert-to-foreign.
  type
    A CFFI type specifier.
  value
    The Lisp value translated from foreign-value. 

Description
  This is an external interface to the type translation facility. In the implementation, all foreign functions are ultimately defined as type translation wrappers around primitive foreign function invocations.

  This function is available mostly for inspection of the type translation process, and possibly optimization of special cases of your foreign function calls.

  Its behavior is better described under translate-from-foreign's documentation.

Examples
  CFFI-USER> (convert-to-foreign "a boat" :string)
  => #<FOREIGN-ADDRESS #x097ACDC0>
  => T

  ( $foreign_address, $result ) =
    convert_to_foreign( 'a boat', ':string' );
  isa_ok( $foreign_address, 'Foreign::Address' );
  is( $result, 1 );

  CFFI-USER> (convert-from-foreign * :string)
  => "a boat"

  is( convert_from_foreign( $foreign_address, ':string' ),
     'a boat'
  );

See Also
  convert_to_foreign
  free_converted_object
  translate_from_foreign

  my $self = shift;
=cut

method convert_from_foreign(
  JGoff::Lisp::CFFI::ForeignAddress $foreign_value,
  Str $type ) {
  my ( $object );
  $object = $foreign_value->object;

  return ( $object );
}
# }}}

# {{{ convert_to_foreign
=head2 convert_to_foreign - Outside interface to forward type translator

(Foreign Types)

Syntax
  Function: convert_to_foreign value type => foreign_value, alloc_params

Arguments and Values
  value
    The Lisp object to be translated to a foreign object.
  type
    A CFFI type specifier.
  foreign-value
    The primitive C value, ready to be passed to a primitive foreign function.
  alloc-params
    Something of a translation state; you must pass it to free-converted-object along with the foreign value for that to work. 

Description
  This is an external interface to the type translation facility. In the implementation, all foreign functions are ultimately defined as type translation wrappers around primitive foreign function invocations.

  This function is available mostly for inspection of the type translation process, and possibly optimization of special cases of your foreign function calls.

  Its behavior is better described under translate-to-foreign's documentation.

Examples
  CFFI-USER> (convert-to-foreign t :boolean)
  => 1
  => NIL

  # XXX no real 'bool' type, prove the point with '2.1' here.
  ( $value, $type ) = convert_to_foreign( 2.1, ':boolean' );
  is( $value, 1 );
  nok( $type );

  CFFI-USER> (convert-to-foreign "hello, world" :string)
  => #<FOREIGN-ADDRESS #x097C5F80>
  => T

  ( $value, $type ) = convert_to_foreign( 'hello, world', ':string' );
  isa_ok( $value, 'Foreign::Address' );
  ok( $type );

  CFFI-USER> (code-char (mem-aref * :char 5))
  => #\,

See Also
  convert_from_foreign
  free_converted_object
  translate_to_foreign

=cut

# Switch return values so 'my $x = ... ' works transparently.
#
method convert_to_foreign( $object, Str $type ) {
  my ( $foreign_value, $alloc_params );
  $foreign_value =
    JGoff::Lisp::CFFI::ForeignAddress->new( object => $object );
$alloc_params = 2;

  return ( $alloc_params, $foreign_value );
}
# }}}

# {{{ defbitfield
=head2 defbitfield - Defines a bitfield

(Foreign Types)

Syntax
  Macro: defbitfield name-and-options &body masks

  masks ::= [docstring] { (symbol value) }*
  name-and-options ::= name | (name &optional (base-type :int))

Arguments and Values
  name
    The name of the new bitfield type.
  docstring
    A documentation string, ignored.
  base-type
    A symbol denoting a foreign type.
  symbol
    A Lisp symbol.
  value
    An integer representing a bitmask. 

Description
  The defbitfield macro is used to define foreign types that map lists of symbols to integer values.

  If value is omitted, it will be computed as follows: find the greatest value previously used, including those so computed, with only a single 1-bit in its binary representation (that is, powers of two), and left-shift it by one. This rule guarantees that a computed value cannot clash with previous values, but may clash with future explicitly specified values.

  Symbol lists will be automatically converted to values and vice versa when being passed as arguments to or returned from foreign functions, respectively. The same applies to any other situations where an object of a bitfield type is expected.

  Types defined with defbitfield canonicalize to base-type which is :int by default.

Examples
  (defbitfield open-flags
    (:rdonly #x0000)
    :wronly               ;#x0001
    :rdwr                 ;...
    :nonblock
    :append
    (:creat  #x0200))
    ;; etc...

  my $open_flags = defbitfield(
    [ ':rdonly' => 0x0000 ],
    ':wronly',   # 0x0001
    ':rdwr',
    ':nonblock',
    ':append',
    [ ':creat' => 0x200 ] );
   
  CFFI> (foreign-bitfield-symbols 'open-flags #b1101)
  => (:RDONLY :WRONLY :NONBLOCK :APPEND)

  # XXX case sensitivity here?
  is_deeply(
    [ $cffi->foreign_bitfield_symbols( $open_flags, 0b1101 ) ],
    [ ':rdonly', ':wronly', ':nonblock', ':append' ] );
   
  CFFI> (foreign-bitfield-value 'open-flags '(:rdwr :creat))
  => 514   ; #x0202

  is( foreign_bitfield_value( $open_flags, [ ':rdwr', ':creat' ] ),
      0x0202 );
   
  (defcfun ("open" unix-open) :int
    (path :string)
    (flags open-flags)
    (mode :uint16)) ; unportable

  # There's already a 'open', so inject stdio's open() as 'unix_open'.
  # Also, as a nicety translate '-' => '_'
  #
  $cffi->defcfun(
    [ 'open' => 'unix-open' ] => ':int',
    [ path => ':string' ],
    [ flags => $open_flags ],
    [ mode => ':uint16' ] );
   
  CFFI> (unix-open "/tmp/foo" '(:wronly :creat) #o644)
  => #<an fd>

  unix_open( '/tmp/foo', [ ':wronly', ':creat' ], 0644 );
   
  ;;; Consider also the following lispier wrapper around open()
  (defun lispier-open (path mode &rest flags)
    (unix-open path flags mode))

See Also
  foreign_bitfield_value
  foreign_bitfield_symbols

=cut

method defbitfield( @keys ) {
  my ( $foreign_bitfield );
  my $bits = {};
  my $all_bits;
  my $current_bitmask = 1;
  for my $key ( @keys ) {
    if ( ref( $key ) ) {
      if ( $key->[ 1 ] ) {
        $current_bitmask = $key->[ 1 ];
        $bits->{ $current_bitmask } = $key->[ 0 ];
      }
      else {
        $all_bits = $key->[ 0 ];
        next;
      }
    }
    else {
      $bits->{ $current_bitmask } = $key;
    }
    $current_bitmask *= 2;
  }

  $foreign_bitfield =
    JGoff::Lisp::CFFI::ForeignBitfield->new(
      bitfield => $bits,
      ( $all_bits ? ( all_bits => $all_bits ) : () )
    );

  return ( $foreign_bitfield );
}
# }}}

# {{{ defcstruct
=head2 defcstruct - Defines a C struct type.

(Foreign Types)

Syntax
  Macro: defcstruct name-and-options &body doc-and-slots => name

  name-and-options ::= structure-name | (structure-name &key size)
  doc-and-slots ::= [docstring] { (slot-name slot-type &key count offset) }*

Arguments and Values
  structure-name
    The name of new structure type.
  docstring
    A documentation string, ignored.
  slot-name
    A symbol naming the slot. It must be unique among slot names in this structure.
  size
    Use this option to override the size (in bytes) of the struct.
  slot-type
    The type specifier for the slot.
  count
    Used to declare an array of size count inside the structure. Defaults to 1 as such an array and a single element are semantically equivalent.
  offset
    Overrides the slot's offset. The next slot's offset is calculated based on this one. 

Description
  This defines a new CFFI aggregate type akin to C structs. In other words, it specifies that foreign objects of the type structure-name are groups of different pieces of data, or “slots”, of the slot-types, distinguished from each other by the slot-names. Each structure is located in memory at a position, and the slots are allocated sequentially beginning at that point in memory (with some padding allowances as defined by the C ABI, unless otherwise requested by specifying an offset from the beginning of the structure (offset 0).

  In other words, it is isomorphic to the C struct, giving several extra features.

  There are two kinds of slots, for the two kinds of CFFI types:

Simple

  Contain a single instance of a type that canonicalizes to a built-in type, such as :long or :pointer. Used for simple CFFI types.

Aggregate

  Contain an embedded structure or union, or an array of objects. Used for aggregate CFFI types. 

  The use of CLOS terminology for the structure-related features is intentional; structure definitions are very much like classes with (far) fewer features.

Examples
  (defcstruct point
    "Point structure."
    (x :int)
    (y :int))

  $point = $cffi->defcstruct(
    "Point structure.",
    [ 'x' => ':int' ],
    [ 'y' => ':int' ] );
   
  CFFI> (with-foreign-object (ptr 'point)
          ;; Initialize the slots
          (setf (foreign-slot-value ptr 'point 'x) 42
                (foreign-slot-value ptr 'point 'y) 42)
          ;; Return a list with the coordinates
          (with-foreign-slots ((x y) ptr point)
            (list x y)))
  => (42 42)

  ;; Using the :size and :offset options to define a partial structure.
  ;; (this is useful when you are interested in only a few slots
  ;; of a big foreign structure)
   
  (defcstruct (foo :size 32)
    "Some struct with 32 bytes."
                          ; <16 bytes we don't care about>
    (x :int :offset 16)   ; an int at offset 16
    (y :int)              ; another int at offset 16+sizeof(int)
                          ; <a couple more bytes we don't care about>
    (z :char :offset 24)) ; a char at offset 24
                          ; <7 more bytes ignored (since size is 32)>

  $foo = $cffi->defcstruct(
    [ ':size' => 32 ],
    "Some struct with 32 bytes.",
    [ 'x' => ':int', ':offset' => 16 ],
    [ 'y' => ':int' ],
    [ 'z' => ':char', ':offset' => 24 ] );
   
  CFFI> (foreign-type-size 'foo)
  => 32

  is( $cffi->foreign_type_size( $foo ), 32 );

  (defcstruct cv-size
    (width :int)
    (height :int))

  $cv_size = $cffi->defstruct(
    [ 'width' => ':int' ],
    [ 'height' => ':int' ] );

  ;;; Using :count to define arrays inside of a struct.
  (defcstruct video_tuner
    (name :char :count 32))

  $video_tuner = $cffi->defcstruct(
    [ 'name' => ':char', ':count' => 32 ] );

See Also
  foreign_slot_pointer
  foreign_slot_value
  with_foreign_slots

=cut

method defcstruct( @keys ) {
  my $documentation;
  my $size;

  if ( !ref( $keys[0] ) ) {
    $documentation = shift @keys;
  }
  elsif ( $keys[0]->[0] eq ':size' ) {
    $size = (shift @keys)[1];
  }
  if ( !ref( $keys[0] ) ) {
    if ( $documentation ) {
      croak "Two documentation strings!";
    }
    $documentation = shift @keys;
  }

  my ( $cstruct );
  $cstruct = JGoff::Lisp::CFFI::ForeignCStruct->new(
    keys => [ @keys ],
    ( $documentation ? ( documentation => $documentation ) : () ),
    ( $size ? ( size => $size ) : () ),
  );

  return ( $cstruct );
}
# }}}

# {{{ defcunion
=head2 defcunion - Defines a C union type.

(Foreign Types)

Syntax
  Macro: defcunion name &body doc-and-slots => name

  doc-and-slots ::= [docstring] { (slot-name slot-type &key count) }*

Arguments and Values
  name
    The name of new union type.
  docstring
    A documentation string, ignored.
  slot-name
    A symbol naming the slot.
  slot-type
    The type specifier for the slot.
  count
    Used to declare an array of size count inside the structure. 

Description
  A union is a structure in which all slots have an offset of zero. It is isomorphic to the C union. Therefore, you should use the usual foreign structure operations for accessing a union's slots.

Examples
  (defcunion uint32-bytes
    (int-value :unsigned-int)
    (bytes :unsigned-char :count 4))

See Also
  foreign_slot_pointer
  foreign_slot_value

=cut

method defcunion( @keys ) {
  my ( $cunion );
  $cunion =
    JGoff::Lisp::CFFI::ForeignCUnion->new( @keys );

  return ( $cunion );
}
# }}}

# {{{ defctype
=head2 defctype - Defines a foreign typedef.

  /* typedef error_code int; */
  $error_code = $cffi->defctype( ':int' );

(Foreign Types)

Syntax
  Macro: defctype name base-type &optional documentation

Arguments and Values
  name
    The name of the new foreign type.
  base-type
    A symbol or a list defining the new type.
  documentation
    A documentation string, currently ignored. 

Description
  The defctype macro provides a mechanism similar to C's typedef to define new types. The new type inherits base-type's translators, if any. There is no way to define translations for types defined with defctype. For that, you should use define-foreign-type.

Examples
  (defctype my-string :string
    "My own string type.")

  $my_string = $cffi->defctype( ':string', 'My own string type.' );
   
  (defctype long-bools (:boolean :long)
    "Booleans that map to C longs.")

  my $long_bools = $cffi->defctype( [ ':boolean', ':long' ],
    'Booleans that map to C longs' );

See Also
  define_foreign_type

=cut

method defctype( $type, Str $documentation ) {
  my ( $ctype );

  return ( $ctype );
}
# }}}

# {{{ defcenum
=head2 defcenum - Defines a C enumeration.

(Foreign Types)

Syntax
  Macro: defcenum name-and-options &body enum-list

  enum-list ::= [docstring] { keyword | (keyword value) }*
  name-and-options ::= name | (name &optional (base-type :int))

Arguments and Values
  name
    The name of the new enum type.
  docstring
    A documentation string, ignored.
  base-type
    A symbol denoting a foreign type.
  keyword
    A keyword symbol.
  value
    An index value for a keyword. 

Description
  The defcenum macro is used to define foreign types that map keyword symbols to integer values, similar to the C enum type.

  If value is omitted its value will either be 0, if it's the first entry, or it it will continue the progression from the last specified value.

  Keywords will be automatically converted to values and vice-versa when being passed as arguments to or returned from foreign functions, respectively. The same applies to any other situations where an object of an enum type is expected.

  Types defined with defcenum canonicalize to base-type which is :int by default.

Examples
  (defcenum boolean
    :no
    :yes)
   
  CFFI> (foreign-enum-value 'boolean :no)
  => 0

  (defcenum numbers
    (:one 1)
    :two
    (:four 4))
   
  CFFI> (foreign-enum-keyword 'numbers 2)
  => :TWO

See Also
  foreign_enum_value
  foreign_enum_keyword

=cut

method defcenum( @keys ) {
  my ( $cenum );

  my $enum = {};
  my $current_index = 0;
  for my $key ( @keys ) {
    if ( ref( $key ) ) {
      if ( $key->[ 1 ] ) {
        $current_index = $key->[ 1 ];
        $enum->{ $current_index } = $key->[ 0 ];
      }
    }
    else {
      $enum->{ $current_index } = $key;
    }
    $current_index ++;
  }

  $cenum =
    JGoff::Lisp::CFFI::ForeignEnum->new(
      keys => $enum
    );

  return ( $cenum );
}
# }}}

# {{{ define_foreign_type
=head2 define_foreign_type: Defines a foreign type specifier.

(Foreign Types)

Syntax
  Macro: define-foreign-type class-name supers slots &rest options => class-name

  options ::= (:actual-type type) | (:simple-parser symbol) | regular defclass option

Arguments and Values
  class-name
    A symbol naming the new foreign type class.
  supers
    A list of symbols naming the super classes.
  slots
    A list of slot definitions, passed to defclass. 

Description
  The macro define-foreign-type defines a new class class-name. It is a thin wrapper around defclass. Among other things, it ensures that class-name becomes a subclass of foreign-type, what you need to know about that is that there's an initarg :actual-type which serves the same purpose as defctype's base-type argument.

Examples
  Taken from CFFI's :boolean type definition:

  (define-foreign-type :boolean (&optional (base-type :int))
    "Boolean type. Maps to an :int by default. Only accepts integer types."
    (ecase base-type
      ((:char
        :unsigned-char
        :int
        :unsigned-int
        :long
        :unsigned-long) base-type)))
   
  CFFI> (canonicalize-foreign-type :boolean)
  => :INT

  CFFI> (canonicalize-foreign-type '(:boolean :long))
  => :LONG

  CFFI> (canonicalize-foreign-type '(:boolean :float))
  ;; error--> signalled by ECASE.
  
See Also
  defctype
  define_parse_method

=cut

method define_foreign_type( @type ) {
  my ( $foreign_type );

  return ( $foreign_type );
}
# }}}

# {{{ define_parse_method
=head2 define_parse_method: Specifies how a type should be parsed.

(Foreign Types)

Syntax
  Macro: define-parse-method name lambda-list &body body => name

Arguments and Values
  type-name
    A symbol naming the new foreign type.
  lambda-list
    A lambda list which is the argument list of the new foreign type.
  body
    One or more forms that provide a definition of the new foreign type. 

Description

Examples
  Taken from CFFI's :boolean type definition:

  (define-foreign-type :boolean (&optional (base-type :int))
    "Boolean type. Maps to an :int by default. Only accepts integer types."
    (ecase base-type
      ((:char
        :unsigned-char
        :int
        :unsigned-int
        :long
        :unsigned-long) base-type)))
   
  CFFI> (canonicalize-foreign-type :boolean)
  => :INT

  CFFI> (canonicalize-foreign-type '(:boolean :long))
  => :LONG

  CFFI> (canonicalize-foreign-type '(:boolean :float))
  ;; error--> signalled by ECASE.

See Also
  define_foreign_type

=cut

method define_parse_method( @type ) {
  my ( $parse_method );

  return ( $parse_method );
}
# }}}

# {{{ foreign_bitfield_symbols
=head2 foreign_bitfield_symbols: Returns a list of symbols for a bitfield type.

(Foreign Types)

Syntax
  Function: foreign-bitfield-symbols type value => symbols

Arguments and Values
  type
    A bitfield type.
  value
    An integer.
  symbols
    A potentially shared list of symbols. nil. 

Description
  The function foreign-bitfield-symbols returns a possibly shared list of symbols that correspond to value in type.

Examples
  (defbitfield flags
    (flag-a 1)
    (flag-b 2)
    (flag-c 4))
   
  CFFI> (foreign-bitfield-symbols 'boolean #b101)
  => (FLAG-A FLAG-C)

See Also
  defbitfield
  foreign_bitfield_value

=cut

method foreign_bitfield_symbols(
  JGoff::Lisp::CFFI::ForeignBitfield $type, $value ) {
  my ( @symbols ) = ( );
  my @foo = sort { $a <=> $b }
            keys %{ $type->bitfield };
  if ( $type->all_bits ) {
    push @symbols, uc( $type->all_bits );
  }
  for my $mask ( @foo ) {
    push @symbols, uc( $type->bitfield->{ $mask } ) if
      ( $value & $mask ) > 0;
  }

  return ( @symbols );
}
# }}}

# {{{ foreign_bitfield_value
=head2 foreign_bitfield_value: Calculates a value for a bitfield type.

(Foreign Types)

Syntax
  Function: foreign-bitfield-value type symbols => value

Arguments and Values
  type
    A bitfield type.
  symbol
    A Lisp symbol.
  value
    An integer. 

Description
  The function foreign-bitfield-value returns the value that corresponds to the symbols in the symbols list.

Examples
  (defbitfield flags
    (flag-a 1)
    (flag-b 2)
    (flag-c 4))

  $flags = $cffi->defbitfield(
    [ 'flag-a' => 1 ],
    [ 'flag-b' => 2 ],
    [ 'flag-c' => 4 ] );
   
  CFFI> (foreign-bitfield-value 'flags '(flag-a flag-c))
  => 5  ; #b101

  is( $cffi->foreign_bitfield_value( $flags, [ 'flag-a', 'flag-c' ] ), 5 );

See Also
  defbitfield
  foreign_bitfield_symbols

=cut

method foreign_bitfield_value(
  JGoff::Lisp::CFFI::ForeignBitfield $type,
  $symbols ) {
  my ( $value );
  my %bitfield = reverse %{ $type->bitfield };

  for my $symbol ( @$symbols ) {
    $value += $bitfield{ $symbol };
  }

  return ( $value );
}
# }}}

# {{{ foreign_enum_keyword
=head2 foreign_enum_keyword: Finds a keyword in an enum type.

(Foreign Types)

Syntax
  Function: foreign-enum-keyword type value &key errorp => keyword

Arguments and Values
  type
    An enum type.
  value
    An integer.
  errorp
    If true (the default), signal an error if value is not defined in type. If false, foreign-enum-keyword returns nil.
  keyword
    A keyword symbol. 

Description
  The function foreign-enum-keyword returns the keyword symbol that corresponds to value in type.

  An error is signaled if type doesn't contain such value and errorp is true.

Examples
  (defcenum boolean
    :no
    :yes)

  $boolean = $cffi->defcenum(
    ':no',
    ':yes'
  );
   
  CFFI> (foreign-enum-keyword 'boolean 1)
  => :YES

  is( $cffi->foreign_enum_keyword( $boolean, 1 ), ':YES' );

See Also
  defcenum
  foreign_enum_value

=cut

method foreign_enum_keyword( $type, Int $value ) {
  my ( $keyword );
  $keyword = uc( $type->keys->{ $value } );

  return ( $keyword );
}
# }}}

# {{{ foreign_enum_value
=head2 foreign_enum_value: Finds a value in an enum type.

(Foreign Types)

Syntax
  Function: foreign-enum-value type keyword &key errorp => value

Arguments and Values
  type
    An enum type.
  keyword
    A keyword symbol.
  errorp
    If true (the default), signal an error if keyword is not defined in type. If false, foreign-enum-value returns nil.
  value
    An integer. 

Description
  The function foreign-enum-value returns the value that corresponds to keyword in type.

  An error is signaled if type doesn't contain such keyword, and errorp is true.

Examples
  (defcenum boolean
    :no
    :yes)
   
  CFFI> (foreign-enum-value 'boolean :yes)
  => 1

See Also
  defcenum
  foreign_enum_keyword

=cut

method foreign_enum_value( $type, Str $keyword ) {
  my ( $value );
  my %reverse = reverse %{ $type->keys };
  $value = $reverse{ $keyword };

  return ( $value );
}
# }}}

# {{{ foreign_slot_names
=head2 foreign_slot_names: Returns a list of slot names in a foreign struct.

(Foreign Types)

Syntax
  Function: foreign-slot-names type => names

Arguments and Values
  type
    A foreign struct type.
  names
    A list. 

Description
  The function foreign-slot-names returns a potentially shared list of slot names for the given structure type. This list has no particular order.

Examples
  (defcstruct timeval
    (tv-secs :long)
    (tv-usecs :long))
   
  CFFI> (foreign-slot-names '(:struct timeval))
  => (TV-SECS TV-USECS)

See Also
  defcstruct
  foreign_slot_offset
  foreign_slot_value
  foreign_slot_pointer

=cut

method foreign_slot_names( $type ) {
  my ( $names );

  return ( $names );
}
# }}}

# {{{ foreign_slot_offset
=head2 foreign_slot_offset: Returns the offset of a slot in a foreign struct.

(Foreign Types)

Syntax
  Function: foreign-slot-offset type slot-name => offset

Arguments and Values
  type
    A foreign struct type.
  slot-name
    A symbol.
  offset
    An integer. 

Description
  The function foreign-slot-offset returns the offset in bytes of a slot in a foreign struct type.

Examples
  (defcstruct timeval
    (tv-secs :long)
    (tv-usecs :long))
   
  CFFI> (foreign-slot-offset '(:struct timeval) 'tv-secs)
  => 0

  CFFI> (foreign-slot-offset '(:struct timeval) 'tv-usecs)
  => 4

See Also
  defcstruct
  foreign_slot_names
  foreign_slot_pointer
  foreign_slot_value

=cut

method foreign_slot_offset( $type, $slot_name ) {
  my ( $offset );

  return ( $offset );
}
# }}}

# {{{ foreign_slot_pointer
=head2 foreign_slot_pointer: Returns a pointer to a slot in a foreign struct.

(Foreign Types)

Syntax
  Function: foreign-slot-pointer ptr type slot-name => pointer

Arguments and Values
  ptr
    A pointer to a structure.
  type
    A foreign structure type.
  slot-names
    A slot name in the type.
  pointer
    A pointer to the slot slot-name. 

Description
  Returns a pointer to the location of the slot slot-name in a foreign object of type type at ptr. The returned pointer points inside the structure. Both the pointer and the memory it points to have the same extent as ptr.

  For aggregate slots, this is the same value returned by foreign-slot-value.

Examples
  (defcstruct point
    "Pointer structure."
    (x :int)
    (y :int))
   
  CFFI> (with-foreign-object (ptr '(:struct point))
          (foreign-slot-pointer ptr '(:struct point) 'x))
  => #<FOREIGN-ADDRESS #xBFFF6E60>
  ;; Note: the exact pointer representation varies from lisp to lisp.

See Also
  defcstruct
  foreign_slot_value
  foreign_slot_names
  foreign_slot_offset

=cut

method foreign_slot_pointer(
  JGoff::Lisp::CFFI::ForeignPiointer $ptr,
  $type, $slot_name ) {
  my ( $pointer );

  return ( $pointer );
}
# }}}

# {{{ foreign_slot_value
=head2 foreign_slot_value: Returns the value of a slot in a foreign struct.

(Foreign Types)

Syntax
  Accessor: foreign-slot-value ptr type slot-name => object

Arguments and Values
  ptr
    A pointer to a structure.
  type
    A foreign structure type.
  slot-name
    A symbol naming a slot in the structure type.
  object
    The object contained in the slot specified by slot-name. 

Description
  For simple slots, foreign-slot-value returns the value of the object, such as a Lisp integer or pointer. In C, this would be expressed as ptr->slot.

  For aggregate slots, a pointer inside the structure to the beginning of the slot's data is returned. In C, this would be expressed as &ptr->slot. This pointer and the memory it points to have the same extent as ptr.

  There are compiler macros for foreign-slot-value and its setf expansion that open code the memory access when type and slot-names are constant at compile-time.

Examples
  (defcstruct point
    "Pointer structure."
    (x :int)
    (y :int))
   
  CFFI> (with-foreign-object (ptr '(:struct point))
          ;; Initialize the slots
          (setf (foreign-slot-value ptr '(:struct point) 'x) 42
                (foreign-slot-value ptr '(:struct point) 'y) 42)
          ;; Return a list with the coordinates
          (with-foreign-slots ((x y) ptr (:struct point))
            (list x y)))
  => (42 42)

See Also
  defcstruct
  foreign_slot_names
  foreign_slot_offset
  foreign_slot_pointer
  with_foreign_slots

=cut

method foreign_slot_value( @keys ) {
}
# }}}

# {{{ foreign_type_alignment
=head2 foreign_type_alignment: Returns the alignment of a foreign type.

(Foreign Types)

Syntax
  Function: foreign-type-alignment type => alignment

Arguments and Values
  type
    A foreign type.
  alignment
    An integer. 

Description
  The function foreign-type-alignment returns the alignment of type in bytes.

Examples
  CFFI> (foreign-type-alignment :char)
  => 1

  CFFI> (foreign-type-alignment :short)
  => 2

  CFFI> (foreign-type-alignment :int)
  => 4

  (defcstruct foo
    (a :char))
   
  CFFI> (foreign-type-alignment '(:struct foo))
  => 1

See Also
  foreign_type_size

=cut

method foreign_type_alignment( $type ) {
  my ( $alignment );

  return ( $alignment );
}
# }}}

# {{{ foreign_type_size
=head2 foreign_type_size: Returns the size of a foreign type.

(Foreign Types)

Syntax
  Function: foreign-type-size type => size

Arguments and Values
  type
    A foreign type.
  size
    An integer. 

Description
  The function foreign-type-size return the size of type in bytes. This includes any padding within and following the in-memory representation as needed to create an array of type objects.

Examples
  (defcstruct foo
    (a :double)
    (c :char))
   
  CFFI> (foreign-type-size :double)
  => 8

  CFFI> (foreign-type-size :char)
  => 1

  CFFI> (foreign-type-size '(:struct foo))
  => 16

See Also
  foreign_type_alignment

=cut

method foreign_type_size( $type ) {
  my ( $size );

  return ( $size );
}
# }}}

# {{{ free_converted_object
=head2 free_converted_object: Outside interface to typed object deallocators.

(Foreign Types)

Syntax
  Function: free-converted-object foreign-value type params

Arguments and Values
  foreign-value
    The C object to be freed.
  type
    A CFFI type specifier.
  params
    The state returned as the second value from convert-to-foreign; used to implement the third argument to free-translated-object. 

Description
  The return value is unspecified.

  This is an external interface to the type translation facility. In the implementation, all foreign functions are ultimately defined as type translation wrappers around primitive foreign function invocations.

  This function is available mostly for inspection of the type translation process, and possibly optimization of special cases of your foreign function calls.

  Its behavior is better described under free-translated-object's documentation.

Examples
  CFFI-USER> (convert-to-foreign "a boat" :string)
  => #<FOREIGN-ADDRESS #x097ACDC0>
  => T

  CFFI-USER> (free-converted-object * :string t)
  => NIL

See Also
  convert_from_foreign
  convert_to_foreign
  free_translated_object

=cut

method free_converted_object(
  JGoff::Lisp::CFFI::ForeignAddress $foreign_value, $type, $params ) {
  # XXX assertions on key params

  return;
}
# }}}

# {{{ free_translated_object
=head2 free_translated_object: Defines how to free a foreign object.

(Foreign Types)

Syntax
  Generic Function: free-translated-object value type-name param

Arguments and Values
  pointer
    The foreign value returned by translate-to-foreign.
  type-name
    A symbol naming a foreign type defined by defctype.
  param
    The second value, if any, returned by translate-to-foreign. 

Description
  This generic function may be specialized by user code to perform automatic deallocation of foreign objects as they are passed to C functions.

  Any methods defined on this generic function must EQL-specialize the type-name parameter on a symbol defined as a foreign type by the defctype macro.

See Also
  Foreign Type Translators
  translate_to_foreign

=cut

method free_translated_object( $value, $type_name, $param ) {
  return;
}
# }}}

# {{{ translate_from_foreign
=head2 translate_from_foreign: Defines a foreign-to-Lisp object translation.

(Foreign Types)

Syntax
  Generic Function: translate-from-foreign foreign-value type-name => lisp-value

Arguments and Values
  foreign-value
    The foreign value to convert to a Lisp object.
  type-name
    A symbol naming a foreign type defined by defctype.
  lisp-value
    The lisp value to pass in place of foreign-value to Lisp code. 

Description
  This generic function is invoked by CFFI to convert a foreign value to a Lisp value, such as when returning from a foreign function, passing arguments to a callback function, or accessing a foreign variable.

  To extend the CFFI type system by performing custom translations, this method may be specialized by eql-specializing type-name on a symbol naming a foreign type defined with defctype. This method should return the appropriate Lisp value to use in place of the foreign value.

  The results are undefined if the type-name parameter is specialized in any way except an eql specializer on a foreign type defined with defctype. Specifically, translations may not be defined for built-in types.

See Also
  Foreign Type Translators
  translate_to_foreign
  free_translated_object

=cut

method translate_from_foreign( $foreign_value, $type_name ) {
  my ( $lisp_value );

  return ( $lisp_value );
}
# }}}

# {{{ translate_to_foreign
=head2 translate_to_foreign: Defines a Lisp-to-foreign object translation.

(Foreign Types)

Syntax
  Generic Function: translate-to-foreign lisp-value type-name => foreign-value, alloc-param

Arguments and Values
  lisp-value
    The Lisp value to convert to foreign representation.
  type-name
    A symbol naming a foreign type defined by defctype.
  foreign-value
    The foreign value to pass in place of lisp-value to foreign code.
  alloc-param
    If present, this value will be passed to free-translated-object. 

Description
  This generic function is invoked by CFFI to convert a Lisp value to a foreign value, such as when passing arguments to a foreign function, returning a value from a callback, or setting a foreign variable. A “foreign value” is one appropriate for passing to the next-lowest translator, including the low-level translators that are ultimately invoked invisibly with CFFI.

  To extend the CFFI type system by performing custom translations, this method may be specialized by eql-specializing type-name on a symbol naming a foreign type defined with defctype. This method should return the appropriate foreign value to use in place of the Lisp value.

  In cases where CFFI can determine the lifetime of the foreign object returned by this method, it will invoke free-translated-object on the foreign object at the appropriate time. If translate-to-foreign returns a second value, it will be passed as the param argument to free-translated-object. This can be used to establish communication between the allocation and deallocation methods.

  The results are undefined if the type-name parameter is specialized in any way except an eql specializer on a foreign type defined with defctype. Specifically, translations may not be defined for built-in types.

See Also
  Foreign Type Translators
  translate_from_foreign
  free_translated_object

=cut

method translate_to_foreign( $lisp_value, $type_name ) {
  my ( $foreign_value, $alloc_param );

  return ( $foreign_value, $alloc_param );
}
# }}}

# {{{ with_foreign_object
=head2 with_foreign_object: Allocates a foreign object with dynamic extent.

(Foreign Types)

Syntax
  Macro: with-foreign-object (var type &optional count) &body body
  Macro: with-foreign-objects (bindings) &body body

  bindings ::= {(var type &optional count)}*

Arguments and Values
  var
    A symbol.
  type
    A foreign type, evaluated.
  count
    An integer. 

Description
  The macros with-foreign-object and with-foreign-objects bind var to a pointer to count newly allocated objects of type type during body. The buffer has dynamic extent and may be stack allocated if supported by the host Lisp.

Examples
  CFFI> (with-foreign-object (array :int 10)
          (dotimes (i 10)
            (setf (mem-aref array :int i) (random 100)))
          (loop for i below 10
                collect (mem-aref array :int i)))
  => (22 7 22 52 69 1 46 93 90 65)

See Also
  foreign_alloc

=cut

method with_foreign_object() {
  my $self = shift;
}
# }}}

# {{{ with_foreign_objects
=head2 with_foreign_objects: Plural form of with-foreign-object.

(Foreign Types)

(see: with_foreign_object)

=cut

method with_foreign_objects() {
}
# }}}

# {{{ with_foreign_slots
=head2 with_foreign_slots: Accesses the slots of a foreign structure. 

(Foreign Types)

Syntax
  Macro: with-foreign-slots (vars ptr type) &body body

Arguments and Values
  vars
    A list of symbols.
  ptr
    A foreign pointer to a structure.
  type
    A structure type.
  body
    A list of forms to be executed. 

Description
  The with-foreign-slots macro creates local symbol macros for each var in vars to reference foreign slots in ptr of type. It is similar to Common Lisp's with-slots macro.

Examples
  (defcstruct tm
    (sec :int)
    (min :int)
    (hour :int)
    (mday :int)
    (mon  :int)
    (year :int)
    (wday :int)
    (yday :int)
    (isdst  :boolean)
    (zone   :string)
    (gmtoff :long))
   
  CFFI> (with-foreign-object (time :int)
          (setf (mem-ref time :int)
                (foreign-funcall "time" :pointer (null-pointer) :int))
          (foreign-funcall "gmtime" :pointer time (:pointer (:struct tm))))
  => #<A Mac Pointer #x102A30>

  CFFI> (with-foreign-slots ((sec min hour mday mon year) * (:struct tm))
          (format nil "~A:~A:~A, ~A/~A/~A"
                  hour min sec (+ 1900 year) mon mday))
  => "7:22:47, 2005/8/2"

See Also
  defcstruct
  defcunion
  foreign_slot_value

=cut

method with_foreign_slots() {
}
# }}}

# {{{ foreign_free
=head2 foreign_free: Deallocates memory.

(Pointers)

Syntax
  Function: foreign-free ptr => undefined

Arguments and Values
  ptr
    A foreign pointer. 

Description
  The foreign-free function frees a ptr previously allocated by foreign-alloc. The consequences of freeing a given pointer twice are undefined.

Examples
  CFFI> (foreign-alloc :int)
  => #<A Mac Pointer #x1022E0>

  CFFI> (foreign-free *)
  => NIL

See Also
  foreign_alloc
  with_foreign_pointer

=cut

method foreign_free( JGoff::Lisp::CFFI::ForeignPointer $ptr ) {
  return undef;
}
# }}}

# {{{ foreign_alloc
=head2 foreign_alloc: Allocates memory.

(Pointers)

Syntax
  Function: foreign-alloc type &key initial-element initial-contents (count 1) null-terminated-p => pointer

Arguments and Values
  type
    A foreign type.
  initial-element
    A Lisp object.
  initial-contents
    A sequence.
  count
    An integer. Defaults to 1 or the length of initial-contents if supplied.
  null-terminated-p
    A boolean, false by default.
  pointer
    A foreign pointer to the newly allocated memory. 

Description
  The foreign-alloc function allocates enough memory to hold count objects of type type and returns a pointer. This memory must be explicitly freed using foreign-free once it is no longer needed.

  If initial-element is supplied, it is used to initialize the count objects the newly allocated memory holds.

  If an initial-contents sequence is supplied, it must have a length less than or equal to count and each of its elements will be used to initialize the contents of the newly allocated memory.

  If count is omitted and initial-contents is specified, it will default to (length initial-contents).

  initial-element and initial-contents are mutually exclusive.

  When null-terminated-p is true, (1+ (max count (length initial-contents))) elements are allocated and the last one is set to NULL. Note that in this case type must be a pointer type (ie. a type that canonicalizes to :pointer), otherwise an error is signaled.

Examples
  CFFI> (foreign-alloc :char)
  => #<A Mac Pointer #x102D80>     ; A pointer to 1 byte of memory.
   
  CFFI> (foreign-alloc :char :count 20)
  => #<A Mac Pointer #x1024A0>     ; A pointer to 20 bytes of memory.
   
  CFFI> (foreign-alloc :int :initial-element 12)
  => #<A Mac Pointer #x1028B0>

  CFFI> (mem-ref * :int)
  => 12
   
  CFFI> (foreign-alloc :int :initial-contents '(1 2 3))
  => #<A Mac Pointer #x102950>
  CFFI> (loop for i from 0 below 3
              collect (mem-aref * :int i))
  => (1 2 3)
   
  CFFI> (foreign-alloc :int :initial-contents #(1 2 3))
  => #<A Mac Pointer #x102960>
  CFFI> (loop for i from 0 below 3
              collect (mem-aref * :int i))
  => (1 2 3)
   
  ;;; Allocate a char** pointer that points to newly allocated memory
  ;;; by the :string type translator for the string "foo".
  CFFI> (foreign-alloc :string :initial-element "foo")
  => #<A Mac Pointer #x102C40>

  ;;; Allocate a null-terminated array of strings.
  ;;; (Note: FOREIGN-STRING-TO-LISP returns NIL when passed a null pointer)
  CFFI> (foreign-alloc :string
                       :initial-contents '("foo" "bar" "baz")
                       :null-terminated-p t)
  => #<A Mac Pointer #x102D20>
  CFFI> (loop for i from 0 below 4
              collect (mem-aref * :string i))
  => ("foo" "bar" "baz" NIL)
  CFFI> (progn
          (dotimes (i 3)
            (foreign-free (mem-aref ** :pointer i)))
          (foreign-free **))
  => nil

See Also
  foreign_free
  with_foreign_object
  with_foreign_pointer

=cut

method foreign_alloc( $type, @key ) {
  my ( $pointer );
  $pointer = JGoff::Lisp::CFFI::ForeignPointer->new;

  return ( $pointer );
}
# }}}

# {{{ foreign_symbol_pointer
=head2 foreign_symbol_pointer: Returns a pointer to a foreign symbol.

(Pointers)

Syntax
  Function: foreign-symbol-pointer foreign-name &key library => pointer

Arguments and Values
  foreign-name
    A string.
  pointer
    A foreign pointer, or nil.
  library
    A Lisp symbol or an instance of foreign-library. 

Description
  The function foreign-symbol-pointer will return a foreign pointer corresponding to the foreign symbol denoted by the string foreign-name. If a foreign symbol named foreign-name doesn't exist, nil is returned.

  ABI name manglings will be performed on foreign-name by foreign-symbol-pointer if necessary. (eg: adding a leading underscore on darwin/ppc)

  library should name a foreign library as defined by define-foreign-library, :default (which is the default) or an instance of foreign-library as returned by load-foreign-library.

  Important note: do not keep these pointers across saved Lisp cores as the foreign-library may move across sessions.

Examples
  CFFI> (foreign-symbol-pointer "errno")
  => #<A Mac Pointer #xA0008130>

  CFFI> (foreign-symbol-pointer "strerror")
  => #<A Mac Pointer #x9002D0F8>

  CFFI> (foreign-funcall-pointer * () :int (mem-ref ** :int) :string)
  => "No such file or directory"
   
  CFFI> (foreign-symbol-pointer "inexistent symbol")
  => NIL

See Also
  defcvar

=cut

method foreign_symbol_pointer( $foreign_name, @key ) {
  my ( $pointer );
  $pointer = JGoff::Lisp::CFFI::ForeignPointer->new;

  return ( $pointer );
}
# }}}

# {{{ inc_pointer
=head2 inc_pointer: Increments the address held by a pointer.

(Pointers)

Syntax
  Function: inc-pointer pointer offset => new-pointer

Arguments and Values
  pointer
  new-pointer
    A foreign pointer.
  offset
    An integer. 

Description
  The function inc-pointer will return a new-pointer pointing offset bytes past pointer.

Examples
  CFFI> (foreign-string-alloc "Common Lisp")
  => #<A Mac Pointer #x102EA0>

  CFFI> (inc-pointer * 7)
  => #<A Mac Pointer #x102EA7>

  CFFI> (foreign-string-to-lisp *)
  => "Lisp"

See Also
  incf_pointer
  make_pointer
  pointerp
  null_pointer
  null_pointer_p

=cut

method inc_pointer( JGoff::Lisp::CFFI::FunctionPointer $pointer, $offset ) {
  my ( $new_pointer );
  $new_pointer = $pointer + 1;

  return ( $new_pointer );
}
# }}}

# {{{ incf_pointer
=head2 incf_pointer: Increments the pointer address in a place.

(Pointers)

Syntax
  Macro: incf-pointer place &optional (offset 1) => new-pointer

Arguments and Values
  place
    A setf place.
  new-pointer
    A foreign pointer.
  offset
    An integer. 

Description
  The incf-pointer macro takes the foreign pointer from place and creates a new-pointer incremented by offset bytes and which is stored in place.

Examples
  CFFI> (defparameter *two-words* (foreign-string-alloc "Common Lisp"))
  => *TWO-WORDS*

  CFFI> (defparameter *one-word* *two-words*)
  => *ONE-WORD*

  CFFI> (incf-pointer *one-word* 7)
  => #.(SB-SYS:INT-SAP #X00600457)

  CFFI> (foreign-string-to-lisp *one-word*)
  => "Lisp"

  CFFI> (foreign-string-to-lisp *two-words*)
  => "Common Lisp"

See Also
  inc_pointer
  make_pointer
  pointerp
  null_pointer
  null_pointer_p

=cut

method incf_pointer( JGoff::Lisp::CFFI::FunctionPointer $foreign_address ) {
  $foreign_address++; # XXX JMG do something
}
# }}}

# {{{ make_pointer
=head2 make_pointer: Returns a pointer to a given address.

(Pointers)

Syntax
  Function: make-pointer address => ptr

Arguments and Values
  address
    An integer.
  ptr
    A foreign pointer. 

Description
  The function make-pointer will return a foreign pointer pointing to address.

Examples
  CFFI> (make-pointer 42)
  => #<FOREIGN-ADDRESS #x0000002A>

  CFFI> (pointerp *)
  => T

  CFFI> (pointer-address **)
  => 42

  CFFI> (inc-pointer *** -42)
  => #<FOREIGN-ADDRESS #x00000000>

  CFFI> (null-pointer-p *)
  => T

  CFFI> (typep ** 'foreign-pointer)
  => T

See Also
  inc_pointer
  null_pointer
  null_pointer_p
  pointerp
  pointer_address
  pointer_eq
  mem_ref

=cut

method make_pointer( JGoff::Lisp::CFFI::ForeignAddress $address ) {
  my ( $ptr );

  return ( $ptr );
}
# }}}

# {{{ mem_aptr
=head2 mem_aptr: The pointer to an element of an array.

(Pointers)

Syntax
  Accessor: mem-aptr ptr type &optional (index 0)

Arguments and Values
  ptr
    A foreign pointer.
  type
    A foreign type.
  index
    An integer.
  new-value
    A Lisp value compatible with type. 

Description
  The mem-aptr function finds the pointer to an element of the array.

  (mem-aptr ptr type n)
   
  ;; is identical to:
   
  (inc-pointer ptr (* n (foreign-type-size type)))

Examples
  CFFI> (with-foreign-string (str "Hello, foreign world!")
          (mem-aptr str :char 6))
  => #.(SB-SYS:INT-SAP #X0063D4B6)

=cut

method mem_aptr( JGoff::Lisp::CFFI::FunctionPointer $ptr, $type, @key ) {
}
# }}}

# {{{ mem_aref
=head2 mem_aref: Accesses the value of an index in an array.

(Pointers)

Syntax
  Accessor: mem-aref ptr type &optional (index 0)

  (setf (mem-aref ptr type &optional (index 0)) new-value)

Arguments and Values
  ptr
    A foreign pointer.
  type
    A foreign type.
  index
    An integer.
  new-value
    A Lisp value compatible with type. 

Description
  The mem-aref function is similar to mem-ref but will automatically calculate the offset from an index.

  (mem-aref ptr type n)
   
  ;; is identical to:
   
  (mem-ref ptr type (* n (foreign-type-size type)))

Examples
  CFFI> (with-foreign-string (str "Hello, foreign world!")
          (mem-aref str :char 6))
  => 32

  CFFI> (code-char *)
  => #\Space
   
  CFFI> (with-foreign-object (array :int 10)
          (loop for i below 10
                do (setf (mem-aref array :int i) (random 100)))
          (loop for i below 10 collect (mem-aref array :int i)))
  => (22 7 22 52 69 1 46 93 90 65)

Compatibility Note

  For compatibility with older versions of CFFI, mem-aref will produce a pointer for the deprecated bare structure specification, but it is consistent with other types for the current specification form (:struct structure-name) and provides a Lisp object translated from the structure (by default a plist). In order to obtain the pointer, you should use the new function mem-aptr.

See Also
  mem_ref
  mem_aptr

=cut

method mem_aref( JGoff::Lisp::CFFI::FunctionPointer $ptr, $type, @n ) {
}
# }}}

# {{{ mem_ref
=head2 mem_ref: Dereferences a pointer.

(Pointers)

Syntax
  Accessor: mem-ref ptr type &optional offset => object

Arguments and Values
  ptr
    A pointer.
  type
    A foreign type.
  offset
    An integer (in byte units).
  object
    The value ptr points to. 

Description

Examples
  CFFI> (with-foreign-string (ptr "Saluton")
          (setf (mem-ref ptr :char 3) (char-code #\a))
          (loop for i from 0 below 8
                collect (code-char (mem-ref ptr :char i))))
  => (#\S #\a #\l #\a #\t #\o #\n #\Null)

  CFFI> (setq ptr-to-int (foreign-alloc :int))
  => #<A Mac Pointer #x1047D0>

  CFFI> (mem-ref ptr-to-int :int)
  => 1054619

  CFFI> (setf (mem-ref ptr-to-int :int) 1984)
  => 1984

  CFFI> (mem-ref ptr-to-int :int)
  => 1984

See Also
  mem_aref

=cut

method mem_ref( JGoff::Lisp::CFFI::FunctionPointer $ptr, $type, @key ) {
}
# }}}

# {{{ null_pointer
=head2 null_pointer: Returns a NULL pointer.

(Pointers)

Syntax
  Function: null-pointer => pointer

Arguments and Values
  pointer
    A NULL pointer. 

Description
  The function null-pointer returns a null pointer.

Examples
  CFFI> (null-pointer)
  => #<A Null Mac Pointer>

  CFFI> (pointerp *)
  => T

See Also
  null_pointer_p
  make_pointer

=cut

method null_pointer( ) {
  my ( $pointer );
  $pointer = JGoff::Lisp::CFFI::ForeignPointer->new;

  return ( $pointer );
}
# }}}

# {{{ null_pointer_p
=head2 null_pointer_p: Tests a pointer for NULL value.

(Pointers)

Syntax
  Function: null-pointer-p ptr => boolean

Arguments and Values
  ptr
    A foreign pointer that may be a null pointer.
  boolean
    T or NIL. 

Description
  The function null-pointer-p returns true if ptr is a null pointer and false otherwise.

Examples
  CFFI> (null-pointer-p (null-pointer))
  => T

  (defun contains-str-p (big little)
    (not (null-pointer-p
          (foreign-funcall "strstr" :string big :string little :pointer))))
   
  CFFI> (contains-str-p "Popcorns" "corn")
  => T

  CFFI> (contains-str-p "Popcorns" "salt")
  => NIL

See Also
  null_pointer
  pointerp

=cut

method null_pointer_p( JGoff::Lisp::CFFI::FunctionPointer $ptr ) {
  my ( $boolean );

  return ( $boolean );
}
# }}}

# {{{ pointerp
=head2 pointerp: Tests whether an object is a pointer or not.

(Pointers)

Syntax
  Function: pointerp ptr => boolean

Arguments and Values
  ptr
    An object that may be a foreign pointer.
  boolean
    T or NIL. 

Description
  The function pointerp returns true if ptr is a foreign pointer and false otherwise.

Implementation-specific Notes

  In Allegro CL, foreign pointers are integers thus in this implementation pointerp will return true for any ordinary integer.

Examples
  CFFI> (foreign-alloc 32)
  => #<A Mac Pointer #x102D20>

  CFFI> (pointerp *)
  => T

  CFFI> (pointerp "this is not a pointer")
  => NIL

See Also
  make_pointer
  null_pointer_p

=cut

method pointerp( JGoff::Lisp::CFFI::FunctionPointer $ptr ) {
}
# }}}

# {{{ pointer_address
=head2 pointer_address: Returns the address pointed to by a pointer.

(Pointers)

Syntax
  Function: pointer-address ptr => address

Arguments and Values
  ptr
    A foreign pointer.
  address
    An integer. 

Description
  The function pointer-address will return the address of a foreign pointer ptr.

Examples
  CFFI> (pointer-address (null-pointer))
  => 0

  CFFI> (pointer-address (make-pointer 123))
  => 123

See Also
  make_pointer
  inc_pointer
  null_pointer
  null_pointer_p
  pointerp
  pointer_eq
  mem_ref

=cut

method pointer_address( JGoff::Lisp::CFFI::FunctionPointer $ptr ) {
  my ( $address );
  $address = $ptr->address;

  return ( $address );
}
# }}}

# {{{ pointer_eq
=head2 pointer_eq: Tests if two pointers point to the same address.

(Pointers)

Syntax
  Function: pointer-eq ptr1 ptr2 => boolean

Arguments and Values
  ptr1
  ptr2
    A foreign pointer.
  boolean
    T or NIL. 

Description
  The function pointer-eq returns true if ptr1 and ptr2 point to the same memory address and false otherwise.

Implementation-specific Notes

  The representation of foreign pointers varies across the various Lisp implementations as does the behaviour of the built-in Common Lisp equality predicates. Comparing two pointers that point to the same address with EQ Lisps will return true on some Lisps, others require more general predicates like EQL or EQUALP and finally some will return false using any of these predicates. Therefore, for portability, you should use POINTER-EQ.

Examples
  This is an example using SBCL, see the implementation-specific notes above.

  CFFI> (eql (null-pointer) (null-pointer))
  => NIL

  CFFI> (pointer-eq (null-pointer) (null-pointer))
  => T

See Also
  inc_pointer

=cut

method pointer_eq( JGoff::Lisp::CFFI::FunctionPointer $ptr1, JGoff::Lisp::CFFI::FunctionPointer $ptr2 ) {
  my ( $boolean );
  $boolean = $self->pointer_to_address( $ptr1 ) ==
             $self->pointer_to_address( $ptr2 );

  return ( $boolean );
}
# }}}

# {{{ with_foreign_pointer
=head2 with_foreign_pointer: Allocates memory with dynamic extent. 

(Pointers)

Syntax
  Macro: with-foreign-pointer (var size &optional size-var) &body body

Arguments and Values
  var
  size-var
    A symbol.
  size
    An integer.
  body
    A list of forms to be executed. 

Description
  The with-foreign-pointer macro, binds var to size bytes of foreign memory during body. The pointer in var is invalid beyond the dynamic extend of body and may be stack-allocated if supported by the implementation.

  If size-var is supplied, it will be bound to size during body.

Examples
  CFFI> (with-foreign-pointer (string 4 size)
          (setf (mem-ref string :char (1- size)) 0)
          (lisp-string-to-foreign "Popcorns" string size)
          (loop for i from 0 below size
                collect (code-char (mem-ref string :char i))))
  => (#\P #\o #\p #\Null)

See Also
  foreign_alloc
  foreign_free

=cut

method with_foreign_pointer() {
}
# }}}

# {{{ default_foreign_encoding
=head2 $default_foreign_encoding: Default encoding for the string types.

Syntax
  Special Variable: *default-foreign-encoding*

Value type

  A keyword.

Initial value

  :utf-8

Description
  This special variable holds the default foreign encoding.

Examples
  CFFI> *default-foreign-encoding*
  => :utf-8

  CFFI> (foreign-funcall "strdup" (:string :encoding :utf-16) "foo" :string)
  => "f"

  CFFI> (let ((*default-foreign-encoding* :utf-16))
          (foreign-funcall "strdup" (:string :encoding :utf-16) "foo" :string))
  => "foo"

See also

  Other Types (:string type)
  foreign_string_alloc
  foreign_string_to_lisp
  lisp_string_to_foreign
  with_foreign_string
  with_foreign_pointer_as_string

=cut

my $default_foreign_encoding = ''; # XXX # Default encoding for the string types.
# }}}

# {{{ foreign_string_alloc
=head2 foreign_string_alloc: Converts a Lisp string to a foreign string.

(Strings)

Syntax
  Function: foreign-string-alloc string &key encoding null-terminated-p start end => pointer

Arguments and Values
  string
    A Lisp string.
  encoding
    Foreign encoding. Defaults to *default-foreign-encoding*.
  null-terminated-p
    Boolean, defaults to true.
  start, end
    Bounding index designators of string. 0 and nil, by default.
  pointer
    A pointer to the newly allocated foreign string. 

Description
  The foreign-string-alloc function allocates foreign memory holding a copy of string converted using the specified encoding. Start specifies an offset into string and end marks the position following the last element of the foreign string.

  This string must be freed with foreign-string-free.

  If null-terminated-p is false, the string will not be null-terminated.

Examples
  CFFI> (defparameter *str* (foreign-string-alloc "Hello, foreign world!"))
  => #<FOREIGN-ADDRESS #x00400560>

  CFFI> (foreign-funcall "strlen" :pointer *str* :int)
  => 21

See Also
  foreign_string_free
  with_foreign_string

=cut

method foreign_string_alloc( Str $string, @key ) {
  my ( $pointer );
  $pointer = JGoff::Lisp::CFFI::FunctionPointer->new;

  return ( $pointer );
}
# }}}

# {{{ foreign_string_free
=head2 foreign_string_free: Deallocates memory used by a foreign string.

(Strings)

Syntax
  Function: foreign-string-free pointer

Arguments and Values
  pointer
    A pointer to a string allocated by foreign-string-alloc. 

Description
  The foreign-string-free function frees a foreign string allocated by foreign-string-alloc.

Examples

See Also
  foreign_string_alloc

=cut

method foreign_string_free( JGoff::Lisp::CFFI::FunctionPointer $pointer ) {
  return;
}
# }}}

# {{{ foreign_string_to_lisp
=head2 foreign_string_to_lisp: Converts a foreign string to a Lisp string.

(Strings)

Syntax
  Function: foreign-string-to-lisp ptr &key offset count max-chars encoding => string

Arguments and Values
  ptr
    A pointer.
  offset
    An integer greater than or equal to 0. Defauls to 0.
  count
    Either nil (the default), or an integer greater than or equal to 0.
  max-chars
    An integer greater than or equal to 0. (1- array-total-size-limit), by default.
  encoding
    Foreign encoding. Defaults to *default-foreign-encoding*.
  string
    A Lisp string. 

Description
  The foreign-string-to-lisp function converts at most count octets from ptr into a Lisp string, using the defined encoding.

  If count is nil (the default), characters are copied until max-chars is reached or a NULL character is found.

  If ptr is a null pointer, returns nil.

  Note that the :string type will automatically convert between Lisp strings and foreign strings.

Examples
  CFFI> (foreign-funcall "getenv" :string "HOME" :pointer)
  => #<FOREIGN-ADDRESS #xBFFFFFD5>

  CFFI> (foreign-string-to-lisp *)
  => "/Users/luis"

See Also
  lisp_string_to_foreign
  foreign_string_alloc

=cut

method foreign_string_to_lisp(
  JGoff::Lisp::CFFI::ForeignAddress $address, @key ) {
  my ( $string );

  return ( $string );
}
# }}}

# {{{ lisp_string_to_foreign
=head2 lisp_string_to_foreign: Copies a Lisp string into a foreign string.

(Strings)

Syntax
  Function: lisp-string-to-foreign string buffer bufsize &key start end offset encoding => buffer

Arguments and Values
  string
    A Lisp string.
  buffer
    A foreign pointer.
  bufsize
    An integer.
  start, end
    Bounding index designators of string. 0 and nil, by default.
  offset
    An integer greater than or equal to 0. Defauls to 0.
  encoding
    Foreign encoding. Defaults to *default-foreign-encoding*. 

Description
  The lisp-string-to-foreign function copies at most bufsize-1 octets from a Lisp string using the specified encoding into buffer+offset. The foreign string will be null-terminated.

  Start specifies an offset into string and end marks the position following the last element of the foreign string.

Examples
  CFFI> (with-foreign-pointer-as-string (str 255)
          (lisp-string-to-foreign "Hello, foreign world!" str 6))
  => "Hello"

See Also
  foreign_string_alloc
  foreign_string_to_lisp
  with_foreign_pointer_as_string

=cut

method lisp_string_to_foreign(
  $string, JGoff::Lisp::CFFI::FunctionPointer $buffer, $bufsize, @key ) {
  my ( $buffer );

  return ( $buffer );
}
# }}}

# {{{ with_foreign_string
=head2 with_foreign_string: Allocates a foreign string with dynamic extent.

(Strings)

Syntax
  Macro: with-foreign-string (var-or-vars string &rest args) &body body
  Macro: with-foreign-strings (bindings) &body body

  var-or-vars ::= var | (var &optional octet-size-var)
  bindings ::= {(var-or-vars string &rest args)}*

Arguments and Values
  var, byte-size-var
    A symbol.
  string
    A Lisp string.
  body
    A list of forms to be executed. 

Description
  The with-foreign-string macro will bind var to a newly allocated foreign string containing string. Args is passed to the underlying foreign-string-alloc call.

  If octet-size-var is provided, it will be bound the length of foreign string in octets including the null terminator.

Examples
  CFFI> (with-foreign-string (foo "12345")
          (foreign-funcall "strlen" :pointer foo :int))
  => 5
   
  CFFI> (let ((array (coerce #(84 117 114 97 110 103 97)
                             '(array (unsigned-byte 8)))))
          (with-foreign-string (foreign-string array)
            (foreign-string-to-lisp foreign-string)))
  => "Turanga"

See Also
  foreign_string_alloc
  with_foreign_pointer_as_string

=cut

method with_foreign_string() {
}
# }}}

# {{{ with_foreign_strings
=head2 with_foreign_strings: Plural form of with-foreign-string.

(Strings)

(see: with-foreign-strings)

=cut

method with_foreign_strings() {
}
# }}}

# {{{ with_foreign_pointer_as_string
=head2 with_foreign_pointer_as_string: Similar to CL's with-output-to-string. 

(Strings)

Syntax
  Macro: with-foreign-pointer-as-string (var size &optional size-var &rest args) &body body => string

Arguments and Values
  var
    A symbol.
  string
    A Lisp string.
  body
    List of forms to be executed. 

Description
  The with-foreign-pointer-as-string macro is similar to with-foreign-pointer except that var is used as the returned value of an implicit progn around body, after being converted to a Lisp string using the provided args.

Examples
  CFFI> (with-foreign-pointer-as-string (str 6 str-size :encoding :ascii)
          (lisp-string-to-foreign "Hello, foreign world!" str str-size))
  => "Hello"

See Also
  foreign_string_alloc
  with_foreign_string

=cut

method with_foreign_pointer_as_string() {
}
# }}}

# {{{ defcvar
=head2 defcvar: Defines a C global variable.

(Variables)

Syntax
  Macro: defcvar name-and-options type &optional documentation => lisp-name

  name-and-options ::= name | (name &key read-only (library :default))
  name ::= lisp-name [foreign-name] | foreign-name [lisp-name]

Arguments and Values
  foreign-name
    A string denoting a foreign function.
  lisp-name
    A symbol naming the Lisp function to be created.
  type
    A foreign type.
  read-only
    A boolean.
  documentation
    A Lisp string; not evaluated. 

Description
  The defcvar macro defines a symbol macro lisp-name that looks up foreign-name and dereferences it acording to type. It can also be setfed, unless read-only is true, in which case an error will be signaled.

  When one of lisp-name or foreign-name is omitted, the other is automatically derived using the following rules:

    Foreign names are converted to Lisp names by uppercasing, replacing underscores with hyphens, and wrapping around asterisks.
    Lisp names are converted to foreign names by lowercasing, replacing hyphens with underscores, and removing asterisks, if any. 

Examples
  CFFI> (defcvar "errno" :int)
  => *ERRNO*

  CFFI> (foreign-funcall "strerror" :int *errno* :string)
  => "Inappropriate ioctl for device"

  CFFI> (setf *errno* 1)
  => 1

  CFFI> (foreign-funcall "strerror" :int *errno* :string)
  => "Operation not permitted"

Trying to modify a read-only foreign variable:

  CFFI> (defcvar ("errno" +error-number+ :read-only t) :int)
  => +ERROR-NUMBER+
  CFFI> (setf +error-number+ 12)
  ;; error--> Trying to modify read-only foreign var: +ERROR-NUMBER+.

  Note that accessing errno this way won't work with every implementation of the C standard library.

See Also
  get_var_pointer

=cut

method defcvar() {
}
# }}}

# {{{ get_var_pointer
=head2 get_var_pointer: Returns a pointer to a defined global variable. 

(Variables)

Syntax
  Function: get-var-pointer symbol => pointer

Arguments and Values
  symbol
    A symbol denoting a foreign variable defined with defcvar.
  pointer
    A foreign pointer. 

Description
  The function get-var-pointer will return a pointer to the foreign global variable symbol previously defined with defcvar.

Examples
  CFFI> (defcvar "errno" :int :read-only t)
  => *ERRNO*

  CFFI> *errno*
  => 25

  CFFI> (get-var-pointer '*errno*)
  => #<A Mac Pointer #xA0008130>

  CFFI> (mem-ref * :int)
  => 25

See Also
  defcvar

=cut

method get_var_pointer( $symbol ) {
  my ( $pointer );
  $pointer = JGoff::Lisp::CFFI::FunctionPointer->new;

  return ( $pointer );
}
# }}}

# {{{ defcfun
=head2 defcfun: Defines a foreign function.

  # Note that this injects strlen() into the current package.
  $cffi->defcfun( 'strlen' => ':int', 'Calculate the length of a string',
    [ n => ':string' ] );

  is( strlen( 'Hello world' ), 11 );

(Functions)

Syntax
  Macro: defcfun name-and-options return-type &body [docstring] arguments [&rest] => lisp-name

  name-and-options ::= name | (name &key library convention)
  name ::= lisp-name [foreign-name] | foreign-name [lisp-name]
  arguments ::= { (arg-name arg-type) }*

Arguments and Values
  foreign-name
    A string denoting a foreign function.
  lisp-name
    A symbol naming the Lisp function to be created.
  arg-name
    A symbol.
  return-type
  arg-type
    A foreign type.
  convention
    One of :cdecl (default) or :stdcall.
  library
    A symbol designating a foreign library.
  docstring
    A documentation string. 

Description
  The defcfun macro provides a declarative interface for defining Lisp functions that call foreign functions.

  When one of lisp-name or foreign-name is omitted, the other is automatically derived using the following rules:

    Foreign names are converted to Lisp names by uppercasing and replacing underscores with hyphens.
    Lisp names are converted to foreign names by lowercasing and replacing hyphens with underscores. 

  If you place the symbol &rest in the end of the argument list after the fixed arguments, defcfun will treat the foreign function as a variadic function. The variadic arguments should be passed in a way similar to what foreign-funcall would expect. Unlike foreign-funcall though, defcfun will take care of doing argument promotion. Note that in this case defcfun will generate a Lisp macro instead of a function and will only work for Lisps that support foreign-funcall.

  If a foreign structure is to be passed or returned by value (that is, the type is of the form (:struct ...)), then the cffi-libffi system must be loaded, which in turn depends on libffi, including the header files. Failure to load that system will result in an error. Variadic functions cannot at present accept or return structures by value.

Examples
  (defcfun "strlen" :int
    "Calculate the length of a string."
    (n :string))

  $cffi->defcfun(
    strlen => ':int',
    'Calculate the length of a string.',
    [ n => ':string' ] );
   
  CFFI> (strlen "123")
  => 3

  is( strlen( '123' ), 3 );

  (defcfun ("abs" c-abs) :int (n :int))

  $cffi->defcfun(
    [ abs => 'c-abs' ] => ':int',
    [ n => ':int' ] );
   
  CFFI> (c-abs -42)
  => 42

  is( c_abs( -42 ), 42 );

Function without arguments:

  (defcfun "rand" :int)

  $cffi->defcfun( rand => ':int' );
   
  CFFI> (rand)
  => 1804289383

  is( rand(), 1804289383 );

Variadic function example:

  (defcfun "sprintf" :int
    (str :pointer)
    (control :string)
    &rest)

  # JMG 'sprintf' already exists, override to 'Sprintf'
  $cffi->defcfun(
    [ sprintf => 'Sprintf' ] => ':int',
    [ str =>  ':pointer' ],
    [ control => ':string' ],
    '&rest' );
   
  CFFI> (with-foreign-pointer-as-string (s 100)
          (Sprintf s "%c %d %.2f %s" :char 90 :short 42 :float pi
                   :string "super-locrian"))
  => "A 42 3.14 super-locrian"

See Also
  foreign_funcall
  foreign_funcall_pointer

=cut

# If $lisp_name is a string, it's the name of the function to link in.
# If it's an arrayref, the first item is the name, and the second is the
# Lisp... er, perl name of the function.
#
# Really the distinction isn't necessary, but I'll keep it around for a while.
#
method defcfun( $lisp_name, $arg_name, $return_type, $arg_type, $convention ) {
  my ( $function_ref );
  *{$lisp_name} = sub { ... }; # XXX

  return ( $function_ref );
}
# }}}

# {{{ foreign_funcall
=head2 foreign_funcall: Performs a call to a foreign function.

(Functions)

Syntax
  Macro: foreign-funcall name-and-options &rest arguments => return-value

  arguments ::= { arg-type arg }* [return-type]
  name-and-options ::= name | ( name &key library convention)

Arguments and Values
  name
    A Lisp string.
  arg-type
    A foreign type.
  arg
    An argument of type arg-type.
  return-type
    A foreign type, :void by default.
  return-value
    A lisp object.
  library
    A lisp symbol; not evaluated.
  convention
    One of :cdecl (default) or :stdcall. 

Description
  The foreign-funcall macro is the main primitive for calling foreign functions.

  If a foreign structure is to be passed or returned by value (that is, the type is of the form (:struct ...)), then the cffi-libffi system must be loaded, which in turn depends on libffi, including the header files. Failure to load that system will result in an error. Variadic functions cannot at present accept or return structures by value.

  Note: The return value of foreign-funcall on functions with a :void return type is still undefined.

Implementation-specific Notes

  Corman Lisp does not support foreign-funcall. On implementations that don't support foreign-funcall cffi-sys::no-foreign-funcall will be present in *features*. Note: in these Lisps you can still use the defcfun interface. 

Examples
  CFFI> (foreign-funcall "strlen" :string "foo" :int)
  => 3

  Given the C code:

  void print_number(int n)
  {
      printf("N: %d\n", n);
  }

  CFFI> (foreign-funcall "print_number" :int 123456)
  -| N: 123456
  => NIL

Or, equivalently:

  CFFI> (foreign-funcall "print_number" :int 123456 :void)
  -| N: 123456
  => NIL

  CFFI> (foreign-funcall "printf" :string (format nil "%s: %d.~%")
                         :string "So long and thanks for all the fish"
                         :int 42 :int)
  -| So long and thanks for all the fish: 42.
  => 41

See Also
  defcfun
  foreign_funcall_pointer

=cut

method foreign_funcall( $function_name, $type ) {
}
# }}}

# {{{ foreign_funcall_pointer
=head2 foreign_funcall_pointer: Performs a call through a foreign pointer.

(Functions)

Syntax
  Macro: foreign-funcall-pointer pointer options &rest arguments => return-value

  arguments ::= { arg-type arg }* [return-type]
  options ::= ( &key convention )

Arguments and Values
  pointer
    A foreign pointer.
  arg-type
    A foreign type.
  arg
    An argument of type arg-type.
  return-type
    A foreign type, :void by default.
  return-value
    A lisp object.
  convention
    One of :cdecl (default) or :stdcall. 

Description
  The foreign-funcall macro is the main primitive for calling foreign functions.

  Note: The return value of foreign-funcall on functions with a :void return type is still undefined.

Implementation-specific Notes

  Corman Lisp does not support foreign-funcall. On implementations that don't support foreign-funcall cffi-sys::no-foreign-funcall will be present in *features*. Note: in these Lisps you can still use the defcfun interface. 

Examples
  CFFI> (foreign-funcall-pointer (foreign-symbol-pointer "abs") ()
                                 :int -42 :int)
  => 42

See Also
  defcfun
  foreign_funcall

=cut

method foreign_funcall_pointer() {
}
# }}}

# {{{ translate_camelcase_name
=head2 translate_camelcase_name: Converts a camelCase foreign name to/from a Lisp name.

(Functions)

Syntax
  Function: translate-camelcase-name name &key upper-initial-p special-words => return-value

Arguments and Values
  name
    Either a symbol or a string.
  upper-initial-p
    A generalized boolean.
  special words
    A list of strings.
  return-value
    If name is a symbol, this is a string, and vice versa. 

Description
  translate-camelcase-name is a helper function for specializations of translate-name-from-foreign and translate-name-to-foreign. It handles the common case of converting between foreign camelCase names and lisp names. upper-initial-p indicates whether the first letter of the foreign name should be uppercase. special-words is a list of strings that should be treated atomically in translation. This list is case-sensitive.

Examples
  CFFI> (translate-camelcase-name some-xml-function)
  => "someXmlFunction"

  CFFI> (translate-camelcase-name some-xml-function :upper-initial-p t)
  => "SomeXmlFunction"

  CFFI> (translate-camelcase-name some-xml-function :special-words '("XML"))
  => "someXMLFunction"

  CFFI> (translate-camelcase-name "someXMLFunction")
  => SOME-X-M-L-FUNCTION

  CFFI> (translate-camelcase-name "someXMLFunction" :special-words '("XML"))
  => SOME-XML-FUNCTION

See Also
  translate_name_from_foreign
  translate_name_to_foreign
  translate_underscore_separated_name

=cut

method translate_camelcase_name ( $name, @key ) {
  my ( $return_value );

  return ( $return_value );
}
# }}}

# {{{ translate_name_from_foreign
=head2 translate_name_from_foreign: Converts a foreign name to a Lisp name.

(Functions)

Syntax
  Function: translate-name-from-foreign foreign-name package &optional varp => symbol

Arguments and Values
  foreign-name
    A string denoting a foreign function.
  package
    A Lisp package
  varp
    A generalized boolean.
  symbol
    The Lisp symbol to be used a function name. 

Description
  translate-name-from-foreign is used by defcfun to handle the conversion of foreign names to lisp names. By default, it translates using translate-underscore-separated-name. However, you can create specialized methods on this function to make translating more closely match the foreign library's naming conventions.

  Specialize package on some package. This allows other packages to load libraries with different naming conventions.

Examples
  CFFI> (defcfun "someXmlFunction" ...)
  => SOMEXMLFUNCTION

  $cffi->defcfun( 'someXmlFunction' => ... );

  CFFI> (defmethod translate-name-from-foreign ((spec string)
                                                (package (eql *package*))
                                                &optional varp)
          (let ((name (translate-camelcase-name spec)))
            (if varp (intern (format nil "*~a*" name)) name)))
  => #<STANDARD-METHOD TRANSLATE-NAME-FROM-FOREIGN (STRING (EQL #<Package "SOME-PACKAGE">))>

  CFFI> (defcfun "someXmlFunction" ...)
  => SOME-XML-FUNCTION

  $cffi->defcfun( 'someXmlFunction' => ... );

See Also
  defcfun
  translate_camelcase_name
  translate_name_to_foreign
  translate_underscore_separated_name

=cut

method translate_name_from_foreign( $foreign_name, $package, $optional_varp ) {
  my ( $symbol );

  return ( $symbol );
}
# }}}

# {{{ translate_name_to_foreign
=head2 translate_name_to_foreign: Converts a Lisp name to a foreign name.

(Functions)

Syntax
  Function: translate-name-from-foreign foreign-name package &optional varp => symbol

Arguments and Values
  foreign-name
    A string denoting a foreign function.
  package
    A Lisp package
  varp
    A generalized boolean.
  symbol
    The Lisp symbol to be used a function name. 

Description
  translate-name-from-foreign is used by defcfun to handle the conversion of foreign names to lisp names. By default, it translates using translate-underscore-separated-name. However, you can create specialized methods on this function to make translating more closely match the foreign library's naming conventions.

  Specialize package on some package. This allows other packages to load libraries with different naming conventions.

Examples
  CFFI> (defcfun "someXmlFunction" ...)
  => SOMEXMLFUNCTION

  $cffi->defcfun( someXmlFunction => ... );

  CFFI> (defmethod translate-name-from-foreign ((spec string)
                                                (package (eql *package*))
                                                &optional varp)
          (let ((name (translate-camelcase-name spec)))
            (if varp (intern (format nil "*~a*" name)) name)))
  => #<STANDARD-METHOD TRANSLATE-NAME-FROM-FOREIGN (STRING (EQL #<Package "SOME-PACKAGE">))>

  CFFI> (defcfun "someXmlFunction" ...)
  => SOME-XML-FUNCTION

  $cffi->defcfun( someXmlFunction => ... );

See Also
  defcfun
  translate_camelcase_name
  translate_name_to_foreign
  translate_underscore_separated_name

=cut

method translate_name_to_foreign( $foreign_name, $package, $optional_varp ) {
  my ( $symbol );

  return ( $symbol );
}
# }}}

# {{{ translate_underscore_separated_name
=head2 translate_underscore_separated_name: Converts an underscore_separated foreign name to/from a Lisp name. 

(Functions)

Syntax
  Function: translate-underscore-separated-name name => return-value

Arguments and Values
  name
    Either a symbol or a string.
  return-value
    If name is a symbol, this is a string, and vice versa. 

Description
  translate-underscore-separated-name is a helper function for specializations of translate-name-from-foreign and translate-name-to-foreign. It handles the common case of converting between foreign underscore_separated names and lisp names.

Examples
  CFFI> (translate-underscore-separated-name some-xml-function)
  => "some_xml_function"

  CFFI> (translate-camelcase-name "some_xml_function")
  => SOME-XML-FUNCTION

See Also
  translate_name_from_foreign
  translate_name_to_foreign
  translate_camelcase_name

=cut

method translate_underscore_separated_name( Str $name ) {
  my ( $return_value );
  $name =~ s{ _ }{ - }gx;
  $return_value = $name;

  return ( $return_value );
}
# }}}

# {{{ close_foreign_library
=head2 close_foreign_library: Closes a foreign library.

(Libraries)

Syntax
  Function: close-foreign-library library => success

Arguments and Values
  library
    A symbol or an instance of foreign-library.
  success
    A Lisp boolean. 

Description
  Closes library which can be a symbol designating a library define through define-foreign-library or an instance of foreign-library as returned by load-foreign-library.

See Also
  define_foreign_library
  load_foreign_library
  use_foreign_library

=cut

method close_foreign_library( $library ) {
  my ( $success );

  return ( $success );
}
# }}}

my $darwin_framework_directories; # XXX #: Search path for Darwin frameworks.

# {{{ define_foreign_library
=head2 define_foreign_library: Explain how to load a foreign library.

  $definition = $cffi->define_foreign_library( ... );

(Libraries)

Syntax
  Macro: define-foreign-library name-and-options { load-clause }* => name

  name-and-options ::= name | (name &key convention search-path)
  load-clause ::= (feature library &key convention search-path)

Arguments and Values
  name
    A symbol.
  feature
    A feature expression.
  library
    A library designator.
  convention
    One of :cdecl (default) or :stdcall
  search-path
    A path or list of paths where the library will be searched if not found in system-global directories. Paths specified in a load clause take priority over paths specified as library option, with *foreign-library-directories* having lowest priority. 

Description
  Creates a new library designator called name. The load-clauses describe how to load that designator when passed to load-foreign-library or use-foreign-library.

  When trying to load the library name, the relevant function searches the load-clauses in order for the first one where feature evaluates to true. That happens for any of the following situations:

    If feature is a symbol present in common-lisp:*features*.
    If feature is a list, depending on (first feature), a keyword:

    :and
        All of the feature expressions in (rest feature) are true.
    :or
        At least one of the feature expressions in (rest feature) is true.
    :not
        The feature expression (second feature) is not true. 

    Finally, if feature is t, this load-clause is picked unconditionally. 

  Upon finding the first true feature, the library loader then loads the library. The meaning of “library designator” is described in load-foreign-library.

  Functions associated to a library defined by define-foreign-library (e.g. through defcfun's :library option, will inherit the library's options. The precedence is as follows:

    defcfun/foreign-funcall specific options;
    load-clause options;
    global library options (the name-and-options argument) 

Examples
  See Loading foreign libraries.

See Also
  close_foreign_library
  load_foreign_library

=cut

#
# Unlike defcfun's first argument, 'libcurl' doesn't tell us anything about
# what the library's real name is. it could probably be 'hooberbloob' and the
# rest of the code would work just fine.
#
# Also, (defcfun function_name) actually exports function_name() into the
# current package, so we need the name.
#
method define_foreign_library( @feature_tuples ) {
  my ( $library_descriptor );
  $library_descriptor = JGoff::Lisp::CFFI::LibraryDescriptor->new(
    object => @feature_tuples );

  return $library_descriptor;
}
# }}}

# {{{ $foreign_library_directories
=head2 $foreign_library_directories

Syntax
  Special Variable: *foreign-library-directories*

Value type

  A list, in which each element is a string, a pathname, or a simple Lisp expression.
Initial value

  The empty list.

Description
  You should not have to use this variable.

  Most, if not all, Lisps supported by CFFI have a reasonable default search algorithm for foreign libraries. For example, Lisps for unix usually call dlopen(3), which in turn looks in the system library directories. Only if that fails does CFFI look for the named library file in these directories, and load it from there if found.

  Thus, this is intended to be a CFFI-only fallback to the library search configuration provided by your operating system. For example, if you distribute a foreign library with your Lisp package, you can add the library's containing directory to this list and portably expect CFFI to find it.

  A simple Lisp expression is intended to provide functionality commonly used in search paths such as ASDF's13, and is defined recursively as follows:14

    A list, whose `first' is a function designator, and whose `rest' is a list of simple Lisp expressions to be evaluated and passed to the so-designated function. The result is the result of the function call.
    A symbol, whose result is its symbol value.
    Anything else evaluates to itself. 

Examples
  $ ls
  -| liblibli.so    libli.lisp

  In libli.lisp:

    (pushnew #P"/home/sirian/lisp/libli/" *foreign-library-directories*
             :test #'equal)
   
    (load-foreign-library '(:default "liblibli"))

  The following example would achieve the same effect:

    (pushnew '(merge-pathnames #p"lisp/libli/" (user-homedir-pathname))
              *foreign-library-directories*
              :test #'equal)
    => ((MERGE-PATHNAMES #P"lisp/libli/" (USER-HOMEDIR-PATHNAME)))
   
    (load-foreign-library '(:default "liblibli"))

See also

  $darwin_framework_directories
  define_foreign_library

=cut

my $foreign_library_directories; # XXX # Search path for shared libraries.
# }}}

# {{{ load_foreign_library
=head2 load_foreign_library: Load a foreign library.

(Libraries)

Syntax
  Function: load-foreign-library library-designator => library

Arguments and Values
  library-designator
    A library designator.
  library-designator
    An instance of foreign-library. 

Description
  Load the library indicated by library-designator. A library designator is defined as follows:

    If a symbol, is considered a name previously defined with define-foreign-library.
    If a string or pathname, passed as a namestring directly to the implementation's foreign library loader. If that fails, search the directories in *foreign-library-directories* with cl:probe-file; if found, the absolute path is passed to the implementation's loader.
    If a list, the meaning depends on (first library):

    :framework
        The second list element is taken to be a Darwin framework name, which is then searched in *darwin-framework-directories*, and loaded when found.
    :or
        Each remaining list element, itself a library designator, is loaded in order, until one succeeds.
    :default
        The name is transformed according to the platform's naming convention to shared libraries, and the resultant string is loaded as a library designator. For example, on unix, the name is suffixed with .so. 

  If the load fails, signal a load-foreign-library-error.

  Please note: For system libraries, you should not need to specify the directory containing the library. Each operating system has its own idea of a default search path, and you should rely on it when it is reasonable.
Implementation-specific Notes

  On ECL platforms where its dynamic FFI is not supported (ie. when :dffi is not present in *features*), cffi:load-foreign-library does not work and you must use ECL's own ffi:load-foreign-library with a constant string argument.

Examples
  See Loading foreign libraries.

See Also
  close_foreign_library
  $darwin_framework_directories
  define_foreign_library
  $foreign_library_directories
  load_foreign_library_error
  use_foreign_library

=cut

method load_foreign_library( $library_designator ) {
  my ( $library );
  $library = JGoff::Lisp::CFFI::FunctionLibrary->new(
    object => $library_designator );

  return ( $library );
}
# }}}

# {{{ load_foreign_library_error
=head2 load_foreign_library_error: Signalled on failure of its namesake.

(Libraries)

Syntax
  Condition Type: load-foreign-library-error

Class precedence list

  load-foreign-library-error, error, serious-condition, condition, t

Description
  Signalled when a foreign library load completely fails. The exact meaning of this varies depending on the real conditions at work, but almost universally, the implementation's error message is useless. However, CFFI does provide the useful restarts retry and use-value; invoke the retry restart to try loading the foreign library again, or the use-value restart to try loading a different foreign library designator.

See also

  load-foreign-library

=cut

method load_foreign_library_error() {
}
# }}}

# {{{ use_foreign_library
=head2 use_foreign_library: Load a foreign library when needed. 

(Libraries)

Syntax
  Macro: use-foreign-library name

Arguments and values
  name
    A library designator; unevaluated. 

Description
  See load-foreign-library, for the meaning of “library designator”. This is intended to be the top-level form used idiomatically after a define-foreign-library form to go ahead and load the library. Finally, on implementations where the regular evaluation rule is insufficient for foreign library loading, it loads it at the required time.

Examples
  See Loading foreign libraries.

See also

  load_foreign_library

=cut

method use_foreign_library(
  JGoff::Lisp::CFFI::ForeignLibraryDescriptor $library_descriptor ) {
}
# }}}

# {{{ callback
=head2 callback: Returns a pointer to a defined callback.

(Callbacks)

Syntax
  Macro: callback symbol => pointer

Arguments and Values
  symbol
    A symbol denoting a callback.
  pointer
  new-value
    A pointer. 

Description
  The callback macro is analogous to the standard CL special operator function and will return a pointer to the callback denoted by the symbol name.

Examples
  CFFI> (defcallback sum :int ((a :int) (b :int))
          (+ a b))
  => SUM

  CFFI> (callback sum)
  => #<A Mac Pointer #x102350>

See Also
  get_callback
  defcallback

=cut

method callback {
}
# }}}

# {{{ defcallback
=head2 defcallback: Defines a Lisp callback.

(Callbacks)

Syntax
  Macro: defcallback name-and-options return-type arguments &body body => name

  name-and-options ::= name | (name &key convention)
  arguments ::= ({ (arg-name arg-type) }*)

Arguments and Values
  name
    A symbol naming the callback created.
  return-type
    The foreign type for the callback's return value.
  arg-name
    A symbol.
  arg-type
    A foreign type.
  convention
    One of :cdecl (default) or :stdcall. 

Description
  The defcallback macro defines a Lisp function that can be called from C. The arguments passed to this function will be converted to the appropriate Lisp representation and its return value will be converted to its C representation.

  This Lisp function can be accessed by the callback macro or the get-callback function.

  Portability note: defcallback will not work correctly on some Lisps if it's not a top-level form.

Examples
  (defcfun "qsort" :void
    (base :pointer)
    (nmemb :int)
    (size :int)
    (fun-compar :pointer))

  $cffi->defcfun(
    qsort => ':void', 
    [ base => ':pointer' ],
    [ nmemb => ':int' ],
    [ size => ':int' ],
    [ fun-compar => ':pointer' ] );
   
  (defcallback < :int ((a :pointer) (b :pointer))
    (let ((x (mem-ref a :int))
          (y (mem-ref b :int)))
      (cond ((> x y) 1)
            ((< x y) -1)
            (t 0))))
   
  CFFI> (with-foreign-object (array :int 10)
          ;; Initialize array.
          (loop for i from 0 and n in '(7 2 10 4 3 5 1 6 9 8)
                do (setf (mem-aref array :int i) n))
          ;; Sort it.
          (qsort array 10 (foreign-type-size :int) (callback <))
          ;; Return it as a list.
          (loop for i from 0 below 10
                collect (mem-aref array :int i)))
  => (1 2 3 4 5 6 7 8 9 10)

See Also
  callback
  get_callback

=cut

method defcallback {
}
# }}}

# {{{ get_callback
=head2 get_callback: Returns a pointer to a defined callback. 

(Callbacks)

Syntax
  Accessor: get-callback symbol => pointer

Arguments and Values
  symbol
    A symbol denoting a callback.
  pointer
    A pointer. 

Description
  This is the functional version of the callback macro. It returns a pointer to the callback named by symbol suitable, for example, to pass as arguments to foreign functions.

Examples
  CFFI> (defcallback sum :int ((a :int) (b :int))
          (+ a b))
  => SUM

  CFFI> (get-callback 'sum)
  => #<A Mac Pointer #x102350>

See Also
  callback
  defcallback

=cut

method get_callback {
}
# }}}

=head1 AUTHOR

Jeffrey Goff, C<< <jgoff at cpan.org> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-jgoff-lisp-cffi at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=JGoff-Lisp-CFFI>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc JGoff::Lisp::CFFI


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
