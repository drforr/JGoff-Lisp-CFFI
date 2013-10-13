#!perl -T

use Test::More tests => 27;
BEGIN {
  use_ok( 'JGoff::Lisp::CFFI' ) || print "Bail out!\n";
}

my $cffi = JGoff::Lisp::CFFI->new;

{ my $foreign =
    $cffi->convert_to_foreign( "a boat", ':string' );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
}

{ my ( $params, $foreign ) =
    $cffi->convert_to_foreign( "a boat", ':string' );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
  is( $params, 2 );
}

{ my $foreign = $cffi->convert_to_foreign( "a boat", ':string' );
  my $object = $cffi->convert_from_foreign( $foreign, ':string' );

  isa_ok(
    $foreign,
    'JGoff::Lisp::CFFI::ForeignAddress'
  );
}

{ my $flags = $cffi->defbitfield(
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

{ my $flags = $cffi->defbitfield(
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

{ my $flags = $cffi->defbitfield(
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

{ my $flags = $cffi->defbitfield(
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

{ my $enum = $cffi->defcenum(
    ':no',
    ':yes'
  );
  isa_ok( $enum, 'JGoff::Lisp::CFFI::ForeignEnum' );

  is( $cffi->foreign_enum_keyword( $enum, 0 ), ':NO' );
  is( $cffi->foreign_enum_keyword( $enum, 1 ), ':YES' );

  is( $cffi->foreign_enum_value( $enum, ':no' ), 0 );
  is( $cffi->foreign_enum_value( $enum, ':yes' ), 1 );
}

{ my $rect = $cffi->defcstruct(
    [ 'x' => ':int' ],
    [ 'y' => ':int' ],
    [ 'width' => ':int' ],
    [ 'height' => ':int' ],
  );
  isa_ok( $rect, 'JGoff::Lisp::CFFI::ForeignCStruct' );

  is_deeply(
    [ $cffi->foreign_slot_names( $rect ) ],
    [ 'X', 'Y', 'WIDTH', 'HEIGHT' ]
  );

  is( $cffi->foreign_slot_offset( $rect, 'x' ), 0 );
  is( $cffi->foreign_slot_offset( $rect, 'y' ), 2 );
  is( $cffi->foreign_slot_offset( $rect, 'width' ), 4 );
  is( $cffi->foreign_slot_offset( $rect, 'height' ), 6 );
}

### ### 4.3 Loading foreign libraries
###
### ###   (define-foreign-library libcurl
### ###     (:unix (:or "libcurl.so.3" "libcurl.so"))
### ###     (t (:default "libcurl")))
###
### my $libcurl = $cffi->define_foreign_library(
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
### $cffi->defcfun( curl_easy_init => ':pointer' );
### 
### ###   (defcfun "curl_easy_cleanup" :void
### ###     (easy-handle :pointer))
### 
### $cffi->defcfun( curl_easy_cleanup => ':void',
###   [ $easy_handle => ':pointer' ] );
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
### my $easy_handle = $cffi->defctype( ':pointer' );
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
###   [ ':actual-type' => ':pointer' ],
###   [ ':simple-parser' => $easy_handle ] );
### 
### ###   (defmethod translate-to-foreign (handle (type easy-handle-type))
### ###     "Extract the pointer from an easy-HANDLE."
### ###     (slot-value handle 'pointer))
### ### 
