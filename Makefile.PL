use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME                => 'JGoff::Lisp::CFFI',
    AUTHOR              => q{Jeffrey Goff <jgoff@cpan.org>},
    VERSION_FROM        => 'lib/JGoff/Lisp/CFFI.pm',
    ABSTRACT_FROM       => 'lib/JGoff/Lisp/CFFI.pm',
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
      'Test::More' => 0,
      'Moose' => 0,
      'Moose::Util::TypeConstraints' => 0,
#      'MooseX::Method::Signatures' => 0,
      'Function::Parameters' => 0,
    },
    dist  => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    clean => { FILES => 'JGoff-Lisp-CFFI-*' },
);
