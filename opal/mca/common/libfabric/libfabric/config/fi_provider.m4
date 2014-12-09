dnl Macros to help setup FI providers

dnl
dnl Helper macro called from top-level configure.ac to get ready to
dnl configure providers.
dnl
AC_DEFUN([FI_PROVIDER_INIT],[
	PROVIDERS_TO_BUILD=
	PROVIDERS_DL=
	PROVIDERS_STATIC=
	PROVIDERS_COUNT=
])

dnl
dnl Helper macro called from top-level configure.ac to finalize
dnl after all providers have been initialized
dnl
AC_DEFUN([FI_PROVIDER_FINI],[
	AC_SUBST(PROVIDERS_TO_BUILD)
	AC_SUBST(PROVIDERS_DL)
	AC_SUBST(PROVIDERS_STATIC)
])

dnl Helper macro called from top-level configure.ac to setup a
dnl provider.
dnl
dnl 1. Sets up --enable-<provider_name>
dnl 2. Checks for --enable-<provider_name>=dl;
dnl    sets $<provider_name>_dl to 0 or 1
dnl 3. Sets $enable_<provider_name> to "yes" or "no"
dnl 4. Calls <provider_name>_CONFIGURE m4 macro
dnl 5. If a directory was provider in --enable-<provider_name>, ensure
dnl    it is sane
dnl 6. Calls <provider_name>_CONDITIONALS m4 macro
dnl 7. Outputs whether this provider will be built or not, and if so,
dnl    whether it is static or a DSO
dnl
dnl Arguments:
dnl
dnl $1: provider name (must be same as directory name)
dnl
dnl Shell variable outputs:
dnl
dnl enable_$1: yes, no, or auto
dnl $1_dl: 1 if the provider is supposed to be built as a DSO, 0 otherwise
dnl
dnl AC_DEFINE outputs:
dnl
dnl HAVE_$1_DL: same value as $1_dl
dnl
AC_DEFUN([FI_PROVIDER_SETUP],[
	AC_MSG_NOTICE([*** Configuring $1 provider])
	AC_ARG_ENABLE([$1],
	      [AS_HELP_STRING([--enable-$1],
			      [Enable $1 provider @<:@default=auto@:>@])
	      ],
	      [],
	      [enable_$1=auto])

	# Check the --enable-<$1> value
	$1_dl=0
	AS_CASE([$enable_$1],
	[yes|no], [],
	[dl],     [enable_$1=yes $1_dl=1],
	[auto],   [],
	[FI_CHECK_PREFIX_DIR([$enable_$1])
	 enable_$1=yes]
	)

	# Call the provider's CONFIGURE and CONDITIONALS macros
	m4_include([prov/]$1[/configure.m4])
	_FI_PROVIDER_INVOKE($1, [CONFIGURE], [yes], [yes])
	_FI_PROVIDER_INVOKE($1, [CONDITIONALS], [no], [no])

	# See if the provider configured successfully
	AS_IF([test $$1_happy -eq 1],
		[PROVIDERS_TO_BUILD="$PROVIDERS_TO_BUILD $1"
		 PROVIDERS_COUNT=$((PROVIDERS_COUNT+1))
		 AS_IF([test $$1_dl -eq 1],
			[PROVIDERS_DL="prov/$1/lib$1.la $PROVIDERS_DL"
			 AS_IF([test x"$enable_static" = x"yes" &&
				test x"$enable_shared" = x"no"],
				[AC_MSG_WARN([$1 provider was selected to be built as DL])
				 AC_MSG_WARN([but libfabric is being built as static-only])
				 AC_MSG_ERROR([This is an impossible situation. Cannot continue.])])
			],
			[PROVIDERS_STATIC="prov/$1/lib$1.la $PROVIDERS_STATIC"])
		],
		[AC_MSG_NOTICE([$1 provider disabled])])

	# Set conditionals for HAVE_<provider> and HAVE_<provider>_DL
	AM_CONDITIONAL([HAVE_]m4_translit([$1], [a-z], [A-Z]),
		[test $$1_happy -eq 1])
	AM_CONDITIONAL([HAVE_]m4_translit([$1], [a-z], [A-Z])[_DL],
		[test $$1_dl -eq 1])

	# If this provier was specifically requested but we can't
	# build it, error.
	AS_IF([test "$enable_$1 $$1_happy" = "yes 0"],
	      [AC_MSG_WARN([$1 provider was requested, but cannot be compiled])
	       AC_MSG_ERROR([Cannot continue])
	      ])
	# If this provider was requested for direct build, ensure that
	# provider's fi_direct.h exists in tree. Error otherwise.
	AS_IF([test x"$enable_direct" = x"$1"],
		[AC_CHECK_FILE(prov/$1/include/rdma/fi_direct.h, [],
			[AC_MSG_WARN([$1 provider was requested as direct, but is missing required files])
			 AC_MSG_ERROR([Cannot continue])])])
])


dnl
dnl Helper macro that can use to check that a user-provided directory
dnl is valid as the root of an installation tree.  I.e., that it has an
dnl include and lib or lib64 directory.  This helps prevent users from
dnl specifying incorrect/invalid directories on the configure command line
dnl (e.g., typoing a directory name and then wondering why a given
dnl provider chooses not to build).
dnl
dnl Arguments:
dnl
dnl $1: directory to check
dnl
AC_DEFUN([FI_CHECK_PREFIX_DIR],[
	# Check that the base directory exists
	AS_IF([test ! -d "$1"],
	       [AC_MSG_WARN([supplied directory "$1" does not exist])
	        AC_MSG_ERROR([Cannot continue])
	       ])

	# Check that base/include exists
	 AS_IF([test -d "$1/include"],
	       [CPPFLAGS="-I$1/include"],
	       [AC_MSG_WARN([could not find "include" subdirectory in supplied "$1" directory"])
	        AC_MSG_ERROR([Cannot continue])
	       ])

	# Check that base/lib or base/lib64 exists
	 AS_IF([test -d "$1/lib"],
	       [LDFLAGS="-L$1/lib"],
	       [AS_IF([test -d "$1/lib64"],
		      [LDFLAGS="-L$1/lib64"],
		      [AC_MSG_WARN([could not find "lib" or "lib64" subdirectories in supplied "$1" directory"])
		       AC_MSG_ERROR([Cannot continue])
		      ])
	       ])
	])

dnl ------------------------------------------------------------------------

dnl
dnl Internal; should not be called from provder .m4 scripts.
dnl Helper macro to invoke the AC_DEFUN'ed macros down in the providers
dnl
dnl Arguments:
dnl
dnl $1: name of the provider
dnl $2: suffix of the macro to invoke
dnl $3: whether to pass the happy/sad parameters to the invoked macro
dnl $4: whether the macro must exist or not
dnl
AC_DEFUN([_FI_PROVIDER_INVOKE],[
	dnl If the FI_<provider>_<suffix> macro is defined, invoke it.
	m4_ifdef([FI_]m4_translit([$1], [a-z], [A-Z])[_$2],
	    [m4_if([$3], [yes],
		[FI_]m4_translit([$1], [a-z], [A-Z])[_$2([$1_happy=1],[$1_happy=0])],
		[FI_]m4_translit([$1], [a-z], [A-Z])[_$2()]
	     )],
	     dnl If $4 is yes and the macro does not exist, error
	    [m4_if([$4], [yes],
		[m4_fatal([$1 provider did not define FI_]m4_translit([$1], [a-z], [A-Z])[_$2 macro in prov/$1/configure.m4])],
		[])]
	    )
])
