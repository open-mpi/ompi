dnl Configury specific to the libfabric sockets provider

dnl Called to configure this provider
dnl
dnl Arguments:
dnl
dnl $1: action if configured successfully
dnl $2: action if not configured successfully
dnl
AC_DEFUN([FI_SOCKETS_CONFIGURE],[
	# Determine if we can support the sockets provider
	sockets_h_happy=0
	sockets_shm_happy=0
	AS_IF([test x"$enable_sockets" != x"no"],
	      [AC_CHECK_HEADER([sys/socket.h], [sockets_h_happy=1],
	                       [sockets_h_happy=0])


	       # check if shm_open is already present
	       AC_CHECK_FUNC([shm_open],
			     [sockets_shm_happy=1],
			     [sockets_shm_happy=0])

	       # look for shm_open in librt if not already present
	       AS_IF([test $sockets_shm_happy -eq 0],
		     [FI_CHECK_PACKAGE([sockets_shm],
				[sys/mman.h],
				[rt],
				[shm_open],
				[],
				[],
				[],
				[sockets_shm_happy=1],
				[sockets_shm_happy=0])])
	      ])

	AS_IF([test $sockets_h_happy -eq 1 && \
	       test $sockets_shm_happy -eq 1], [$1], [$2])
])
