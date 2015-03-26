dnl Configury specific to the libfabric verbs provider

dnl Called to configure this provider
dnl
dnl Arguments:
dnl
dnl $1: action if configured successfully
dnl $2: action if not configured successfully
dnl
AC_DEFUN([FI_VERBS_CONFIGURE],[
	# Determine if we can support the verbs provider
	verbs_ibverbs_happy=0
	verbs_rdmacm_happy=0
	AS_IF([test x"$enable_verbs" != x"no"],
	      [FI_CHECK_PACKAGE([verbs_ibverbs],
				[infiniband/verbs.h],
				[ibverbs],
				[ibv_open_device],
				[],
				[],
				[],
				[verbs_ibverbs_happy=1],
				[verbs_ibverbs_happy=0])

	       FI_CHECK_PACKAGE([verbs_rdmacm],
				[rdma/rsocket.h],
				[rdmacm],
				[rsocket],
				[],
				[],
				[],
				[verbs_rdmacm_happy=1],
				[verbs_rdmacm_happy=0])
	      ])

	AS_IF([test $verbs_ibverbs_happy -eq 1 && \
	       test $verbs_rdmacm_happy -eq 1], [$1], [$2])
])
