dnl Configry specific to the libfabrics usNIC provider

dnl Called to configure this provider
dnl
dnl Arguments:
dnl
dnl $1: action if configured successfully
dnl $2: action if not configured successfully
dnl
AC_DEFUN([FI_USNIC_CONFIGURE],[
    # Determine if we can support the usnic provider
    usnic_happy=0
    AS_IF([test "x$enable_usnic" != "xno"],
	[usnic_happy=1
	 AC_CHECK_HEADER([infiniband/verbs.h], [], [usnic_happy=0])
	 AC_CHECK_HEADER([linux/netlink.h], [], [usnic_happy=0], [
#include <sys/types.h>
#include <net/if.h>
])
	 AC_CHECK_LIB([nl], [nl_connect], [], [usnic_happy=0])
	])
])
