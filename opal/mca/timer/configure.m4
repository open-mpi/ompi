m4_define(MCA_timer_CONFIGURE_MODE, STOP_AT_FIRST)

AC_DEFUN([MCA_timer_CONFIG],[
        timer_base_include=

        # first, compile all the components
        MCA_CONFIGURE_FRAMEWORK($1, $2)

        # someone should have set this...
        if test "$timer_base_include" == "" ; then
            timer_base_include="base/base_null.h"
        fi

        AC_CONFIG_LINKS([opal/mca/timer/base/base_impl.h:opal/mca/timer/$timer_base_include])
])
