/*
 * $HEADER$
 */

#include "util/sys_info.h"

ompi_sys_info_t ompi_system_info;

int ompi_sys_info(void)
{

    return (uname(&ompi_system_info));

}
