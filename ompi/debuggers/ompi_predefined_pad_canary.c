/*
 * Copyright (c) 2014 Cisco Systems, Inc.  All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

/*
 * This is a simple canary compile-time test.  If we have no padding
 * left on predefined MPI object types, it'll fail to compile, thereby
 * alerting/annoying a human, who can go fix the real problem.
 */

#include "ompi/communicator/communicator.h"
#include "ompi/group/group.h"
#include "ompi/request/request.h"
#include "ompi/op/op.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/win/win.h"
#include "ompi/info/info.h"
#include "ompi/file/file.h"
#include "ompi/message/message.h"

#define S(TYPE) (sizeof(ompi_predefined_##TYPE##_t) - sizeof(ompi_##TYPE##_t))

/**************************************************************************
 * IF THIS FILE FAILS TO COMPILE, IT IS A SYMPTOM OF A LARGER PROBLEM!
 **************************************************************************
 *
 * Do not attempt to fix the compile failure in this file; go fix the
 * fact that there's no more padding left for predefined MPI objects.
 *
 **************************************************************************/

char comm_pad[S(communicator)];
char group_pad[S(group)];
char request_pad[S(request)];
char op_pad[S(op)];
char datatype_pad[S(datatype)];
char win_pad[S(win)];
char info_pad[S(info)];
char file_pad[S(file)];
char message_pad[S(message)];
