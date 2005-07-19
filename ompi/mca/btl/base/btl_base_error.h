/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef MCA_BTL_BASE_ERROR_H
#define MCA_BTL_BASE_ERROR_H

#define BTL_ERROR(fmt, args...) {                                           \ 
    opal_output(0, "[%s:%d:%d " fmt, __FILE__, __LINE__, __func__, ##args); \
} 

#endif
