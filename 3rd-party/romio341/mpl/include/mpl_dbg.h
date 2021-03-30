/*
 * Copyright (C) by Argonne National Laboratory
 *     See COPYRIGHT in top-level directory
 */

#ifndef MPL_DBG_H_INCLUDED
#define MPL_DBG_H_INCLUDED

#include "mplconfig.h"
#include <assert.h>

/*
 * Multilevel debugging and tracing macros.
 * The design is discussed at
 * http://wiki.mpich.org/mpich/index.php/Debug_Event_Logging
 *
 * Basically, this provide a way to place debugging messages into
 * groups (called *classes*), with levels of detail, and arbitrary
 * messages.  The messages can be turned on and off using environment
 * variables and/or command-line options.
 */

#ifdef MPL_USE_DBG_LOGGING

#define MPL_DBG_SELECTED(_class, _level)                                \
    ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level)

#define MPL_DBG_MSG(_class, _level, _string)                            \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            MPL_dbg_outevent(__FILE__, __LINE__, _class, 0, "%s", _string); \
        }                                                               \
    }

#define MPL_DBG_MSG_S(_class, _level, _fmat, _string)                   \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            MPL_dbg_outevent(__FILE__, __LINE__, _class, 1, _fmat, _string); \
        }                                                               \
    }

#define MPL_DBG_MSG_D(_class, _level, _fmat, _int)                      \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            MPL_dbg_outevent(__FILE__, __LINE__, _class, 2, _fmat, _int); \
        }                                                               \
    }

#define MPL_DBG_MSG_P(_class, _level, _fmat, _pointer)                  \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            MPL_dbg_outevent(__FILE__, __LINE__, _class, 3, _fmat, _pointer); \
        }                                                               \
    }

#define MPL_DBG_MAXLINE 256
#define MPL_DBG_FDEST _s,(size_t)MPL_DBG_MAXLINE
/*M
  MPL_DBG_MSG_FMT - General debugging output macro

  Notes:
  To use this macro, the third argument should be an "sprintf" - style
  argument, using MPL_DBG_FDEST as the buffer argument.  For example,
.vb
    MPL_DBG_MSG_FMT(CMM,VERBOSE,(MPL_DBG_FDEST,"fmat",args...));
.ve
  M*/

#define MPL_DBG_MSG_FMT(_class, _level, _fmatargs)                      \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            char _s[MPL_DBG_MAXLINE];                                   \
            int _ret = MPL_snprintf _fmatargs ;                          \
            /* by checking _ret, we supress -Wformat-trunction in gcc-8 */ \
            assert(_ret >= 0);                                          \
            MPL_dbg_outevent(__FILE__, __LINE__, _class, 0, "%s", _s);  \
        }                                                               \
    }

#define MPL_DBG_STMT(_class, _level, _stmt)                             \
    {                                                                   \
        if ((_class & MPL_dbg_active_classes) && MPL_DBG_##_level <= MPL_dbg_max_level) { \
            _stmt;                                                      \
        }                                                               \
    }

#define MPL_DBG_OUT(_class, _msg)                               \
    MPL_dbg_outevent(__FILE__, __LINE__, _class, 0, "%s", _msg)

#define MPL_DBG_OUT_FMT(_class,_fmatargs)                               \
    {                                                                   \
        char _s[MPL_DBG_MAXLINE];                                       \
        MPL_snprintf _fmatargs ;                                        \
        MPL_dbg_outevent(__FILE__, __LINE__, _class, 0, "%s", _s);      \
    }

#else
#define MPL_DBG_SELECTED(_class,_level) 0
#define MPL_DBG_MSG(_class,_level,_string)
#define MPL_DBG_MSG_S(_class,_level,_fmat,_string)
#define MPL_DBG_MSG_D(_class,_level,_fmat,_int)
#define MPL_DBG_MSG_P(_class,_level,_fmat,_int)
#define MPL_DBG_MSG_FMT(_class,_level,_fmatargs)
#define MPL_DBG_STMT(_class,_level,_stmt)
#define MPL_DBG_OUT(_class,_msg)
#define MPL_DBG_OUT_FMT(_class,_fmtargs)
#endif

typedef unsigned int MPL_dbg_class;

/* Special constants */
enum MPL_DBG_LEVEL {
    MPL_DBG_TERSE = 0,
    MPL_DBG_TYPICAL = 50,
    MPL_DBG_VERBOSE = 99
};

extern int MPL_dbg_active_classes;
extern int MPL_dbg_max_level;

extern MPL_dbg_class MPL_DBG_ROUTINE_ENTER;
extern MPL_dbg_class MPL_DBG_ROUTINE_EXIT;
extern MPL_dbg_class MPL_DBG_ROUTINE;
extern MPL_dbg_class MPL_DBG_ALL;

MPL_dbg_class MPL_dbg_class_alloc(const char *ucname, const char *lcname);
void MPL_dbg_class_register(MPL_dbg_class cls, const char *ucname, const char *lcname);

#define MPL_DBG_CLASS_CLR(cls)                  \
    do {                                        \
        (cls) = 0;                              \
    } while (0)

#define MPL_DBG_CLASS_APPEND(out_class, in_class)       \
    do {                                                \
        (out_class) |= (in_class);                      \
    } while (0)

/* *INDENT-OFF* */
int MPL_dbg_outevent(const char *, int, int, int, const char *, ...) ATTRIBUTE((format(printf, 5, 6)));
/* *INDENT-ON* */

int MPL_dbg_pre_init(int *, char ***);
int MPL_dbg_init(int, int);

#endif /* MPL_DBG_H_INCLUDED */
