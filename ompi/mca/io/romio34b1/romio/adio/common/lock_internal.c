#include "lock_internal.h"

const char *ADIOI_GEN_flock_cmd_to_string(int cmd)
{
    switch (cmd) {
#ifdef F_GETLK64
        case F_GETLK64:
            return "F_GETLK64";
#else
        case F_GETLK:
            return "F_GETLK";
#endif
#ifdef F_SETLK64
        case F_SETLK64:
            return "F_SETLK64";
#else
        case F_SETLK:
            return "F_SETLK";
#endif
#ifdef F_SETLKW64
        case F_SETLKW64:
            return "F_SETLKW64";
#else
        case F_SETLKW:
            return "F_SETLKW";
#endif
        default:
            return "UNEXPECTED";
    }
}

const char *ADIOI_GEN_flock_type_to_string(int type)
{
    switch (type) {
        case F_RDLCK:
            return "F_RDLCK";
        case F_WRLCK:
            return "F_WRLCK";
        case F_UNLCK:
            return "F_UNLOCK";
        default:
            return "UNEXPECTED";
    }
}
