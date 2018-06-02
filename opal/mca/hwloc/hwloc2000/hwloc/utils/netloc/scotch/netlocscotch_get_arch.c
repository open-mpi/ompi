#include <stdio.h> // for scotch
#include <stdint.h>
#include <scotch.h>
#include <netloc.h>
#include <netlocscotch.h>

void help(char *name, FILE *f)
{
    fprintf(f, "Usage: %s <archfile> [subarchfile]\n"
            "\t%s --help\n", name, name);
}

int main(int argc, char **argv)
{
    int ret;
    SCOTCH_Arch arch;
    SCOTCH_Arch subarch;

    char *arch_filename = NULL;
    char *subarch_filename = NULL;

    if (argc == 1 || argc > 3) {
        help(argv[0], stdout);
        return 1;
    }

    if (argc == 2) {
        if (!strcmp(*argv, "--help")) {
            help(argv[0], stdout);
            return 0;
        } else {
            arch_filename = argv[1];
        }
    } else if (argc == 3) {
        arch_filename = argv[1];
        subarch_filename = argv[2];
    }

    ret = netlocscotch_build_global_arch(&arch);
    if( NETLOC_SUCCESS != ret ) {
        return ret;
    }
    FILE *arch_file = fopen(arch_filename, "w");
    SCOTCH_archSave(&arch, arch_file);
    fclose(arch_file);

    if (subarch_filename) {
        ret = netlocscotch_build_current_arch(&arch, &subarch);
        if( NETLOC_SUCCESS != ret ) {
            return ret;
        }
        FILE *subarch_file = fopen(subarch_filename, "w");
        SCOTCH_archSave(&subarch, subarch_file);
        fclose(subarch_file);
        SCOTCH_archExit(&subarch);
    }

    SCOTCH_archExit(&arch);

    return 0;
}

