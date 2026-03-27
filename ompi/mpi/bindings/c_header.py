# Copyright (c) 2025      Joe Downs. All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

import argparse
import re
import json
import sys
import os
from pathlib import Path

# ============================= Constants / Globals ============================
DIR = Path(".")
OUTPUT = DIR / "mpi.h"
INPUT = DIR / "abi.h.in"
JSON_PATH = DIR / "abi.json"

ABI_INTERNAL = "_ABI_INTERNAL"

categories_dict = {}
MANGLE_NAMES = False

INTERNAL_DATATYPES = [
    "MPI_Comm",
    "MPI_Datatype",
    "MPI_Errhandler",
    "MPI_File",
    "MPI_Group",
    "MPI_Info",
    "MPI_Message",
    "MPI_Op",
    "MPI_Request",
    "MPI_Session",
    "MPI_Status",
    "MPI_Win",
    "MPI_Comm_copy_attr_function",
    "MPI_Comm_delete_attr_function",
    "MPI_Comm_errhandler_function",
    "MPI_File_errhandler_function",
    "MPI_Session_errhandler_function",
    "MPI_Type_copy_attr_function",
    "MPI_Type_delete_attr_function",
    "MPI_Win_delete_attr_function",
    "MPI_Win_copy_attr_function",
    "MPI_Win_errhandler_function",
    "MPI_T_enum",
    "MPI_T_cvar_handle",
    "MPI_T_pvar_handle",
    "MPI_T_pvar_session",
    "MPI_T_event_instance",
    "MPI_T_event_registration",
    "MPI_T_cb_safety",
    "MPI_T_source_order",
    # TODO: these two are deprecated, get rid of them
    "MPI_Copy_function",
    "MPI_Delete_function",
]

DEPRECATED_FUNCTIONS = [
    "MPI_Address",
    "MPI_Type_hindexed",
    "MPI_Type_hvector",
    "MPI_Type_struct",
    "MPI_Type_extent",
    "MPI_Type_lb",
    "MPI_Type_lb",
    "MPI_LB",
    "MPI_UB",
    "MPI_Errhandler_create",
    "MPI_Errhandler_get",
    "MPI_Errhandler_set",
    "MPI_Keyval_create",
    "MPI_Keyval_free",
    "MPI_DUP_FN"
    "MPI_NULL_COPY_FN"
    "MPI_NULL_DELETE_FN"
    "MPI_Attr_delete",
    "MPI_Attr_get",
    "MPI_Attr_put",
    "MPI_COMBINER_HVECTOR_INTEGER",
    "MPI_COMBINER_HINDEXED_INTEGER",
    "MPI_COMBINER_STRUCT_INTEGER",
]

ENUM_CATEGORIES = [
    "ERROR_CLASSES",
    "MODE_CONSTANTS",
    "ASSORTED_CONSTANTS",
    "THREADS_CONSTANTS",
    "FILE_OPERATIONS_CONSTANTS",
    "DATATYPE_DECODING_CONSTANTS",
    "F90_DATATYPE_MATCHING_CONSTANTS",
    "COMMUNICATOR_GROUP_COMP_RESULTS",
    "TOPOLOGIES",
    "COMMUNICATOR_SPLIT_TYPE",
    "WINDOW_LOCK_TYPE_CONSTANTS",
    "WINDOW_CREATE_FLAVORS",
    "WINDOW_MODELS",
    "FILE_POS_CONSTANTS",
    "FILE_OP_CONSTANTS",
    "ENV_INQ_AND_ATTR_KEYS",
    "FORTRAN_STATUS_ARRAY_SIZE_AND_INDEX_C",
    "C_PREPROCESSOR_CONSTANTS_FORTRAN_PARAMETERS",
    "TOOL_INFO_IFACE_VERBOSITY_LEVELS",
    "TOOL_INFO_IFACE_VAR_ASSOCIATIONS",
    "TOOL_INFO_IFACE_VAR_SCOPES",
    "TOOL_INFO_IFACE_PVAR_CLASSES",
    "TOOL_INFO_IFACE_SOURCE_ORDERINGS",
    "TOOL_INFO_IFACE_CB_SAFETY_REQ_LEVELS",
]


# ============================== Argument Parsing ==============================
parser = argparse.ArgumentParser()
parser.add_argument("--abi-json", type=str, help=f"path to ABI JSON file [{DIR}/]")
parser.add_argument("-i", "--input", type=str, help="input path for the .h.in file")
parser.add_argument("-o", "--output", type=str, help="output path for the header file")
parser.add_argument("--mangle-names", help="enable name mangling for constants and datatypes", action="store_true")
parser.add_argument("--no-mangle", help="disable name mangling (default)", action="store_true")
parser.add_argument("--pympistd-dir", type=str, help="directory for the pympistandard library")

args = parser.parse_args()

if args.abi_json:
    JSON_PATH = Path(args.abi_json)
if args.mangle_names:
    MANGLE_NAMES = True
if args.no_mangle:
    MANGLE_NAMES = False
if args.output:
    OUTPUT = Path(args.output)
if args.input:
    INPUT = Path(args.input)
if args.pympistd_dir:
    PYMPISTANDARD_DIR = Path(args.pympistd_dir)

# A bit of a hack to load the pympistandard module, which is in
# the Open MPI '3rd-party" tree in the source dir.
pympi_dir = PYMPISTANDARD_DIR / "src"
sys.path.insert(0, str(pympi_dir.resolve()))

import pympistandard as std

# ================================== Load JSON =================================
with open(JSON_PATH) as f:
    abi = json.load(f)

CONSTS = abi["constants"]
CATEGORIES = abi["categories"]

# ==============================================================================

# Populating the `categories` dictionary
for category in CATEGORIES.values():
    name = category["name"]
    categories_dict[name] = []
    for value in CONSTS.values():
        if value["category"] == name:
            categories_dict[name].append(value)

# ================================== Functions =================================
# TODO: we need to add/fix/figure out the pympistandard's way for properly
# defining callback functions. pympistandard just prints out the regular
# prototype instead of `(function_name)(arguments)`
def cb_declaration(proc_expression):
    func_str = str(proc_expression)
    func_str_list = func_str.split()
    func_name, arg_1 = func_str_list[2].split("(")
    if MANGLE_NAMES:
        func_name = f"{func_name}{ABI_INTERNAL}"
    decl_string = f"{' '.join(func_str_list[:2])} ({func_name})({arg_1} {' '.join(func_str_list[3:])};\n"
    return decl_string

def output_constant(const, use_enum: bool, mangle_name: bool):
    spacing = 45
    name = const["name"]
    abi_value = const["abi_value"]
    c_type = const["handle_types"]["c"]["type"]
    if c_type is None:
        return None
    if mangle_name:
        name = f"{name}{ABI_INTERNAL}"
        spacing = 55
        #c_type = f"{c_type}{ABI_INTERNAL}"
    def_name = f"#define {name}"
    if use_enum:
        def_name = f"    {name}"
        value = f"= {abi_value},"
    elif c_type == "int":
        value = f"{abi_value}"
    else:
        value = f"(({c_type}) {abi_value})"
    return def_name + " " * (spacing - len(def_name) + 1) + value + "\n"

# ========================= Manipulate Template Header =========================
lines = []
with open(INPUT, 'r') as header_in:
    lines = header_in.readlines()

# Match lines that start with `$CATEGORY:`. Any amount of whitespace is allowed
# in the case of indenting. However, only preceding whitespace is allowed so
# that commented-out lines are NOT included.
category_pattern = re.compile(r"^[\s]*\$CATEGORY:([A-Z_0-9]+)\$$")
output = []
c_type = str()

for line in lines:
    category = category_pattern.search(line)
    # If there's no match, category is None. Then we just want to preserve the
    # line as-is.
    if category:
        category = category.group(1)
        use_enum = False
        # Only some values should be in `enums`, otherwise just use `#define`s
        if category in ENUM_CATEGORIES:
            use_enum = True
        if use_enum:
            for constant in categories_dict[category]:
                c_type = constant["handle_types"]["c"]["type"]
                if c_type == "int":
                    output.append("enum {\n")
                else:
                    if MANGLE_NAMES:
                        line = f'typedef enum {c_type}{ABI_INTERNAL}'
                    else:
                        line = f'typedef enum {c_type}'
                    line = line + " {\n"
                    output.append(line)
                break
        # Print out each `#define` / assignment for the constants
        for constant in categories_dict[category]:
            line = output_constant(constant, use_enum, MANGLE_NAMES)
            if line is not None:
                output.append(line)
        if use_enum:
            if c_type == "int":
                output.append("};\n")
            else:
                if MANGLE_NAMES:
                    line = f'{c_type}{ABI_INTERNAL};'
                else:
                    line = f'{c_type};'
                line = "} " + line + "\n"
                output.append(line)
    else:
        output.append(line)

# ============================= Function Prototypes ============================
std.use_api_version()

output.append("\n")
output.append("/* Callback functions */\n")
for proc in std.CALLBACKS.values():
    output.append(cb_declaration(proc.express.iso_c))
    if proc.has_embiggenment():
        output.append(cb_declaration(proc.express.embiggen.iso_c))

output.append("\n")
output.append("/* MPI API */\n")
for proc in std.all_iso_c_procedures():
    output.append(f"{proc.express.iso_c};\n")
    if proc.has_embiggenment():
        output.append(f"{proc.express.embiggen.iso_c};\n")

output.append("\n")
output.append("/* Profiling MPI API */\n")
for proc in std.all_iso_c_procedures():
     output.append(f"{proc.express.profile.iso_c};\n")
     # TODO: this is a hack and we need to add/fix/figure out the pympistandard
     # module to natively print out an embiggened version
     if proc.has_embiggenment():
         binding = proc.express.embiggen.iso_c.__str__().split()
         output.append(f"{binding[0]} P{' '.join(binding[1:])};\n")

# ================================ Odds and ends for mangle case ===============
if MANGLE_NAMES:
    output.append("\n")
    output.append("/*\n")
    output.append(" * define externs to help with attributes\n")
    output.append(" */\n")
    output.append("extern int ompi_abi_mpi_proc_null_val;\n")
    output.append("extern int ompi_abi_mpi_any_source_val;\n")
    output.append("extern int ompi_abi_mpi_win_flavor_create;\n")
    output.append("extern int ompi_abi_mpi_win_flavor_allocate;\n")
    output.append("extern int ompi_abi_mpi_win_flavor_shared;\n")
    output.append("extern int ompi_abi_mpi_win_flavor_dynamic;\n")
    output.append("extern int ompi_abi_mpi_win_model_unified;\n")
    output.append("extern int ompi_abi_mpi_win_model_separate;\n")
    output.append("extern int ompi_abi_mpi_lastusedcode;\n")
    output.append("\n")
    output.append("int ABI_C_MPI_COMM_NULL_DELETE_FN( MPI_Comm_ABI_INTERNAL comm, int comm_keyval, void* attribute_val_out, void* extra_state );\n")
    output.append("int ABI_C_MPI_COMM_NULL_COPY_FN( MPI_Comm_ABI_INTERNAL  comm, int comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("int ABI_C_MPI_COMM_DUP_FN( MPI_Comm_ABI_INTERNAL comm, int comm_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("int ABI_C_MPI_TYPE_NULL_DELETE_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval, void* attribute_val_out, void* extra_state );\n")
    output.append("int ABI_C_MPI_TYPE_NULL_COPY_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("int ABI_C_MPI_TYPE_DUP_FN( MPI_Datatype_ABI_INTERNAL datatype, int type_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("int ABI_C_MPI_WIN_NULL_DELETE_FN( MPI_Win_ABI_INTERNAL window, int win_keyval, void* attribute_val_out, void* extra_state );\n")
    output.append("int ABI_C_MPI_WIN_NULL_COPY_FN( MPI_Win_ABI_INTERNAL window, int win_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("int ABI_C_MPI_WIN_DUP_FN( MPI_Win_ABI_INTERNAL window, int win_keyval, void* extra_state, void* attribute_val_in, void* attribute_val_out, int* flag );\n")
    output.append("\n")

# ================================ Final Output ================================
output.append("#if defined(__cplusplus)\n")
output.append("}\n")
output.append("#endif\n")
output.append("#endif /* MPI_H_ABI */")
# === some compilers are finicky about not having empy line at end of include file =====
output.append("\n")

#replacements = {'MPI_COUNT':'MPI_Count_ABI_INTERNAL',
#                'MPI_AINT':'MPI_Aint_ABI_INTERNAL',
#                'MPI_OFFSET':'MPI_Offset_ABI_INTERNAL'}
replacements = {'MPI_COUNT':'MPI_Count',
                'MPI_AINT':'MPI_Aint',
                'MPI_OFFSET':'MPI_Offset'}

for i, line in enumerate(output):
    line = line.replace(r"\ldots", "...")
    for key, value in replacements.items():
        if MANGLE_NAMES:
            value = value+'_ABI_INTERNAL'
        line = line.replace(f'@{key}@', value)
    if MANGLE_NAMES:

        # Replace datatypes with their internal ABI counterparts
        for datatype in INTERNAL_DATATYPES:
            # Need to include the extra space here or else we'll edit functions
            # like "MPI_Group_difference"
            datatype_pattern = r"([\( ]?)(" + datatype + r")([; \*\)]{1})"
            line = re.sub(datatype_pattern, f"\\g<1>\\g<2>{ABI_INTERNAL}\\g<3>", line)
    # TODO: need to enhance pympistandard to be able to prune out deprecated functions
    # This stands in as a workaround
    comment_out = False
    comment_out = any(i in line for i in DEPRECATED_FUNCTIONS)

    # function is not in the ABI standard (things in MPI 5.1 chapter 19 sections 19.3.4 and 19.3.5
    # TODO: need to enhance pympistandard to have field in json to indicate a 
    # function is not in the ABI standard (things in MPI 5.1 chapter 19 sections 19.3.4 and 19.3.5
    if "MPI_Fint" in line or "MPI_F08_status" in line:
        comment_out = True

    if comment_out == True:
        # Comment out a line if it has references to MPI_Fint or MPI_F08_status, since
        # functions with these argument types are not in the ABI
        line = line[:-1]
        line = f"/* {line} */" + "\n"
    # TODO: pympistandard creates `MPI_Info_create_env` with its `argv`
    # parameter being of type `char *` instead of `char **` --- as defined in
    # the standard.
    if "MPI_Info_create_env" in line:
        line = line.replace("char argv[]", "char *argv[]")
    output[i] = line

with open(OUTPUT, 'tw') as header_out:
    header_out.writelines(output)
