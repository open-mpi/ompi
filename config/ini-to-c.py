#!/usr/bin/env python3
#
# Copyright (c) 2025      Jeffrey M. Squyres.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# Use minimal modules (e.g., don't use a module for INI) so that we
# can avoid needing to PIP-install anything.

import re
import argparse
import os

def ini_section_to_c_name(section: str, prefix: str) -> str:
    # Convert an INI section name to a valid C variable name with a
    # prefix
    c_name = re.sub(r'[^a-zA-Z0-9_]', '_', section)

    # Enforce C11 identifier length limit (63 characters for internal
    # identifiers).
    return f"{prefix}{c_name}"[:63]

def ini_content_to_c_string(content: str) -> str:
    # Convert an INI section content into a valid C string
    return content.replace("\n", "\\n").replace("\"", "\\\"")

def parse_ini_file(file_path: str):
    # Parse the INI-style text file, excluding comment lines.
    sections = {}
    current_section = None

    with open(file_path, 'r', encoding='utf-8') as file:
        for line in file:
            line = line.strip()
            if line.startswith('#') or not line:
                continue
            if line.startswith('[') and line.endswith(']'):
                current_section = line[1:-1]
                sections[current_section] = ""
            elif current_section is not None:
                sections[current_section] += line + "\n"
    return sections

def generate_c_files(sections, input_filename, base_filename: str,
                     prefix: str):
    # Generate a C file and a header file with the parsed INI data
    c_filename = f"{base_filename}.c"
    h_filename = f"{base_filename}.h"

    macro_name = re.sub(r'[^a-zA-Z0-9_]', '_', base_filename.upper())

    with open(c_filename, 'w', encoding='utf-8') as c_file, \
         open(h_filename, 'w', encoding='utf-8') as h_file:

        header = f"""// THIS IS AN AUTOMATICALLY-GENERATED FILE!
// It was generated from {input_filename}
// Edits will be lost"""

        h_file.write(f"""{header}

#ifndef {macro_name}_H
#define {macro_name}_H

""")
        c_file.write(f"""{header}

#include "{base_filename}.h"
""")

        for section, content in sections.items():
            c_var_name = ini_section_to_c_name(section, prefix)
            c_string = ini_content_to_c_string(content)

            h_file.write(f'extern const char {c_var_name}[];\n')
            c_file.write(f'\nconst char {c_var_name}[] = "{c_string}";\n')

        h_file.write(f"\n#endif // {macro_name}_H\n")

def main():
    parser = argparse.ArgumentParser(description="Convert an INI-style text file to C variable definitions.")
    parser.add_argument("--in",
                        dest='input_file',
                        required=True,
                        help="Path to the input INI-style file.")
    parser.add_argument("--out",
                        required=True,
                        help="Basename for output .c and .h files.")
    parser.add_argument("--prefix",
                        required=True,
                        help="Prefix for all C macro names and variable names.")
    parser.add_argument("--verbose",
                        action='store_true',
                        help="Show output or not")

    args = parser.parse_args()

    sections = parse_ini_file(args.input_file)
    generate_c_files(sections, args.input_file, args.out, args.prefix)
    if args.verbose:
        print(f"Generated {args.out}.c and {args.out}.h.")

if __name__ == "__main__":
    main()
