import consts

lines = []

for value in consts.consts.values():
    name = value["name"]
    abi_value = value["abi_value"]
    c_type = value["handle_types"]["c"]["type"]
    if c_type is None:
        continue
    def_name = f"#define {name}"
    if c_type == "int":
        value = f"{abi_value}\n"
    else:
        value = f"(({c_type}) {abi_value})\n"
    line = def_name + " " * (45 - len(def_name)) + value
    lines.append(line)

with open(consts.DIR / "mpi_abi.h", 'wt') as header:
    header.writelines(lines)
