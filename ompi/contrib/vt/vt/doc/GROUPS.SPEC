# VampirTrace region groups specification
#
# group definitions and region assignments
#
# syntax: <group>=<regions>
#
#   group      group name 
#   regions    semicolon-seperated list of regions
#              (can be wildcards)
#
# example:
#
# CALC=add;sub;mul;div
# USER=app_*
#
# These group assignments make the functions "add", "sub", "mul" and "div"
# associated with group "CALC" and all functions with the prefix "app_" are
# associated with group "USER".
