# VampirTrace region filter specification
#
# call limit definitions and region assignments
#
# syntax: <regions> -- <limit>
#
#   regions    semicolon-seperated list of regions
#              (can be wildcards)
#   limit      assigned call limit
#               0 = region(s) denied
#              -1 = unlimited
#
# example:
#
# add;sub;mul;div -- 1000
# * -- 3000000
#
# These region filter directives cause that the functions "add", "sub",
# "mul" and "div" be recorded at most 1000 times. The remaining
# functions (*) will be recorded at most 3000000 times. 
