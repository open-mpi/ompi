#
# Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

C_ABI_CASES = \
        cases/c-abi/abi_version.cbody.in \
        cases/c-abi/barrier_two_rank.cbody.in \
        cases/c-abi/converter_dynamic_handles.cbody.in \
        cases/c-abi/converter_error_keyvals.cbody.in \
        cases/c-abi/converter_fortran_datatypes.cbody.in \
        cases/c-abi/converter_predefined_handles.cbody.in \
        cases/c-abi/converter_status_sentinels.cbody.in \
        cases/c-abi/init_finalize.cbody.in \
        cases/c-abi/sendrecv_two_rank.cbody.in

C_RUNTIME_CASES = \
        cases/c-runtime/runtime_collective_basic.cbody.in \
        cases/c-runtime/runtime_collective_blocking.cbody.in \
        cases/c-runtime/runtime_collective_nonblocking.cbody.in \
        cases/c-runtime/runtime_collective_persistent.cbody.in \
        cases/c-runtime/runtime_comm_attr_predefined.cbody.in \
        cases/c-runtime/runtime_comm_basic.cbody.in \
        cases/c-runtime/runtime_comm_buffer.cbody.in \
        cases/c-runtime/runtime_comm_create_split.cbody.in \
        cases/c-runtime/runtime_comm_errhandler_basic.cbody.in \
        cases/c-runtime/runtime_datatype_constructors.cbody.in \
        cases/c-runtime/runtime_datatype_contents.cbody.in \
        cases/c-runtime/runtime_dynamic_connect.cbody.in \
        cases/c-runtime/runtime_dynamic_name_service.cbody.in \
        cases/c-runtime/runtime_dynamic_spawn.cbody.in \
        cases/c-runtime/runtime_error_info_misc.cbody.in \
        cases/c-runtime/runtime_file_io.cbody.in \
        cases/c-runtime/runtime_file_lifecycle.cbody.in \
        cases/c-runtime/runtime_group_setops.cbody.in \
        cases/c-runtime/runtime_init_state.cbody.in \
        cases/c-runtime/runtime_init_thread.cbody.in \
        cases/c-runtime/runtime_intercomm_group.cbody.in \
        cases/c-runtime/runtime_op_predefined.cbody.in \
        cases/c-runtime/runtime_pack_status.cbody.in \
        cases/c-runtime/runtime_point_to_point_basic.cbody.in \
        cases/c-runtime/runtime_point_to_point_isendrecv.cbody.in \
        cases/c-runtime/runtime_point_to_point_matched_message.cbody.in \
        cases/c-runtime/runtime_point_to_point_partitioned.cbody.in \
        cases/c-runtime/runtime_point_to_point_persistent.cbody.in \
        cases/c-runtime/runtime_point_to_point_probe.cbody.in \
        cases/c-runtime/runtime_point_to_point_send_modes.cbody.in \
        cases/c-runtime/runtime_request_lifecycle.cbody.in \
        cases/c-runtime/runtime_rma_operations.cbody.in \
        cases/c-runtime/runtime_session_buffer.cbody.in \
        cases/c-runtime/runtime_session_core.cbody.in \
        cases/c-runtime/runtime_session_group.cbody.in \
        cases/c-runtime/runtime_topology_cart_graph.cbody.in \
        cases/c-runtime/runtime_topology_dist_neighbor.cbody.in \
        cases/c-runtime/runtime_win_lifecycle.cbody.in

C_CALLBACK_CASES = \
        cases/c-callback/callback_comm_attr.cbody.in \
        cases/c-callback/callback_comm_attr.prologue.in \
        cases/c-callback/callback_type_attr.cbody.in \
        cases/c-callback/callback_type_attr.prologue.in \
        cases/c-callback/callback_win_attr.cbody.in \
        cases/c-callback/callback_win_attr.prologue.in

EXTRA_DIST = \
        README.md \
        requirements.txt \
        mpi_abi_tests.py \
        manual/README.md \
        templates/c_probe.c.in \
        templates/fortran_probe.f90.in \
        templates/README.md \
        $(C_ABI_CASES) \
        $(C_RUNTIME_CASES) \
        $(C_CALLBACK_CASES)

abi_test_runner = $(PYTHON) "$(srcdir)/mpi_abi_tests.py" \
        --srcdir "$(top_srcdir)" \
        --builddir "$(abs_top_builddir)" \
        --outdir "$(abs_builddir)/.mpi-abi"

check-local:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-fast; \
	fi

check-abi:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-abi; \
	fi

check-abi-cross:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) check-abi-cross; \
	fi

manifest:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) manifest; \
	fi

coverage:
	@if test "$(PYTHON)" = ":"; then \
	    echo "SKIP: Python is not available"; \
	else \
	    $(abi_test_runner) coverage; \
	fi

clean-local:
	rm -rf .mpi-abi

distclean-local:
	rm -rf .mpi-abi
