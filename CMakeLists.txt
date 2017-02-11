cmake_minimum_required (VERSION 2.8 FATAL_ERROR)

project(aces4)

enable_language(Fortran)


#----------------------------------------------------------------------
# Options, Settings, Libraries, Packages
#----------------------------------------------------------------------

# All custom find modules
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

# Find required packages
find_package(LAPACK REQUIRED)
find_package(OpenMP)

##################
# HAVE_MPI Option
##################
option(HAVE_MPI "Whether to use MPI" ON)

# Check if MPI available on system
if (HAVE_MPI)
    find_package(MPI REQUIRED)
endif()


####################
# HAVE_CUDA Options - Whether to use CUDA accelerated super instructions
# Disabled for the time being
####################

#option(HAVE_CUDA "Whether to use CUDA accelerated super instructions" OFF)

## Check if CUDA available on system
#if (HAVE_CUDA)
#    find_package(CUDA REQUIRED)
#endif()

####################
# SIP_DEVEL Options - Whether to enable extra logging for the entire SIP
####################
option(SIP_DEVEL "Whether to enable extra logging for the entire SIP" OFF)

if (SIP_DEVEL)
    message(STATUS "   SIP_DEVEL Flag enabled    ")
    message(STATUS "-----------------------------")
    message(STATUS "There will be lots of print !")
    message(STATUS "-----------------------------")
endif()



#Config file
configure_file(${CMAKE_SOURCE_DIR}/config.h.cmake ${CMAKE_BINARY_DIR}/config.h)

#----------------------------------------------------------------------
# SIA sources, libraries, includes
#----------------------------------------------------------------------
set(SIA_SETUP_FILES
    sia/src/runtime/setup/io_utils.h;
    sia/src/runtime/setup/setup_reader.h;
    sia/src/runtime/setup/setup_interface.h;
    sia/src/runtime/setup/setup_writer.h;
    sia/src/runtime/setup/setup_reader.cpp;
    sia/src/runtime/setup/setup_interface.cpp;
    sia/src/runtime/setup/setup_writer.cpp;
    sia/src/runtime/setup/io_utils.cpp)

set(SIA_CORE_FILES
    sia/src/runtime/core/array_constants.h;
    sia/src/runtime/core/array_constants.cpp;
    sia/src/runtime/core/sip.h;
    sia/src/runtime/core/sip.cpp;
    sia/src/runtime/core/sia_defs.h;
    sia/src/runtime/core/sia_log.h;
    sia/src/runtime/core/sia_log.cpp;
    sia/src/runtime/core/job_control.h;
    sia/src/runtime/core/job_control.cpp)

# MPI - Conditional compile for MPI files
if (HAVE_MPI AND MPI_CXX_FOUND)
    set(SIA_MPI_FILES
        sia/src/runtime/server/data_distribution.cpp ;
        sia/src/runtime/server/data_distribution.h;
        sia/src/runtime/server/sip_mpi_utils.h;
        sia/src/runtime/server/sip_mpi_utils.cpp;
        sia/src/runtime/server/sip_mpi_constants.h;
        sia/src/runtime/server/sip_mpi_constants.cpp;
        sia/src/runtime/server/barrier_support.h;
        sia/src/runtime/server/async_acks.h;
        sia/src/runtime/server/async_acks.cpp;
        sia/src/runtime/server/sip_server.h;
        sia/src/runtime/server/sip_server.cpp;
        sia/src/runtime/worker/sial_ops_parallel.h;
        sia/src/runtime/worker/sial_ops_parallel.cpp;
        sia/src/runtime/server/server_block.h;
        sia/src/runtime/server/server_block.cpp;
        sia/src/runtime/server/disk_backed_block_map.h;
        sia/src/runtime/server/disk_backed_block_map.cpp;
        sia/src/runtime/server/server_persistent_array_manager.h;
        sia/src/runtime/server/server_persistent_array_manager.cpp;
        sia/src/runtime/server/async_ops.h;
        sia/src/runtime/server/async_ops.cpp;
        sia/src/runtime/server/counter.h;
        sia/src/runtime/server/counter.cpp;
        sia/src/runtime/server/array_file.h;
        sia/src/runtime/server/array_file.cpp;
        sia/src/runtime/server/chunk_manager.h;
        sia/src/runtime/server/chunk_manager.cpp;
        sia/src/runtime/server/chunk.h;
        sia/src/runtime/server/chunk.cpp;
        sia/src/runtime/dynamic_data/mpi_state.h;
        sia/src/runtime/dynamic_data/distributed_block_consistency.h;
        sia/src/runtime/dynamic_data/distributed_block_consistency.cpp
 )
else ()
    set(SIA_MPI_FILES "")
endif()

# CUDA - Conditional compile for CUDA super instructions
if (HAVE_CUDA AND CUDA_FOUND)
    set(SIA_CUDA_FILES
        sia/src/runtime/super_instructions/cuda/gpu_super_instructions.cu
        sia/src/runtime/super_instructions/cuda/cuda_check.h)
else ()
    set(SIA_CUDA_FILES "")
endif()

if (HAVE_JSON)
    set(SIA_JSON_DIR
       ${CMAKE_SOURCE_DIR}/sia/src/runtime/jsoncpp)
    set(JSON_LIBRARIES
       jasoncpp)
else ()
    set(SIA_JSON_FILES "")
endif()



set(SIA_DATA_FILES
    sia/src/runtime/static_data/array_table.h;
    sia/src/runtime/static_data/array_table.cpp;
    sia/src/runtime/static_data/index_table.h;
    sia/src/runtime/static_data/index_table.cpp;
    sia/src/runtime/static_data/int_table.h;
    sia/src/runtime/static_data/int_table.cpp;
    sia/src/runtime/static_data/sip_tables.h;
    sia/src/runtime/static_data/sip_tables.cpp;
    sia/src/runtime/static_data/opcode.h;
    sia/src/runtime/static_data/opcode.cpp;
    sia/src/runtime/static_data/op_table.h;
    sia/src/runtime/static_data/op_table.cpp;
    sia/src/runtime/static_data/create_map.h;
    sia/src/runtime/dynamic_data/block_id.h;
    sia/src/runtime/dynamic_data/block_id.cpp;
    sia/src/runtime/dynamic_data/block_shape.h;
    sia/src/runtime/dynamic_data/block_shape.cpp;
    sia/src/runtime/dynamic_data/block_selector.h;
    sia/src/runtime/dynamic_data/block_selector.cpp;
    sia/src/runtime/dynamic_data/block.h;
    sia/src/runtime/dynamic_data/block.cpp;
    sia/src/runtime/dynamic_data/block_manager.h;
    sia/src/runtime/dynamic_data/block_manager.cpp;
    sia/src/runtime/dynamic_data/contiguous_array_manager.h;
    sia/src/runtime/dynamic_data/contiguous_array_manager.cpp;
    sia/src/runtime/dynamic_data/data_manager.h;
    sia/src/runtime/dynamic_data/data_manager.cpp;
    sia/src/runtime/dynamic_data/id_block_map.h;
    sia/src/runtime/dynamic_data/worker_persistent_array_manager.h;
    sia/src/runtime/dynamic_data/worker_persistent_array_manager.cpp;
    sia/src/runtime/dynamic_data/lru_array_policy.h;
    sia/src/runtime/dynamic_data/cached_block_map.h;
    sia/src/runtime/dynamic_data/cached_block_map.cpp;
    sia/src/runtime/dynamic_data/contiguous_local_array_manager.h;
    sia/src/runtime/dynamic_data/contiguous_local_array_manager.cpp;
    sia/src/runtime/dynamic_data/memory_tracker.h;
    sia/src/runtime/dynamic_data/memory_tracker.cpp;)
#need only header files for templates--these include .cpp

set(SIA_WORKER_FILES
    sia/src/runtime/worker/loop_manager.cpp;
    sia/src/runtime/worker/loop_manager.h;
    sia/src/runtime/worker/fragment_loop_manager.cpp;
    sia/src/runtime/worker/fragment_loop_manager.h;
    sia/src/runtime/worker/pardo_loop_factory.cpp;
    sia/src/runtime/worker/pardo_loop_factory.h;
    sia/src/runtime/worker/interpreter.cpp;
    sia/src/runtime/worker/interpreter.h;
    sia/src/runtime/worker/siox_reader.h;
    sia/src/runtime/worker/siox_reader.cpp;
    sia/src/runtime/worker/sial_ops_sequential.h;
    sia/src/runtime/worker/sial_ops_sequential.cpp;
    sia/src/runtime/worker/sial_printer.h;
    sia/src/runtime/worker/sial_printer.cpp;
    sia/src/runtime/worker/sial_math.h;
    sia/src/runtime/worker/sial_math.cpp;
    sia/src/runtime/worker/tracer.h;
    sia/src/runtime/worker/tracer.cpp;
    sia/src/runtime_interface/sip_interface.h;
    sia/src/runtime_interface/sip_interface.cpp;
    sia/src/runtime/tensor_algebra/tensor_ops_c_prototypes.h;
    sia/src/runtime/super_instructions/cuda/gpu_super_instructions.h;
    sia/src/runtime/server/sip_mpi_attr.h;
    sia/src/runtime/server/sip_mpi_attr.cpp;
    sia/src/runtime/server/rank_distribution.h ;
    sia/src/runtime/server/rank_distribution.cpp ;)

# Dmitry's Tensor library
set(tensordil_SRCS sia/src/runtime/tensor_algebra/tensor_dil_omp.F90)

set(INCLUDE_DIRS
    ${CMAKE_BINARY_DIR};
    ${MPI_INCLUDE_PATH};
    ${CMAKE_SOURCE_DIR}/sia/src/runtime_interface;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/tensor_algebra;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/core;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/setup;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/worker;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/static_data;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/dynamic_data;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/server;
    ${CMAKE_SOURCE_DIR}/sia/src/runtime/super_instructions/cuda;
    ${SIA_JSON_DIR};
    ${CMAKE_SOURCE_DIR}/sia/test;
    ${CMAKE_SOURCE_DIR}/aces/src/super_instructions)

set (TOLINK_LIBRARIES
    ${TOLINK_LIBRARIES}
    tensordil;
    ${JSON_LIBRARIES};
    ${MPI_LIBRARIES};
    acessuperinstructions # Points to : ${CMAKE_BINARY_DIR}/aces/src/runtime/super_instructions/libacessuperinstructions.a;
    siasuperinstructions # Points to : ${CMAKE_BINARY_DIR}/sia/src/super_instructions/libsiasuperinstructions.a;
    ${LAPACK_LIBRARIES})


# CUDA Super instructions
if (CUDA_FOUND)
    set(CUDA_HOST_COMPILATION_CPP ON)
    CUDA_INCLUDE_DIRECTORIES(${INCLUDE_DIRS})

    # Support Double precision
    set(CUDA_NVCC_FLAGS ${CUDA_NVCC_FLAGS} -arch=sm_13)
    CUDA_ADD_LIBRARY(cudasuperinstructions ${SIA_CUDA_FILES})
    CUDA_ADD_CUBLAS_TO_TARGET(cudasuperinstructions)
    set(TOLINK_LIBRARIES ${TOLINK_LIBRARIES} cudasuperinstructions)
endif()

# Accumulating Compile & Link Flags

set(SIA_COMPILE_FLAGS "")
set(LIBTENSORDIL_DEFINITIONS "")
set(SIA_LINK_FLAGS "")

# Add LAPACK Linker Flags
set (SIA_LINK_FLAGS ${SIA_LINK_FLAGS} ${LAPACK_LINKER_FLAGS})

# Add OpenMP Flags
if (OPENMP_FOUND)
    set(LIBTENSORDIL_DEFINITIONS _OPENMP)
    set(SIA_LINK_FLAGS ${SIA_LINK_FLAGS} ${OpenMP_CXX_FLAGS})
endif()

# Add MPI Flags
if (MPI_CXX_FOUND)
    set(SIA_COMPILE_FLAGS ${SIA_COMPILE_FLAGS} ${MPI_COMPILE_FLAGS})
    set(SIA_LINK_FLAGS ${SIA_LINK_FLAGS} ${MPI_LINK_FLAGS})
endif()

#----------------------------------------------------------------------
# Testing Using Google Tests
#----------------------------------------------------------------------

##############
# Setup Tests
##############
set(gtest_disable_pthreads ON CACHE BOOL "Disable uses of pthreads in gtest.")
add_subdirectory (test/googletest-release-1.7.0)
enable_testing()
set(INCLUDE_DIRS ${INCLUDE_DIRS} ${gtest_SOURCE_DIR}/include ${gtest_SOURCE_DIR})

# COPY FILES NEEDED FOR TESTS TO THE BUILD DIRECTORY
configure_file(${CMAKE_SOURCE_DIR}/aces/test/ccsdpt_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/second_ccsdpt_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/cis_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
#configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
#configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_water_dimer_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
#configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_lccsd_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
#configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_lccd_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
#configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_mp2_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_ccsd_water_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/eom_ccsd_water_right_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/mcpt2_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/frag_scf_333_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/mcpt2_water_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/fef_ccpt2_water_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/mcpt2_222_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/lindep_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/scf_rhf_aguess_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/scf_uhfrhf_aguess_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/scf_uhf_triplet_aguess_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/rlambda_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/lccd_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/lccd_frozencore_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/lccsd_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/lamccsdpt_test.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/aces/test/test_1el_ecp_ints.dat ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/helloworld1.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/local_arrays_wild.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/local_arrays.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/loop_over_simple_indices.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/scalar_valued_blocks.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/scalars.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/static_array_test.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/static_array_test2.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/tmp_arrays_2.txt ${CMAKE_BINARY_DIR}/ COPYONLY)
configure_file(${CMAKE_SOURCE_DIR}/sia/test/expected_output/tmp_arrays.txt ${CMAKE_BINARY_DIR}/ COPYONLY)

#----------------------------------------------------------------------
# Build aces4 & other executables
#----------------------------------------------------------------------

include_directories(${INCLUDE_DIRS})

# Static Libraries
add_library(sip_lib
            ${SIA_SETUP_FILES}
            ${SIA_CORE_FILES}
            ${SIA_WORKER_FILES}
            ${SIA_DATA_FILES}
            ${SIA_MPI_FILES})

set(TOLINK_LIBRARIES sip_lib ${TOLINK_LIBRARIES})

# All Super instructions are in these directories
add_subdirectory(aces/src/super_instructions)
add_subdirectory(sia/src/runtime/super_instructions)

# Add Jsoncpp library
if(HAVE_JSON)
add_library(jsoncpp;
    sia/src/runtime/jsoncpp/jsoncpp.cpp;
    sia/src/runtime/jsoncpp/json_value.cpp;
    sia/src/runtime/jsoncpp/json_writer.cpp.cpp;
    sia/src/runtime/jsoncpp/json/json.h;
    sia/src/runtime/jsoncpp/json/json-forwards.h)
endif()

# The tensor library is added separately
add_library(tensordil ${tensordil_SRCS})
set_target_properties(tensordil PROPERTIES COMPILE_DEFINITIONS "${LIBTENSORDIL_DEFINITIONS}")

# executable for main program
add_executable(aces4 sia/src/main/main.cpp)

# executables for tools in the util directory
add_executable(print_siptables sia/src/util/print_siptables.cpp)
add_executable(print_array_info sia/src/util/print_array_info.cpp)
add_executable(print_init_file sia/src/util/print_init_file.cpp)
add_executable(print_worker_checkpoint sia/src/util/print_worker_checkpoint.cpp)

if(HAVE_MPI)
	add_executable(check_system sia/src/util/check_system.cpp)
endif()

## dump_array_file executable
#add_executable(dump_array_file sia/src/util/dump_array_file.cpp)

# Setting Compile & link flags
set_target_properties(aces4 PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
set_target_properties(aces4 PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")

set_target_properties(print_siptables PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
set_target_properties(print_siptables PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")

set_target_properties(print_array_info PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
set_target_properties(print_array_info PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")

set_target_properties(print_init_file PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
set_target_properties(print_init_file PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")

set_target_properties(print_worker_checkpoint PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
set_target_properties(print_worker_checkpoint PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")

if (HAVE_MPI)
	set_target_properties(check_system PROPERTIES COMPILE_FLAGS "${SIA_COMPILE_FLAGS}")
	set_target_properties(check_system PROPERTIES LINK_FLAGS "${SIA_LINK_FLAGS}")
endif()



# Libraries to link
target_link_libraries(aces4 ${TOLINK_LIBRARIES})
target_link_libraries(print_siptables ${TOLINK_LIBRARIES})
target_link_libraries(print_array_info ${TOLINK_LIBRARIES})
target_link_libraries(print_init_file ${TOLINK_LIBRARIES})
target_link_libraries(print_worker_checkpoint ${TOLINK_LIBRARIES})

if (HAVE_MPI)
	target_link_libraries(check_system ${TOLINK_LIBRARIES})
endif()

# Dependencies
if(HAVE_CUDA)
	add_dependencies(aces4 tensordil siasuperinstructions acessuperinstructions cudasuperinstructions)
	add_dependencies(print_siptables tensordil siasuperinstructions acessuperinstructions cudasuperinstructions)
	add_dependencies(print_array_info tensordil siasuperinstructions acessuperinstructions cudasuperinstructions)
	add_dependencies(print_init_file tensordil siasuperinstructions acessuperinstructions cudasuperinstructions)
	add_dependencies(print_worker_checkpoint tensordil siasuperinstructions acessuperinstructions cudasuperinstructions)
else()
	add_dependencies(aces4 tensordil siasuperinstructions acessuperinstructions)
	add_dependencies(print_siptables tensordil siasuperinstructions acessuperinstructions)
	add_dependencies(print_array_info tensordil siasuperinstructions acessuperinstructions)
	add_dependencies(print_init_file tensordil siasuperinstructions acessuperinstructions)
	add_dependencies(print_worker_checkpoint tensordil siasuperinstructions acessuperinstructions)
endif()

add_dependencies(siasuperinstructions acessuperinstructions sip_lib tensordil)

#----------------------------------------------------------------------
# Add Tests
#----------------------------------------------------------------------

add_library(test_controllers
            sia/test/test_controller.h;
            sia/test/test_controller.cpp;
            sia/test/test_controller_parallel.h;
            sia/test/test_controller_parallel.cpp;
            sia/test/test_constants.h;
            sia/test/test_constants.cpp)


# SIP test routines
add_executable(test_basic_sial      sia/test/test_basic_sial.cpp;
                                    sia/test/test_transpose_op.F;
                                    sia/test/test_transpose4d_op.F;
                                    sia/test/test_contraction_small2.F)
add_executable(test_sial  	        sia/test/test_sial.cpp)
add_executable(test_unit            sia/test/test_unit.cpp)

# QC test routines
add_executable(test_basic_qm        aces/test/test_basic_qm.cpp)
add_executable(test_qm              aces/test/test_qm.cpp)
add_executable(test_qm_aux          aces/test/test_qm_aux.cpp)
add_executable(test_qm_frag         aces/test/test_qm_frag.cpp)

# Link test executable against gtest & gtest_main
set_target_properties(test_basic_sial   PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_sial         PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_basic_qm     PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_qm           PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_qm_aux       PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_qm_frag      PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")
set_target_properties(test_unit         PROPERTIES COMPILE_FLAGS  "${SIA_COMPILE_FLAGS}")

target_link_libraries(test_basic_sial   test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_sial         test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_basic_qm     test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_qm           test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_qm_aux       test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_qm_frag      test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})
target_link_libraries(test_unit         test_controllers gtest gtest_main  ${TOLINK_LIBRARIES})

set_target_properties(test_basic_sial   PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_sial         PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_basic_qm     PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_qm           PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_qm_aux       PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_qm_frag      PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")
set_target_properties(test_unit         PROPERTIES LINK_FLAGS  "${SIA_LINK_FLAGS}")


add_dependencies(test_basic_sial   gtest gtest_main test_controllers)
add_dependencies(test_sial         gtest gtest_main test_controllers)
add_dependencies(test_basic_qm     gtest gtest_main test_controllers)
add_dependencies(test_qm           gtest gtest_main test_controllers)
add_dependencies(test_qm_aux       gtest gtest_main test_controllers)
add_dependencies(test_qm_frag      gtest gtest_main test_controllers)
add_dependencies(test_unit         gtest gtest_main test_controllers)


if (HAVE_MPI) # Multi Node Version Tests
    add_test(NAME test_unit         COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_unit)
    add_test(NAME test_basic_sial   COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_basic_sial)
    add_test(NAME test_sial         COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_sial)
    add_test(NAME test_basic_qm     COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_basic_qm)
    add_test(NAME test_qm           COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_qm)
    add_test(NAME test_qm_frag      COMMAND ${MPIEXEC} ${MPIEXEC_NUMPROC_FLAG} 2 ${MPIEXEC_PREFLAGS} ./test_qm)
else() # Single Node Version Tests
    add_test(NAME test_unit         COMMAND test_unit)
    add_test(NAME test_basic_sial   COMMAND test_basic_sial)
    add_test(NAME test_sial         COMMAND test_sial)
    add_test(NAME test_basic_qm     COMMAND test_basic_qm)
    add_test(NAME test_qm           COMMAND test_qm)
endif()

#----------------------------------------------------------------------
# Compile all SIALX files
#----------------------------------------------------------------------

# this is the list of SIA sial files
set(DOMAIN_SIAL_FILES
    aces/src/sialx/utility/drop_core_in_sial.sialx;
    aces/src/sialx/utility/tran_rhf_no4v.sialx;
    aces/src/sialx/utility/tran_uhf_no4v.sialx;
    aces/src/sialx/utility/tran_rhf_no4v_defs.sialx;
    aces/src/sialx/utility/tran_rhf_no3v.sialx;
    aces/src/sialx/scf/scf_rhf_coreh.sialx;
    aces/src/sialx/scf/scf_defs.sialx;
    aces/src/sialx/scf/scf_uhf.sialx;
    aces/src/sialx/scf/scf_uhf_defs.sialx;
    aces/src/sialx/scf/scf_rhf.sialx;
    aces/src/sialx/scf/scf_rhf_defs.sialx;
    aces/src/sialx/scf/diis_4index_rhf.sialx;
    aces/src/sialx/scf/diis_4index_uhf.sialx;
    aces/src/sialx/cc/rccsd_rhf.sialx;
    aces/src/sialx/cc/rlambda_rhf.sialx;
    aces/src/sialx/cc/rlccd_rhf.sialx;
    aces/src/sialx/cc/rlccsd_rhf.sialx;
    aces/src/sialx/cc/rccsd_rhf_defs.sialx;
    aces/src/sialx/cc/rccsdpt_aaa.sialx;
    aces/src/sialx/cc/rccsdpt_aab.sialx;
    aces/src/sialx/cc/rlamccsdpt_aaa.sialx;
    aces/src/sialx/cc/rlamccsdpt_aab.sialx;
    aces/src/sialx/cc/rccpt1p2_prop.sialx;
    aces/src/sialx/cc/ccsd_uhf_defs.sialx;
    aces/src/sialx/cc/ccsd_uhf.sialx;
    aces/src/sialx/cc/ccsd_uhf_vars.sialx;
    aces/src/sialx/cc/diis_4index_ccsd_uhf.sialx;
    aces/src/sialx/cc/diis_4index_lambda_uhf.sialx;
    aces/src/sialx/cc/lambda_uhf_defs.sialx;
    aces/src/sialx/cc/lambda_uhf.sialx;
    aces/src/sialx/cc/lambda_uhf_vars.sialx;
    aces/src/sialx/eom/eom_rhf_defs.sialx
    aces/src/sialx/eom/eom_rhf_hbar.sialx;
    aces/src/sialx/eom/eom_rhf_tran.sialx;
    aces/src/sialx/eom/eom_rhf_vars.sialx;
    aces/src/sialx/eom/eom_ccsd_rhf_right.sialx;
    aces/src/sialx/eom/eom_ccsd_rhf_left.sialx;
    aces/src/sialx/eom/rcis_rhf_defs.sialx;
    aces/src/sialx/eom/rcis_rhf_vars.sialx;
    aces/src/sialx/eom/rcis_rhf.sialx;
    aces/src/sialx/eom/rcis_d_rhf.sialx;
    aces/src/sialx/eom/rcis_ccpt2_rhf.sialx;
    aces/src/sialx/fef-cc/frag_rhf.sialx;
    aces/src/sialx/fef-cc/frag_pol_rhf.sialx;
    aces/src/sialx/fef-cc/frag_rcis_rhf.sialx;
    aces/src/sialx/fef-cc/tran_frag.sialx;
    aces/src/sialx/fef-cc/MOI_reom_defs.sialx;
    aces/src/sialx/fef-cc/MOI_reom_vars.sialx;
    aces/src/sialx/fef-cc/MOI_cis_guess.sialx;
    aces/src/sialx/fef-cc/MOI_rcis.sialx;
    aces/src/sialx/fef-cc/MOI_reom_elements.sialx;
    aces/src/sialx/fef-cc/MOI_reom_fefccpt2.sialx;
    aces/src/sialx/fef-cc/fef_ccpt2.sialx;
    aces/src/sialx/fef-cc/fef_cis_ccpt2.sialx;
    aces/src/sialx/fef-cc/mcpt2_corr.sialx;
    aces/src/sialx/fef-cc/mcpt2_corr_lowmem.sialx;
    aces/src/sialx/fef-cc/mcpt2_vars.sialx;
    aces/src/sialx/fef-cc/mcpt2_defs.sialx;
    aces/src/sialx/fef-cc/mcpt2_singles.sialx;
    aces/src/sialx/fef-cc/mcpt2_doubles.sialx;
    aces/src/sialx/fef-cc/mcpt2_util.sialx;
    aces/src/sialx/mbpt/mp2_rhf_disc.sialx;
    aces/src/sialx/mbpt/mp2_uhf_defs.sialx;
    aces/src/sialx/mbpt/mp2_uhf.sialx;
    aces/src/sialx/scf_deriv/scf_grad_ru_hf_defs.sialx;
    aces/src/sialx/scf_deriv/scf_grad_ru_hf.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_rhf_defs.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_rhf.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_rhf_vars.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_uhf_defs.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_uhf.sialx;
    aces/src/sialx/mbpt_deriv/mp2grad_uhf_vars.sialx;
    aces/src/sialx/cc_deriv/ccsd_1grad_rhf.sialx;
    aces/src/sialx/cc_deriv/ccsd_1grad_rhf_vars.sialx;
    aces/src/sialx/cc_deriv/ccsd_1grad_uhf.sialx;
    aces/src/sialx/cc_deriv/ccsd_1grad_uhf_vars.sialx;
    aces/src/sialx/cc_deriv/ccsd_2grad_rhf.sialx;
    aces/src/sialx/cc_deriv/ccsd_2grad_rhf_vars.sialx;
    aces/src/sialx/cc_deriv/ccsd_2grad_uhf.sialx;
    aces/src/sialx/cc_deriv/ccsd_2grad_uhf_vars.sialx;
    aces/src/sialx/cc_deriv/ccsdgrad_rhf_defs.sialx;
    aces/src/sialx/cc_deriv/ccsdgrad_uhf_defs.sialx;
    aces/src/sialx/cc_deriv/diis_2index_ccgrad_rhf.sialx;
    aces/src/sialx/cc_deriv/diis_2index_ccgrad_uhf.sialx)

#    aces/src/sialx/scf_deriv/scf_hess_uhf_defs.sialx;
#    aces/src/sialx/scf_deriv/scf_hess_uhf.sialx)

set(DOMAIN_SIAL_FILES
    sia/src/sialx/test/empty.sialx;
    sia/src/sialx/test/static_array_test.sialx;
    sia/src/sialx/test/ifelse.sialx;
    sia/src/sialx/test/put_test.sialx;
    sia/src/sialx/test/index_decs.sialx;
    sia/src/sialx/test/predef_scalars.sialx;
    sia/src/sialx/test/print_block_test.sialx;
    sia/src/sialx/test/pardo_loop_with_pragma.sialx;
    sia/src/sialx/test/tmp_arrays.sialx;
    sia/src/sialx/test/scalars.sialx;
    sia/src/sialx/test/insert_slice_test.sialx;
    sia/src/sialx/test/print_scalar.sialx;
    sia/src/sialx/test/where_clause.sialx;
    sia/src/sialx/test/loop_over_simple_indices.sialx;
    sia/src/sialx/test/exit_statement_test.sialx;
    sia/src/sialx/test/no_arg_user_sub.sialx;
    sia/src/sialx/test/helloworld.sialx;
    sia/src/sialx/test/pardo_loop.sialx;
    sia/src/sialx/test/pardo_loop_with_pragma.sialx;
    sia/src/sialx/test/subindex_test.sialx;
    sia/src/sialx/test/local_arrays.sialx;
    sia/src/sialx/test/scalarTable.sialx;
    sia/src/sialx/test/stringLiterals.sialx;
    sia/src/sialx/test/aces_defs.sialx;
    sia/src/sialx/test/fill_sequential_test.sialx;
    sia/src/sialx/test/local_arrays_wild.sialx;
    sia/src/sialx/test/local_arrays_wild_fail.sialx;
    sia/src/sialx/test/basic_assign_to_static_array_test.sialx;
    sia/src/sialx/test/contiguous_local.sialx;
    sia/src/sialx/test/contiguous_local_2.sialx;
    sia/src/sialx/test/contiguous_local_allocate_twice_fail.sialx;
    sia/src/sialx/test/block_scale_assign.sialx;
    sia/src/sialx/test/return_sval_test.sialx;
    sia/src/sialx/test/broadcast_static.sialx;
    sia/src/sialx/test/sum_op_test.sialx;
    sia/src/sialx/test/transpose_tmp.sialx;
    sia/src/sialx/test/transpose4d_tmp.sialx;
    sia/src/sialx/test/transpose4d_square_tmp.sialx;
    sia/src/sialx/test/tmp_arrays_2.sialx;
    sia/src/sialx/test/contraction_small_test.sialx;
    sia/src/sialx/test/contraction_small_test2.sialx;
    sia/src/sialx/test/get_int_array_test.sialx;
    sia/src/sialx/test/get_scalar_array_test.sialx;
    sia/src/sialx/test/get_scratch_array_test.sialx;
    sia/src/sialx/test/gpu_contraction_small_test.sialx;
    sia/src/sialx/test/gpu_ops.sialx;
    sia/src/sialx/test/contract_to_scalar.sialx;
    sia/src/sialx/test/gpu_sum_op_test.sialx;
    sia/src/sialx/test/gpu_contract_to_scalar.sialx;
    sia/src/sialx/test/gpu_transpose_tmp.sialx;
    sia/src/sialx/test/self_multiply_test.sialx;
    sia/src/sialx/test/gpu_self_multiply_test.sialx;
    sia/src/sialx/test/simple_indices_assignments.sialx;
    sia/src/sialx/test/add_and_subtract_scalars.sialx;
    sia/src/sialx/test/gpu_contraction_predefined_test.sialx;
    sia/src/sialx/test/assign_to_static_array_test.sialx;
    sia/src/sialx/test/set_persistent_test.sialx;
    sia/src/sialx/test/restore_persistent_test.sialx;
    sia/src/sialx/test/persistent_static_array_test1.sialx;
    sia/src/sialx/test/persistent_static_array_test2.sialx;
    sia/src/sialx/test/persistent_distributed_array_test1.sialx;
    sia/src/sialx/test/persistent_distributed_array_test2.sialx;
    sia/src/sialx/test/put_test_mpi.sialx;
    sia/src/sialx/test/put_initialize.sialx;
    sia/src/sialx/test/put_increment.sialx;
    sia/src/sialx/test/put_accumulate_mpi.sialx;
    sia/src/sialx/test/delete_mpi.sialx;
    sia/src/sialx/test/get_mpi.sialx;
    sia/src/sialx/test/persistent_distributed_array_mpi1.sialx;
    sia/src/sialx/test/persistent_distributed_array_mpi2.sialx;
    sia/src/sialx/test/persistent_distributed_array_one_of_three.sialx;
    sia/src/sialx/test/persistent_distributed_array_two_of_three.sialx;
    sia/src/sialx/test/persistent_distributed_array_three_of_three.sialx;
    sia/src/sialx/test/persistent_empty_mpi1.sialx;
    sia/src/sialx/test/persistent_empty_mpi2.sialx;
    sia/src/sialx/test/persistent_scalars_1.sialx;
    sia/src/sialx/test/persistent_scalars_2.sialx;
    sia/src/sialx/test/unmatched_get.sialx;
    sia/src/sialx/test/all_rank_print_test.sialx;
    sia/src/sialx/test/message_number_wraparound_test.sialx;
    sia/src/sialx/test/pardo_loop_1d.sialx;
    sia/src/sialx/test/pardo_loop_2d.sialx;
    sia/src/sialx/test/pardo_loop_3d.sialx;
    sia/src/sialx/test/pardo_loop_4d.sialx;
    sia/src/sialx/test/pardo_loop_5d.sialx;
    sia/src/sialx/test/pardo_loop_6d.sialx;
    sia/src/sialx/test/scalar_ops.sialx;
    sia/src/sialx/test/int_ops.sialx;
    sia/src/sialx/test/int_self_ops.sialx;
    sia/src/sialx/test/scalar_valued_blocks.sialx;
    sia/src/sialx/test/broadcast_static.sialx;
    sia/src/sialx/test/block_scale_assign.sialx;
    sia/src/sialx/test/return_sval_test.sialx;
    sia/src/sialx/test/index_scalar_cast.sialx;
    sia/src/sialx/test/contig_local3.sialx;
    sia/src/sialx/test/cached_block_map_test.sialx;
    sia/src/sialx/test/cached_block_map_test_no_dangling_get.sialx;
    sia/src/sialx/test/pardo_with_where.sialx;
    sia/src/sialx/test/pardo_load_balance_test.sialx;
    sia/src/sialx/test/read_block_test.sialx;
    sia/src/sialx/test/cast_indices_to_simple.sialx;
    sia/src/sialx/test/aoladder.sialx;
    sia/src/sialx/test/test_ecpint.sialx;
    sia/src/sialx/test/put_accumulate_stress.sialx;
    sia/src/sialx/test/disk_backing_test.sialx;
    sia/src/sialx/test/disk_backing_put_acc_stress.sialx;
    sia/src/sialx/test/decreasing_segs.sialx;
    sia/src/sialx/test/check_block_number_calc.sialx)


# Compile all the sialx files
# this section is made more complicated than needed because
# a) custom compiler for sial
# b) taking sub directories and then dumping all the compiled files into a big lump directory is not trivial.
set(SIAL_COMPILER_JAR ${CMAKE_SOURCE_DIR}/sia/sial_compiler/SialCompiler.jar)
set(SIAL_COMPILER java -jar ${SIAL_COMPILER_JAR})
set (SIAL_COMPILED_FILES "")
foreach(F ${SIAL_FILES})
    get_filename_component(F_WE ${CMAKE_SOURCE_DIR}/${F} NAME_WE)
    get_filename_component(SIALX_FILE_PATH ${CMAKE_SOURCE_DIR}/${F} PATH)
#    get_filename_component(SIALX_FILE_LAST_PATH ${SIALX_FILE_PATH} NAME)
#    set(SIAL_COMPILE_DIR "${CMAKE_BINARY_DIR}/src/sialx/${SIALX_FILE_LAST_PATH}")
    set(SIAL_COMPILE_DIR "${CMAKE_BINARY_DIR}/siox")
    add_custom_command(
        OUTPUT ${SIAL_COMPILE_DIR}/${F_WE}.siox
        COMMAND ${SIAL_COMPILER} -sp ${SIALX_FILE_PATH} ${CMAKE_SOURCE_DIR}/${F}
        COMMAND ${CMAKE_COMMAND} -E make_directory ${SIAL_COMPILE_DIR}
        COMMAND mv -f ${SIALX_FILE_PATH}/${F_WE}.siox ${SIAL_COMPILE_DIR}
        DEPENDS ${SIAL_COMPILER_JAR} ${CMAKE_SOURCE_DIR}/${F}
        COMMENT "Compiling ${F} to ${SIAL_COMPILE_DIR}/${F_WE}.siox")
    list (APPEND SIAL_COMPILED_FILES "${SIAL_COMPILE_DIR}/${F_WE}.siox")
endforeach()

add_custom_target(SIAL_FILES_COMPILE ALL DEPENDS ${SIAL_COMPILED_FILES})
