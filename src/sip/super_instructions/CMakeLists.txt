project(super_instructions_library)

# General Super Instructions
set(GENERAL_SOURCE_FILES
    general/list_block_map.cpp;
    general/print_block.cpp;
    general/write_block_to_file.cpp;
    general/read_block_from_file.cpp;
    general/print_static_array.cpp;
    general/get_my_rank.cpp;
    general/swap_blocks.cpp;
    general/disable_debug_print.cpp;
    general/enable_debug_print.cpp;
    general/enable_all_rank_print.cpp;
    general/disable_all_rank_print.cpp;
    general/get_first_block_element.cpp;
    general/read_block_from_text_file.cpp;
    general/abort_job.F;
    general/one_arg_no_op.cpp;
    general/list_blocks_with_number.cpp;
    general/check_block_number_calculation.cpp;
    qm/instructions_for_testing.f;
    qm/tests/get_and_print_int_array_dummy.F;
    qm/tests/get_and_print_scalar_array_dummy.F;
    qm/tests/get_scratch_array_dummy.F;
    qm/tests/fill_block_sequential.f;
    qm/tests/fill_block_cyclic.f;
    qm/tests/get_and_print_mpi_rank.F)


set(SRCS 
    ${GENERAL_SOURCE_FILES})

include_directories(    
    ${CMAKE_SOURCE_DIR}/aces/src/super_instructions;
    ${CMAKE_SOURCE_DIR}/src/sip;
    ${CMAKE_SOURCE_DIR}/src/sip/tensor_algebra;
    ${CMAKE_SOURCE_DIR}/src/sip/core;
    ${CMAKE_SOURCE_DIR}/src/sip/setup;
    ${CMAKE_SOURCE_DIR}/src/sip/worker;
    ${CMAKE_SOURCE_DIR}/src/sip/static_data;
    ${CMAKE_SOURCE_DIR}/src/sip/dynamic_data;
    ${CMAKE_SOURCE_DIR}/src/sip/mpi;
    ${CMAKE_SOURCE_DIR}/src/sip/cuda)

add_library(siasuperinstructions ${SRCS})


