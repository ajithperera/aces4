/** special_instructions.cpp   This class manages special (i.e. user defined) super instructions.
 *
 *To extend ACES4 with a super instruction the following changes to this class are required:
 *
 * 	4. add a C prototype (inside the "extern "C"" block for instruction written in fortran and c, outside for instructions written in c++)
 * 	2. add a statement to the init_procmap method to add it to the procmap_. This is done as follows
 * 	    (*procmap)["sialname"] = (fp)&cname;
 * 	    where "sialname" is a string that matches the name declared in sial programs
 * 	    and cname is the name of the routine implementing the program as declared in the "extern "C"" block.
 * 	    It is strongly encouraged that these names be the same!!!!
 *
 *
 * 	Comment:  This design allow the sial compiler to set up an arbitrary index mapping for special super instructions and
 * 	thus adding a super instruction only requires changes to this file.
 *
 *  Created on: Aug 3, 2043
 *      Author: Beverly Sanders
 */

//TODO change c-style casts to reinterpret casts.

#include "special_instructions.h"
#include <iostream>
#include <stdexcept>
#include <string>
#include <sstream>
#include "sip.h"
#include "sip_tables.h"

#ifdef HAVE_MPI
#include "sip_mpi_attr.h"
#endif


extern "C"{
//the following 3 super instructions are for testing and will be deleted at some point
//void dadd(double, double, double*);
//void dsub(double, double, double*);
void print_something();
void get_and_print_mpi_rank();
//the following super instructions are real
//ADD C PROTOTYPE FOR SPECIAL SUPERINSTRUCTION WRITTEN IN C OR FORTRAN HERE
//THESE MUST BE INSIDE THE extern "C" block

void get_scratch_array_dummy(
        int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0, 
	int &ierr);

void get_and_print_int_array_dummy(
        int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0, 
	int &ierr);

void get_and_print_scalar_array_dummy(
        int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0, 
	int &ierr);

void fill_block_sequential(
        int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0,
        int& array_slot_1, int& rank_1, int * index_values_1, int& size_1, int * extents_1, double * data_1, 
	int& ierr);

void fill_block_cyclic(
        int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0,
        int& array_slot_1, int& rank_1, int * index_values_1, int& size_1, int * extents_1, double * data_1, 
	int& ierr);

//##############################
}

//ADD PROTOTYPE FOR SPECIAL INSTRUCTIONS WRITTEN IN C++ HERE (i.e. not inside
 //the extern C block)

//ADD PROTOTYPE FOR SPECIAL INSTRUCTIONS WRITTEN IN C++ HERE (i.e. not inside
 //the extern C block)
void print_block(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
void read_block_from_text_file(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
//void test_print_block(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
void write_block_to_file(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
void read_block_from_file(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
void print_static_array(int& array_slot, int& rank, int* index_values, int& size, int* extents, double* data, int& ierr);
void get_my_rank(int& array_slot, int& rank, int* index_values, int& size, int* extents, double* data, int& ierr);
void list_block_map();

void disable_debug_print();
void enable_debug_print();

void enable_all_rank_print();
void disable_all_rank_print();
void get_first_block_element(int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0,
        int& array_slot_1, int& rank_1, int * index_values_1, int& size_1, int * extents_1, double * data_1, int& ierr);


// Special Super Instructions Just For Testing
void swap_blocks(int& array_slot_0, int& rank_0, int * index_values_0, int& size_0, int * extents_0, double * data_0,
        int& array_slot_1, int& rank_1, int * index_values_1, int& size_1, int * extents_1, double * data_1, int& ierr);
void one_arg_no_op(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);
void list_blocks_with_number();
void check_block_number_calculation(int& array_slot, int& rank, int* index_values, int& size, int* extents,  double* data, int& ierr);

namespace sip{

SpecialInstructionManager::SpecialInstructionManager(){
//	std::cout << "in SpecialInstructionManager constructor";
//    CHECK(procmap_.empty(), "attempting to initialize non-empty procmap");
	init_procmap();
}

SpecialInstructionManager::~SpecialInstructionManager(){
//	std::cout << "in SpecialInstructionManager destructor";
}

void SpecialInstructionManager::read(SpecialInstructionManager& manager, setup::InputStream &siox_file){
	int n = siox_file.read_int();
	for (int i = 0; i != n; ++i){
		std::string name = siox_file.read_string();
		manager.add_special(name);
	}
	manager.add_special_finalize();
}

int SpecialInstructionManager::add_special(const std::string name_with_sig){
	SIP_LOG(std::cout<< "add_special: " << name_with_sig << std::endl);
	if (procmap_.empty()) init_procmap();
	int index = procvec_.size();
//	void(*func)(int* ierr) = procmap_.at(name);
//	procvec_.push_back(func);
	SIP_LOG(std::cout << " adding special instruction " << name_with_sig << std::endl);
//	std::cout << *this << std::endl;
//	std::cout << "*********************" << std::endl;
	size_t name_with_sig_length = name_with_sig.size();
	unsigned at_position = name_with_sig.find('@');
	SIP_LOG(std::cout<< "at_position " << at_position << std::endl);
	const std::string name = name_with_sig.substr(0,at_position);//name of instruction

	SIP_LOG(std::cout << "name: " << name << std::endl);
	const std::string sig = name_with_sig.substr(at_position+1);//skip the '@' char
	SIP_LOG(std::cout << "sig: " << sig << std::endl);
	proc_index_name_map_[index] = name;
	try{
		std::map<std::string, fp0>::iterator it = procmap_.find(name);
		if (it == procmap_.end()){
			SIP_LOG(WARN(false, std::string("Special instruction ") + name + " not found"));
			procvec_.push_back(procvec_entry_t(NULL, sig));
		} else {
			fp0 func = it->second;
			//procvec_[index] = procvec_entry_t(func,sig);
			procvec_.push_back(procvec_entry_t(func, sig));
		}
	}
	catch (const std::out_of_range& oor) {
        warn(std::string("Special instruction " + name + " declared in SIAL program, but no implementation was found"));
        procvec_.push_back(procvec_entry_t(NULL, sig));
    };
	return index;

}

std::string SpecialInstructionManager::name(int procvec_slot) const{
	//return proc_index_name_map_.at(procvec_slot);
	proc_index_name_map_t::const_iterator it = proc_index_name_map_.find(procvec_slot);
	if(it == proc_index_name_map_.end()){
		std::stringstream ss;
		ss << "Could not find procedure slot " << procvec_slot ;
		throw std::out_of_range(ss.str());
	}
	return it->second;
}
void SpecialInstructionManager::add_special_finalize(){
//	procmap_.clear();
}

SpecialInstructionManager::fp0 SpecialInstructionManager::get_instruction_ptr(int function_slot) const{
	try{
	fp0 func = procvec_.at(function_slot).first;
	if (func == NULL){
		//SIP_LOG(std::cout<< "special instruction " << proc_index_name_map_[function_slot] << " at slot " << function_slot << " not installed" << std::endl);
		throw std::out_of_range(std::string("function not found"));
	}
	return func;
	}
	catch (const std::out_of_range& oor){
		std::cout << oor.what() << std::endl;
		proc_index_name_map_t::const_iterator it = proc_index_name_map_.find(function_slot);
		std::cout << "special instruction " << it->second << " at slot " << function_slot << " not installed" << std::endl;
		CHECK(false, std::string(" terminating get_instruction_ptr "));
		return NULL;
	}
}


SpecialInstructionManager::fp0 SpecialInstructionManager::get_no_arg_special_instruction_ptr(int function_slot) const{
	return get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp1 SpecialInstructionManager::get_one_arg_special_instruction_ptr(int function_slot)  const{
	return (fp1)get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp2 SpecialInstructionManager::get_two_arg_special_instruction_ptr(int function_slot) const{
	return (fp2)get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp3 SpecialInstructionManager::get_three_arg_special_instruction_ptr(int function_slot) const{
	return (fp3)get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp4 SpecialInstructionManager::get_four_arg_special_instruction_ptr(int function_slot) const{
	return (fp4)get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp5 SpecialInstructionManager::get_five_arg_special_instruction_ptr(int function_slot) const{
	return (fp5)get_instruction_ptr(function_slot);
}
SpecialInstructionManager::fp6 SpecialInstructionManager::get_six_arg_special_instruction_ptr(int function_slot) const{
	return (fp6)get_instruction_ptr(function_slot);
}

const std::string SpecialInstructionManager::get_signature(int function_slot) const{
	try{
	return procvec_.at(function_slot).second;
	}
	catch (const std::out_of_range& oor){
		proc_index_name_map_t::const_iterator it = proc_index_name_map_.find(function_slot);
		std::cout << "special instruction " << it->second << ", at slot " << function_slot << " not installed" << std::endl;
		std::cout << *this << std::endl;
		CHECK(false, std::string(" terminating get_signature"));
		return std::string("should not get here");
	}
}

std::ostream& operator<<(std::ostream& os, const SpecialInstructionManager& obj){
	int n = obj.procvec_.size();
	for (int i = 0; i != n; ++i){
		SpecialInstructionManager::proc_index_name_map_t::const_iterator it = obj.proc_index_name_map_.find(i);
		if(it == obj.proc_index_name_map_.end()){
			std::stringstream ss;
			ss << "Could not find proc index for value " << i ;
			throw std::out_of_range(ss.str());
		}
		os << i << ": " << it->second << ": " << obj.procvec_.at(i).second << std::endl;
	}
	return os;
}


void SpecialInstructionManager::init_procmap(){
    // TEST  The next few instructions are used for testing
//	procmap_["dadd"] = (fp0)&dadd;
//	procmap_["dsub"] = (fp0)&dsub;
	procmap_["print_something"] = (fp0)&print_something;
	procmap_["fill_block_sequential"]= (fp0)&fill_block_sequential;
	procmap_["fill_block_cyclic"]= (fp0)&fill_block_cyclic;
//	procmap_["test_print_block"]=(fp0)&test_print_block;

	procmap_["print_block"]=(fp0)&print_block;
	procmap_["write_block_to_file"]=(fp0)&write_block_to_file;
	procmap_["read_block_from_file"]=(fp0)&read_block_from_file;
	procmap_["read_block_from_text_file"]=(fp0)&read_block_from_text_file;
	procmap_["print_static_array"]=(fp0)&print_static_array;
	procmap_["list_block_map"]=(fp0)&list_block_map;
	procmap_["get_my_rank"]=(fp0)&get_my_rank;
    procmap_["check_block_number_calculation"]=(fp0)&check_block_number_calculation;

	//ADD STATEMENT TO ADD SPECIAL SUPERINSTRUCTION TO MAP HERE.  COPY ONE OF THE ABOVE LINES AND REPLACE THE
	//CHARACTERS IN QUOTES WITH THE (CASE SENSITIVE NAME USED IN SIAL PROGRAMS.  REPLACE THE CHARACTERS FOLLOWING
	//THE & WITH THE NAME IN THE C PROTOTYPE.
}

} /*namespace sip*/
