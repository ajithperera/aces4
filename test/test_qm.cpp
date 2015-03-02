#include "gtest/gtest.h"
#include "siox_reader.h"
#include "io_utils.h"
#include "setup_reader.h"

#include "sip_tables.h"
#include "interpreter.h"
#include "setup_interface.h"
#include "sip_interface.h"
#include "data_manager.h"
#include "test_controller.h"
#include "test_controller_parallel.h"
#include "test_constants.h"
#include "sip_timer.h"

#include "block.h"

#ifdef HAVE_MPI
#include "sip_mpi_utils.h"
#endif


//bool VERBOSE_TEST = false;
bool VERBOSE_TEST = true;

// old ccsd(t) test.... difficult to deal with because the ZMAT source is gone
TEST(Sial_QM,DISABLED_ccsdpt_test){
	std::string job("ccsdpt_test");

	std::stringstream output;

	TestControllerParallel controller(job, true, VERBOSE_TEST, "", output);

	controller.initSipTables(qm_dir_name);
	controller.run();

	controller.initSipTables(qm_dir_name);
	controller.run();

	controller.initSipTables(qm_dir_name);
	controller.run();

	controller.initSipTables(qm_dir_name);
	controller.run();

	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double eaab = controller.scalar_value("eaab");
		ASSERT_NEAR(-0.0010909774775509193, eaab, 1e-10);
		double esaab =controller.scalar_value("esaab");
		ASSERT_NEAR(8.5547845910409156e-05, esaab, 1e-10);
//		controller.print_timers(std::cout);
	}

}

/* new CCSD(T) test.  ZMAT source is:
test
H 0.0 0.0 0.0
F 0.0 0.0 0.917

*ACES2(BASIS=3-21G
scf_conv=12
cc_conv=12
spherical=off
CALC=ccsd)

*SIP
MAXMEM=1500
SIAL_PROGRAM = scf_rhf_coreh.siox
SIAL_PROGRAM = tran_rhf_no4v.siox
SIAL_PROGRAM = rccsd_rhf.siox
SIAL_PROGRAM = rccsdpt_aaa.siox
SIAL_PROGRAM = rccsdpt_aab.siox

*/
TEST(Sial_QM,second_ccsdpt_test){
	std::string job("second_ccsdpt_test");

	std::stringstream output;

	TestControllerParallel controller(job, true, VERBOSE_TEST, "", output);
//
// SCF
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double scf_energy = controller.scalar_value("scf_energy");
		ASSERT_NEAR(-99.45975176375698, scf_energy, 1e-10);
	}
//
// tran
	controller.initSipTables(qm_dir_name);
	controller.run();
//
// ccsd
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double ccsd_correlation = controller.scalar_value("ccsd_correlation");
		ASSERT_NEAR(-0.12588695910754, ccsd_correlation, 1e-10);
		double ccsd_energy = controller.scalar_value("ccsd_energy");
		ASSERT_NEAR(-99.58563872286452, ccsd_energy, 1e-10);
	}
//
// ccsdpt aaa
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double eaaa = controller.scalar_value("eaaa");
		ASSERT_NEAR(-0.00001091437340, eaaa, 1e-10);
		double esaab =controller.scalar_value("esaaa");
		ASSERT_NEAR(0.00000240120432, esaab, 1e-10);
	}
//
// ccsdpt aab
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double eaab = controller.scalar_value("eaab");
		ASSERT_NEAR(-0.00058787722879, eaab, 1e-10);
		double esaab =controller.scalar_value("esaab");
		ASSERT_NEAR(0.00003533079603, esaab, 1e-10);
		double ccsdpt_energy = controller.scalar_value("ccsdpt_energy");
		ASSERT_NEAR(-99.58619978246637, ccsdpt_energy, 1e-10);
	}

}

/* CIS(D) test, ZMAT is:
test
H 0.0 0.0 0.0
F 0.0 0.0 0.917

*ACES2(BASIS=3-21G
scf_conv=10
cc_conv=10
spherical=off
excite=eomee
estate_sym=2
estate_tol=10
CALC=ccsd)

*SIP
MAXMEM=1500
SIAL_PROGRAM = scf_rhf_coreh.siox
SIAL_PROGRAM = tran_rhf_no3v.siox
SIAL_PROGRAM = rcis_rhf.siox
SIAL_PROGRAM = rcis_d_rhf.siox

*/
TEST(Sial_QM,cis_test){
	std::string job("cis_test");

	std::stringstream output;

	TestControllerParallel controller(job, true, VERBOSE_TEST, "", output);
//
// SCF
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double scf_energy = controller.scalar_value("scf_energy");
		ASSERT_NEAR(-99.45975176375698, scf_energy, 1e-10);
	}
//
// TRAN
	controller.initSipTables(qm_dir_name);
	controller.run();
//
// CIS
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double * sek0 = controller.static_array("sek0");
		double expected[] = {0.43427913493064, 0.43427913493252};
		int i = 0;
		for (i; i < 2; i++){
		    ASSERT_NEAR(sek0[i], expected[i], 1e-10);
		}
	}
//
// CIS(D)
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double * ekd = controller.static_array("ekd");
		double expected[] = {-0.03032115569246, -0.03032115569276};
		int i = 0;
		for (i; i < 2; i++){
		    ASSERT_NEAR(ekd[i], expected[i], 1e-10);
		}
	}

}

TEST(Sial_QM,mcpt2_test){
	std::string job("mcpt2_test");

	std::stringstream output;

	TestControllerParallel controller(job, true, VERBOSE_TEST, "", output);
//
// mcpt
	controller.initSipTables(qm_dir_name);
	controller.run();

	if (attr->global_rank() == 0) {
		double e1x_at = controller.scalar_value("e1x_at");
		ASSERT_NEAR(0.01029316845696, e1x_at, 1e-10);
		double e10pol_at = controller.scalar_value("e10pol_at");
		ASSERT_NEAR(-0.01865175583286, e10pol_at, 1e-10);
		double singles = controller.scalar_value("singles");
		ASSERT_NEAR(-0.00106131022528, singles, 1e-10);
		double dimer_doubles = controller.scalar_value("dimer_doubles");
		ASSERT_NEAR(-0.00054912647095, dimer_doubles, 1e-10);
		double fragment_doubles = controller.scalar_value("fragment_doubles");
		ASSERT_NEAR(-0.25081040375757, fragment_doubles, 1e-10);
		double mono_lccd = controller.scalar_value("mono_lccd");
		ASSERT_NEAR(0.25084388980906, mono_lccd, 1e-10);
	}

}

//****************************************************************************************************************

int main(int argc, char **argv) {


#ifdef HAVE_MPI
	MPI_Init(&argc, &argv);
	int num_procs;
	sip::SIPMPIUtils::check_err(MPI_Comm_size(MPI_COMM_WORLD, &num_procs));

	if (num_procs < 2) {
		std::cerr << "Please run this test with at least 2 mpi ranks"
				<< std::endl;
		return -1;
	}
#endif

	sip::SIPMPIAttr &sip_mpi_attr = sip::SIPMPIAttr::get_instance();
	attr = &sip_mpi_attr;
	barrier();

	//INIT_GLOBAL_TIMERS(&argc, &argv);

	check_expected_datasizes();

	printf("Running main() from %s\n",__FILE__);
	testing::InitGoogleTest(&argc, argv);
	barrier();
	int result = RUN_ALL_TESTS();

	//FINALIZE_GLOBAL_TIMERS();

	barrier();

#ifdef HAVE_MPI
	MPI_Finalize();
#endif

	return result;
}


