#include <crab/config.h>
#include <crab/common/types.hpp>
#include <crab/common/debug.hpp>

#include <crab/cfg/cfg.hpp>
#include <crab/cfg/var_factory.hpp>
#include <crab/analysis/fwd_analyzer.hpp>

#include <crab/domains/linear_constraints.hpp> 
#include <crab/domains/intervals.hpp>                      
#include <crab/domains/split_dbm.hpp>
#include <crab/domains/apron_domains.hpp>                      
#include <crab/domains/dis_intervals.hpp>
#include <crab/domains/term_equiv.hpp>
//#include <crab/domains/array_graph.hpp>                      
//#include <crab/domains/array_smashing.hpp>
//#include <crab/domains/combined_domains.hpp>                      

#include <boost/python.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <fstream>
#include <string>

using namespace std;

namespace crab {

  namespace cfg_impl {

    using namespace cfg;
    using namespace std;

    template<> inline std::string get_label_str(std::string e) 
    { return e; }

    // A variable factory based on strings
    typedef cfg::var_factory_impl::str_variable_factory variable_factory_t;
    typedef typename variable_factory_t::varname_t varname_t;

    // CFG
    typedef variable< z_number, varname_t >      z_var;
    typedef std::string                          basic_block_label_t;
    typedef Cfg< basic_block_label_t, varname_t> cfg_t;
    typedef cfg_ref<cfg_t>                       cfg_ref_t;
    typedef cfg_rev<cfg_ref_t>                   cfg_rev_t;
    typedef cfg_t::basic_block_t                 basic_block_t;
  }

  namespace domain_impl {
    using namespace crab::cfg_impl;
    using namespace crab::domains; 
    using namespace ikos;
    typedef linear_expression<z_number, varname_t> z_lin_t;
    typedef linear_constraint<z_number, varname_t> z_lin_cst_t;
    typedef linear_constraint_system<z_number, varname_t> z_lin_cst_sys_t;
    // Numerical domains
    // -- intervals
    typedef interval_domain< z_number, varname_t > interval_domain_t;
    // -- difference constraints
    typedef SDBM_impl::DefaultParams<z_number, SDBM_impl::GraphRep::adapt_ss> SplitDBMGraph;
    typedef SplitDBM<z_number, varname_t, SplitDBMGraph> zones_domain_t;
    // -- octagon constraints
    typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_OPT_OCT > oct_domain_t;
    // -- polyhedra
    typedef apron_domain< z_number, varname_t, apron_domain_id_t::APRON_PK > pk_domain_t;
    // -- disjunctive intervals
    typedef dis_interval_domain<z_number, varname_t > dis_interval_domain_t;
    // -- intervals with uninterpreted functions
    typedef term_domain<term::TDomInfo<z_number, varname_t, interval_domain_t> > term_domain_t;
    // typedef term_domain<term::TDomInfo<z_number, varname_t, sdbm_domain_t> > term_dbm_t;
    // typedef term_domain<term::TDomInfo<z_number, varname_t, dis_interval_domain_t> > term_dis_int_t;
    // typedef reduced_numerical_domain_product2<term_dis_int_t, sdbm_domain_t> num_domain_t; 
    // Array domains
    // typedef array_graph_domain<sdbm_domain_t, interval_domain_t> array_graph_domain_t;
    // typedef array_smashing<dis_interval_domain_t> array_smashing_t;
  } 
}


// A class for storing user options
struct crabOpts 
{
  bool stats_enabled;
  std::string filename;

  unsigned widening ;
  unsigned narrowing;
  unsigned widening_thresholds;

  crabOpts ()
  : stats_enabled(false), filename(""),
    widening (1), narrowing(2), widening_thresholds (50) { }

  void write (std::ostream& o) const 
  {
    o << "Read user options\n";
    o << "\tFilename=" << filename << (filename == "" ? "\"\"" : filename) << "\n";
    o << "\tEnabled stats=" << (stats_enabled? "yes" : "not") << "\n";
    o << "\tWidening=" << widening << "\n";
    o << "\tNarrowing=" << narrowing << "\n";
    o << "\tWidening thresholds=" << widening_thresholds << "\n";
  }
};

enum abs_domain_t { INTERVALS, ZONES, OCTAGONS, POLYHEDRA};

// A class that builds a CFG from a file
class CfgBuilder {

  crab::cfg_impl::variable_factory_t &m_vfac;  
  crab::cfg_impl::cfg_t* m_cfg;
  abs_domain_t m_abs_domain;

  // z_var mk_z_var (boost::python::object py_var) 
  // { // TODO 
  // }

  // z_lin_t mk_z_lin_exp (boost::python::object py_lin_exp) 
  // { // TODO 
  // }
  
  // z_lin_cst_t mk_z_lin_cst (boost::python::object py_lin_cst) 
  // { // TODO 
  // }

  // z_lin_cst_t mk_z_lin_cst (boost::python::object py_lin_cst) 
  // { // TODO 
  // }
    
  // basic_block_t& mk_basic_block (boost::python::object py_bb) 
  // { // TODO
  // }

  // void mk_edge (boost::python::object py_edge) 
  // { // TODO
  // }

 public:

  CfgBuilder (crab::cfg_impl::variable_factory_t &vfac)
      : m_vfac (vfac), m_cfg(nullptr), m_abs_domain (INTERVALS) { }

  void run (boost::python::object py_cfg) 
  {
    // TODO
  }

  crab::cfg_impl::cfg_t* get_cfg () { return m_cfg;}

  abs_domain_t get_abs_dom () const { return m_abs_domain;}
};

// A class that call Crab to infer invariants from a CFG
class Crab {

  crab::cfg_impl::cfg_t *m_cfg;
  crab::cfg_impl::variable_factory_t &m_vfac;
  const crabOpts &m_copts;

 public:

  Crab (crab::cfg_impl::cfg_t* cfg, 
        crab::cfg_impl::variable_factory_t& vfac,
        const crabOpts & copts)
  : m_cfg(cfg), m_vfac(vfac), m_copts (copts) {}
  
  template<typename Dom>
  void run ()
  {
    if (!m_cfg) 
    {
      crab::outs () << "No Cfg found\n";
      return;
    }

    // Run fixpoint 
    typename crab::analyzer::num_fwd_analyzer
    <crab::cfg_impl::cfg_ref_t,Dom,crab::cfg_impl::variable_factory_t>::type 
    a (*m_cfg, m_vfac, nullptr, m_copts.widening, m_copts.narrowing, m_copts.widening_thresholds);
    Dom inv = Dom::top ();
    crab::outs() << "Invariants using " << inv.getDomainName () << "\n";
    a.Run (inv);

    // Print invariants
    for (auto &b : *m_cfg) {
      auto inv = a[b.label ()];
      crab::outs() << crab::cfg_impl::get_label_str (b.label ()) << "=" << inv << "\n";
    }
    crab::outs() << "\n";

    // Print stats
    if (m_copts.stats_enabled) {
      crab::CrabStats::Print(crab::outs());
      crab::CrabStats::reset();
    }
  }
};


static int crab_options (int argc, char**argv, crabOpts &copts) 
{

  bool stats_enabled = false;                                                                         
  unsigned widening = 1;
  unsigned narrowing = 2;
  unsigned widening_thresholds = 50;

  boost::program_options::options_description po("Crab Options");                                     
  po.add_options()                                                                                    
      ("help", "Print help message");                                                                 
  po.add_options()                                                                                    
      ("stats",boost::program_options::bool_switch(&stats_enabled), "Enable stats");                  
  po.add_options()                                                                                    
      ("log",  boost::program_options::value<std::vector<string> >(), "Enable specified log level");  
  po.add_options()
      ("input-file", boost::program_options::value<string>(), "input file") ;
  po.add_options()                                                                                    
      ("widening-delay",boost::program_options::value<unsigned>(&widening), "Max number of iterations until performing widening");                  
  po.add_options()                                                                                    
      ("narrowing",boost::program_options::value<unsigned>(&narrowing), "Max number of narrowing iterations");                  
  po.add_options()                                                                                    
      ("widening-thresholds",boost::program_options::value<unsigned>(&widening_thresholds), "Max number of widening thresholds");                  

  boost::program_options::options_description cmmdline_options;                                       
  cmmdline_options.add(po);                                                                           
  boost::program_options::variables_map vm;                                                           
  boost::program_options::positional_options_description p;                                           
  p.add("input-file", 1);
  boost::program_options::store(boost::program_options::command_line_parser(argc, argv).              
            options(cmmdline_options).                                                                
            positional(p).                                                                            
            run(), vm);                                                                             
  boost::program_options::notify(vm);                                                                 

  copts.stats_enabled = stats_enabled;
  if (vm.count("help")) {
    std::cout  << po << "\n";
    return -1; //stop execution
  }

  if (vm.count("input-file")) {
    copts.filename = vm ["input-file"].as<std::string>();
  }

  if (vm.count("log")) {                                                                              
    std::vector<std::string> loggers = vm ["log"].as<std::vector<std::string> > ();                                       
    for(unsigned int i=0; i<loggers.size (); i++)                                                     
      crab::CrabEnableLog (loggers [i]);                                                              
  }                                                                                                   

  if (vm.count("widening")) {
    copts.widening = vm ["widening"].as<unsigned>();
  }

  if (vm.count("narrowing")) {
    copts.narrowing = vm ["narrowing"].as<unsigned>();
  }

  if (vm.count("widening-thresholds")) {
    copts.widening_thresholds = vm ["widening-thresholds"].as<unsigned>();
  }

  return 1;
}

int main (int argc, char**argv)
{
  namespace py = boost::python;

  crabOpts copts; 
  if (crab_options (argc, argv, copts) > 0) {
    copts.write (std::cout); // for debugging

    crab::cfg_impl::variable_factory_t vfac;
    CfgBuilder B (vfac);
    try {
      Py_Initialize();
      // insert the current working directory into the python path so module search 
      // can take advantage. This must happen after python has been initiialized.
      boost::filesystem::path workingDir = boost::filesystem::absolute("./").normalize();
      PyObject* sysPath = PySys_GetObject((char*) "path");
      PyList_Insert(sysPath, 0, PyString_FromString(workingDir.string().c_str()));
      
      // load the main namespace  
      py::object main_module = py::import("__main__");
      // load the dictionary for the namespace        
      py::object main_namespace = main_module.attr("__dict__");
      // import the crabGrammar module into the namespace    
      py::exec("import crabGrammar", main_namespace); 
      // create the locally-held CrabParser object
      py::object parser  = py::eval("crabGrammar.CrabParser()", main_namespace);  

      std::ifstream ifs(copts.filename);
      if (ifs)
      {
        // std::string content( (std::istreambuf_iterator<char>(ifs)),
        //                      (std::istreambuf_iterator<char>()));
        // py::object ast = parser.attr("parse")("abs_domain : interval;decl : [h:int; k:int;];bb { bb1[] }");
        // py::object ast = parser.attr("parse")(content);
        // B.run (ast);
      }
      else 
      {
        std::cerr << "File " << copts.filename << " not found\n";
        return 0;
      }
      
      
    } catch (py::error_already_set ) {
      PyErr_Print();
    }

    Crab crab_tool (B.get_cfg (), vfac, copts);
    switch (B.get_abs_dom ())
    {
      case ZONES:     crab_tool.run<crab::domain_impl::zones_domain_t> (); break;
      // case OCTAGONS: crab_tool.run<crab::domain_impl::oct_domain_t> (); break;
      // case POLYHEDRA:crab_tool.run<crab::domain_impl::pk_domain_t> (); break;
      default:       crab_tool.run<crab::domain_impl::interval_domain_t> (); 
        
    }
  }
  
  return 0;
}
