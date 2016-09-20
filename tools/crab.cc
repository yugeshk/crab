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

#include <crab/tools/config.h>

#include <boost/python.hpp>
#include <boost/python/stl_iterator.hpp>

#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>
#include <boost/property_tree/xml_parser.hpp>

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
  std::string infile;
  std::string outfile;

  unsigned widening ;
  unsigned narrowing;
  unsigned widening_thresholds;

  crabOpts ()
      : stats_enabled(false), infile(""), outfile (""),
        widening (1), narrowing(2), widening_thresholds (50) { }

  void write (std::ostream& o) const 
  {
    o << "Read user options\n";
    o << "\tFilename=" << (infile == "" ? "\"\"" : infile) << "\n";
    o << "\tEnabled stats=" << (stats_enabled? "yes" : "not") << "\n";
    o << "\tWidening=" << widening << "\n";
    o << "\tNarrowing=" << narrowing << "\n";
    o << "\tWidening thresholds=" << widening_thresholds << "\n";
  }
};

enum abs_domain_t { INTERVALS, ZONES, OCTAGONS, POLYHEDRA};

// Generic class that builds a CFG from a file.
// The file must contain at least two things:
// - a CFG in some format (custom, XML, json, etc)
// - the abstract domain to be used for analyzing the CFG
class CfgBuilder {

 protected:

  crab::cfg_impl::variable_factory_t &m_vfac;  
  crab::cfg_impl::cfg_t* m_cfg;
  abs_domain_t m_abs_domain;

  CfgBuilder (crab::cfg_impl::variable_factory_t &vfac)
      : m_vfac (vfac), m_cfg(nullptr), m_abs_domain (INTERVALS) { }

 public:

  // Name of the file that contains the CFG
  virtual void run (std::string filename) = 0;

  crab::cfg_impl::cfg_t* get_cfg () { return m_cfg;}
  abs_domain_t get_abs_dom () const { return m_abs_domain;}
};


// A class that builds a CFG using a python parser
class PyCfgBuilder: public CfgBuilder {
  
  typedef crab::cfg_impl::cfg_t cfg_t;
  typedef cfg_t::basic_block_t basic_block_t;
  typedef crab::cfg_impl::z_var z_var;
  typedef crab::domain_impl::z_lin_t z_lin_t;
  typedef crab::domain_impl::z_lin_cst_t z_lin_cst_t;

  z_var mk_z_var (boost::python::object py_var) 
  { 
    if (py_var.attr("ty") != "int") 
      CRAB_ERROR ("Sorry only supported integer variables for now");
    
    std::string vname = boost::python::extract<std::string>(py_var.attr("name"));
    return z_var (this->m_vfac [vname]);
  }

  z_lin_t mk_z_lin_exp (boost::python::object py_lin_exp) 
  { 
    auto terms = py_lin_exp.attr("terms");
    boost::python::stl_input_iterator< boost::python::object > it(terms), et;
    z_lin_t e (getNumber (py_lin_exp, "cst"));
    for(; it != et; ++it) {
      auto term = *it;
      z_number factor = getNumber (term, "factor");
      z_var var = getVar (term, "var");
      e = e + (factor * var);
    }
    return e;
  }
  
  z_lin_cst_t mk_z_lin_cst (boost::python::object py_lin_cst) 
  { 
    z_lin_t e = mk_z_lin_exp (py_lin_cst.attr("exp"));
    z_number cst = getNumber(py_lin_cst, "cst");
    auto op = py_lin_cst.attr("op");
    if (op == ">=")
      return e >= cst;
    else if (op == "<=")
      return e <= cst;
    else if (op == "=")
      return e == cst;
    else if (op == "!=")
      return e != cst;
    else if (op == ">") // only because we use integers we can replace x > y with x >= y+1
      return e >= cst + z_number(1);
    else if (op == "<") // only because we use integers we can replace x < y with x+1 <= y
      return e + z_number(1) <= cst;
    else 
      CRAB_ERROR ("unrecognized comparison operator");
  }

  void add_assign (basic_block_t &bb, boost::python::object py_stmt) 
  { 
    if (py_stmt.attr("rhs").attr("__class__").attr("__name__") == "Var") {
      bb.assign (mk_z_var (py_stmt.attr("lhs")).name (), 
                 mk_z_var(py_stmt.attr("rhs")));
    }
    else {
      bb.assign (mk_z_var (py_stmt.attr("lhs")).name (), 
                 z_number(boost::python::extract<long>(py_stmt.attr("rhs"))));
    }
  }

  void add_assume (basic_block_t &bb, boost::python::object py_stmt) 
  { 
    z_lin_cst_t cst = mk_z_lin_cst(py_stmt.attr("constraint"));    
    bb.assume (cst);
  }

  void add_assert (basic_block_t &bb, boost::python::object py_stmt) 
  { 
    z_lin_cst_t cst = mk_z_lin_cst(py_stmt.attr("constraint"));    
    bb.assertion (cst);
  }

  void add_havoc (basic_block_t &bb, boost::python::object py_stmt) 
  { 
    z_var var = mk_z_var(py_stmt.attr("var"));    
    bb.havoc (var.name());
  }

  z_var getVar (boost::python::object o, const char* Att) {
    return mk_z_var (o.attr(Att));
  }

  z_number getNumber (boost::python::object o, const char* Att) {
    return z_number (boost::python::extract<long>(o.attr(Att)));
  }

  void add_binop (basic_block_t &bb, boost::python::object py_stmt) 
  { 
     z_var lhs = mk_z_var(py_stmt.attr("lhs"));
     z_var op1 = mk_z_var(py_stmt.attr("op1"));

     if (py_stmt.attr("op") == "+") {
       if (py_stmt.attr("op2").attr("__class__").attr("__name__") == "Var")
         bb.add (lhs, op1, getVar (py_stmt, "op2"));
       else 
         bb.add (lhs, op1, getNumber (py_stmt, "op2"));
     } else if (py_stmt.attr("op") == "-") {
       if (py_stmt.attr("op2").attr("__class__").attr("__name__") == "Var")
         bb.sub (lhs, op1, getVar (py_stmt, "op2"));
       else 
         bb.sub (lhs, op1, getNumber (py_stmt, "op2"));
     } else if (py_stmt.attr("op") == "*") {
       if (py_stmt.attr("op2").attr("__class__").attr("__name__") == "Var")
         bb.mul (lhs, op1, getVar (py_stmt, "op2"));
       else 
         bb.mul (lhs, op1, getNumber (py_stmt, "op2"));
     } else if (py_stmt.attr("op") == "/") {
       if (py_stmt.attr("op2").attr("__class__").attr("__name__") == "Var")
         bb.div (lhs, op1, getVar (py_stmt, "op2"));
       else 
         bb.div (lhs, op1, getNumber (py_stmt, "op2"));
     } else
       CRAB_ERROR ("unrecognized arithmetic operator");
  }
    
 public:

  PyCfgBuilder (crab::cfg_impl::variable_factory_t &vfac)
      : CfgBuilder (vfac) { }

  void run (std::string filename) 
  {
    namespace py = boost::python;
    try {
      Py_Initialize();

      boost::filesystem::path workingDir = boost::filesystem::absolute(__pythonpath__).normalize();
      PyObject* sysPath = PySys_GetObject((char*) "path");
      PyList_Insert(sysPath, 0, PyString_FromString(workingDir.string().c_str()));
      
      // load the main namespace  
      py::object main_module = py::import("__main__");
      // load the dictionary for the namespace        
      py::object main_namespace = main_module.attr("__dict__");
      // import the crabParser module into the namespace    
      py::exec("import crabParser", main_namespace); 
      // create the locally-held CrabParser object
      py::object parser  = py::eval("crabParser.CrabParser()", main_namespace);  

      std::ifstream ifs(filename);
      if (ifs)
      {
        std::string content( (std::istreambuf_iterator<char>(ifs)), (std::istreambuf_iterator<char>()));
        parser.attr("parse")(content);

        // Get abstract domain
        std::string abs_dom = py::extract<std::string>(parser.attr("getAbsDom")());
        if (abs_dom == "intervals")      m_abs_domain = INTERVALS;
        else if (abs_dom == "zones")     m_abs_domain = ZONES;
        else if (abs_dom == "octagons")  m_abs_domain = OCTAGONS;          
        else if (abs_dom == "polyhedra") m_abs_domain = POLYHEDRA;
        else CRAB_ERROR ("Abstract domain " + abs_dom + " not recognized.\nTry intervals, zones, octagons or polyhedra"); 

        // Get blocks
        py::object bbs = parser.attr("getBasicBlocks")();
        std::vector<std::string> bb_names;
        py::stl_input_iterator< py::object > bit(bbs), bet;
        for(; bit != bet; ++bit) {
          std::string bb_name = py::extract<std::string>(*bit);
          bb_names.push_back (bb_name);
        }

        if (bb_names.empty ()) return;
        
        // Create CFG and set the entry block
        // XXX: We need to know the entry block of the CFG
        // For now, we assume the first block is the entry!
        this->m_cfg = new cfg_t(bb_names[0]);
        // Add blocks
        for (auto bb_name: bb_names) {
          auto &bb = this->m_cfg->insert (bb_name);
          py::object insts = parser.attr("getInstructions")(bb_name);
          py::stl_input_iterator< py::object > iit(insts), iet;
          for(; iit != iet; ++iit) {
            py::object py_inst = *iit;
            py::object class_name = py_inst.attr("__class__").attr("__name__");            
            if (class_name == "BinaryOperation") add_binop (bb, py_inst);
            else if (class_name == "Assign") add_assign (bb, py_inst); 
            else if (class_name == "Assert") add_assert (bb, py_inst);
            else if (class_name == "Assume") add_assume (bb, py_inst);
            else if (class_name == "Havoc")  add_havoc (bb, py_inst);
            else CRAB_ERROR("unrecognized python instruction");
          }
        }

        // Add edges        
        py::object edges = parser.attr("getEdges")();
        py::stl_input_iterator< py::object > eit(edges), eet;
        for(; eit != eet; ++eit) {
          py::object tuple = *eit;
          std::string src_str = py::extract<std::string>(tuple[0]);
          std::string dst_str = py::extract<std::string>(tuple[1]);
          auto &src = m_cfg->get_node (src_str);
          auto &dest = m_cfg->get_node (dst_str);
          src >> dest;
        }
      }
      else 
        CRAB_ERROR ("File " + filename + " not found\n");
    } catch (py::error_already_set ) {
      PyErr_Print();
    }
  }

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
      crab::outs () << "Crab: no Cfg found\n";
      return;
    }

    // Run fixpoint 
    typename crab::analyzer::num_fwd_analyzer
    <crab::cfg_impl::cfg_ref_t,Dom,crab::cfg_impl::variable_factory_t>::type 
    a (*m_cfg, m_vfac, nullptr, m_copts.widening, m_copts.narrowing, m_copts.widening_thresholds);
    Dom inv = Dom::top ();
    crab::outs() << "Invariants using " << inv.getDomainName () << "\n";
    a.Run (inv);

    // Print invariants to standard output
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

    if (m_copts.outfile != "") {
      // Create an empty property tree object
      using boost::property_tree::ptree;
      ptree pt;
      ptree root ;      
      for (auto &b : *m_cfg) {
        auto inv = a[b.label ()];
        ptree bb_node;      
        crab::crab_string_os str_os;
        auto csts = inv.to_linear_constraint_system ();
        str_os << csts;
        bb_node.put ("name", b.label());
        // FIXME: the invariants must be enclosed in strings for xml
        bb_node.put ("invariants", str_os.str());
        root.add_child ("basic_block", bb_node);
      }
      pt.add_child("crab", root);

      boost::filesystem::path p(m_copts.outfile);      
      if (p.extension ().string() == ".json")
        write_json (m_copts.outfile, pt);
      else if (p.extension ().string () == ".xml") {
        boost::property_tree::xml_writer_settings<char> settings('\t', 1);
        write_xml(m_copts.outfile, pt, std::locale(), settings);
      } else 
        CRAB_ERROR ("output format " + p.extension ().string() + " not recognized");
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
      ("input-file", boost::program_options::value<std::string>(), "input file with CFG") ;
  po.add_options()
      ("output-file,o", boost::program_options::value<std::string>(), "output file with invariants per basic block");
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
  p.add("input-file", -1);
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
    boost::filesystem::path abspath = boost::filesystem::absolute(vm ["input-file"].as<std::string>());
    copts.infile = abspath.string();
  }

  if (vm.count("output-file")) {
    boost::filesystem::path abspath = boost::filesystem::absolute(vm ["output-file"].as<std::string>());
    copts.outfile = abspath.string();
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
  crabOpts copts; 
  if (crab_options (argc, argv, copts) > 0) {
    //copts.write (std::cout); // for debugging
    crab::cfg_impl::variable_factory_t vfac;
    PyCfgBuilder B (vfac);
    B.run (copts.infile);
    Crab crab_tool (B.get_cfg (), vfac, copts);
    crab::outs () << *(B.get_cfg ()) << "\n";
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
