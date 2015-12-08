/*******************************************************************************
 *
 * Reduced product of a numerical domain and congruences.
 *
 * Author: Jorge A. Navas Laserna (jorge.a.navaslaserna@nasa.gov)
 *
 * The reduce operator based on "Static Analysis of Arithmetical
 * Congruences" by P. Granger published in International Journal of
 * Computer Mathematics, 1989.
 *
 * Notices:
 *
 * Copyright (c) 2011 United States Government as represented by the
 * Administrator of the National Aeronautics and Space Administration.
 * All Rights Reserved.
 *
 * Disclaimers:
 *
 * No Warranty: THE SUBJECT SOFTWARE IS PROVIDED "AS IS" WITHOUT ANY WARRANTY OF
 * ANY KIND, EITHER EXPRESSED, IMPLIED, OR STATUTORY, INCLUDING, BUT NOT LIMITED
 * TO, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL CONFORM TO SPECIFICATIONS,
 * ANY IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE,
 * OR FREEDOM FROM INFRINGEMENT, ANY WARRANTY THAT THE SUBJECT SOFTWARE WILL BE
 * ERROR FREE, OR ANY WARRANTY THAT DOCUMENTATION, IF PROVIDED, WILL CONFORM TO
 * THE SUBJECT SOFTWARE. THIS AGREEMENT DOES NOT, IN ANY MANNER, CONSTITUTE AN
 * ENDORSEMENT BY GOVERNMENT AGENCY OR ANY PRIOR RECIPIENT OF ANY RESULTS,
 * RESULTING DESIGNS, HARDWARE, SOFTWARE PRODUCTS OR ANY OTHER APPLICATIONS
 * RESULTING FROM USE OF THE SUBJECT SOFTWARE.  FURTHER, GOVERNMENT AGENCY
 * DISCLAIMS ALL WARRANTIES AND LIABILITIES REGARDING THIRD-PARTY SOFTWARE,
 * IF PRESENT IN THE ORIGINAL SOFTWARE, AND DISTRIBUTES IT "AS IS."
 *
 * Waiver and Indemnity:  RECIPIENT AGREES TO WAIVE ANY AND ALL CLAIMS AGAINST
 * THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS, AS WELL
 * AS ANY PRIOR RECIPIENT.  IF RECIPIENT'S USE OF THE SUBJECT SOFTWARE RESULTS
 * IN ANY LIABILITIES, DEMANDS, DAMAGES, EXPENSES OR LOSSES ARISING FROM SUCH
 * USE, INCLUDING ANY DAMAGES FROM PRODUCTS BASED ON, OR RESULTING FROM,
 * RECIPIENT'S USE OF THE SUBJECT SOFTWARE, RECIPIENT SHALL INDEMNIFY AND HOLD
 * HARMLESS THE UNITED STATES GOVERNMENT, ITS CONTRACTORS AND SUBCONTRACTORS,
 * AS WELL AS ANY PRIOR RECIPIENT, TO THE EXTENT PERMITTED BY LAW.
 * RECIPIENT'S SOLE REMEDY FOR ANY SUCH MATTER SHALL BE THE IMMEDIATE,
 * UNILATERAL TERMINATION OF THIS AGREEMENT.
 *
 ******************************************************************************/

#ifndef NUMERICAL_WITH_CONGRUENCES_HPP
#define NUMERICAL_WITH_CONGRUENCES_HPP

#include <crab/common/types.hpp>
#include <crab/domains/numerical_domains_api.hpp>
#include <crab/domains/bitwise_operators_api.hpp>
#include <crab/domains/division_operators_api.hpp>
#include <crab/domains/domain_products.hpp>
#include <crab/domains/congruences.hpp>
#include <crab/domains/intervals.hpp>

namespace crab {

   namespace domains {

     template < typename Number, int typeSize = -1 >
     class interval_congruence : public writeable {
      public:
       typedef interval_congruence< Number, typeSize > interval_congruence_t;
       
      private:
       typedef interval< Number > interval_t;
       typedef bound< Number > bound_t;
       typedef congruence< Number, typeSize > congruence_t;
       
      private:
       interval_t _first;
       congruence_t _second;
       
      private:
       interval_congruence(bool is_bottom)
           : writeable(),
             _first(is_bottom ? interval_t::bottom() : interval_t::top()),
             _second(is_bottom ? congruence_t::bottom() : congruence_t::top()) {}
       
      public:
       static interval_congruence_t top() { return interval_congruence(false); }
       
       static interval_congruence_t bottom() { return interval_congruence(true); }
       
      private:
       inline Number abs(Number x) { return x < 0 ? -x : x; }
       
       // operator % can return a negative number
       // mod(a, b) always returns a positive number
       inline Number mod(Number a, Number b) {
         Number m = a % b;
         if (m < 0)
           return m + b;
         else
           return m;
       }
       
       // R(c,a) is the least element of c greater or equal than a
       inline Number R(congruence_t c, Number a) {
         Number m = c.get().first;
         Number p = c.get().second;
         return a + mod(p - a, abs(m));
       }

       // L(c,a) is the greatest element of c smaller or equal than a
       inline Number L(congruence_t c, Number a) {
         Number m = c.get().first;
         Number p = c.get().second;
         return a - mod(a - p, abs(m));
       }
       
      public:
       interval_congruence(Number n)
           : _first(interval_t(n)), _second(congruence_t(n)) {}
       
       interval_congruence(interval_t i, congruence_t c) : 
           _first(i), _second(c) {
         this->reduce();
       }
       
       interval_congruence(interval_t i) : 
           _first(i), _second(congruence_t::top()) {
         this->reduce();
       }
       
       interval_congruence(congruence_t c) : 
           _first(interval_t::top()), _second(c) {
         this->reduce();
       }
       
       interval_congruence(const interval_congruence& other)
           : _first(other._first), _second(other._second) {}
       
       interval_congruence_t& operator=(interval_congruence_t other) {
         this->_first = other._first;
         this->_second = other._second;
         return *this;
       }
       
       bool is_bottom() {
         return this->_first.is_bottom() || this->_second.is_bottom();
       }
       
       bool is_top() { return this->_first.is_top() && this->_second.is_top(); }
       
       interval_t& first() { return this->_first; }
       
       congruence_t& second() { return this->_second; }
       
       /*

         Let (i,c) be a pair of interval and congruence these are the
         main rules described by Granger:

         if (c.is_bottom() || i.is_bottom()) (bottom(), bottom());
         if (c = 0Z+a and a \notin i)        (bottom(), bottom());
         if (c = 0Z+a)                       ([a,a]   , c);
         if (i=[a,b] and R(c,a) > L(c,b))    (bottom(), bottom());
         if (i=[a,b])                        ([R(c,a), L(c,b)], c);
         if (i=[a,+oo])                      ([R(c,a), +oo], c);
         if (i=[-oo,b])                      ([-oo, L(c,b)], c);
         otherwise                           (i,c)
       */
       
       void reduce() {
         interval_t& i = first();
         congruence_t& c = second();
         
         if (i.is_bottom() || c.is_bottom()) {
           i = interval_t::bottom();
           c = congruence_t::bottom();
         }
         
         // congruence is top and interval is a singleton
         if (c.is_top()) {
           boost::optional< Number > n = i.singleton();
           if (n) {
             c = congruence_t(*n);
           }
           return;
         }
         
         Number modulo = c.get().first;
         if (modulo == 0) {
           // congruence is a singleton so we refine the interval
           interval_t a(c.get().second);
           if (!(a <= i)) {
             i = interval_t::bottom();
             c = congruence_t::bottom();
           } else {
             i = a;
           }
         } else {
           // refine lower and upper bounds of the interval using
           // congruences
           bound_t lb = i.lb();
           bound_t ub = i.ub();
           
           if (lb.is_finite() && ub.is_finite()) {
             Number x = R(c, *(lb.number()));
             Number y = L(c, *(ub.number()));
             if (x > y) {
               i = interval_t::bottom();
               c = congruence_t::bottom();
             } else if (x == y) {
               i = interval_t(x);
               c = congruence_t(x);
             } else {
               i = interval_t(bound_t(x), bound_t(y));
             }
           } else if (lb.is_finite()) {
             Number x = R(c, *(lb.number()));
             i = interval_t(bound_t(x), bound_t::plus_infinity());
           } else if (ub.is_finite()) {
             Number y = L(c, *(ub.number()));
             i = interval_t(bound_t::minus_infinity(), bound_t(y));
           } else {
             // interval is top
           }
         }
       }
       
       void write(std::ostream& o) {
         o << "(" << this->_first << ", " << this->_second << ")";
       }
       
      public:
       interval_congruence_t operator+(interval_congruence_t x) {
         return interval_congruence_t(this->_first.operator+(x.first()),
                                      this->_second.operator+(x.second()));
       }
       
       interval_congruence_t operator-(interval_congruence_t x) {
         return interval_congruence_t(this->_first.operator-(x.first()),
                                      this->_second.operator-(x.second()));
       }
       
       interval_congruence_t operator*(interval_congruence_t x) {
         return interval_congruence_t(this->_first.operator*(x.first()),
                                      this->_second.operator*(x.second()));
       }
       
       interval_congruence_t operator/(interval_congruence_t x) {
         return interval_congruence_t(this->_first.operator/(x.first()),
                                      this->_second.operator/(x.second()));
       }
       
       interval_congruence_t operator|(interval_congruence_t other) {
         return interval_congruence_t(this->_first | other._first,
                                      this->_second | other._second);
       }
       
       interval_congruence_t operator&(interval_congruence_t other) {
         return interval_congruence_t(this->_first & other._first,
                                      this->_second & other._second);
       }

      public:
       // division and remainder operations
       
       interval_congruence_t SDiv(interval_congruence_t x) {
         return interval_congruence_t(this->_first.SDiv(x.first()),
                                      this->_second.SDiv(x.second()));
       }
       
       interval_congruence_t UDiv(interval_congruence_t x) {
         return interval_congruence_t(this->_first.UDiv(x.first()),
                                      this->_second.UDiv(x.second()));
       }
       
       interval_congruence_t SRem(interval_congruence_t x) {
         return interval_congruence_t(this->_first.SRem(x.first()),
                                      this->_second.SRem(x.second()));
       }
       
       interval_congruence_t URem(interval_congruence_t x) {
         return interval_congruence_t(this->_first.URem(x.first()),
                                      this->_second.URem(x.second()));
       }
       
       // bitwise operations
       
       interval_congruence_t Trunc(unsigned width) {
         return interval_congruence_t(this->_first.Trunc(width),
                                      this->_second.Trunc(width));
       }
       
       interval_congruence_t ZExt(unsigned width) {
         return interval_congruence_t(this->_first.ZExt(width),
                                      this->_second.ZExt(width));
       }
       
       interval_congruence_t SExt(unsigned width) {
         return interval_congruence_t(this->_first.SExt(width),
                                      this->_second.SExt(width));
       }
       
       interval_congruence_t And(interval_congruence_t x) {
         return interval_congruence_t(this->_first.And(x.first()),
                                      this->_second.And(x.second()));
       }
       
       interval_congruence_t Or(interval_congruence_t x) {
         return interval_congruence_t(this->_first.Or(x.first()),
                                      this->_second.Or(x.second()));
       }
       
       interval_congruence_t Xor(interval_congruence_t x) {
         return interval_congruence_t(this->_first.Xor(x.first()),
                                      this->_second.Xor(x.second()));
       }
       
       interval_congruence_t Shl(interval_congruence_t x) {
         return interval_congruence_t(this->_first.Shl(x.first()),
                                      this->_second.Shl(x.second()));
       }
       
       interval_congruence_t LShr(interval_congruence_t x) {
         return interval_congruence_t(this->_first.LShr(x.first()),
                                      this->_second.LShr(x.second()));
       }
       
       interval_congruence_t AShr(interval_congruence_t x) {
         return interval_congruence_t(this->_first.AShr(x.first()),
                                      this->_second.AShr(x.second()));
       }
     };


     // Reduced product of a numerical domain with congruences.
     // It assumes that all variables have the same bitwdith which is
     // not realistic.
     template < typename NumAbsDom, int typeSize=-1 >
     class numerical_congruence_domain:
         public writeable,
         public numerical_domain<typename NumAbsDom::number_t, 
                                 typename NumAbsDom::varname_t>,
         public bitwise_operators<typename NumAbsDom::number_t, 
                                  typename NumAbsDom::varname_t>,
         public division_operators<typename NumAbsDom::number_t,
                                   typename NumAbsDom::varname_t> {
      private:
       typedef typename NumAbsDom::number_t N;
       typedef typename NumAbsDom::varname_t V;

      public:
       typedef numerical_congruence_domain< NumAbsDom, typeSize > rnc_domain_t;
       using typename numerical_domain< N, V>::linear_expression_t;
       using typename numerical_domain< N, V>::linear_constraint_t;
       using typename numerical_domain< N, V>::linear_constraint_system_t;
       using typename numerical_domain< N, V>::variable_t;
       using typename numerical_domain< N, V>::number_t;
       using typename numerical_domain< N, V>::varname_t;

       typedef congruence_domain<number_t, varname_t, typeSize> congruence_domain_t;
       typedef interval_congruence<number_t, typeSize> interval_congruence_t;
       typedef interval<number_t> interval_t;
              
      private:
       typedef patricia_tree_set<variable_t > variable_set_t;
       typedef numerical_domain_product2<number_t, varname_t, 
                                         NumAbsDom, congruence_domain_t > domain_product2_t; 

       domain_product2_t _product;
       
       numerical_congruence_domain(const domain_product2_t& product):
           _product(product) {}

       void reduce_variable(const varname_t& v) {
         if (is_bottom())
           return;

         auto i = this->_product.first()[v]; // project on intervals
         auto c = this->_product.second()[v];
         interval_congruence_t val(i, c);

         if (val.is_bottom()) {
           *this = bottom();
         } else {
           if (val.first() != i) { 
             // FIXME: this is imprecise for relational domains
             this->_product.first().set(v, val.first());
           }
           
           if (val.second() != c) 
             this->_product.second().set(v, val.second());
         }
       }

       void reduce_variables(variable_set_t variables) {
         for (typename variable_set_t::iterator it = variables.begin();
              !is_bottom() && it != variables.end();
              ++it) {
           this->reduce_variable((*it).name());
         }
       }
       
      public:
       
       static rnc_domain_t top() {
         return rnc_domain_t (domain_product2_t::top());
       }
       
       static rnc_domain_t bottom() {
         return rnc_domain_t(domain_product2_t::bottom());
       }
       
      public:
       
       numerical_congruence_domain() : 
           writeable(),
           numerical_domain<number_t,varname_t>(),
           bitwise_operators<number_t,varname_t>(),
           division_operators<number_t,varname_t>(),
           _product() {}
       
       numerical_congruence_domain(const rnc_domain_t& other) :
           writeable(),
           numerical_domain<number_t,varname_t>(),
           bitwise_operators<number_t,varname_t>(),
           division_operators<number_t,varname_t>(),
           _product(other._product) { }
       
       rnc_domain_t& operator=(const rnc_domain_t& other) {
         if (this != &other)
           this->_product = other._product;
         
         return *this;
       }

       bool is_bottom() { return this->_product.is_bottom(); }
       
       bool is_top() { return this->_product.is_top(); }
       
       NumAbsDom& first() { return this->_product.first(); }

       congruence_domain_t& second() { return this->_product.second(); }

       bool operator<=(rnc_domain_t other) {
         return this->_product <= other._product;
       }
       
       bool operator==(rnc_domain_t other) {
         return this->_product == other._product;
       }

       rnc_domain_t operator|(rnc_domain_t other) {
         return rnc_domain_t(this->_product | other._product);
       }
       
       rnc_domain_t operator&(rnc_domain_t other) {
         return rnc_domain_t(this->_product & other._product);
       }

       rnc_domain_t operator||(rnc_domain_t other) {
         return rnc_domain_t(this->_product || other._product);
       }
       
       template<typename Thresholds>
       rnc_domain_t widening_thresholds (rnc_domain_t other,
                                         const Thresholds& ts) {
         return rnc_domain_t(this->_product.widening_thresholds (other._product, ts));
       }
       
       rnc_domain_t operator&&(rnc_domain_t other) {
         return rnc_domain_t(this->_product && other._product);
       }

       // pre: x is already reduced
       void set (varname_t v, interval_congruence_t x) {
         this->_product.first().set(v, x.first());
         this->_product.second().set(v, x.second());
       }
       
       interval_congruence_t get(varname_t v) {
         return interval_congruence_t(this->_product.first()[v],
                                      this->_product.second()[v]);
       }
       
       interval_t operator[](varname_t v) {
         interval_congruence_t x = get (v);
         return x.first ();
       }
       
       void operator+=(linear_constraint_system_t csts) {
         this->_product += csts;
         this->reduce_variables(csts.variables());
       }

       void operator-=(varname_t v) { this->_product -= v; }


       void assign(varname_t x, linear_expression_t e) {
         this->_product.assign(x, e);
         this->reduce_variable(x);
       }

       void apply(operation_t op, varname_t x, varname_t y, varname_t z) {
         this->_product.apply(op, x, y, z);
         this->reduce_variable(x);
       }
       
       void apply(operation_t op, varname_t x, varname_t y, number_t k) {
         this->_product.apply(op, x, y, k);
         this->reduce_variable(x);
       }

       // bitwise_operators_api
       
       void apply(conv_operation_t op, varname_t x, varname_t y, unsigned width) {
         this->_product.apply(op, x, y, width);
         this->reduce_variable(x);
       }
       
       void apply(conv_operation_t op, varname_t x, number_t k, unsigned width) {
         this->_product.apply(op, x, k, width);
         this->reduce_variable(x);
       }
       
       void apply(bitwise_operation_t op, varname_t x, varname_t y, varname_t z) {
         this->_product.apply(op, x, y, z);
         this->reduce_variable(x);
       }
       
       void apply(bitwise_operation_t op, varname_t x, varname_t y, number_t k) {
         this->_product.apply(op, x, y, k);
         this->reduce_variable(x);
       }
       
       // division_operators_api
       
       void apply(div_operation_t op, varname_t x, varname_t y, varname_t z) {
         this->_product.apply(op, x, y, z);
         this->reduce_variable(x);
       }
       
       void apply(div_operation_t op, varname_t x, varname_t y, number_t k) {
         this->_product.apply(op, x, y, k);
         this->reduce_variable(x);
       }
       
       void write(std::ostream& o) { 
         this->_product.write(o); 
       }
       
       linear_constraint_system_t to_linear_constraint_system() {
         return this->_product.first().to_linear_constraint_system();
       }
       
       static const char* getDomainName() { 
         return domain_product2_t::getDomainName (); 
       }
       
     }; // class numerical_congruence_domain
   
   } // namespace domains
} // namespace crab

#endif 
