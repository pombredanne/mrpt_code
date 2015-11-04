/*******************************************************
 * Random testing
 * Ville Hyvönen
 * HIIT
 * ville.o.hyvonen<at>helsinki.fi 
 * 22.10.2015
 ********************************************************/


#include <RcppArmadillo.h>
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]


struct Jee {
  int moi;
  int oi;
  
  Jee(int m=0, int o=0) : moi(m), oi(o) {}
  
  ~Jee() {
    std::cout << "Destroying, moi = " << moi << std::endl;
  }
};

class Simple {
private:
  int m_nID;
  
public:
  Simple(int nID): m_nID(nID) {}
  
  void setID(int nID) { m_nID = nID; }
  int getID() { return m_nID; }
  
};

class Calc {
private:
  int m_value;
  
public:
  Calc() { m_value = 0; }
  
  Calc& Add(int x) { m_value += x; return *this; }
  Calc& Sub(int x) { m_value -= x; return *this; }
  Calc& Mult(int x) { m_value *= x; return *this; }
  
  int GetValue() { return m_value; }

};

// [[Rcpp::export]]
void struct_test() {
  Jee eka(1, 14);
  Jee toka(10);
  Jee kolmas;
  Jee* neljas = new Jee(100, 12);
  
  std::cout << "moi: " << eka.moi << ", oi: " << eka.oi << std::endl;
  std::cout << "toka.moi: " << toka.moi << ", toka.oi: " << toka.oi << std::endl;
  std::cout << "kolmas.moi: " << kolmas.moi << ", kolmas.oi: " << kolmas.oi << std::endl;
  std::cout << "neljas->moi: " << neljas->moi << ", neljas.oi: " << neljas->oi << std::endl;

  delete neljas;
  neljas = 0;
  
  std::cout << "The end." << std::endl;

  Simple cSimple(1);
  cSimple.setID(2);
  std::cout << "cSimple.getID(): " << cSimple.getID() << std::endl;
  
  Calc calc;
  calc.Add(5).Sub(3).Mult(4);
  
  std::cout << "calc.GetValue(): " << calc.GetValue() << std::endl;
}


void asort_test(arma::uvec& x) {
  std::sort(x.begin(), x.end());
  x[1] = 2000;
}

void rsort_test(IntegerVector x) {
  std::sort(x.begin(), x.end());
}


// [[Rcpp::export]]
void arma_test(arma::uvec x) {
  std::cout << "x:\n" << x << std::endl;
  asort_test(x);
  x[4] = 30000;
  std::cout << "x:\n" << x << std::endl;
}

// [[Rcpp::export]]
void rcpp_test(IntegerVector x) {
  std::cout << "x: " << x << std::endl;
  // rsort_test(x);
  x[2] = 1000;
  std::cout << "x: " << x << std::endl;
}

// [[Rcpp::export]]
void arma_ref_test(arma::uvec& x) {
  std::cout << "x:\n" << x << std::endl;
  asort_test(x);
  x[3] = 5000;
  std::cout << "x:\n" << x << std::endl;
}


// [[Rcpp::export]]
void arma_test_mat(NumericMatrix X) {
  X(0,0) = 1000;
}

// [[Rcpp::export]]
double test_nm( Rcpp::NumericMatrix X ) {
  X(0,0) = 1000;
  return 0.0 ;
}

// [[Rcpp::export]]
double test_arma( arma::mat X ) {
  X(0,0) = 1000;
  return 0.0 ;
}

// [[Rcpp::export]]
double test_const_arma( arma::mat& X ) { 
  X(0,0) = 1000;
  return 0.0 ;
}

class TrackingClass {
private:
  int value;
  
public:
  TrackingClass() {
    std::cout << "Default constructor." << std::endl;
  }
  
  TrackingClass(int _x) : value(_x) {
    std::cout << "Constructor, val: " << value << std::endl;
  }
  
  TrackingClass(const TrackingClass& cc) : value(cc.value) {
    std::cout << "Copy constructor, val: " << value << std::endl;
  }
  
  ~TrackingClass() {
    std::cout << "Destructor." << std::endl;
  }
  
  int getValue() const { return value; }
  void setValue(int x) { value = x; }
  
};

TrackingClass fibonacci(int n) {
  TrackingClass t;
  if(n < 2) {
    t.setValue(1);
  } else {
    int a = 1, b = 1;
    for(int i = 2; i < n; i++) {
      int c = a + b;
      a = b;
      b = c;
    }
    t.setValue(b);
  }  

    return t;
}

TrackingClass increment(TrackingClass t) {
  return TrackingClass(t.getValue() + 1);
}

TrackingClass incrementRef(const TrackingClass& t) {
  return TrackingClass(t.getValue() + 1);
}

// [[Rcpp::export]]
void test_tracking_class(int n) {
  TrackingClass t = fibonacci(10);
  std::cout << "pre-increment (by ref): fibonacci(" << n << "): " << t.getValue() <<  std::endl;
  incrementRef(t);
  std::cout << "post-increment: fibonacci(" << n << "): " << t.getValue() <<  std::endl;
}


// [[Rcpp::export]]
void safe(const arma::mat& X, const arma::rowvec& z) {
  int dim = X.n_cols;
  for(int i = 0; i < dim; i++)
    arma::vec x = X.col(i);
}


// [[Rcpp::export]]
void unsafe(const arma::mat& X, const arma::rowvec& z) {
  int dim = X.n_cols;
  for(int i = 0; i < dim; i++)
    arma::vec x = X.unsafe_col(i);
}

// [[Rcpp::export]]
void sizes() {
  using namespace std;
  cout << "bool:\t\t" << sizeof(bool) << " bytes" << endl;
  cout << "char:\t\t" << sizeof(char) << " bytes" << endl;
  cout << "wchar_t:\t" << sizeof(wchar_t) << " bytes" << endl;
  cout << "char16_t:\t" << sizeof(char16_t) << " bytes" << endl; // C++11, may not be supported by your compiler
  cout << "char32_t:\t" << sizeof(char32_t) << " bytes" << endl; // C++11, may not be supported by your compiler
  cout << "short:\t\t" << sizeof(short) << " bytes" << endl;
  cout << "int:\t\t" << sizeof(int) << " bytes" << endl;
  cout << "long:\t\t" << sizeof(long) << " bytes" << endl;
  cout << "long long:\t" << sizeof(long long) << " bytes" << endl; // C++11, may not be supported by your compiler
  cout << "float:\t\t" << sizeof(float) << " bytes" << endl;
  cout << "double:\t\t" << sizeof(double) << " bytes" << endl;
  cout << "long double:\t" << sizeof(long double) << " bytes" << endl;
}


// [[Rcpp::export]]
void sort_col(arma::uvec col) {
  arma::uvec ordered = sort_index(col);  
}

// [[Rcpp::export]]
void sort_row(arma::urowvec row) {
  arma::uvec ordered = sort_index(row);  
}

// [[Rcpp::export]]
void randn(int rows, int cols) {
  arma::mat A = arma::randn(rows, cols);
}

// [[Rcpp::export]]
void do_nothing(int rows, int cols) {
  ;
}
