#include <Rcpp.h>
using namespace Rcpp;

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