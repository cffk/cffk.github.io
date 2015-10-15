// Compile/link with, e.g., 
// g++ -O2 -o RandomExample RandomExample.cpp Random.cpp

#include "Random.hpp"
#include <iostream>

namespace {
  char rcsid[] = "$Id: RandomShort.cpp 6090 2005-12-09 17:09:46Z ckarney $";
}

using namespace std;

int main() {
  Random r;			// r created with random seed
  cout << "Seed set to " << r.SeedString() << endl;

  cout << "Estimate pi = ";
  size_t in = 0, num = 1000;
  for (size_t i = 0; i < num; ++i) {
    const double x = r.RealS();    // r.RealS() is in the interval (-1/2, 1/2)
    const double y = r.RealS();
    if (x * x + y * y < 0.25)
      ++in;			// Inside the circle
  }
  cout << (4.0 * in) / num << endl;

  cout << "Tossing a coin 20 times";
  for (size_t i = 0; i < 20; ++i)
    cout << " " << (r.Boolean() ? "H" : "T");
  cout << endl;

  cout << "Throwing a pair of dice 15 times";
  for (size_t i = 0; i < 15; ++i)
    cout << " " << r.Integer(6) + r.Integer(6) + 2;
  cout << endl;

  cout << "Draw 5 balls from urn containing 5 red and 5 white balls";
  int t = 10, w = 5;
  while (t > 5)
    cout << " " << (r.Prob(w, t--) ? w--, "W" : "R");
  cout << endl;

  cout << "Used " << r.Count() << " random numbers" << endl;
}
