//Compile/link with, e.g., 
// g++ -O2 -funroll-loops -o RandomExample RandomExample.cpp Random.cpp

#include "Random.hpp"
#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#if !WINDOWS
#include <sys/time.h>
#else
#include <windows.h>
#include <winbase.h>
#endif
#include <cassert>
#include <algorithm>

namespace {
  char rcsid[] = "$Id: RandomExample.cpp 6142 2006-04-18 19:09:13Z ckarney $";
}

using namespace std;

int foo(int) {
  return 3;
}

typedef int (Random::*RandomI)(int);

void Deal(Random& r) {
    vector<string> deck(52);
    string suits("SHDC");
    string numbers("A23456789TJQK");
    for (size_t s = 0, i = 0; s < 4; s++)
	for (size_t n = 0; n < 13; n++)
	    deck[i++] = numbers.substr(n, 1) + suits.substr(s, 1);
    random_shuffle(deck.begin(), deck.end(), r);
    cout << "Shuffling and dealing cards" << endl;
    for (size_t i = 52; i > 0;) {
	cout << deck[--i];
	if (i % 13 == 0)
	    cout << endl;
	else
	    cout << " ";
    }
}

void Example1() {
  Random r;			// r created with random seed
  cout << "Seed set to " << r.SeedString() << endl;

  cout << "Estimate pi = ";
  size_t in = 0, num = 1000000;
  for (size_t i = 0; i < num; ++i) {
    double x = r.RealS();    // r.RealS() is in the interval (-1/2, 1/2)
    double y = r.RealS();
    if (x * x + y * y < 0.25)
      ++in;			// Inside the circle
  }
  cout << double(in) * 4.0 / double(num) << endl;

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

  Deal(r);

  cout << "Estimate mean and variance of random populations" << endl;
  double m = 0;
  double s = 0;
  int k = 0;
  while (k < 1000) {
    ++k;
    double x = r.Normal();
    double m1 = m + (x - m)/k;  // Knuth, TAOCP 4.2.2
    s += (x - m) * (x - m1);
    m = m1;
  }
  cout << "Normal: mean = " << m << " variance = " << s/(k - 1) << endl;

  m = 0;
  s = 0;
  k = 0;
  while (k < 1000) {
    ++k;
    double x = r.RealC();
    double m1 = m + (x - m)/k;  // Knuth, TAOCP 4.2.2
    s += (x - m) * (x - m1);
    m = m1;
  }
  cout << "Uniform: mean = " << m << " variance = " << s/(k - 1) << endl;

  typedef float real;
  enum { prec = 4 };
  cout << "Some low precision reals (1/" << (1<<prec) << ")" << endl;
  for (size_t i = 0; i < 10; ++i)
    cout << " " << r.Real<real, prec>();
  cout << endl;

  cout << "Used " << r.Count() << " random numbers" << endl;

}

void BasicTest() {
  cout << "BasicTest" << endl;
  const size_t length = 4;
  // unsigned long init[length]={0x123, 0x234, 0x345, 0x456};
  // Random r(vector<unsigned long>(init, init+length));
  Random r("[0x123, 0x234, 0x345, 0x456]");

  cout << "Seed set to " << r.SeedString() << endl;
  r.SetCount(1000-1);
  unsigned long l = r();
  cout << r.Count() << "th random number (should be 3460025646): "
       << l << endl;
  r.SetCount(1000000-1);
  l = r();
  cout << r.Count() << "th random number (should be 572929828): "
       << l << endl;
}

void WriteReadTest() {
  Random r(0);
  r.SetCount(1000000-1);
  {
    ofstream f("tempRandom.dat", ios::binary);
    r.Save(f);			// Save to binary file
    f.close();
  }
  {
    ofstream f("tempRandom.txt");
    f << r;			// Write to text file
    f.close();
  }
  string buf;
  {
    ostringstream stream;
    r.Save(stream);		// Binary to string
    buf = stream.str();
  }

  Random r1(r);			// Copy constructor
  Random r2 = r;		// Ditto
  Random r3(0); r3 = r;		// Copy assignment
  Random r4(r.Seed()); r4.SetCount(r.Count()); // Set seed and count
  Random r5(0);
  {
    ifstream f("tempRandom.dat", ios::binary);
    r5.Restore(f);		// Restore from binary file
    f.close();
  }
  Random r6(0);
  {
    ifstream f("tempRandom.txt");
    f >> r6;			// Read from text file
    f.close();
  }
  ifstream f("tempRandom.dat", ios::binary);
  Random r7(f);			// Initialize from binary stream

  Random r8(0);
  {
    istringstream stream(buf);
    r8.Restore(stream);
  }

  cout << "Test saving and restoring random state" << endl;

  cout << "These should all be the same; seed = " << r.SeedString() << endl;
  cout << r() << " (original)" << endl
       << r1() << " (copy constructor)" << endl
       << r2() << " (alt copy constructor)" << endl
       << r3() << " (copy assigment)" << endl
       << r4() << " (seed and count)" << endl
       << r5() << " (restore from binary file)" << endl
       << r6() << " (restore from text file)" << endl
       << r7() << " (initialize from stream)" << endl
       << r8() << " (restore from string)" << endl;

  assert(
	 r == r1 &&
	 r == r2 &&
	 r == r3 &&
	 r == r4 &&
	 r == r5 &&
	 r == r6 &&
	 r == r7 &&
	 r == r8 );

  r.SetCount(1000000-1);
  unsigned long l = r();
  cout << r.Count() << "th random is " << l << endl;
  r.SetCount(2000000-1);
  l = r();
  cout << r.Count() << "th random is " << l << endl;
  r.SetCount(1000000-1);
  l = r();
  cout << r.Count() << "th random is " << l << " again" << endl;
  r.SetCount(-1000000-1);
  l = r();
  cout << r.Count() << "th random is " << l << endl;
  r.SetCount(1000000-1);
  l = r();
  cout << r.Count() << "th random is " << l << " again" << endl;

}

double HighPrecMult() {
#if WINDOWS
  LARGE_INTEGER t;
  QueryPerformanceFrequency((LARGE_INTEGER *)&t);
  return 1.0/(t.HighPart*pow(2.0, 32) + t.LowPart);
#else
  return 1.e-6;
#endif
}
long long HighPrecTime() {
#if WINDOWS
  LARGE_INTEGER t;
  QueryPerformanceCounter((LARGE_INTEGER *)&t);
  return (static_cast<long long>(t.HighPart) << 32) +
    static_cast<long long>(t.LowPart);
#else
  timeval t;
  gettimeofday(&t, NULL);
  return static_cast<long long>(t.tv_sec) * 1000000LL +
    static_cast<long long>(t.tv_usec);
#endif
}

#define TIME(expr,esttime) {						\
    long long t1, t2;							\
    t1=HighPrecTime();							\
    long long c1 = r.Count();						\
    size_t m = int(1.e9/esttime+1);					\
    for (size_t j = m; j > 0; --j) { expr; }				\
    t2=HighPrecTime();							\
    cout << setw(6) << int((t2-t1)*HighPrecMult()*1.0e9/m+0.5) << "ns "	\
	 << setprecision(4) << setw(5) << (r.Count()-c1)/float(m) << "rv" \
	 << " per " << #expr << endl;					\
  }

double Timer() {
  Random r;
  r();
  unsigned long i = 0;
  unsigned long long l = 0;
  float f = 0;
  double d = 0;
  long double e = 0;
  cout << "Timing tests" << endl;
  cout << "Seed set to " << r.SeedString() << endl;
  TIME(r.StepCount(10000) ,100000);
  TIME(r.StepCount(-10000) ,150000);
  TIME(i ^= r.Integer(), 30);
  TIME(l ^= r.Integer<unsigned long long>(), 60);
  TIME(f += r.Real<float>(), 90);
  TIME(f += r.RealU<float>(), 90);
  TIME(f += r.RealN<float>(), 90);
  TIME(f += r.RealS<float>(), 90);
  TIME(f += r.RealO<float>(), 90);
  TIME(f += r.RealC<float>(), 90);
  TIME(f += r.Float<float>(), 90);
  TIME(f += r.FloatU<float>(), 90);
  TIME(f += r.FloatN<float>(), 120);
  TIME(d += (r.Real<double, 32>()), 90);
  TIME(d += (r.RealU<double, 32>()), 90);
  TIME(d += (r.RealN<double, 32>()), 90);
  TIME(d += (r.RealS<double, 32>()), 90);
  TIME(d += (r.RealO<double, 32>()), 90);
  TIME(d += (r.RealC<double, 32>()), 120);
  TIME(d += (r.Float<double, 32, 10>()), 90);
  TIME(d += (r.FloatU<double, 32, 10>()), 90);
  TIME(d += (r.FloatN<double, 32, 10>()), 90);
  TIME(d += r.Real<double>(), 90);
  TIME(d += r.RealU<double>(), 90);
  TIME(d += r.RealN<double>(), 90);
  TIME(d += r.RealS<double>(), 90);
  TIME(d += r.RealO<double>(), 90);
  TIME(d += r.RealC<double>(), 150);
  TIME(d += r.Float<double>(), 150);
  TIME(d += r.FloatU<double>(), 150);
  TIME(d += r.FloatN<double>(), 150);
  TIME(e += r.Real<long double>(), 90);
  TIME(e += r.RealU<long double>(), 90);
  TIME(e += r.RealN<long double>(), 90);
  TIME(e += r.RealS<long double>(), 90);
  TIME(e += r.RealO<long double>(), 90);
  TIME(e += r.RealC<long double>(), 150);
  TIME(e += r.Float<long double>(), 150);
  TIME(e += r.FloatU<long double>(), 150);
  TIME(e += r.FloatN<long double>(), 150);

  TIME(d += r.Normal<float>(), 400);
  TIME(d += r.Normal<double>(), 400);
  TIME(d += r.Normal<long double>(), 400);

  return i + l + f + d + e;
}

int main() {

  cout << "Seed: " << Random::Global.SeedString() << " ";
  cout << Random::Global.Integer() << endl;
  Random::Global.Reseed();
  cout << "Seed set to " << Random::Global.SeedString() << endl;

  BasicTest();
  Example1();
  WriteReadTest();
  Timer();
  return EXIT_SUCCESS;
}
/*
103391ns 1e+04rv per r.StepCount(10000)
144096ns -1e+04rv per r.StepCount(-10000)
    25ns     1rv per i ^= r.Integer()
    59ns     2rv per l ^= r.Integer<unsigned long long>()
    47ns     1rv per f += r.Real<float>()
    51ns     1rv per f += r.RealU<float>()
    54ns     1rv per f += r.RealN<float>()
    51ns     1rv per f += r.RealS<float>()
    53ns     1rv per f += r.RealO<float>()
    52ns 1.004rv per f += r.RealC<float>()
   100ns   1.5rv per f += r.Float<float>()
   103ns   1.5rv per f += r.FloatU<float>()
   125ns     2rv per f += r.FloatN<float>()
    47ns     1rv per d += (r.Real<double, 32>())
    51ns     1rv per d += (r.RealU<double, 32>())
    53ns     1rv per d += (r.RealN<double, 32>())
    51ns     1rv per d += (r.RealS<double, 32>())
    55ns     1rv per d += (r.RealO<double, 32>())
   116ns     3rv per d += (r.RealC<double, 32>())
    91ns   1.5rv per d += (r.Float<double, 32, 10>())
    95ns   1.5rv per d += (r.FloatU<double, 32, 10>())
   113ns     2rv per d += (r.FloatN<double, 32, 10>())
    88ns     2rv per d += r.Real<double>()
    92ns     2rv per d += r.RealU<double>()
    96ns     2rv per d += r.RealN<double>()
    91ns     2rv per d += r.RealS<double>()
    98ns     2rv per d += r.RealO<double>()
   156ns     4rv per d += r.RealC<double>()
   134ns   2.5rv per d += r.Float<double>()
   137ns   2.5rv per d += r.FloatU<double>()
   160ns     3rv per d += r.FloatN<double>()
    90ns     2rv per e += r.Real<long double>()
    92ns     2rv per e += r.RealU<long double>()
   106ns     2rv per e += r.RealN<long double>()
    96ns     2rv per e += r.RealS<long double>()
    99ns     2rv per e += r.RealO<long double>()
   164ns 4.001rv per e += r.RealC<long double>()
   145ns   2.5rv per e += r.Float<long double>()
   148ns   2.5rv per e += r.FloatU<long double>()
   175ns     3rv per e += r.FloatN<long double>()
   267ns 2.735rv per d += r.Normal<float>()
   378ns 5.476rv per d += r.Normal<double>()
   416ns 5.475rv per d += r.Normal<long double>()
*/
