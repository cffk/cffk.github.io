#include <iostream>
#include <iomanip>
#include <limits>
#include <fstream>
#include <time.h>
#include <sstream>
#include <vector>
#include <string>
#if !WINDOWS
#include <sys/time.h>
#else
#include <windows.h>
#include <winbase.h>
#endif
#include <cassert>
#include <algorithm>
#include "RandomNumber.hpp"
#include "NormalDistribution.hpp"
#include "ExponentialDistribution.hpp"
#include "RandomSelect.hpp"

namespace {
  char rcsid[] = "$Id: RandomNew.cpp 6147 2006-04-27 18:03:36Z ckarney $";
  char* h_rcsid[] = {
    RCSID_NORMALDISTRIBUTION_H,
    RCSID_EXPONENTIALDISTRIBUTION_H,
    RCSID_RANDOMSELECT_H,
  };
}

using namespace std;

void Example1() {
  RandomNumber r;			// r created with random seed
  cout << "Seed set to " << r.SeedString() << endl;

  cout << "Estimate pi = ";
  double x, y;
  size_t in = 0, num = 10000;
  for (size_t i = 0; i < num; ++i) {
    x = r.RealS();	     // r.RealS() is in the interval [-1/2, 1/2]
    y = r.RealS();
    if (x * x + y * y < 0.25)
      ++in;
  }
  cout << double(in) * 4.0 / double(num) << endl;

  cout << "Tossing a coin 10 times";
  for (size_t i = 0; i < 10; ++i)
    cout << " " << (r.Boolean() ? "H" : "T");
  cout << endl;

  cout << "Throwing a pair of dice 10 times";
  for (size_t i = 0; i < 10; ++i)
    cout << " " << r.Integer(6) + r.Integer(6) + 2;
  cout << endl;

  cout << "Draw 5 balls from urn containing 5 red and 5 white balls";
  int t = 10, w = 5;
  while (t > 5)
    if (r.Prob(w, t--)) {
      w--;
      cout << " W";
    } else
      cout << " R";
  cout << endl;

  cout << "Used " << r.Count() << " random numbers" << endl;

}

void Deal(RandomNumber& r) {
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
void Toss(RandomNumber& r) {
    const size_t count = 240;
    cout << "Tossing a coin " <<count <<" times:"<<endl;
    size_t h=0;
    for (size_t i = 0; i < count;) {
#if 1
	cout << (r.Boolean() ? h++, "H" : "T");
#else
	bool b=r.Boolean();
	if (b)
	  h++;
	cout << (b ? "H" : "T");
#endif
	if (++i % 60 == 0)
	    cout << endl;
    }
    cout << "Heads: " << int(h) << ", tails: " << int(count - h) << endl;
}
/*

void PrintRans(size_t n) {
  using std::cout; using std::endl; using std::setw;
  RandomNumber r(-1);
  for (size_t i = 0; i < n; ++i)
    cout << setw(11) << r.Integer<size_t>() << endl;
  return;
}
*/
#if WINDOWS
#define fn "foo.bin"
#else
#define fn "/tmp/foo.bin"
#endif

void WriteReadTest() {
  using std::cout; using std::endl; using std::numeric_limits;
  using std::string;
  RandomNumber r;
  r.Reseed(10);
  r.StepCount(3000);
  unsigned long x = r();
  r.Reseed(10);
  {
    std::ofstream f(fn, std::ios::binary | std::ios::out);
    r.Save(f);
    f.close();
  }
  r.Reseed(0); r();
  {
    std::ifstream f(fn, std::ios::binary | std::ios::in);
    r.Restore(f);
    f.close();
  }
  r.StepCount(1000);
  {
    std::ofstream f(fn, std::ios::binary | std::ios::out);
    r.Save(f);
    f.close();
  }
  r.Reseed(0); r();
  {
    std::ifstream f(fn, std::ios::binary | std::ios::in);
    r.Restore(f);
    f.close();
  }
  r.StepCount(1000);
  string buf;
  {
    std::ostringstream stream;
    r.Save(stream);
    buf = stream.str();
  }
  r.Reseed(0); r();
  {
    std::istringstream stream(buf);
    r.Restore(stream);
  }
  r.StepCount(1000);
  assert(x == r());
  {
    RandomNumber s(123);
    for (size_t k = 0; k < 100; ++k) {
      s.StepCount(624);
      string buf;
      {
	std::ostringstream stream;
	s.Save(stream);
	buf = stream.str();
      }
      s.Reseed(0);
      s();
      {
	std::istringstream stream(buf);
	s.Restore(stream);
      }
    }
    unsigned long x = s();
    s.Reseed(123);
    s.StepCount(62400);
    assert(x == s());
  }

  std::istringstream stream(buf);
  RandomNumber s(stream);
  RandomNumber h(s);
  RandomNumber j;
  j = h;
  s.StepCount(1000);
  assert(x == s());
  h.StepCount(1000);
  assert(x == h());
  j.StepCount(1000);
  assert(x == j());
  cout << "I/O test OK" << endl;
}

void BasicTest(RandomNumber& r) {
  using std::cout; using std::endl; using std::numeric_limits;
  cout << "BasicTest" << endl;
  const size_t length=4;
  unsigned long init[length]={0x123, 0x234, 0x345, 0x456};
  r.Reseed(vector<unsigned long>(init, init+length));

  cout << "Seed set to: " << r.SeedString() << endl;
  unsigned long l;
  for (size_t i = 1000; i > 0; --i)
    l = r();
  cout << r.Count() << "th random number (should be 3460025646): "
       << l << endl;
  r.StepCount(999999-1000);
  l = r();
  cout << r.Count() << "th random number (should be 572929828): "
       << l << endl;

  r.Reseed(vector<unsigned long>());
  cout << "Seed set to: " << r.SeedString() << endl;
  for (size_t i = 1000; i > 0; --i)
    l = r();
  cout << r.Count() << "th random number (should be 1341017984): "
       << l << endl;
  r.StepCount(999999-1000);
  l = r();
  cout << r.Count() << "th random number (should be 1063718465): "
       << l << endl;

  {
    const int p = 3;
    const int g = 2;
    const int nb = 10;
    vector<int> bins(1<<nb);
    const int count = 1<<22;
    for (int j = 0; j < count; j++) {
      float f = r.Float<float, p, g>();
      assert(f >= 0 && f < 1);
      int b = static_cast<int>(floor(f*(1<<nb)+0.5));
      bins[b]++;
    }
    cout << "Prec " << p << " range " << g << endl;
    for (int j = 0; j < (1<<nb); j++) {
      if (bins[j] > 0)
	cout << setw(10) << float(j)/(1<<nb)*(1<<(p+g)) << " "
	     << setw(10) << float(bins[j])/count*(1<<(p+g)) << endl;
    }
  }
  {
    const int p = 3;
    const int g = 2;
    const int nb = 10;
    vector<int> bins((1<<nb)+1);
    const int count = 1<<22;
    for (int j = 0; j < count; j++) {
      float f = r.FloatU<float, p, g>();
      assert(f > 0 && f <= 1);
      int b = static_cast<int>(floor(f*(1<<nb)+0.5));
      bins[b]++;
    }
    cout << "PrecU " << p << " range " << g << endl;
    for (int j = 0; j < (1<<nb)+1; j++) {
      if (bins[j] > 0)
	cout << setw(10) << float(j)/(1<<nb)*(1<<(p+g)) << " "
	     << setw(10) << float(bins[j])/count*(1<<(p+g)) << endl;
    }
  }
  {
    const int p = 3;
    const int g = 2;
    const int nb = 10;
    vector<int> bins((1<<nb)+1);
    const int count = 1<<22;
    for (int j = 0; j < count; j++) {
      float f = r.FloatN<float, p, g>();
      assert(f >= 0 && f <= 1);
      int b = static_cast<int>(floor(f*(1<<nb)+0.5));
      bins[b]++;
    }
    cout << "PrecM " << p << " range " << g << endl;
    for (int j = 0; j < (1<<nb)+1; j++) {
      if (bins[j] > 0)
	cout << setw(10) << float(j)/(1<<nb)*(1<<(p+g)) << " "
	     << setw(10) << float(bins[j])/count*(1<<(p+g)) << endl;
    }
  }
#if 0
  {
    const int p = 3;
    const int nb = (1<<p) + 1;
    vector<int> bd(nb);
    vector<int> bu(nb);
    vector<int> bn(nb);
    const int count = 1<<23;
    int b;
    for (int j = 0; j < count; ++j) {
      double x = max(r.RealN<double>(), r.RealN<double>());
      b = static_cast<int>(floor(x*(1<<p)+0.5));
      bn[b]++;
      b = static_cast<int>(floor(x*(1<<p)));
      bd[b]++;
      b = static_cast<int>(ceil(x*(1<<p)));
      bu[b]++;
    }
    cout << "max then round " << p << endl;
    for (int j = 0; j < nb; j++) {
      double h = 1<<p;
      h = 1/h;
      double l = max(0.0, (j-0.5)*h);
      double u = min(1.0, (j+0.5)*h);
      cout << setw(10) << j << " "
	   << setw(10) << float(bd[j])/count << " "
	   << setw(10) << float(bu[j])/count << " "
	   << setw(10) << float(bn[j])/count << " "
	   << setw(10) << u*u - l*l << endl;
    }
  }

  {
    const int p = 3;
    const int nb = (1<<p) + 1;
    vector<int> bd(nb);
    vector<int> bu(nb);
    vector<int> bn(nb);
    vector<int> bn1(nb);
    const int count = 1<<23;
    int b; double x;
    for (int j = 0; j < count; ++j) {
      x = max(r.Real<double, p>(), r.Real<double, p>());
      b = static_cast<int>(x*(1<<p));
      bd[b]++;
      x = max(r.RealN<double, p>(), r.RealN<double, p>());
      b = static_cast<int>(x*(1<<p));
      bn[b]++;
      x = max(r.RealU<double, p>(), r.RealU<double, p>());
      b = static_cast<int>(x*(1<<p));
      bu[b]++;
      x = r.Boolean() ? max(r.Real<double, p>(), r.Real<double, p>()) :
	max(r.RealU<double, p>(), r.RealU<double, p>());
      b = static_cast<int>(x*(1<<p));
      bn1[b]++;
    }
    cout << "rounded then max " << p << endl;
    for (int j = 0; j < nb; j++) {
      cout << setw(10) << j << " "
	   << setw(10) << float(bd[j])/count << " "
	   << setw(10) << float(bu[j])/count << " "
	   << setw(10) << float(bn[j])/count << " "
	   << setw(10) << float(bn1[j])/count << endl;
    }
  }
#endif
  return;
}

void StepTest() {

  {
    RandomNumber s(vector<unsigned long>(9));
    s.SetCount(-624);
    cout << "AAAAAA " << s();
    cout << " " << s() << endl;
  }
  {
    RandomNumber s(vector<unsigned long>(9));
    s();
    s.SetCount(-624);
    cout << "AAAAAA " << s();
    cout << " " << s() << endl;
  }
  unsigned long x;
  int n = 2000;
  {
    RandomNumber r(0);
    for (size_t i = n; i > 0; --i)
      r();
    x = r();
  }
  for (size_t i = 0; i < 950; ++i) {
    RandomNumber r(0);
    for (size_t j = 0; j < i; ++j)
      r();
    r.StepCount(n - i - i);
    for (size_t j = 0; j < i; ++j)
      r();
    assert(x == r());
  }
  {
    RandomNumber r(0);
    r.StepCount(n);
    assert(x == r());
  }
  {
    RandomNumber r;
    r.Reseed(123);
    for (size_t i = n; i > 0; --i)
      r();
    x = r();
  }
  for (size_t i = 0; i < 950; ++i) {
    RandomNumber r;
    r.Reseed(123);
    for (size_t j = 0; j < i; ++j)
      r();
    r.StepCount(n - i - i - 1);
    for (size_t j = 0; j < i+1; ++j)
      r();
    assert(x == r());
  }
  {
    RandomNumber r(123);
    r.StepCount(n);
    assert(x == r());
  }
  {
    RandomNumber r(123);
    r.StepCount(-1000);
    r.SetCount(n);
    assert(x == r());
  }
  {
    RandomNumber r(123);
    r.StepCount(-624);
    r.SetCount(n);
    assert(x == r());
  }
  {
    RandomNumber r(123);
    r.StepCount(1000);
    r.SetCount(n);
    assert(x == r());
  }
  {
    RandomNumber r(123);
    for (size_t i = 0; i < 100; ++i) {
      int m = r.Integer(2001)-1000;
      r.StepCount(m);
    }
    r.SetCount(n);
    assert(x == r());
  }
  cout << "StepCount test OK" << endl;
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

void Time(RandomNumber& r, int n) {

  bool b = false;
  unsigned char c = 0;
  unsigned short s = 0;
  unsigned long i = 0;
  unsigned long long l = 0;
  float f = 0;
  double d = 0;
  long double e = 0;

  r.Reseed(1);
  cout << "Seed set to: " << r.SeedString() << endl;
  r.StepCount(r.Integer(624UL));

  TIME(b ^= r.Prob<float>(.2857142857142857f),    64);
  TIME(b ^= r.Prob<long>(2,7),    41);
  TIME(b ^= r.Prob<float>(5.0f/17.0f),    65);
  TIME(b ^= r.Prob<double>(5.0/17.0),    65);
  TIME(b ^= r.Prob<long double>(5.0L/17.0L),    69);
  TIME(b ^= r.Prob<long>(5,17),    81);
  TIME(b ^= r.Prob<float>(5.0f,17.0f),    81);
  TIME(b ^= r.Prob<double>(5.0,17.0),    81);
  TIME(b ^= r.Prob<long double>(5.0l,17.0l),    81);
  TIME(b ^= r.Prob<float>(10.0f,31.0f),    81);
  TIME(b ^= r.Prob<double>(10.0,31.0),    81);
  TIME(b ^= r.Prob<long double>(10.0l,31.0l),    81);
  TIME(b ^= r.Prob<float>(1.0f,31.0f),    81);
  TIME(b ^= r.Prob<double>(1.0,31.0),    81);
  TIME(b ^= r.Prob<long double>(1.0l,31.0l),    81);
  TIME(b ^= r.Prob<float>(19.0f,31.0f),    81);
  TIME(b ^= r.Prob<double>(19.0,31.0),    81);
  TIME(b ^= r.Prob<long double>(19.0l,31.0l),    81);
  int mm = (1L << 30) / 17;
  TIME(b ^= r.Prob<long>(5*mm,17*mm),    34);
#if 0
  TIME(f += r.RealN<float>(),    53);
  TIME(f += r.RealW<float>(),    53);
  TIME(d += r.RealN(),    95);
  TIME(d += r.RealW(),    95);
  TIME(d += r.RealW(),    95);
  TIME(d += r.RealN<double>(),    95);
  TIME(d += r.RealW<double>(),    95);
  TIME(d += r.RealW<double>(),    95);
  TIME(e += r.RealN<long double>(),   101);
  TIME(e += r.RealW<long double>(),   101);
  TIME(f += r.FloatN<float>(),    99);
  TIME(f += r.FloatW<float>(),    99);
  TIME(d += r.FloatN(),   141);
  TIME(d += r.FloatW(),   141);
  TIME(d += r.FloatN<double>(),   141);
  TIME(d += r.FloatW<double>(),   141);
  TIME(e += r.FloatN<long double>(),   139);
  TIME(e += r.FloatW<long double>(),   139);
#endif
#if 0
  TIME(r.StepCount(10000), 900000);
  TIME(r.StepCount(-10000), 900000);
  TIME(r(),    16);
  TIME(i ^= r(),    27);
  TIME(b ^= r.Boolean(),    29);
  TIME(i ^= r.Integer(),    27);
  TIME(i ^= r.Integer<unsigned long>(),    27);
  TIME(i ^= r.Integer<21>(),    27);
  TIME(i ^= r.Integer(0),    27);
  TIME(i ^= r.Integer(0U),    27);
  TIME(i ^= r.Integer(0UL),    27);
  
  TIME(l ^= r.Integer<unsigned long long>(),    62);

  TIME(i ^= r.Integer(6), 80);
  TIME(i ^= r.Integer(52), 80);
  TIME(for (int q = 512; q > 0;) i ^= r.Integer(n+q--), 34193);
  TIME(i ^= r.Integer<unsigned long>(0x3fffffffL),    83);
  TIME(i ^= r.Integer<unsigned long>(0x40000000L),    83);
  TIME(i ^= r.Integer<unsigned long>(0x40000001L),    83);
  TIME(i ^= r.Integer<unsigned long>(0x55555555L),    58);
  TIME(i ^= r.Integer<unsigned long>(0xAAAAAAAAUL),    57);
  TIME(i ^= r.Integer<unsigned long long>(0xAAAAAAAAAAAAAAAAULL),   108);

  TIME(f += r.Real<float>(),    50);
  TIME(f += r.RealU<float>(),    53);
  TIME(f += r.RealN<float>(),    53);
  TIME(f += r.RealW<float>(),    53);
  TIME(f += r.RealS<float>(),    52);
  TIME(f += r.RealO<float>(),    54);
  TIME(f += r.RealC<float>(),    51);
  TIME(f += r.Float<float>(),    97);
  TIME(f += r.FloatU<float>(),    99);
  TIME(f += r.FloatN<float>(),    99);
  TIME(f += r.FloatW<float>(),    99);

  TIME(d += r.Real(),    92);
  TIME(d += r.RealU(),    95);
  TIME(d += r.RealN(),    95);
  TIME(d += r.RealW(),    95);
  TIME(d += r.RealS(),    95);
  TIME(d += r.RealO(),    99);
  TIME(d += r.RealC(),    99);
  TIME(d += r.Float(),   134);
  TIME(d += r.FloatU(),   141);
  TIME(d += r.FloatN(),   141);
  TIME(d += r.FloatW(),   141);

  TIME(d += r.Real<double>(),    92);
  TIME(d += r.RealU<double>(),    95);
  TIME(d += r.RealN<double>(),    95);
  TIME(d += r.RealW<double>(),    95);
  TIME(d += r.RealS<double>(),    95);
  TIME(d += r.RealO<double>(),    99);
  TIME(d += r.RealC<double>(),    99);
  TIME(d += r.Float<double>(),   134);
  TIME(d += r.FloatU<double>(),   141);
  TIME(d += r.FloatN<double>(),   141);
  TIME(d += r.FloatW<double>(),   141);

  TIME(e += r.Real<long double>(),    97);
  TIME(e += r.RealU<long double>(),   101);
  TIME(e += r.RealN<long double>(),   101);
  TIME(e += r.RealW<long double>(),   101);
  TIME(e += r.RealS<long double>(),   101);
  TIME(e += r.RealO<long double>(),   100);
  TIME(e += r.RealC<long double>(),   173);
  TIME(e += r.Float<long double>(),   137);
  TIME(e += r.FloatU<long double>(),   139);
  TIME(e += r.FloatN<long double>(),   139);
  TIME(e += r.FloatW<long double>(),   139);

  TIME(d += (r.Real<double, 31>()),    52);
  TIME(d += (r.Real<double, 32>()),    52);
  TIME(d += (r.Real<double, 48>()),   101);
  TIME(d += (r.RealC<double, 31>()),    99);
  TIME(d += (r.RealC<double, 32>()),    99);
  TIME(d += (r.RealC<double, 48>()),   101);
#if MT_LONGDOUBLEPREC >= 64
  TIME(e += (r.Real<long double, 63>()),   102);
  TIME(e += (r.RealC<long double, 63>()),   159);
  TIME(e += (r.Real<long double, 64>()),    97);
  TIME(e += (r.RealC<long double, 64>()),   177);
#endif

  TIME(for (int q = 1; q < 97; ++q) b ^= r.Prob<float>(q/97.0f), 12364);
  TIME(for (int q = 1; q < 97; ++q) b ^= r.Prob<double>(q/97.0), 12442);
  TIME(for (int q = 1; q < 97; ++q) b ^= r.Prob<long double>(q/97.0L), 13045);
  int ll = (1L << 30) / 97;
  TIME(for (int q = 1; q < 97; ++q) b ^= r.Prob<int>(q, 97UL), 4978);
  TIME(for (int q = 1; q < 97; ++q) b ^= r.Prob<int>(q*ll, 97UL*ll), 3346);

  TIME(b ^= r.Prob<float>(.2857142857142857f),    64);
  TIME(b ^= r.Prob<long>(2,7),    41);
  TIME(b ^= r.Prob<float>(5.0f/17.0f),    65);
  TIME(b ^= r.Prob<double>(5.0/17.0),    65);
  TIME(b ^= r.Prob<long double>(5.0L/17.0L),    69);
  TIME(b ^= r.Prob<long>(5,17),    81);
  int mm = (1L << 30) / 17;
  TIME(b ^= r.Prob<long>(5*mm,17*mm),    34);
  /*
  TIME(f += r.Normal<float>(),   261);
  TIME(d += r.Normal<double>(),   361);
  TIME(d += r.Normal(),   361);
  TIME(e += r.Normal<long double>(),   398);
  */
  NormalDistribution<float> nf;
  NormalDistribution<double> nd;
  NormalDistribution<long double> ne;
  TIME(f += nf(r),   261);
  TIME(d += nd(r),   361);
  TIME(e += ne(r),   398);
#endif
  cout << b << " "
       << int(c) << " "
       << s << " "
       << i << " "
       << l << endl
       << setprecision(20) << f << " "
       << setprecision(26) << d << " "
       << setprecision(30) << e << " "
       << r.Count() << setprecision(2) << endl;

  return;
}

void TestSystem() {
  using std::cout; using std::endl; using std::numeric_limits;

#if 0
Linux 32
unsigned long 2 32
unsigned long long 2 64
float 2 24 -125
double 2 53 -1021
long double 2 64 -16381
float 2 24 -125 1 0 1.17549e-38 1.4013e-45
double 2 53 -1021 1 0 3.3621e-4932 4.94066e-324
long double 2 64 -16381 1 0 3.3621e-4932 3.6452e-4951
float limits div 149 ldexp 149 pow 149
double limits div 1074 ldexp 1074 pow 1074
long double limits div 16445 ldexp 16445 pow 16383

Linux 64
unsigned long 2 64
unsigned long long 2 64
float 2 24 -125
double 2 53 -1021
long double 2 64 -16381
float 2 24 -125 1 0 1.17549e-38 1.4013e-45
double 2 53 -1021 1 0 3.3621e-4932 4.94066e-324
long double 2 64 -16381 1 0 3.3621e-4932 3.6452e-4951
float limits div 149 ldexp 149 pow 127
double limits div 1074 ldexp 1074 pow 1023
long double limits div 16445 ldexp 16445 pow 16383

Windows 32
unsigned long 2 32
unsigned long long 2 64
float 2 24 -125
double 2 53 -1021
long double 2 53 -1021
float 2 24 -125 1 1 1.17549e-038 1.4013e-045
double 2 53 -1021 1 1 2.22507e-308 4.94066e-324
long double 2 53 -1021 1 1 2.22507e-308 4.94066e-324
float limits div 149 ldexp 149 pow 149
double limits div 1074 ldexp 1074 pow 1074
long double limits div 1074 ldexp 1074 pow 1074

Sun
unsigned long 2 32
unsigned long long 2 64
float 2 24 -125
double 2 53 -1021
long double 2 113 -16381
float 2 24 -125 1 0 1.17549e-38 1.4013e-45
double 2 53 -1021 1 0 3.3621e-4932 4.94066e-324
long double 2 113 -16381 1 0 3.3621e-4932 6.47518e-4966
float limits div 149 ldexp 149 pow 127
double limits div 1074 ldexp 1074 pow 1023
long double limits div 16494 ldexp 1074 pow 16383


#endif
  cout << "Precisions of various types:" << endl;
  cout << "unsigned long "
       << numeric_limits<unsigned long>::radix << " "
       << numeric_limits<unsigned long>::digits << endl;
  cout << "unsigned long long "
       << numeric_limits<unsigned long long>::radix << " "
       << numeric_limits<unsigned long long>::digits << endl;
  cout << "float "
       << numeric_limits<float>::radix << " "
       << numeric_limits<float>::digits << " "
       << numeric_limits<float>::min_exponent << endl;
  cout << "double "
       << numeric_limits<double>::radix << " "
       << numeric_limits<double>::digits << " "
       << numeric_limits<double>::min_exponent << endl;
  cout << "long double "
       << numeric_limits<long double>::radix << " "
       << numeric_limits<long double>::digits << " "
       << numeric_limits<long double>::min_exponent << endl;

  cout << "float "
       << numeric_limits<float>::radix << " "
       << numeric_limits<float>::digits << " "
       << numeric_limits<float>::min_exponent << " "
       << numeric_limits<float>::is_iec559 << " "
       << numeric_limits<float>::has_denorm << " "
       << numeric_limits<float>::has_denorm_loss << " "
       << (numeric_limits<float>::min)() << " "
       << numeric_limits<float>::denorm_min() << endl;
  cout << "double "
       << numeric_limits<double>::radix << " "
       << numeric_limits<double>::digits << " "
       << numeric_limits<double>::min_exponent <<  " "
       << numeric_limits<double>::is_iec559 << " "
       << numeric_limits<double>::has_denorm << " "
       << numeric_limits<double>::has_denorm_loss << " "
       << (numeric_limits<long double>::min)() << " "
       << numeric_limits<double>::denorm_min() << endl;
  cout << "long double "
       << numeric_limits<long double>::radix << " "
       << numeric_limits<long double>::digits << " "
       << numeric_limits<long double>::min_exponent <<  " "
       << numeric_limits<long double>::is_iec559 << " "
       << numeric_limits<long double>::has_denorm << " "
       << numeric_limits<long double>::has_denorm_loss << " "
       << (numeric_limits<long double>::min)() << " "
       << numeric_limits<long double>::denorm_min() << endl;

  {
    vector<float> x(1);
    vector<float> y(1);
    int e;
    x[0] = 1;
    e = 0;
    do {
      e++;
      x[0] /= 2.0f;
      y = x;
    } while (y[0] > 0.0f);
    cout << "float limits div " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::ldexp(float(1), -e);
      y = x;
    } while (y[0] > 0.0f);
    cout << "ldexp " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::pow(float(2), -e);
      y = x;
    } while (y[0] > 0.0f);
    cout << "pow " << --e << endl;
  }
  {
    vector<double> x(1);
    vector<double> y(1);
    int e;
    x[0] = 1;
    e = 0;
    do {
      e++;
      x[0] /= 2.0;
      y = x;
    } while (y[0] > 0.0);
    cout << "double limits div " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::ldexp(double(1), -e);
      y = x;
    } while (y[0] > 0.0);
    cout << "ldexp " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::pow(double(2), -e);
      y = x;
    } while (y[0] > 0.0);
    cout << "pow " << --e << endl;
  }
  {
    vector<long double> x(1);
    vector<long double> y(1);
    int e;
    x[0] = 1;
    e = 0;
    do {
      e++;
      x[0] /= 2.0L;
      y = x;
    } while (y[0] > 0.0L);
    cout << "long double limits div " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::ldexp((long double)(1), -e);
      y = x;
    } while (y[0] > 0.0L);
    cout << "ldexp " << --e << " ";
    e = 0;
    do {
      e++;
      x[0] = std::pow((long double)(2), -e);
      y = x;
    } while (y[0] > 0.0L);
    cout << "pow " << --e << endl;
  }
      


  cout << "Calls to timer clock SeedWord:" << endl
       << "mult = " << HighPrecMult() << endl;
  for (size_t i = 0; i < 20; ++i)
    cout << HighPrecTime() << " "
	 << clock() << " "
	 << RandomNumber::SeedWord() << endl;

  return;
}

void TestSeeding(RandomNumber& r) {
  unsigned long init[4]={123, 234, 345, 456};
  r.Reseed(0);
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(1234);
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed();
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  vector<unsigned long> nseed(r.Seed());
  nseed.push_back(RandomNumber::SeedWord());
  r.Reseed(nseed);
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(vector<unsigned long>(0));
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(vector<unsigned long>(init,init+1));
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(vector<unsigned long>(init,init+2));
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(vector<unsigned long>(init,init+3));
  cout << "Seed: " << r.SeedString() << ", 1st rv: " << r() << endl;
  r.Reseed(vector<unsigned long>(init,init+4));

  r.Reseed();
  r.StepCount(101);
  RandomNumber g(r);
  cout << "Constructor copy" << endl;
  cout << "Seed: " << r.SeedString() << ", Count: " << r.Count()
       << ", rv: " << r() << endl;
  cout << "Seed: " << g.SeedString() << ", Count: " << g.Count()
       << ", rv: " << g() << endl;

  g.Reseed(100);
  cout << "Seeds different:" << r.SeedString() << " "
       << g.SeedString() << endl;

  cout << "Equal copy" << endl;
  RandomNumber h(0);
  h = r;
  cout << "Seed: " << r.SeedString() << ", Count: " << r.Count()
       << ", rv: " << r() << endl;
  cout << "Seed: " << h.SeedString() << ", Count: " << h.Count()
       << ", rv: " << h() << endl;

  r.Reseed(200);
  cout << "Seeds different:" << r.SeedString() << " "
       << h.SeedString() << endl;

}

int main(int, char*[]) {
  using std::cout; using std::endl; using std::numeric_limits;

  typedef unsigned short us;
  TestSystem();
  Example1();
  //  return 0;
  RandomNumber r;
  typedef double real;
  //  enum { prec = 32 };
  const int prec = 32;
  for (size_t i = 0; i < 100; i++)
    cout << r.RealC() << " "
	 << r.RealC<float>() << " "
	 << r.RealC<real, prec>() << endl;
  size_t num=1000000;
  float s1 = 0, s2 = 0;
  for (size_t i = 0; i < num; i++) {
    float x = r.RealS<float>();
    s1 += x * x;
    x = r.RealC<float>() - 0.5f;
    s2 += x * x;
  }
  cout << "var " << 12*s1/num << " " << 12*s2/num << endl;
  
  /*
  float q = 0;

  TIME(q+=(r.Normal<float>()), 300);
  TIME(q+=(r.Real<float>()), 50);
  TIME(q+=(r.Real<double>()), 50);
  TIME(q+=(r.Real<long double>()), 50);
  TIME(q+=(r.Float<float>()), 80);
  TIME(q+=(r.Float<double>()), 130);
  TIME(q+=(r.Float<long double>()), 130);
  for (size_t j = 0; j < 0; ++j) {
      const int p = 2;
      const int s = 2;
      cout << "Float " << setfill('0') << setw(3)
	   << (int)(pow(2.0, p + s)*
		    r.Float<float, p, s>() + 0.5) << endl;
  }
  return 0;
  */

  /*  cout << RandomNumber::Global << endl;
  cout << RandomNumber::Global() << endl;
  if (RandomNumber::Global == r)
    cout << "foog" << endl;
  vector<unsigned long> v(624, 0);
  v[0] = 0x4d547231UL | 0x80000000UL;
  r.Reseed(v);
  r.Reseed(0);
  //  cout << 0 << " " << r() << endl;
  //  r.SetCount(-624);
  size_t i = 0;
  long long n = 0;
  while(i < 10000000) {
    if (r() == 0)
      cout << i++ << " " << n << endl;
    n++;
    }*/


  BasicTest(r);
  RandomSelect<unsigned long, unsigned long> s;
  RandomSelect<unsigned long, double> t;
  r.Reseed("1 2 3 1 4 8");
  cout << r.SeedString() << endl;
  vector<unsigned long> v = r.Seed();
  s.Init(v);
  t.Init(v);
  // s.Check();
  for (size_t i = 0; i < 20; ++i)
    cout << s(r) << " " << t(r) << endl;
  
  /*
  TestSeeding(r);
  r.Reseed();
  Deal(r);
  Toss(r);
  StepTest();
  WriteReadTest();
  */
  Time(r, 0);

  return 0;
}
