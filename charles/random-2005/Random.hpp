// Random.hpp
//
// Interface to Mersenne Twister random number generator, MT19937.  See
//
// Makoto Matsumoto <m-mat@math.sci.hiroshima-u.ac.jp> and Takuji
// Nishimura, Mersenne Twister: A 623-Dimensionally Equidistributed
// Uniform Pseudo-Random Number Generator, ACM TOMACS 8, 3-30 (1998)
//
// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
//
// Interface routines written by Charles Karney <charles@karney.com>
// licensed under the LGPL.  For documentation, see
//
// http://charles.karney.info/random/

#if !defined(RANDOM_H)
#define RANDOM_H

#define RCSID_RANDOM_H "$Id: Random.hpp 6144 2006-04-25 13:04:00Z ckarney $"

#include <limits>
#include <iostream>
#include <cmath>
#include <stdexcept>
#include <vector>

// Set to 1 to use a table for the powers of two instead of calling pow.
// Not necessary with g++ 4.x.  Needed with g++ 3.x
#if defined(__GNUC__) && __GNUC__ >= 4
// g++ 4.x appears to evaluate pow(2.0, const) at compile time
#define MT_POWERTABLE 0
#else
// otherwise use a lookup table
#define MT_POWERTABLE 1
#endif

#if defined(_MSC_VER)
#define WINDOWS 1
#define MT_LONGDOUBLEPREC 53
#pragma warning (disable: 4290)
#else
#define WINDOWS 0
#endif

#if defined(__sparc)
#define SUN 1
#define MT_LONGDOUBLEPREC 113
#else
#define SUN 0
#endif

#if !WINDOWS && !SUN
#define MT_LONGDOUBLEPREC 64
#endif

// A simple compile-time error checker.
#define MT_STATIC_ASSERT(cond) { enum{ STATIC_ASSERT_ERROR = 1/int(cond) }; }

#if 0
// This should work.  But on some platforms it's incorrectly set.
#define MT_HASDENORM(T) std::numeric_limits<T>::has_denorm
// Could use this.  But this also fails on some platforms
#define MT_HASDENORM(T) std::numeric_limits<T>::is_iec559
#else
// Use this for now, since this is right for all(?) modern computers
#define MT_HASDENORM(T) 1
#endif

class Random {
private:
  // Constants
  enum { N = 624,		// Long lag
	 M = 397,		// Short lag
#if MT_LONGDOUBLEPREC > 64
	 minpow = -120,
#else
	 minpow = -64,
#endif
	 maxpow = 32,
	 W = 32,		// Number of random bits per word
	 longsperline = 72/11	// how many longs per line of output
  };
  typedef unsigned long ulong;	// Enough to hold W bits
  typedef unsigned long long ulong2; // Enough to hold 2*W bits
  static const ulong VERSION = 0x4d547231UL, // "MTr1"
    MATRIX_A   = 0x9908b0dfUL,
    UPPER_MASK = 0x80000000UL,
    LOWER_MASK = 0x7fffffffUL,
    LONG_MASK  = 0xffffffffUL;
#if MT_POWERTABLE
  static const float power2[maxpow - minpow + 1]; // Powers of two
#endif

  // Data
  ulong m_state[N];		// the array for the state vector
  unsigned m_ptr;		// index of next rv
  long long m_rounds;		// how many times has Reload() been called
  std::vector<ulong> m_seed;	// the seed vector

public:

  // Setting the seed via the constructor.

  // Set seed to [seed]
  explicit Random(ulong s);
  // Set seed to [Random::SeedWord()]
  Random();
  // Set seed to the vector v
  explicit Random(const std::vector<ulong>& v);
  // Set seed from string
  explicit Random(const std::string& s);

  // Resetting the seed via Random::Reseed(...)
  // There are parallel routines to reseed an already instantiated RNG:

  // Set seed to [s]
  void Reseed(ulong s);
  // Set seed to [Random::SeedWord()]
  void Reseed();
  // Set seed to the vector v
  void Reseed(const std::vector<ulong>& v);
  // Set seed from string
  void Reseed(const std::string& s);

  // Return a word of more or less random data suitable for seeding the
  // RNG.  This is obtained by reading /dev/urandom on Linux and
  // combining this with the value the microsoft clock.
  static ulong SeedWord();

  // Direct read-only access to seed vector
  const std::vector<ulong>& Seed() const throw() { return m_seed; }

  // A string representation of the seed vector
  std::string SeedString() const;

  static std::string SeedString(const std::vector<ulong>& v);

  // Comparing Random's
  bool operator==(const Random& r) const throw();
  bool operator!=(const Random& r) const throw();

  // Writing and reading from a stream

  // Save the full state of the RNG to stream.
  void Save(std::ostream& os, bool bin = true) const throw(std::ios::failure);

  // Restore the full state of the RNG.
  void Restore(std::istream& is, bool bin = true) throw(std::ios::failure,
							std::out_of_range);

  // Or you can initialize the RNG from a stream.
  explicit Random(std::istream& is, bool bin = true) throw(std::ios::failure,
							   std::out_of_range);

  // A raw random result (primarily for testing)
  ulong operator()() throw();

  // A random integer of type T in [0,n)
  //  template<typename T> T operator()(T n) throw();
  ulong operator()(ulong n) throw();

  // Boolean results

  // A coin toss
  bool Boolean() throw();

  // True with probability z (a real of type T)

  // true if (z >= 1), false if (z <= 0 || isnan(z)).

  template<typename T> bool Prob(T z) throw();

  // True with probability m/n.  (m and n are integers of type T)

  // Produces sensible answers for n > 0 and any m (m <= 0 gives false,
  // m >= n gives true).  Result with n == 0 is equivalent to n =
  // max(T).  n < 0 returns the result (0 < m).
  template<typename T> bool Prob(T m, T n) throw();

  // Integer results (binary range)

  // A random unsigned long
  ulong Integer() throw() { return IntegerZ<ulong>(); }

  // A random integer of type T in [min(T), max(T)].
  template<typename T> T Integer() throw() { return IntegerZ<T>(); }

  // A random unsigned long in [0, 2^b)
  template<int b> ulong Integer() throw()
  { return IntegerQ<ulong, b>(); }

  // A random integer of type T in [0, 2^b)
  template<typename T, int b> T Integer() throw() { return IntegerQ<T, b>(); }

  // Integer results (finite range)

  // A random integer of type T in [0, n). EXCLUDES n

  // If n == 0, treat as max(T) + 1.  If n < 0, return 0.
  template<typename T> T Integer(T n) throw();

  // A random integer of type T in Closed interval [0, n].  INCLUDES n.

  // If n < 0, return 0.
  template<typename T> T IntegerC(T n) throw();

  // Real results (full precision)

  // These "fixed" real results produce a set of numbers with equal
  // spacing, eps = 1/2^p, where p = digits(T).  Typically p = 24 for
  // float, p = 53 for double.

  // A random fixed double in the interval [0, 1).

  // 2^p results.  Min = 0, Max = 1 - eps
  double Real() throw() { return RealZ<double>(); }

  // A fixed double in the Upper interval (0, 1]

  // 2^p results.  Min = eps, Max = 1
  double RealU() throw() { return RealUZ<double>(); }

  // A fixed double in the Nearest interval <0, 1>

  // 2^p + 1 results.  Min = 0, Max = 1
  double RealN() throw() { return RealNZ<double>(); }

  // A fixed double in the Symmetric interval <-1/2, 1/2>

  // 2^p results.  Min = -(1 - eps)/2, Max = (1 + eps)/2.
  double RealS() throw() { return RealSZ<double>(); }

  // 2^(p+1) + 1 results.  Min = -1, Max = 1.
  double RealW() throw() { return RealWZ<double>(); }

  // A fixed double in the Open interval (0, 1)

  // 2^p - 1 results.  Min = eps, Max = 1 - eps
  double RealO() throw() { return RealOZ<double>(); }

  // A fixed double in the Closed interval [0, 1]

  // 2^p + 1 results.  Min = 0, Max = 1
  double RealC() throw() { return RealCZ<double>(); }

  // The floating results produces results on a floating scale.  Here
  // the separation between possible results is smaller for smaller
  // numbers.

  // A floating double in interval [0, 1)

  // Floating results s.t. prob(X) = next(X) - X, where next(X) is the
  // next double following X.  Min = 0, Max = 1 - eps
  double Float() throw() { return FloatZ<double>(); }

  // A floating double in Upper interval (0, 1]

  // Floating results s.t. prob(X) = X - prev(X), where prev(X) is the
  // previous double before X.  Min = denorm_min(T), Max = 1
  double FloatU() throw() { return FloatUZ<double>(); }

  // A floating double in Nearest interval <0, 1>

  // Floating results s.t. prob(X) = (next(X) - prev(X))/2.  Min = 0,
  // Max = 1
  double FloatN() throw() { return FloatNZ<double>(); }

  // A floating double in Wide interval <-1, 1>

  // Floating results s.t. prob(X) = (next(X) - prev(X))/4.  Min = -1,
  // Max = 1
  double FloatW() throw() { return FloatWZ<double>(); }

  // A random fixed real of type T in the interval [0, 1).

  // 2^p results.  Min = 0, Max = 1 - eps
  template<typename T> T Real() throw() { return RealZ<T>(); }

  // A fixed real of type T in the Upper interval (0, 1]

  // 2^p results.  Min = eps, Max = 1
  template<typename T> T RealU() throw() { return RealUZ<T>(); }

  // A fixed real of type T in the Nearest interval <0, 1>

  // 2^p + 1 results.  Min = 0, Max = 1
  template<typename T> T RealN() throw() { return RealNZ<T>(); }

  // A fixed real of type T in the Symmetric interval <-1/2, 1/2>

  // 2^p results.  Min = -(1 - eps)/2, Max = (1 + eps)/2.
  template<typename T> T RealS() throw() { return RealSZ<T>(); }

  // 2^(p+1) + 1 results.  Min = -1, Max = 1.
  template<typename T> T RealW() throw() { return RealWZ<T>(); }

  // A fixed real of type T in the Open interval (0, 1)

  // 2^p - 1 results.  Min = eps, Max = 1 - eps
  template<typename T> T RealO() throw() { return RealOZ<T>(); }

  // A fixed real of type T in the Closed interval [0, 1]

  // 2^p + 1 results.  Min = 0, Max = 1
  template<typename T> T RealC() throw() { return RealCZ<T>(); }

  // A floating real of type T in interval [0, 1).

  // Floating results s.t. prob(X) = next(X) - X, where next(X) is the
  // next real of type T to X.  Min = 0, Max = 1 - eps
  template<typename T> T Float() throw() { return FloatZ<T>(); }

  // A floating real of type T in Upper interval (0, 1]

  // Floating results s.t. prob(X) = X - prev(X), where prev(X) is the
  // previous real of type T to X.  Min = denorm_min(T), Max = 1
  template<typename T> T FloatU() throw() { return FloatUZ<T>(); }

  // A floating real of type T in Nearest interval <0, 1>

  // Floating results s.t. prob(X) = (next(X) - prev(X))/2.  Min = 0,
  // Max = 1
  template<typename T> T FloatN() throw() { return FloatNZ<T>(); }

  // A floating double in Wide interval <-1, 1>

  // Floating results s.t. prob(X) = (next(X) - prev(X))/4.  Min = -1,
  // Max = 1
  template<typename T> T FloatW() throw() { return FloatWZ<T>(); }

  // Real results (specified precision)

  // A fixed real in [0, 1) with prec. p, i/2^p for integer i in [0,2^p).
  template<typename T, int p> T Real() throw() { return RealQ<T, p>(); }

  // A fixed real in Upper interval (0, 1] with precision p.
  template<typename T, int p> T RealU() throw() { return RealUQ<T, p>(); }

  // A fixed real in Nearest interval <0, 1> with precision p.
  template<typename T, int p> T RealN() throw() { return RealNQ<T, p>(); }

  // A fixed real in Symmetric interval <-1/2, 1/2> with precision p.
  template<typename T, int p> T RealS() throw() { return RealSQ<T, p>(); }

  // A fixed real in Wide interval <-1, 1> with precision p.
  template<typename T, int p> T RealW() throw() { return RealWQ<T, p>(); }

  // A fixed real in Open interval (0, 1) with precision p
  template<typename T, int p> T RealO() throw() { return RealOQ<T, p>(); }

  // A fixed real in Closed interval [0, 1] with precision p
  template<typename T, int p> T RealC() throw() { return RealCQ<T, p>(); }

  // A random floating real in interval [0, 1) with precision p and
  // floating exponent range e.
  template<typename T, int p, int e> T Float() throw()
  { return FloatQ<T, p, e>(); }

  // A floating real in Upper interval (0, 1] with precision p and
  // floating exponent range e.
  template<typename T, int p, int e> T FloatU() throw()
  { return FloatUQ<T, p, e>(); }

  // A floating real in Nearest interval <0, 1> with precision p and
  // floating exponent range e.
  template<typename T, int p, int e> T FloatN() throw()
  { return FloatNQ<T, p, e>(); }

  // A floating double in Wide interval <-1, 1> with precision p and
  // floating exponent range e.
  template<typename T, int p, int e> T FloatW() throw()
  { return FloatWQ<T, p, e>(); }

  // Miscellaneous

  // Sample of type T from normal distribution (zero mean, unit variance).
  double Normal() throw() { return Normal<double>(); }
  template<typename T> T Normal() throw();

  // Management

  // Return the number of random numbers used.
  long long Count() const throw();

  // Step generator forward (or backward) so that the new Count() is n.
  void SetCount(long long n) throw();

  // Step the generator forward (or backward) by n
  void StepCount(long long n) throw();

  // A "global" random number generator (not thread-safe!)
  static Random Global; 

private:
  // Compute initial state from seed
  void Init() throw();
  // Advance state by N steps
  void Reload() throw();
  // Back up state by N steps
  void Unload() throw();
  // Original scalar seed routine
  void init_genrand(ulong s) throw();
  // Original vector seed routine
  void init_by_array(const std::vector<ulong>& init_key) throw();
  // A "raw" random number with W bits in [0, 2^W)
  ulong Ran() throw();
  // A "raw" random number with 2*W bits in [0, 2^(2*W)).
  ulong2 Ran2() throw();
  // Helper for Integer(n).  A random unsigned in [0, n)
  template<typename T> T Unsigned(T n) throw();

  // Powers of 2
  template<typename T> static inline T pow2(int n) throw();
  // Multiply a real by a power of 2
  template<typename T> static inline T shiftf(T x, int n) throw();

  // Helper for Float and FloatU
  template<typename T, int p, int e, bool up> T
  FloatX(int, ulong) throw();

  template<typename T, int p, int e, bool up> T
  FloatY(int, ulong) throw();

  // One word write
  static void Write32(std::ostream& os, bool bin, int count, ulong x)
    throw(std::ios::failure);
  // One word read
  static ulong Read32(std::istream& is, bool bin) throw(std::ios::failure);

  static void syscheck();

  // Internal versions of template functions.  Do things this way since
  // g++ 3.x doesn't let you overload template functions unless the
  // functions are included in the class declaraion, e.g., Integer[QZ]
  // below can't have the same name.  The convention here is suffix Q is
  // the version with precision template argument.  Suffix Z is the
  // version with the precision set by the type.  To undo this
  // workaround, remove the Q and Z suffices, remove the following
  // declarations, and remove the definitions in the public class
  // declaration.

  template<typename T, int b       > T IntegerQ() throw();
  template<typename T              > T IntegerZ() throw();
  template<typename T, int p       > T RealQ()    throw();
  template<typename T              > T RealZ()    throw();
  template<typename T, int p       > T RealUQ()   throw();
  template<typename T              > T RealUZ()   throw();
  template<typename T, int p       > T RealNQ()   throw();
  template<typename T              > T RealNZ()   throw();
  template<typename T, int p       > T RealSQ()   throw();
  template<typename T              > T RealSZ()   throw();
  template<typename T, int p       > T RealWQ()   throw();
  template<typename T              > T RealWZ()   throw();
  template<typename T, int p       > T RealOQ()   throw();
  template<typename T              > T RealOZ()   throw();
  template<typename T, int p       > T RealCQ()   throw();
  template<typename T              > T RealCZ()   throw();
  template<typename T, int p, int e> T FloatQ()   throw();
  template<typename T              > T FloatZ()   throw();
  template<typename T, int p, int e> T FloatUQ()  throw();
  template<typename T              > T FloatUZ()  throw();
  template<typename T, int p, int e> T FloatNQ()  throw();
  template<typename T              > T FloatNZ()  throw();
  template<typename T, int p, int e> T FloatWQ()  throw();
  template<typename T              > T FloatWZ()  throw();

};

inline Random::ulong Random::Ran() throw() {
  if (m_ptr >= N)
    Reload();

  ulong y = m_state[m_ptr++];

  // Tempering
  y ^= (y >> 11);
  y ^= (y <<  7) & 0x9d2c5680UL;
  y ^= (y << 15) & 0xefc60000UL;
  y ^= (y >> 18);

  return y;
}

inline Random::ulong Random::operator()() throw() {
  return Ran();
}

inline Random::ulong2 Random::Ran2() throw() {
  ulong2 x = Ran();
  return (x << W) ^ static_cast<ulong2>(Ran());
}

// Equivalent to Integer<bool>().
inline bool Random::Boolean() throw() {
  return Ran() & 1UL;
}

// A random integer of type T in [0, 2^b)
template<typename T, int b> inline  T Random::IntegerQ() throw() {
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer);	// Check T is integer
  MT_STATIC_ASSERT(b > 0 && b <= std::numeric_limits<T>::digits);
  // Check that we have enough digits in Ran2
  MT_STATIC_ASSERT(b <= 2 * W);
  if (b < W)
    // We need to mask the result to ensure a positive result is
    // returned for signed types.
    return T(Ran() & ~(LONG_MASK << (b < W ? b : 0)));
  else if (b == W)
    return T(Ran());
  else if (b < 2 * W)
    return T(Ran2() & ~(0xffffffffULL << (b < 2 * W ? b : 0)));
  else				// b == 2*W
    return T(Ran2());
}

// A random integer of type T in [min(T), max(T)].
template<typename T> inline T Random::IntegerZ() throw() {
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer);	// Check T is integer
  const int d = std::numeric_limits<T>::digits +
    std::numeric_limits<T>::is_signed; // Include the sign bit
  // Check that we have enough digits in Ran2
  MT_STATIC_ASSERT(d <= 2 * W);
  if (d <= W)
    return T(IntegerQ<ulong, (d > W ? W : d)>());
  else				// d <= 2*W
    return T(IntegerQ<ulong2, d>());
}

// This specialization is optional.  It prevents Windows from issuing
// warning about the bool instantiations of Integer<T...>.  Maybe also
// bool is a sufficiently peculiar integer to warrant explicit
// treatment.
template<> inline bool Random::Integer<bool>() throw() {
  return Boolean();
}

// A random unsigned in [0, n)
template<typename T> inline T Random::Unsigned(T n) throw() {
  const int d = std::numeric_limits<T>::digits;
  // Check expected instantiations.
  MT_STATIC_ASSERT((d == std::numeric_limits<ulong>::digits) ||
		   (d == std::numeric_limits<ulong2>::digits));
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_signed);
  if (n == 0)			// Special case. 0 -> max(T) + 1.
    return d <= W ? T(Ran()) : T(Ran2());
#if 0
  // "Binary" method.  The technique to obtain the mask in log(W)
  // steps is taken from randInt in the MT library of Richard J. Wagner
  // <rjwagner@writeme.com>.  The optimization is due to Magnus Jonsson
  // <magnus@smartelectronix.com>.  See
  // http://www-personal.engin.umich.edu/~wagnerr/MersenneTwister.html.
  const bool big = d > W && n - 1 >  LONG_MASK;
  T mask = n - 1;
  mask |= mask >> 1;
  mask |= mask >> 2;
  mask |= mask >> 4;
  mask |= mask >> 8;
  mask |= mask >> 16;
   // Don't use "if (big)" because conditional can't be decided at
  // compile time.
  if (d > W)
    mask |= mask >> d - W;
  //  Now mask = 2^ceil(lg(n)) - 1
  T u;
  // Loop executed 2^ceil(log(n))/n times on average.  This lies
  // in [1,2).  Averaging this over possible values of n gives
  // int(2/x,x,1,2) = 2*log(2) = 1.386 iterations.
  do
    u = (big ? T(Ran2()) : T(Ran())) & mask;
  while (u >= n);
  return u;
#else
  // "Ratio method".  Find m = r * n, close to max(T), sample in u in
  // [0, m) and return u / r.
  // If oneword then we can use Ran.  Note we demote small enough ULLs
  // to ULs, where "small enough" threshold is 2^(W - 1) to avoid
  // multiple trips thru the loop for ULL close to 2^W.  This also deals
  // with the case of 64-bits unsigned longs.
  bool oneword = d <= W || n < 1UL << (W - 1);
  // 0 < (q - n) < m <= q, where q is max(T)
  const T r = (oneword ? LONG_MASK : ~T(0)) / n,
    m = r * n;
  T u;				// Find a random number in [0, m)
  do
    // For small n, this is executed once (since m is nearly q).  In the
    // worst case the loop is executed slightly less than twice on
    // average.
    u = oneword ? T(Ran()) : T(Ran2());
  while (u >= m);
  // Now u is in [0, m) = [0, r * n), so u / r is in [0, n).  An
  // alternative unbiased method would be u % n; but / appears to be
  // faster.
  return u / r;
#endif
}

// A random integer of type T in [0, n).  If n == 0, treat as max(T) +
// 1.  If n < 0, treat as 1 and return 0.  N.B. Integer<T>(0) is
// equivalent to Integer<T>() for unsigned types.  For signed types, the
// former returns a non-negative result and the latter returns a result
// in the full range.
template<typename T> inline T Random::Integer(T n) throw() {
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer);	// Check T is integer
  const int d = std::numeric_limits<T>::digits;
  // Check that we have enough digits in Ran2
  MT_STATIC_ASSERT(d <= 2 * W);
  if (std::numeric_limits<T>::is_signed)
    n = n <= T(0) ? T(0) : n;	// n = max(0, n)
  return n ?
    (d <= W ? T(Unsigned<ulong>(ulong(n))) :
     T(Unsigned<ulong2>(ulong2(n)))) :
    IntegerQ<T, d>();
}

// A random integer of type T in [0, n]
template<typename T> inline T Random::IntegerC(T n) throw() {
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer);	// Check T is integer
  const int d = std::numeric_limits<T>::digits;
  // Check that we have enough digits in Ran2
  MT_STATIC_ASSERT(d <= 2 * W);
  if (std::numeric_limits<T>::is_signed)
    n = n <= T(0) ? T(0) : n;	// n = max(0, n)
  return n < std::numeric_limits<T>::max() ?
    (d <= W ? T(Unsigned<ulong>(ulong(n + 1))) :
     T(Unsigned<ulong2>(ulong2(n + 1)))) :
    IntegerQ<T, d>();
}    

//template<typename T> T Random::operator()(T n) throw() {
//  return Integer<T>(n);
//}
inline Random::ulong Random::operator()(ulong n) throw() {
  return Integer<ulong>(n);
}

// Powers of 2
template<typename T> inline T Random::pow2(int n) throw() {
#if MT_POWERTABLE
  return static_cast<T>(power2[n - minpow]);
#else
  return std::pow(static_cast<T>(2), n);
#endif
}

// Multiply a real by a power of 2
template<typename T> inline T Random::shiftf(T x, int n) throw() {
  // std::ldexp(x, n); is equivalent, but typically slower
  return x * pow2<T>(n);
}

// Random reals in [0, 1).  Results are of the form i/2^p for integer i
// in [0,2^p).
template<typename T, int p> inline T Random::RealQ() throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  T x = 0;			// Accumulator
  int s = 0;			// How many bits so far
  // Let n be the loop count.  Typically p = 24, n = 1 for float; p =
  // 53, n = 2 for double; p = 64, n = 2 for long double.  For Sun
  // Sparc's, we have p = 113, n = 4 for long double.  For Windows, long
  // double is the same as double (p = 53).
  do {
    s += W;
    x += shiftf<T>(static_cast<T>(Ran() >> (s > p ? s - p : 0)),
		   -(s > p ? p : s));
  } while (s < p);
  return x;
}
#if 0
// Specializations for Real with common precisions unrolling the loop in
// Real<T>().  These specializations are optional and are bit-equivalent
// to the template definitions.  They give a slight (5%) boost in
// performance.
template<> inline float Random::RealQ<float, 24>() throw() {
  MT_STATIC_ASSERT(24 <= W);
  MT_STATIC_ASSERT(std::numeric_limits<float>::digits >= 24);
  return shiftf<float>(static_cast<float>(Ran() >> W - 24), -24);
}

template<> inline double Random::RealQ<double, 32>() throw() {
  MT_STATIC_ASSERT(32 == W);
  MT_STATIC_ASSERT(std::numeric_limits<double>::digits >= 32);
  return shiftf<double>(static_cast<double>(Ran()), -32);
}

template<> inline double Random::RealQ<double, 48>() throw() {
  MT_STATIC_ASSERT(53 > W && 48 <= 2 * W);
  MT_STATIC_ASSERT(std::numeric_limits<double>::digits >= 48);
  return shiftf<double>(static_cast<double>(Ran2() >> 2 * W - 48), -48);
}

template<> inline double Random::RealQ<double, 53>() throw() {
  MT_STATIC_ASSERT(53 > W && 53 <= 2 * W);
  MT_STATIC_ASSERT(std::numeric_limits<double>::digits >= 53);
  return shiftf<double>(static_cast<double>(Ran2() >> 2 * W - 53), -53);
}

#if MT_LONGDOUBLEPREC == 53
template<> inline long double Random::RealQ<long double, 53>() throw() {
  MT_STATIC_ASSERT(53 > W && 53 <= 2 * W);
  MT_STATIC_ASSERT(std::numeric_limits<long double>::digits >= 53);
  return
    shiftf<long double>(static_cast<long double>(Ran2() >> 2 * W - 53), -53);
}
#endif

#if MT_LONGDOUBLEPREC >= 64
template<> inline long double Random::RealQ<long double, 64>() throw() {
  MT_STATIC_ASSERT(64 == 2 * W);
  MT_STATIC_ASSERT(std::numeric_limits<long double>::digits >= 2 * W);
  return shiftf<long double>(static_cast<double long>(Ran2()), - 2 * W);
}
#endif

#if MT_LONGDOUBLEPREC >= 113
// For Sun
template<> inline long double Random::RealQ<long double, 113>() throw() {
  MT_STATIC_ASSERT(113 > 3 * W && 113 <= 4 * W);
  MT_STATIC_ASSERT(std::numeric_limits<long double>::digits >= 113);
  long double
    x = shiftf<long double>(static_cast<long double>(Ran2()), - 2 * W);
  return x +
    shiftf<long double>(static_cast<long double>(Ran2() >> 4 * W - 113),
			-113);
}
#endif
#endif
// Random reals in [0, 1) with full precision.
template<typename T> inline T Random::RealZ() throw() {
  return RealQ<T, std::numeric_limits<T>::digits>();
}

// A real of type T in (0, 1] with precision p.
template<typename T, int p> inline T Random::RealUQ() throw() {
  return T(1) - RealQ<T, p>();
}
// A real of type T in (0, 1] with full precision
template<typename T> inline T Random::RealUZ() throw() {
  return RealUQ<T, std::numeric_limits<T>::digits>();
}

// A real of type T in <0, 1> with precision p.
template<typename T, int p> inline T Random::RealNQ() throw() {
  T x = RealQ<T, p>();
  return x || Boolean() ? x : T(1);
}
// A real of type T in <0, 1> with full precision
template<typename T> inline T Random::RealNZ() throw() {
  return RealNQ<T, std::numeric_limits<T>::digits>();
}

// A real of type T in <-1/2, 1/2> with precision p. Results are of the
// form (i+1/2)/2^p for integer i in [-2^p/2, 2^p/2). This only "works"
// for radix 2 systems (with larger bases, the results can't be
// represented exactly).
template<typename T, int p> inline T Random::RealSQ() throw() {
  return RealQ<T, p>() - ( T(1) - pow2<T>(-p) ) / 2;
}
// A real of type T in <-1/2, 1/2> with full precision
template<typename T> inline T Random::RealSZ() throw() {
  return RealSQ<T, std::numeric_limits<T>::digits>();
}

// Random reals in [-1, 1].  Round random in [-1, 1] to nearest multiple
// of 1/2^p.  Results are of the form i/2^p for integer i in [-2^p,2^p].
template<typename T, int p> inline T Random::RealWQ() throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  T x = -T(1);			// Accumulator
  int s = -1;			// How many bits so far
  do {
    s += W;
    x += shiftf<T>(static_cast<T>(Ran() >> (s > p ? s - p : 0)),
		   -(s > p ? p : s));
  } while (s < p);
  return (x + T(1) != T(0)) || Boolean() ? x : T(1);
}
// A real of type T in (0, 1] with full precision
template<typename T> inline T Random::RealWZ() throw() {
  return RealWQ<T, std::numeric_limits<T>::digits>();
}

// A real of type T in (0, 1) with precision p
template<typename T, int p> inline T Random::RealOQ() throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  T x;
  // Loop executed 2^p/(2^p-1) times on average.
  do
    x = RealQ<T, p>();
  while (x == 0);
  return x;
}

// A real of type T in (0, 1) with full precision
template<typename T> inline T Random::RealOZ() throw() {
  return RealOQ<T, std::numeric_limits<T>::digits>();
}

// A real of type T in [0, 1] with precision p
template<typename T, int p> inline T Random::RealCQ() throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  if (p < W) {
    // Sample an integer in [0, n) where n = 2^p + 1.  This uses the
    // same logic as Unsigned(n).
    const ulong n = (1UL << (p < W ? p : 0)) + 1,
      r = LONG_MASK / n,
      m = r * n;
    ulong u;
    do
      u = Ran();
    while (u >= m);
    // u is rv in [0, r * n)
    return shiftf<T>(static_cast<T>(u / r), -p);
    // Could also special case p < 2 * W, using Ran2().  However the
    // general code below is faster.
  } else {			// p >= W
    // Synthesize a p+1 bit random W bits at a time.  If number is odd,
    // return RealQ<T, p>() (w prob 1/2); else if number is zero, return
    // 1 (w prob 1/2^(p+1)); else repeat.  Normalizing probabilities on
    // returned results we return RealQ<T, p>() with prob 2^p/(2^p+1),
    // else 1 with prob 1/(2^p+1), as required.  Loop executed twice on
    // average and so consumes 2rvs more than rvs for RealQ<T, p>().  As
    // in FloatX, do NOT try to save on calls to Ran() by using the
    // leftover bits from RealQ.
    while (true) {
      // If p + 1 < W then mask x with (1 << p + 1) - 1
      ulong x = Ran();		// Low W bits of p+1 bit random
      if (x & 1UL)		// Is it odd?
	return RealQ<T, p>();	// Prob 1/2 on each loop iteration
      // Now check whole number for zeroness
      if (x)
	continue;
      int s = p + 1 - W;	// Bits left to check.  Note s >= 0.
      while (true) {
	if (s <= 0)		// We're done.  All are zero.
	  // Prob 1/2^(p+1) on each loop iteration
	  return T(1);	   // We get here once every 60000 yrs (p = 64)!
	// Check the next min(s, W) bits.
	if (Ran() >> (s > W ? 0 : W - s))
	  break;
	s -= W;
      }
    }
  }
}

// A real of type T in [0, 1] with full precision
template<typename T> inline T Random::RealCZ() throw() {
  return RealCQ<T, std::numeric_limits<T>::digits>();
}

#if 1
template<typename T, int p, int e, bool up> inline
T Random::FloatY(int, ulong) throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  MT_STATIC_ASSERT(e >= 0 && e < std::numeric_limits<T>::digits);
  MT_STATIC_ASSERT(p + e <= std::numeric_limits<T>::digits -
		   std::numeric_limits<T>::min_exponent); // Need 1/2^(e+p) > 0
  T x = RealQ<T, e + 1>();
  int n;			// Determine exponent (-e <= n <= 0)
  std::frexp(x, &n);		// Prob(n) = 2^(n-1)
  // scale number in [1,2) by 2^(n-1).  If x == 0 scale number in [0,1).
  return shiftf<T>((up ? RealUQ<T, p - 1>() : RealQ<T, p - 1>()) +
		   (x ? T(1) : T(0)), n - 1);
}
#endif

// Produce up ? FloatU() : Float().  On entry the low b bits of m are
// usable random bits.
template<typename T, int p, int e, bool up> inline
T Random::FloatX(int b, ulong m) throw() {
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  MT_STATIC_ASSERT(p > 0 && p <= std::numeric_limits<T>::digits);
  MT_STATIC_ASSERT(e >= 0);
  // With subnormals: condition that smallest number is representable
  MT_STATIC_ASSERT(!MT_HASDENORM(T) ||
		   // Need 1/2^(e+p) > 0
		   p + e <= std::numeric_limits<T>::digits -
		    std::numeric_limits<T>::min_exponent);
  // Without subnormals :condition for no underflow in while loop
  MT_STATIC_ASSERT(MT_HASDENORM(T) ||
		   // Need 1/2^(e+1) > 0
		   e <= - std::numeric_limits<T>::min_exponent);
#if 0
  // Something along these lines could also be made to work.  Basically,
  // the first RealZ is used to select the exponent with prob 2^(n-1)
  // and the next selects the fraction at full precision.  However this
  // is slower (by 40%) and the code needs to be elaborated to deal with
  // the full exponent range, the case when x = 0, n = 0, different
  // rounding directions, etc.
  template<typename T> inline T Random::Float() throw() {
    T x = RealZ<T>();
    if (x >= T(0.5))		// Data from x can be used directly
      return x;			// Corresponds to n = 0.
    else {
      int n;			// Determine exponent (n < 0)
      std::frexp(x, &n);	// Prob(n) = 2^(n-1)
      // scale number in [1,2) by 2^(n-1).  Really should ask for 1 less
      // bit of precision here.
      return shiftf<T>(RealZ<T>() + T(1), n - 1);
    }
  }
#endif
  // Use {a, b} to denote the inteval: up ? (a, b] : [a, b)
  T x = up ? RealUQ<T, p>() : RealQ<T, p>(); // Generate p bits in {0, 1}
  // Use whole interval if e == 0 and handle the interval {1/2, 1}
  if (e == 0 || (up ? x > T(0.5) : x >= T(0.5)))
    return x;
  x += T(0.5);			// Shift remaining portion to {1/2, 1}
  if (b == 0) {
    m = Ran();			// Random bits
    b = W;			// Bits available in m
  }
  int sm = e;			// sm = e - s + 2
  // Here x in {1, 2} / 2, prob 1/2
  do {				// s = 2 thru e+1, sm = e thru 1
    x /= 2;
    if (m & 1UL)
      return x;			// x in {1, 2} / 2^s, prob 1/2^s
    if (--b)
      m >>= 1;
    else {
      m = Ran();
      b = W;
    }
  } while (--sm);
  // x in {1, 2} / 2^(e+1), prob 1/2^(e+1).  Don't worry about the
  // possible overhead of the calls to pow here.  We rarely get here.
  if (MT_HASDENORM(T) || // subnormals allowed
      // No subnormals but smallest number still representable
      p + e <= -std::numeric_limits<T>::min_exponent + 1 ||
      // Possibility of underflow, so have to test on x.  Here, we have
      // -p + 1 < e + min_exp <= 0 so pow2 can be used
      x >= (T(1) + pow2<T>(e + std::numeric_limits<T>::min_exponent)) *
      (e + 1 > -minpow ? std::pow(T(2), - e - 1) : pow2<T>(- e - 1)))
    // shift x to {0, 1} / 2^(e+1)
    // Use product of pow's since max(e + 1) =
    // std::numeric_limits<T>::digits -
    // std::numeric_limits<T>::min_exponent and pow may underflow
    return x -
      (e + 1 > -minpow ?
       std::pow(T(2), -(e + 1)/2) * std::pow(T(2), -(e + 1) + (e + 1)/2) :
       pow2<T>(- e - 1));
  else
  // Underflow to up ? min() : 0
    return up ?
      // pow is OK here.
      std::pow(T(2), std::numeric_limits<T>::min_exponent - 1) : T(0);
}

// Real numbers of type T in [0, 1) with precision p and floating
// exponent range e.
template<typename T, int p, int e> inline T Random::FloatQ() throw() {
  // This produces reals as follows
  //
  // Interval        count    prob = spacing
  // [1,2) / 2^s     2^(p-1)  1/2^(p+s-1)         for s = 1..e
  // [0,1) / 2^e     2^p      1/2^(p+e)
  //
  // If e = 0, this reduces to Real()
  // Interval        count    prob = spacing
  // [0,1)           2^p      1/2^p
  //
  // Number of different spacings = e
  // Ratio of coarsest to finest spacing = 2^e.
  // Smallest result = 0
  // Smallest nonzero result = 1/2^(p+e).
  // Largest result = 1 - 1/2^p.
  // Number of results = 2^p * (e/2 + 1)
  //
  // Provided X is in the set of numbers above (with the addition of 1)
  // we have Prob(Float() < X) = X
  //
  // We code as
  // Interval        count    prob = spacing
  // [1,2) / 2       2^(p-1)  1/2^p
  // [1,2) / 2^s     2^(p-1)  1/2^(p+s-1)         for s = 2..e+1
  // [0,1) / 2^(e+1) 2^(p-1)  1/2^(p+e)
  //
  // Example p = 3, e = 2
  //    X                 p(X)		
  //             Float    FloatU    FloatN	
  //  --------------------------------------
  //    0         1/32     0        0.5/32	
  //    1/32      1/32     1/32      1/32 	
  //    2/32      1/32     1/32      1/32 	
  //    3/32      1/32     1/32      1/32 	
  //    4/32      1/32     1/32      1/32 	
  //    5/32      1/32     1/32      1/32 	
  //    6/32      1/32     1/32      1/32 	
  //    7/32      1/32     1/32      1/32 	
  //    4/16      1/16     1/32     1.5/32	
  //    5/16      1/16     1/16      1/16 	
  //    6/16      1/16     1/16      1/16 	
  //    7/16      1/16     1/16      1/16 	
  //    4/8       1/8      1/16     1.5/16	
  //    5/8       1/8      1/8       1/8  	
  //    6/8       1/8      1/8       1/8  	
  //    7/8       1/8      1/8       1/8  	
  //    1         0        1/8      0.5/8
  //
  // For Float, we can verify that sum(p(Y), all Y < X) = X for all
  // representable X in [0,1].  Similarly we have p(Y) = next(Y) - Y,
  // where next(Y) is the next number to Y.
  //
  // For FloatU, we can verify that sum(p(Y), all Y <= X) = X for all
  // representable X in [0,1].  Similarly we have p(Y) = Y - prev(Y),
  // where prev(Y) is the previous number to Y.
  //
  // For FloatN, we can verify that sum(p(Y), all Y < X) = X for all X
  // in (0, 1) midway between representable numbers.  Similarly we have
  // p(Y) = (min(1, next(Y)) - max(0, prev(Y)))/2, where next(Y)/prev(Y)
  // is the next/previous number to Y.

  return FloatX<T, p, e, false>(0, 0);
}

// Real numbers of type T in [0, 1) with full precision p and floating
// exponent range
template<typename T> inline T Random::FloatZ() throw() {
  return FloatQ<T, std::numeric_limits<T>::digits,
    -std::numeric_limits<T>::min_exponent>();
}

// Real numbers of type T in (0, 1] with precision p and floating
// exponent range e.
template<typename T, int p, int e> inline T Random::FloatUQ() throw() {
  // This produces reals as in FloatQ with the [a,b) intervals converted
  // to (a,b].
  //
  // If e = 0, this reduces to RealU()
  //
  // Number of different spacings = e
  // Ratio of coarsest to finest spacing = 2^e.
  // Smallest result = 1/2^(p+e).
  // Largest result = 1
  // Largest result smaller than 1 = 1 - 1/2^p.
  // Number of results = 2^p * (e/2 + 1)
  //
  // Provided X is in the set of numbers above (with the addition of 0)
  // we have Prob(FloatU() <= X) = X
  return FloatX<T, p, e, true>(0, 0);
}

// Real numbers of type T in <0, 1> with precision p and floating
// exponent range e.
template<typename T, int p, int e> inline T Random::FloatNQ() throw() {
  // Equivalent to rounding an ideal (infinite precision) random in
  // [0,1] to the nearest representable float.
  //
  // If e = 0, this is equivalent to RealN()
  //
  // Number of different spacings = e
  // Ratio of coarsest to finest spacing = 2^e.
  // Smallest result = 0
  // Smallest non-zero result = 1/2^(p+e).
  // Largest result = 1
  // Largest result smaller than 1 = 1 - 1/2^p.
  // Number of results = 2^p * (e/2 + 1) + 1
  ulong x = Ran();
  // Use Float or FloatU each with prob 1/2, i.e., return Boolean() ?
  // Float() : FloatU().  However, rather than use Boolean(), we pick
  // the high bit off a Ran() and pass the rest of the number to FloatX
  // to use.  This saves 1/2 a call to Ran().
  //  return x >> W - 1 ?		// equivalent to Boolean()
  return x >> (W - 1) ?		// equivalent to Boolean()
    FloatX<T, p, e, false>(W - 1, x) : // Float<T, p, e>()
    FloatX<T, p, e, true>(W - 1, x); // FloatU<T, p, e>()
}

// Real numbers of type T in <-1, 1> with precision p and floating
// exponent range e.
template<typename T, int p, int e> inline T Random::FloatWQ() throw() {
  // Equivalent to rounding an ideal (infinite precision) random in
  // [-1,1] to the nearest representable float.
  //
  // If e = 0, this is equivalent to RealW()
  ulong x = Ran();
  return ((x & 0x80000000UL) ? -1 : 1) * // Equiv to (Boolean() ? -1 : 1) *
    ( x & 0x40000000UL ?	// equivalent to Boolean()
      FloatX<T, p, e, false>(W - 2, x) : // Float<T, p, e>()
      FloatX<T, p, e, true>(W - 2, x) ); // FloatU<T, p, e>()
}

// Real numbers of type T in (0, 1] with full precision p and floating
// exponent range
template<typename T> inline T Random::FloatUZ() throw() {
  return FloatUQ<T, std::numeric_limits<T>::digits,
    -std::numeric_limits<T>::min_exponent>();
}

template<typename T> inline T Random::FloatNZ() throw() {
  return FloatNQ<T, std::numeric_limits<T>::digits,
    -std::numeric_limits<T>::min_exponent>();
}

template<typename T> inline T Random::FloatWZ() throw() {
  return FloatWQ<T, std::numeric_limits<T>::digits,
    -std::numeric_limits<T>::min_exponent>();
}

// True with probability z.  true if z >= 1, false if z <= 0 or
// isnan(z).
template<typename T> inline bool Random::Prob(T z) throw() {
  // Simulate return Real() < z; where Real() contains as many bits
  // as necessary to yield a probability of exactly z.  (Thus, we
  // want Prob(pow(2.0f, -28)) to "work", even though float has only 24
  // bits of precision.)  The definition involves < (instead of <=)
  // because Real is in [0,1) so it is "biased downwards".  Compared
  // with the inexact "return Real<T>() < z;", this one is about 10%
  // slower with floats and 15% faster with doubles and long doubles.
  MT_STATIC_ASSERT(!std::numeric_limits<T>::is_integer); // Check T is real
  // Generate Real() with c bits at a time where c is chosen so that
  // static_cast doesn't loose any bits and so that it uses up just one
  // rv.
  const int c = std::numeric_limits<T>::digits > W ?
    W : std::numeric_limits<T>::digits;
  MT_STATIC_ASSERT(c > 0);
  const T mult = pow2<T>(c);
  // Pre-loop tests needed to avoid overflow
  if (!(z > T(0)))		// Ensure false if isnan(z)
    return false;
  else if (z >= T(1))
    return true;
  do {			       // Loop executed slightly more than once.
    // Here z is in (0,1).  Write Real() = (X + y)/mult where X is an
    // integer in [0, mult) and eps is a real in [0,1).  Then Real() < z
    // becomes z' > y where z' = z * mult - X.
    z *= mult;			// Form z'.  Multiplication is exact
    z -= static_cast<T>(IntegerQ<ulong, c>()); // Also exact
    if (z <= T(0))
      return false;	   // If z' <= 0 the result is definitely false.
    // Exit if z' >= 1; the result is definitely true.  Otherwise z' is
    // in (0,1) and the result is true with probability z'.
  } while (z < T(1));
  return true;
}

// True with probability m/n
template<typename T> inline bool Random::Prob(T m, T n) throw() {
  // Produces sensible answers for n > 0 and any m (m <= 0 gives false,
  // m > n gives true).  Result with n == 0 is equivalent to max(T).  n
  // < 0 returns the result (0 < m).  Efficiency improved by scaling m
  // and n so that n is equal to or slighly less than a power of two.
  // Thus to obtain a probability of 5/17 use: int m = (1L << 30) / 17;
  // bool b = Prob(5 * m, 17 * m);
  return Integer<T>(n) < m;
}

// Return the number of random numbers used.  This needs to return a
// long long result since it can reasonably exceed 2^31.
inline long long Random::Count() const throw() {
  return m_ptr == N + 1 ? 0 : m_rounds * N + m_ptr;
}

// An alternate way of stepping the RNG.
inline void Random::SetCount(long long n) throw() {
  StepCount(n - Count());
}

// Write to stream as text
inline std::ostream& operator<<(std::ostream& os, const Random& r) {
  r.Save(os, false);
  return os;
}

// Read from stream as text
inline std::istream& operator>>(std::istream& is, Random& r) {
  r.Restore(is, false);
  return is;
}

#endif

/*
103281ns 1e+04rv per r.StepCount(10000)
141683ns -1e+04rv per r.StepCount(-10000)
    15ns     1rv per r()
    26ns     1rv per i ^= r()
    28ns     1rv per b ^= r.Boolean()
    26ns     1rv per i ^= r.Integer()
    26ns     1rv per i ^= r.Integer<unsigned long>()
    27ns     1rv per i ^= r.Integer<21>()
    27ns     1rv per i ^= r.Integer(0)
    26ns     1rv per i ^= r.Integer(0U)
    26ns     1rv per i ^= r.Integer(0UL)
    60ns     2rv per l ^= r.Integer<unsigned long long>()
 34986ns 705.3rv per for (int q = 512; q > 0;) i ^= r.Integer(n+q--)
    30ns     1rv per i ^= r.Integer<unsigned long>(0x3fffffffL)
    29ns     1rv per i ^= r.Integer<unsigned long>(0x40000000L)
    80ns     2rv per i ^= r.Integer<unsigned long>(0x40000001L)
    56ns   1.5rv per i ^= r.Integer<unsigned long>(0x55555555L)
    56ns   1.5rv per i ^= r.Integer<unsigned long>(0xAAAAAAAAUL)
   101ns     3rv per i ^= r.Integer<unsigned long long>(0xAAAAAAAAAAAAAAAAULL)
    48ns     1rv per f += r.Real<float>()
    52ns     1rv per f += r.RealU<float>()
    53ns     1rv per f += r.RealN<float>()
    52ns     1rv per f += r.RealS<float>()
    54ns     1rv per f += r.RealO<float>()
    52ns 1.004rv per f += r.RealC<float>()
    98ns   1.5rv per f += r.Float<float>()
   101ns   1.5rv per f += r.FloatU<float>()
   124ns     2rv per f += r.FloatN<float>()
    87ns     2rv per d += r.Real()
    92ns     2rv per d += r.RealU()
   100ns     2rv per d += r.RealN()
    94ns     2rv per d += r.RealS()
    98ns     2rv per d += r.RealO()
   161ns     4rv per d += r.RealC()
   139ns   2.5rv per d += r.Float()
   142ns   2.5rv per d += r.FloatU()
   164ns     3rv per d += r.FloatN()
    90ns     2rv per d += r.Real<double>()
    94ns     2rv per d += r.RealU<double>()
   100ns     2rv per d += r.RealN<double>()
    91ns     2rv per d += r.RealS<double>()
   103ns     2rv per d += r.RealO<double>()
   161ns     4rv per d += r.RealC<double>()
   136ns   2.5rv per d += r.Float<double>()
   139ns   2.5rv per d += r.FloatU<double>()
   165ns     3rv per d += r.FloatN<double>()
    94ns     2rv per e += r.Real<long double>()
    96ns     2rv per e += r.RealU<long double>()
   109ns     2rv per e += r.RealN<long double>()
    99ns     2rv per e += r.RealS<long double>()
   102ns     2rv per e += r.RealO<long double>()
   172ns 4.001rv per e += r.RealC<long double>()
   137ns   2.5rv per e += r.Float<long double>()
   144ns   2.5rv per e += r.FloatU<long double>()
   166ns     3rv per e += r.FloatN<long double>()
    52ns     1rv per d += (r.Real<double, 31>())
    48ns     1rv per d += (r.Real<double, 32>())
    91ns     2rv per d += (r.Real<double, 48>())
    94ns     2rv per d += (r.RealC<double, 31>())
   119ns 2.999rv per d += (r.RealC<double, 32>())
   162ns     4rv per d += (r.RealC<double, 48>())
   101ns     2rv per e += (r.Real<long double, 63>())
   175ns     4rv per e += (r.RealC<long double, 63>())
    94ns     2rv per e += (r.Real<long double, 64>())
   169ns     4rv per e += (r.RealC<long double, 64>())
 11242ns    96rv per for (int q = 1; q < 97; ++q) b ^= r.Prob<float>(q/97.0f)
 11633ns    96rv per for (int q = 1; q < 97; ++q) b ^= r.Prob<double>(q/97.0)
 11558ns    96rv per for (int q = 1; q < 97; ++q) b ^= r.Prob<long double>(q/97.0L)
  4968ns 126.7rv per for (int q = 1; q < 97; ++q) b ^= r.Prob<int>(q, 97UL)
  3187ns    96rv per for (int q = 1; q < 97; ++q) b ^= r.Prob<int>(q*ll, 97UL*ll)
    71ns     1rv per b ^= r.Prob<float>(.2857142857142857f)
    41ns 1.143rv per b ^= r.Prob<long>(2,7)
    71ns     1rv per b ^= r.Prob<float>(5.0f/17.0f)
    71ns     1rv per b ^= r.Prob<double>(5.0/17.0)
    72ns     1rv per b ^= r.Prob<long double>(5.0L/17.0L)
    80ns 1.883rv per b ^= r.Prob<long>(5,17)
    32ns     1rv per b ^= r.Prob<long>(5*mm,17*mm)
   266ns 2.739rv per f += r.Normal<float>()
   383ns 5.477rv per d += r.Normal<double>()
   383ns 5.476rv per d += r.Normal()
   422ns 5.476rv per e += r.Normal<long double>()
*/
