// Random.cpp
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

#include "Random.hpp"
#include <sstream>		// For SeedString
#include <cassert>		// For checks in Init
#include <fstream>		// For SeedWord reading /dev/urandom
#include <time.h>		// For SeedWord calling time()
#if !WINDOWS
#include <sys/time.h>		// For SeedWord calling gettimeofday
#include <unistd.h>		// For SeedWord calling getpid()
#include <netinet/in.h>		// For Save/Restore
#else
#include <windows.h>		// For SeedWord calling high prec timer
#include <winbase.h>
#include <process.h>		// For SeedWord calling getpid()
#define getpid _getpid
#endif

namespace {
  char rcsid[] = "$Id: Random.cpp 6144 2006-04-25 13:04:00Z ckarney $";
  char h_rcsid[] = RCSID_RANDOM_H;
}

// Initialize Global with a fixed (simple) seed.

Random Random::Global(0);

Random::Random() {
  Reseed();
}

Random::Random(ulong s) {
  Reseed(s);
}

Random::Random(const std::vector<ulong>& v) {
  Reseed(v);
}

Random::Random(const std::string& s) {
  Reseed(s);
}

Random::Random(std::istream& is, bool bin)
  throw(std::ios::failure, std::out_of_range) {
  Restore(is, bin);
}

bool Random::operator==(const Random& r) const throw() {
  // Ensure that the two Random's behave the same way.  Note however
  // that the internal states may still be different, e.g., the
  // following all result in Random's which are == (with Count() == 0)
  // but which all have different internal states:
  //
  // Random r(0);                                   m_ptr == N + 1
  // Random r(0); r.StepCount(1); r.StepCount(-1);  m_ptr == 0, m_rounds ==  0
  // Random r(0); r.StepCount(-1); r.StepCount(1);  m_ptr == N, m_rounds == -1
  return Count() == r.Count() && m_seed == r.m_seed;
}

bool Random::operator!=(const Random& r) const throw() {
  return !operator==(r);
}

// Set the seed from a single word.
void Random::Reseed(ulong seed) {
  syscheck();
  m_seed.resize(1);
  m_seed[0] = seed & LONG_MASK;
  m_ptr = N + 1;
}

// Same but generate the single word from SeedWord()
void Random::Reseed() {
  return Reseed(SeedWord());
}

// Set the seed from a vector
void Random::Reseed(const std::vector<ulong>& v) {
  syscheck();
  m_seed = v;
  for (std::vector<ulong>::iterator n = m_seed.begin();
       n != m_seed.end(); ++n)
    // Needed so that SeedString prints a canonical representation
    *n &= LONG_MASK;
  m_ptr = N + 1;
}

// Set seed from string.  Construct vector from consecutive digits in
// string.  Thus "[1,2,3]" => [1,2,3]; "-0.123e-4" => [0,123,4], etc.
// strtoul understands C's notation for octal and hexadecimal, for
// example "012 10 0xa" => [10,10,10].  Reading of a number stops at the
// first illegal character for the base.  Thus "2006-04-08" =>
// [2006,4,0,8] (i.e., 08 becomes two numbers).  Note that input numbers
// greater then ULONG_MAX overflow to ULONG_MAX, which probably will
// result in the number being interpreted as LONG_MASK.
void Random::Reseed(const std::string& s) {
  m_seed.clear();
  const char* c = s.c_str();
  char* q;
  std::string::size_type p = 0;
  while (true) {
    p = s.find_first_of("0123456789", p);
    if (p == std::string::npos)
      break;
    m_seed.push_back(std::strtoul(c + p, &q, 0) & LONG_MASK);
    p = q - c;
  }
  m_ptr = N + 1;
}

std::string Random::SeedString(const std::vector<ulong>& v) {
  std::ostringstream os;
  os << "[";
  for (std::vector<ulong>::const_iterator n = v.begin();
       n != v.end(); ++n) {
    if (n != v.begin())
      os << ",";
    os << *n;
  }
  os << "]";
  return os.str();
}

std::string Random::SeedString() const {
  return SeedString(m_seed);
}

// Seed the state vector.

// The original C interface provided two seed-setting interfaces: (a)
// init_genrand, which took a single unscaled long as argument and (b)
// init_by_array(m_seed), which took an array (length > 0)of unscaled
// longs as an argument.  Thus, the set of allowed seeds was {a, [a],
// [a, b], [a, b, c], ...}, where a and [a] were distinct.  But this
// then presents a confusing interface to the user.  In addition, it's
// not clear how best to report back the seed to the user.

// In this C++ interface the "vector" initializer, init_by_array, is
// used even with a single seed word.  The "scalar" initializer is
// invoked only in the case of an empty seed vector (an the argument
// provided in this case is the fall back seed used by the C interface).
// Thus the set of allowable seeds is {[], [a], [a, b], [a, b, c], ...}
// which is easily and unambiguously represented by the STL vector
// container.  Note also that [], [0], [0, 0], etc. are all distinct as
// are [a], [a, 0], [a, 0, 0].  The variable Random::Seed is a const
// reference to this seed vector.  In addition, the method PrintSeed()
// is provided to print a human readable representation of the seed
// vector.


void Random::Init() throw () {
  if (m_seed.size())
    // If m_seed is not empty, call the "vector" initializer
    init_by_array(m_seed);
  else
    // else use a default seed with the "scalar" initializer
    init_genrand(5489UL);

  // This sets the low 31 bits of m_state[0] consistent with the rest
  // of the state.  Needed to handle SetCount(-N); Ran();
  // immediately following reseeding.  This wasn't required in the
  // original code because a Reload was always done first.
  ulong q = m_state[N - 1] ^ m_state[M - 1], s = q >> 31;
  q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;
  m_state[0] = m_state[0] & UPPER_MASK | q & LOWER_MASK;

  m_rounds = -1;
  m_ptr = N;
}

// Some checking of real types
#if defined(NDEBUG)
#define MT_CHECKREAL(T)						\
  MT_STATIC_ASSERT(std::numeric_limits<T>::radix == 2 &&	\
		   std::numeric_limits<T>::digits > 0);
#else
#define MT_CHECKREAL(T)							\
  MT_STATIC_ASSERT(std::numeric_limits<T>::radix == 2 &&		\
		   std::numeric_limits<T>::digits > 0);			\
  assert((T)(1) - pow2<T>(-std::numeric_limits<T>::digits) < 1);	\
  if (MT_HASDENORM(T)) {						\
    int e = std::numeric_limits<T>::digits -				\
      std::numeric_limits<T>::min_exponent;				\
    assert(std::pow((T)(2), -e/2) * std::pow((T)(2), -e + e/2) > 0);	\
  } else {								\
    assert(std::pow((T)(2),						\
		    std::numeric_limits<T>::min_exponent - 1) > 0);	\
  }
#endif
void Random::syscheck() {
  // Check that the assumptions made about the capabilities of the
  // number system are valid.

  // Compile-time checks on integer types
  MT_STATIC_ASSERT(std::numeric_limits<ulong>::radix == 2 &&
		   std::numeric_limits<ulong2>::radix == 2);

  MT_STATIC_ASSERT(std::numeric_limits<ulong>::digits >= W &&
		   std::numeric_limits<ulong>::digits <=
		   std::numeric_limits<ulong2>::digits &&
		   std::numeric_limits<ulong2>::digits == 2 * W);

  // Compile-time checks on real type
  MT_STATIC_ASSERT(std::numeric_limits<float>::digits <=
		   std::numeric_limits<double>::digits &&
		   std::numeric_limits<double>::digits <=
		   std::numeric_limits<long double>::digits);

  MT_STATIC_ASSERT(std::numeric_limits<long double>::digits ==
		   MT_LONGDOUBLEPREC);

  // More compile-time and run-time checks on real types
  MT_CHECKREAL(float);
  MT_CHECKREAL(double);
  MT_CHECKREAL(long double);

#if MT_POWERTABLE
  // Checks on power2
  // Make sure table hasn't underflowed
  MT_STATIC_ASSERT(minpow >= std::numeric_limits<float>::min_exponent -
		   (MT_HASDENORM(float) ?
		    std::numeric_limits<float>::digits : 1));
  MT_STATIC_ASSERT(maxpow >= minpow + 1);
  assert(pow2<float>(minpow) > 0.0f);
  assert(2.0f * pow2<float>(minpow) == pow2<float>(minpow + 1));
  // Needed by Real<long double>()
  MT_STATIC_ASSERT(minpow <= -std::numeric_limits<long double>::digits);
  MT_STATIC_ASSERT(maxpow >= W); // Needed by Prob(T p)
#endif
}
#undef MT_CHECKREAL

Random::ulong Random::SeedWord() {
  // Combine data from /dev/urandom, clock, gettimeofday, time to
  // provide a reasonable random word of data for feeding into Reseed.
  // Selectively multiply some of these sources by primes to avoid
  // cancellation effects.

  ulong t = 0;
  // Linux has /dev/urandom to initialize the seed randomly.  (Use
  // /dev/urandom instead of /dev/random because it does not block.)
  std::ifstream f("/dev/urandom", std::ios::binary | std::ios::in);
  if (f.good()) {
    f.read(reinterpret_cast<char *>(&t), sizeof(t));
    f.close();
  }
  // XOR with processor time.  Multiply by a prime to prevent unwanted
  // correlation with tv_usec from gettimeofday.  This probably doesn't
  // provide a whole lot of new information, since clock() has a 10ms
  // granualarity and it may be consistently small near the beginning of
  // a program.
  t ^= static_cast<ulong>(clock())*1013UL;
  // XOR with PID to prevent correlations between jobs started close
  // together on the same node.  Multiply by a different prime to avoid
  // correlations with the time (e.g., if jobs are started at regular
  // intervals so that both the PID and the time are arithmetic
  // progressions).
  t ^= static_cast<ulong>(getpid())*10000019UL;
#if !WINDOWS
  timeval tv;
  if (gettimeofday(&tv, NULL) == 0)
    // XOR with microsec time.  Multiply secs by a prime, in case
    // gettimeofday lamely returns zero for the usecs.
    t ^= static_cast<ulong>(tv.tv_sec)*1000003UL
      + static_cast<ulong>(tv.tv_usec);
#else
  LARGE_INTEGER taux;
  if (QueryPerformanceCounter((LARGE_INTEGER *)&taux))
    t ^= static_cast<ulong>(taux.HighPart*1000003UL) +
      static_cast<ulong>(taux.LowPart);
#endif
  else
    // XOR with plain time as a fall back
    t ^= static_cast<ulong>(time(NULL));

  return t & LONG_MASK;
}

template<typename T> T Random::Normal() throw() {
  // Ratio method for normal deviates (mean 0, variance 1).
  // See Knuth, TAOCP, Vol II, Sec. 3.4.1, Algorithm R.

  // Original citation is A. J. Kinderman, J. F. Monahan, Computer
  // Generation of Random Variables Using the Ratio of Uniform Deviates,
  // ACM TOMS 3, 257-260 (1977)

  // Improved "quadratic" bounds given by J. L. Leva, A Fast Normal
  // Random Number Generator, ACM TOMS 18, 449-453 and 454-455 (1992).
  // Log is evaluated 1.369 times per normal deviate with no bounds,
  // 0.232 times with Knuth's bounds, and 0.012 times with the quadratic
  // bounds.  The speed up in about 18%.  Time is approx 0.3 us per
  // deviate (1GHz machine, optimized)

  // Normal() is symmetric about zero and is nonzero.  The max float
  // result is 4.5*sqrt(8/e) = 7.72; [u,v] = [1,sqrt(8/e)*4.5]/2^24; at
  // max exp(-x^2/2)/sqrt(2*pi) = 4.6 * 10^-14

  // N.B. These constants can be regarded as "exact", so that the same
  // number of sig. figs. are used in all versions.  (They serve the
  // "bracket" the real boundary specified by the log expression.)
  const T m = T(1.7156),	// sqrt(8/e) (rounded up)
    s  = T( 0.449871),		// Constants from Leva
    t  = T(-0.386595),
    a  = T( 0.19600 ),
    b  = T( 0.25472 ),
    r1 = T( 0.27597 ),
    r2 = T( 0.27846 );
  T u, v, Q;
  do {			 // This loop is executed 1.369 times on average
    // Pick point P = (u, v)
    u = RealUZ<T>();		// Avoid singularity at u = 0
    v = m * RealSZ<T>();	// Symmetric about 0 and avoid 0.
    // Compute quadradric form Q
    T x = u - s;
    T y = (v < 0 ? -v : v) - t; // Sun has no long double abs!
    Q = x*x + y * (a*y - b*x);
  } while ( Q >= r1 &&		// accept P if Q < r1
	    ( Q > r2 ||		// reject P if Q > r2
	      v*v > - 4 * u*u * std::log(u) ) ); // accept P if v^2 <= ...
  return v / u;			// return the slope of P (note u != 0)
}

template float Random::Normal<float>() throw();
template double Random::Normal<double>() throw();
template long double Random::Normal<long double>() throw();

// Step the generator forward
void Random::StepCount(long long n) throw() {
  if (n == 0)
    return;
  if (m_ptr == N + 1)
    Init();
  const long long ncount = n + Count(); // new Count()
  long long nrounds = ncount / N;
  int nptr = static_cast<int>(ncount - nrounds * N);
  if (nptr < 0 || n > 0 && nptr == 0) {
    // In the boundary cases, leave m_ptr = N for n > 0, and m_ptr = 0
    // for n < 0.  In each case we avoid doing one (potentially
    // unneeded) called to Reload/Unload.
    --nrounds;
    nptr += N;
  }
  if (n > 0) {
    while (m_rounds < nrounds)
      Reload();
  } else {
    while (m_rounds > nrounds)
      Unload();
  }
  m_ptr = nptr;
}

// Save and restore.  Format is a sequence of unsigned 32-bit integers
// written either in decimal (text format) or in network order with most
// significant byte first (binary format).  Data consists of
//
//     VERSION (1 word)
//     count of following words (1 word)
//     m_ptr (1 word)
//     m_seed (m_seed.size() words)
//     [if m_ptr <= N] m_rounds (2 words)
//     [if m_ptr <= N] m_state (N words)
//
// Shortest possible saved result consists of 3 words:
//     VERSION = MTr1
//     count = 1
//     m_ptr = N + 1
// This corresponds to Seed() = [] and Count() = 0.


#if WINDOWS
#define htonl(x) ((x)<<24 | ((x)&0xff00UL)<<8 | ((x)>>8)&0xff00UL | (x)>>24)
#define ntohl(x) htonl(x)
// Require an unsigned type exactly 4 bytes long.
#define uint32_t ulong
#endif
void Random::Write32(std::ostream& os, bool bin, int cnt, ulong x)
  throw(std::ios::failure) {
  if (bin) {
    // Verify properties of uint32_t.
    MT_STATIC_ASSERT(sizeof(uint32_t) == 4 &&
		     !std::numeric_limits<uint32_t>::is_signed);
    const uint32_t n = htonl(uint32_t(x)); // Convert to network order
    os.write(reinterpret_cast<const char *>(&n), sizeof(n));
  } else {
    // No spacing before or after
    if (cnt > 0)
      // Newline every longsperline longs
      os << (cnt % longsperline ? " " : "\n");
    os << x;
  }
}

Random::ulong Random::Read32(std::istream& is, bool bin)
  throw(std::ios::failure) {
  if (bin) {
    uint32_t n;
    is.read(reinterpret_cast<char *>(&n), sizeof(n));
    return ulong(ntohl(n));	// Convert to host order
  } else {
    ulong n;
    is >> std::ws >> n;
    return n;
  }
}
#undef htonl
#undef ntohl
#undef uint32_t

void Random::Save(std::ostream& os, bool bin) const throw(std::ios::failure) {
  int c = 0;
  Write32(os, bin, c++, VERSION);
  // Write count of words that follow (not including the count itself)
  ulong num = 1 +		// m_ptr
    static_cast<ulong>(m_seed.size()) +	// Seed
    (m_ptr > N ? 0UL : 2UL + N); // m_rounds + m_state
  Write32(os, bin, c++, num);
  Write32(os, bin, c++, m_ptr);
  for (std::vector<ulong>::const_iterator n = m_seed.begin();
       n != m_seed.end(); ++n)
    Write32(os, bin, c++, *n);
  if (m_ptr <= N) {
    ulong t = static_cast<ulong>(m_rounds >> W) & LONG_MASK;
    Write32(os, bin, c++, t);
    t = static_cast<ulong>(m_rounds) & LONG_MASK;
    Write32(os, bin, c++, t);
    for (unsigned i = 0; i < N; ++i)
      Write32(os, bin, c++, m_state[i]);
  }
}

// Include as much error checking as possible here to make sure the
// input has not been corrupted.

void Random::Restore(std::istream& is, bool bin) throw(std::ios::failure,
						       std::out_of_range) {
  ulong t = Read32(is, bin);
  if (!(t == VERSION))
    throw std::out_of_range("Random: Unknown version");
  t = Read32(is, bin);		// Count of words
  m_ptr = Read32(is, bin);
  // Don't need to worry about sign extension because m_ptr is unsigned.
  if (m_ptr > N + 1)
    throw std::out_of_range("Random: Invalid pointer");
  if (t < 1 + (m_ptr > N ? 0UL : 2UL + N))
    throw std::out_of_range("Random: Invalid word count");
  t -= 1 + (m_ptr > N ? 0UL : 2UL + N); // Seed size
  m_seed.resize(t);
  for (std::vector<ulong>::iterator n = m_seed.begin();
       n != m_seed.end(); ++n)
    *n = Read32(is, bin);
  if (m_ptr <= N) {
    m_rounds = Read32(is, bin);
    // Sign extension in case long long is bigger than 64 bits.
    if (m_rounds >> (W-1))	// Sign bit set
      m_rounds -= 1LL << W;	// Subtract 2^32
    m_rounds = (m_rounds << W) |
      static_cast<unsigned long long>(Read32(is, bin));
    t = 0;
    for (unsigned i = 0; i < N; ++i) {
      m_state[i] = Read32(is, bin);
      t |= m_state[i];
    }
    if (t == 0)
      throw std::out_of_range("Random: All-zero state");

    // There are only W*(N-1) + 1 = 19937 independent bits of state.
    // Thus the low W-1 bits of m_state[0] are derivable from the other
    // bits in m_state.  Verify that the restored bits are consistent.
    // This is a good way of catching a binary save to a text file on a
    // Windows machine (which may insert bogus line ending characters).
    ulong q = m_state[N - 1] ^ m_state[M - 1], s = q >> 31; 
    q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;
    if ((q ^ m_state[0]) & LOWER_MASK)
      throw std::out_of_range("Random: Invalid state");
  }
}

// A C-program for MT19937, with initialization improved 2002/1/26.
// Coded by Takuji Nishimura and Makoto Matsumoto.
//
// Before using, initialize the state by using init_genrand(seed)
// or init_by_array(init_key, key_length).
//
// Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
//
//   1. Redistributions of source code must retain the above copyright
//	notice, this list of conditions and the following disclaimer.
//
//   2. Redistributions in binary form must reproduce the above copyright
//	notice, this list of conditions and the following disclaimer in the
//	documentation and/or other materials provided with the distribution.
//
//   3. The names of its contributors may not be used to endorse or promote
//	products derived from this software without specific prior written
//	permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
// A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT
// OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
// LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Any feedback is very welcome.
// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
// email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)

// #include <stdio.h>

// Period parameters

// #define N 624
// #define M 397
// #define MATRIX_A 0x9908b0dfUL   // constant vector a
// #define UPPER_MASK 0x80000000UL // most significant w-r bits
// #define LOWER_MASK 0x7fffffffUL // least significant r bits

// static unsigned long mt[N]; // the array for the state vector
// static int mti=N+1; // mti==N+1 means mt[N] is not initialized

// initializes mt[N] with a seed
void Random::init_genrand(ulong s) throw()
{
  m_state[0]= s & LONG_MASK;
  for (int k = 1; k < N; ++k) {
    m_state[k] = 1812433253UL * (m_state[k - 1] ^ m_state[k - 1] >> 30) + k;
    // See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier.
    // In the previous versions, MSBs of the seed affect
    // only MSBs of the array mt[].
    // 2002/01/09 modified by Makoto Matsumoto
    m_state[k] &= LONG_MASK;	// for >32 bit machines
  }
}

// initialize by an array with array-length
// init_key is the array for initializing keys
// key_length is its length
// slight change for C++, 2004/2/26
void Random::init_by_array(const std::vector<ulong>& init_key) throw()
{
  init_genrand(19650218UL);
  int key_length = static_cast<int>(init_key.size()), i = 1;
  for (int k = (N > key_length ? N : key_length), j = 0; k; --k) {
    m_state[i] ^= (m_state[i - 1] ^ m_state[i - 1] >> 30) * 1664525UL;
    m_state[i] += init_key[j] + j; // nonlinear
    m_state[i] &= LONG_MASK;	// for WORDSIZE > 32 machines
    ++i;
    ++j;
    if (i >= N) {
      m_state[0] = m_state[N - 1];
      i = 1;
    }
    if (j >= key_length)
      j = 0;
  }
  for (int k = N - 1; k; --k) {
    m_state[i] ^= (m_state[i - 1] ^ m_state[i - 1] >> 30) * 1566083941UL;
    m_state[i] -= i;		// nonlinear
    m_state[i] &= LONG_MASK;	// for WORDSIZE > 32 machines
    ++i;
    if (i >= N) {
      m_state[0] = m_state[N - 1];
      i = 1;
    }
  }
  // MSB is 1; assuring non-zero initial array.  The low 31 bits are
  // irrelevant.
  m_state[0] = UPPER_MASK;
}

// Let y[i] = m_state[i] & UPPER_MASK | m_state[i + 1] & LOWER_MASK,
// then m_state[N] depends on y[0] and m_state[M}.
//
// The "state" does NOT depends on the low bits of m_state[0].  However,
// after calls to Init(), Reload() and Unload(), m_state[0] needs to
// contain the right whole word result since it's used (after tempering)
// as one of the outputs of Ran().
//
// Here input is I, J = I + 1, K = I + M; output is I = I + N (mod N)

#define MT_STEP(I, J, K) {					\
    m_state[I] = m_state[K] ^					\
      (m_state[J] & 1UL ? MATRIX_A : 0UL) ^			\
      (m_state[I] & UPPER_MASK | m_state[J] & LOWER_MASK) >> 1;	\
  }

void Random::Reload() throw() {

  if (m_ptr == N + 1)		// Init() has not been called
    Init();

  // This ONLY uses high bit of m_state[0]
  int kk = 0;
  for (; kk < N - M; ++kk) MT_STEP(kk   , kk + 1    , kk + M    );
  for (; kk < N - 1; ++kk) MT_STEP(kk   , kk + 1    , kk + M - N);
  // Nov kk == N - 1;      MT_STEP(kk   , kk + 1 - N, kk + M - N);
                           MT_STEP(N - 1, 0         ,      M - 1);
  m_ptr = 0;
  ++m_rounds;
}
#undef MT_STEP

// This undoes MT_STEP.  This piece of coding is from revrand() by
// Katsumi Hagita.  See
//
// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/
// MT/VERSIONS/FORTRAN/REVmt19937b.f
//
// The code is cleaned up a little from the Fortran version by getting
// rid of the unnecessary masking by YMASK and by using simpler logic to
// restore the correct value of m_state[0].
//
// y[0] depends on m_state[N] and m_state[M].
//
// Here input is J = I + N - 1, K = I + M - 1, and p = y[I] (only the
// high bits are used); output m_state[I] and p = y[I - 1].

#define MT_REVSTEP(I, J, K) {				\
    ulong q = m_state[J] ^ m_state[K], s = q >> 31;	\
    q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;		\
    m_state[I] = p & UPPER_MASK | q & LOWER_MASK;	\
    p = q;						\
  }

void Random::Unload() throw() {

  // This ONLY uses high bit of m_state[0]
  ulong p = m_state[0];
  // Fix low bits of m_state[0] and compute y[-1]
  int kk = N;              MT_REVSTEP(kk - N, kk - 1, kk + M - 1 - N); --kk;
  for (; kk > N - M; --kk) MT_REVSTEP(kk, kk - 1, kk + M - 1 - N);
  for (; kk > 0    ; --kk) MT_REVSTEP(kk, kk - 1, kk + M - 1);
  kk = 0;                  MT_REVSTEP(kk, kk - 1 + N, kk + M - 1);
  m_ptr = N;
  --m_rounds;
}
#undef MT_REVSTEP

#if MT_POWERTABLE

// Powers of two.  Just use floats here.  As long as there's no overflow
// or underflow these are exact.  In particular they can be cast to
// doubles or long doubles with no error.
const float Random::power2[maxpow - minpow + 1] = {
#if MT_LONGDOUBLEPREC > 64
  // It would be nice to be able to use the C99 notation of 0x1.0p-120
  // for 2^-120 here.
  1/1329227995784915872903807060280344576.f, // 2^-120
  1/664613997892457936451903530140172288.f, // 2^-119
  1/332306998946228968225951765070086144.f, // 2^-118
  1/166153499473114484112975882535043072.f, // 2^-117
  1/83076749736557242056487941267521536.f, // 2^-116
  1/41538374868278621028243970633760768.f, // 2^-115
  1/20769187434139310514121985316880384.f, // 2^-114
  1/10384593717069655257060992658440192.f, // 2^-113
  1/5192296858534827628530496329220096.f, // 2^-112
  1/2596148429267413814265248164610048.f, // 2^-111
  1/1298074214633706907132624082305024.f, // 2^-110
  1/649037107316853453566312041152512.f, // 2^-109
  1/324518553658426726783156020576256.f, // 2^-108
  1/162259276829213363391578010288128.f, // 2^-107
  1/81129638414606681695789005144064.f, // 2^-106
  1/40564819207303340847894502572032.f, // 2^-105
  1/20282409603651670423947251286016.f, // 2^-104
  1/10141204801825835211973625643008.f, // 2^-103
  1/5070602400912917605986812821504.f, // 2^-102
  1/2535301200456458802993406410752.f, // 2^-101
  1/1267650600228229401496703205376.f, // 2^-100
  1/633825300114114700748351602688.f, // 2^-99
  1/316912650057057350374175801344.f, // 2^-98
  1/158456325028528675187087900672.f, // 2^-97
  1/79228162514264337593543950336.f, // 2^-96
  1/39614081257132168796771975168.f, // 2^-95
  1/19807040628566084398385987584.f, // 2^-94
  1/9903520314283042199192993792.f, // 2^-93
  1/4951760157141521099596496896.f, // 2^-92
  1/2475880078570760549798248448.f, // 2^-91
  1/1237940039285380274899124224.f, // 2^-90
  1/618970019642690137449562112.f, // 2^-89
  1/309485009821345068724781056.f, // 2^-88
  1/154742504910672534362390528.f, // 2^-87
  1/77371252455336267181195264.f, // 2^-86
  1/38685626227668133590597632.f, // 2^-85
  1/19342813113834066795298816.f, // 2^-84
  1/9671406556917033397649408.f, // 2^-83
  1/4835703278458516698824704.f, // 2^-82
  1/2417851639229258349412352.f, // 2^-81
  1/1208925819614629174706176.f, // 2^-80
  1/604462909807314587353088.f, // 2^-79
  1/302231454903657293676544.f, // 2^-78
  1/151115727451828646838272.f, // 2^-77
  1/75557863725914323419136.f,	// 2^-76
  1/37778931862957161709568.f,	// 2^-75
  1/18889465931478580854784.f,	// 2^-74
  1/9444732965739290427392.f,	// 2^-73
  1/4722366482869645213696.f,	// 2^-72
  1/2361183241434822606848.f,	// 2^-71
  1/1180591620717411303424.f,	// 2^-70
  1/590295810358705651712.f,	// 2^-69
  1/295147905179352825856.f,	// 2^-68
  1/147573952589676412928.f,	// 2^-67
  1/73786976294838206464.f,	// 2^-66
  1/36893488147419103232.f,	// 2^-65
#endif
  1/18446744073709551616.f,	// 2^-64
  1/9223372036854775808.f,	// 2^-63
  1/4611686018427387904.f,	// 2^-62
  1/2305843009213693952.f,	// 2^-61
  1/1152921504606846976.f,	// 2^-60
  1/576460752303423488.f,	// 2^-59
  1/288230376151711744.f,	// 2^-58
  1/144115188075855872.f,	// 2^-57
  1/72057594037927936.f,	// 2^-56
  1/36028797018963968.f,	// 2^-55
  1/18014398509481984.f,	// 2^-54
  1/9007199254740992.f,		// 2^-53
  1/4503599627370496.f,		// 2^-52
  1/2251799813685248.f,		// 2^-51
  1/1125899906842624.f,		// 2^-50
  1/562949953421312.f,		// 2^-49
  1/281474976710656.f,		// 2^-48
  1/140737488355328.f,		// 2^-47
  1/70368744177664.f,		// 2^-46
  1/35184372088832.f,		// 2^-45
  1/17592186044416.f,		// 2^-44
  1/8796093022208.f,		// 2^-43
  1/4398046511104.f,		// 2^-42
  1/2199023255552.f,		// 2^-41
  1/1099511627776.f,		// 2^-40
  1/549755813888.f,		// 2^-39
  1/274877906944.f,		// 2^-38
  1/137438953472.f,		// 2^-37
  1/68719476736.f,		// 2^-36
  1/34359738368.f,		// 2^-35
  1/17179869184.f,		// 2^-34
  1/8589934592.f,		// 2^-33
  1/4294967296.f,		// 2^-32
  1/2147483648.f,		// 2^-31
  1/1073741824.f,		// 2^-30
  1/536870912.f,		// 2^-29
  1/268435456.f,		// 2^-28
  1/134217728.f,		// 2^-27
  1/67108864.f,			// 2^-26
  1/33554432.f,			// 2^-25
  1/16777216.f,			// 2^-24
  1/8388608.f,			// 2^-23
  1/4194304.f,			// 2^-22
  1/2097152.f,			// 2^-21
  1/1048576.f,			// 2^-20
  1/524288.f,			// 2^-19
  1/262144.f,			// 2^-18
  1/131072.f,			// 2^-17
  1/65536.f,			// 2^-16
  1/32768.f,			// 2^-15
  1/16384.f,			// 2^-14
  1/8192.f,			// 2^-13
  1/4096.f,			// 2^-12
  1/2048.f,			// 2^-11
  1/1024.f,			// 2^-10
  1/512.f,			// 2^-9
  1/256.f,			// 2^-8
  1/128.f,			// 2^-7
  1/64.f,			// 2^-6
  1/32.f,			// 2^-5
  1/16.f,			// 2^-4
  1/8.f,			// 2^-3
  1/4.f,			// 2^-2
  1/2.f,			// 2^-1
  1.f,				// 2^0
  2.f,				// 2^1
  4.f,				// 2^2
  8.f,				// 2^3
  16.f,				// 2^4
  32.f,				// 2^5
  64.f,				// 2^6
  128.f,			// 2^7
  256.f,			// 2^8
  512.f,			// 2^9
  1024.f,			// 2^10
  2048.f,			// 2^11
  4096.f,			// 2^12
  8192.f,			// 2^13
  16384.f,			// 2^14
  32768.f,			// 2^15
  65536.f,			// 2^16
  131072.f,			// 2^17
  262144.f,			// 2^18
  524288.f,			// 2^19
  1048576.f,			// 2^20
  2097152.f,			// 2^21
  4194304.f,			// 2^22
  8388608.f,			// 2^23
  16777216.f,			// 2^24
  33554432.f,			// 2^25
  67108864.f,			// 2^26
  134217728.f,			// 2^27
  268435456.f,			// 2^28
  536870912.f,			// 2^29
  1073741824.f,			// 2^30
  2147483648.f,			// 2^31
  4294967296.f,			// 2^32
};
#endif
