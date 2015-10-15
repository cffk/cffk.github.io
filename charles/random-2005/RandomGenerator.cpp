// RandomGenerator.cpp
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

#include "RandomGenerator.hpp"

namespace {
  char rcsid[] = "$Id: RandomGenerator.cpp 6149 2006-04-27 20:11:56Z ckarney $";
  char h_rcsid[] = RCSID_RANDOMGENERATOR_H;
}

/**
 * \file RandomGenerator.cpp
 * Code for RandomGenerator
 *****************************************************************************/

#include <limits>
#include <sstream>		// For VectorToString
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

/**
 * Initialize with seed [RandomGenerator::SeedWord()]
 ***********************************************************************/
RandomGenerator::RandomGenerator() {
  Reseed();
}

/**
 * Initialize with seed [n]
 ***********************************************************************/
RandomGenerator::RandomGenerator(unsigned long n) {
  Reseed(n);
}

/**
 * Initialize from a vector of unsigned longs.
 ***********************************************************************/
RandomGenerator::RandomGenerator(const std::vector<unsigned long>& v) {
  Reseed(v);
}

/**
 * Initialize from a string.  See RandomGenerator::Reseed.
 ***********************************************************************/
RandomGenerator::RandomGenerator(const std::string& s) {
  Reseed(s);
}

/**
 * Initialize from an input stream.  See RandomGenerator::Restore.
 ***********************************************************************/
RandomGenerator::RandomGenerator(std::istream& is, bool bin)
  throw(std::ios::failure, std::out_of_range) {
  Restore(is, bin);
}

/**
 * Test equality of two RandomGenerator's.  This test that the seeds
 * match and that they have produced the same number of random
 * numbers.
 ***********************************************************************/
bool RandomGenerator::operator==(const RandomGenerator& r) const throw() {
  // Ensure that the two RandomGenerator's behave the same way.  Note
  // however that the internal states may still be different, e.g.,
  // the following all result in RandomGenerator's which are == (with
  // Count() == 0) but which all have different internal states:
  //
  // RandomGenerator r(0);              m_ptr == N + 1
  // r.StepCount( 1); r.StepCount(-1);  m_ptr == 0, m_rounds ==  0
  // r.StepCount(-1); r.StepCount( 1);  m_ptr == N, m_rounds == -1
  return Count() == r.Count() && m_seed == r.m_seed;
}

/**
 * Test inequality of two RandomGenerator's.  See RandomGenerator::operator==
 ***********************************************************************/
bool RandomGenerator::operator!=(const RandomGenerator& r) const throw() {
  return !operator==(r);
}

/**
 * Set the seed to [seed]
 ***********************************************************************/
void RandomGenerator::Reseed(unsigned long n) {
  syscheck();
  m_seed.resize(1);
  m_seed[0] = n & LONG_MASK;
  m_ptr = N + 1;
}

/**
 * Set the seed to [RandomGenerator::SeedWord()]
 ***********************************************************************/
void RandomGenerator::Reseed() {
  return Reseed(SeedWord());
}

/**
 * Set the seed to v
 ***********************************************************************/
void RandomGenerator::Reseed(const std::vector<unsigned long>& v) {
  syscheck();
  m_seed = v;
  for (std::vector<u32>::iterator n = m_seed.begin();
       n != m_seed.end(); ++n)
    *n &= LONG_MASK;
  m_ptr = N + 1;
}

/**
 * Convert a string into a vector of unsigned long by reading
 * consecutive digits in string.  Thus "[1,2,3]" => [1,2,3];
 * "-0.123e-4" => [0,123,4], etc.  strtoul understands C's notation
 * for octal and hexadecimal, for example "012 10 0xa" => [10,10,10].
 * Reading of a number stops at the first illegal character for the
 * base.  Thus "2006-04-08" => [2006,4,0,8] (i.e., 08 becomes two
 * numbers).  Note that input numbers greater then U32_MAX overflow to
 * U32_MAX, which probably will result in the number being interpreted
 * as LONG_MASK.
 ***********************************************************************/
std::vector<unsigned long>
RandomGenerator::StringToVector(const std::string& s) {
  std::vector<unsigned long> v(0);
  const char* c = s.c_str();
  char* q;
  std::string::size_type p = 0;
  while (true) {
    p = s.find_first_of("0123456789", p);
    if (p == std::string::npos)
      break;
    v.push_back(std::strtoul(c + p, &q, 0) & LONG_MASK);
    p = q - c;
  }
  return v;
}

/**
 * Set the seed from the string s using RandomGenerator::StringToVector.
 ***********************************************************************/
void RandomGenerator::Reseed(const std::string& s) {
  m_seed = StringToVector(s);
  m_ptr = N + 1;
}

/**
 * Convert a vector into a string suitable for printing or as an argument
 * for RandomGenerator::Reseed.
 ***********************************************************************/
std::string
RandomGenerator::VectorToString(const std::vector<unsigned long>& v) {
  std::ostringstream os;
  os << "[";
  for (std::vector<u32>::const_iterator n = v.begin();
       n != v.end(); ++n) {
    if (n != v.begin())
      os << ",";
    // Mask with LONG_MASK in case this is called by user.
    os << (*n & LONG_MASK);
  }
  os << "]";
  return os.str();
}

/**
 * Format the current seed suitable for printing.
 ***********************************************************************/
std::string RandomGenerator::SeedString() const {
  return VectorToString(m_seed);
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
// are [a], [a, 0], [a, 0, 0].  The variable RandomGenerator::Seed is a const
// reference to this seed vector.  In addition, the method PrintSeed()
// is provided to print a human readable representation of the seed
// vector.


void RandomGenerator::Init() throw () {
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
  u32 q = m_state[N - 1] ^ m_state[M - 1], s = q >> 31;
  q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;
  m_state[0] = m_state[0] & UPPER_MASK | q & LOWER_MASK;

  m_rounds = -1;
  m_ptr = N;
}

void RandomGenerator::syscheck() {
  // Check that the assumptions made about the capabilities of the
  // number system are valid.

  // Compile-time checks on integer types
  MT_STATIC_ASSERT(std::numeric_limits<u32>::radix == 2 &&
		   std::numeric_limits<u64>::radix == 2);

  MT_STATIC_ASSERT(std::numeric_limits<u32>::digits >= W &&
		   std::numeric_limits<u32>::digits <=
		   std::numeric_limits<u64>::digits &&
		   std::numeric_limits<u64>::digits == 2 * W);
}

/**
 * Return a word of more or less random data suitable for seeding the
 * RNG.  This is obtained by combining data from /dev/urandom, clock,
 * gettimeofday, time, and getpid to provide a reasonable random word of
 * data for feeding into Reseed.  Selectively multiply some of these
 * sources by primes to avoid cancellation effects.
 ***********************************************************************/
unsigned long RandomGenerator::SeedWord() {
  //

  u32 t = 0;
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
  t ^= static_cast<u32>(clock())*1013UL;
  // XOR with PID to prevent correlations between jobs started close
  // together on the same node.  Multiply by a different prime to avoid
  // correlations with the time (e.g., if jobs are started at regular
  // intervals so that both the PID and the time are arithmetic
  // progressions).
  t ^= static_cast<u32>(getpid())*10000019UL;
#if !WINDOWS
  timeval tv;
  if (gettimeofday(&tv, NULL) == 0)
    // XOR with microsec time.  Multiply secs by a prime, in case
    // gettimeofday lamely returns zero for the usecs.
    t ^= static_cast<u32>(tv.tv_sec)*1000003UL
      + static_cast<u32>(tv.tv_usec);
#else
  LARGE_INTEGER taux;
  if (QueryPerformanceCounter((LARGE_INTEGER *)&taux))
    t ^= static_cast<u32>(taux.HighPart*1000003UL) +
      static_cast<u32>(taux.LowPart);
#endif
  else
    // XOR with plain time as a fall back
    t ^= static_cast<u32>(time(NULL));

  return t & LONG_MASK;
}


/**
 * Step the generator forward n steps.  n can be negative.
 ***********************************************************************/
void RandomGenerator::StepCount(long long n) throw() {
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

// Save and restore.

#if WINDOWS
#define htonl(x) ((x)<<24 | ((x)&0xff00UL)<<8 | ((x)>>8)&0xff00UL | (x)>>24)
#define ntohl(x) htonl(x)
// Require an unsigned type exactly 4 bytes long.
#define uint32_t u32
#endif
void RandomGenerator::Write32(std::ostream& os, bool bin, int cnt, u32 x)
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

RandomGenerator::u32 RandomGenerator::Read32(std::istream& is, bool bin)
  throw(std::ios::failure) {
  if (bin) {
    uint32_t n;
    is.read(reinterpret_cast<char *>(&n), sizeof(n));
    return u32(ntohl(n));	// Convert to host order
  } else {
    u32 n;
    is >> std::ws >> n;
    return n;
  }
}
#undef htonl
#undef ntohl
#undef uint32_t

/**
 * Save the state of the RandomGenerator to an output stream.  Format is
 * a sequence of unsigned 32-bit integers written either in decimal (bin
 * false, text format) or in network order with most significant byte
 * first (bin true, binary format).  Data consists of:
 *  - VERSION (1 word)
 *  - count of following words (1 word)
 *  - m_ptr (1 word)
 *  - m_seed (m_seed.size() words)
 *  - if m_ptr <= N, m_rounds (2 words)
 *  - if m_ptr <= N, m_state (N words)
 *
 * Shortest possible saved result consists of 3 words:
 *  - VERSION = MTr1
 *  - count = 1
 *  - m_ptr = N + 1
 *
 * This corresponds to Seed() = [] and Count() = 0.
 ***********************************************************************/
void RandomGenerator::Save(std::ostream& os, bool bin) const throw(std::ios::failure) {
  int c = 0;
  Write32(os, bin, c++, VERSION);
  // Write count of words that follow (not including the count itself)
  u32 num = 1 +			// m_ptr
    static_cast<u32>(m_seed.size()) + // Seed
    (m_ptr > N ? 0UL : 2UL + N); // m_rounds + m_state
  Write32(os, bin, c++, num);
  Write32(os, bin, c++, m_ptr);
  for (std::vector<u32>::const_iterator n = m_seed.begin();
       n != m_seed.end(); ++n)
    Write32(os, bin, c++, *n);
  if (m_ptr <= N) {
    u32 t = static_cast<u32>(m_rounds >> W) & LONG_MASK;
    Write32(os, bin, c++, t);
    t = static_cast<u32>(m_rounds) & LONG_MASK;
    Write32(os, bin, c++, t);
    for (unsigned i = 0; i < N; ++i)
      Write32(os, bin, c++, m_state[i]);
  }
}

/**
 * Restore the state of the RandomGenerator from an input stream.  If
 * bin, read in binary, else use text format.  See documentation of
 * RandomGenerator::Save for the format.  Include as much error
 * checking as possible here to make sure the input has not been
 * corrupted.
 ***********************************************************************/
void RandomGenerator::Restore(std::istream& is, bool bin) throw(std::ios::failure,
						       std::out_of_range) {
  u32 t = Read32(is, bin);
  if (!(t == VERSION))
    throw std::out_of_range("RandomGenerator: Unknown version");
  t = Read32(is, bin);		// Count of words
  m_ptr = Read32(is, bin);
  // Don't need to worry about sign extension because m_ptr is unsigned.
  if (m_ptr > N + 1)
    throw std::out_of_range("RandomGenerator: Invalid pointer");
  if (t < 1 + (m_ptr > N ? 0UL : 2UL + N))
    throw std::out_of_range("RandomGenerator: Invalid word count");
  t -= 1 + (m_ptr > N ? 0UL : 2UL + N); // Seed size
  m_seed.resize(t);
  for (std::vector<u32>::iterator n = m_seed.begin();
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
      throw std::out_of_range("RandomGenerator: All-zero state");

    // There are only W*(N-1) + 1 = 19937 independent bits of state.
    // Thus the low W-1 bits of m_state[0] are derivable from the other
    // bits in m_state.  Verify that the restored bits are consistent.
    // This is a good way of catching a binary save to a text file on a
    // Windows machine (which may insert bogus line ending characters).
    u32 q = m_state[N - 1] ^ m_state[M - 1], s = q >> 31; 
    q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;
    if ((q ^ m_state[0]) & LOWER_MASK)
      throw std::out_of_range("RandomGenerator: Invalid state");
  }
}

// A C-program for RandomGenerator, with initialization improved 2002/1/26.
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
void RandomGenerator::init_genrand(u32 s) throw()
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
void RandomGenerator::init_by_array(const std::vector<u32>& init_key) throw()
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
// The "state" does NOT depends on the low bits of m_state[0].
// However, after calls to Init(), Reload() and Unload(), m_state[0]
// needs to contain the right whole word result since it's used (after
// tempering) as one of the outputs of Ran().
//
// Here input is I, J = I + 1, K = I + M; output is I = I + N (mod N)

#define MT_STEP(I, J, K) {					\
    m_state[I] = m_state[K] ^					\
      (m_state[J] & 1UL ? MATRIX_A : 0UL) ^			\
      (m_state[I] & UPPER_MASK | m_state[J] & LOWER_MASK) >> 1;	\
  }

void RandomGenerator::Reload() throw() {

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
    u32 q = m_state[J] ^ m_state[K], s = q >> 31;	\
    q = (q ^ (s ? MATRIX_A : 0UL)) << 1 | s;		\
    m_state[I] = p & UPPER_MASK | q & LOWER_MASK;	\
    p = q;						\
  }

void RandomGenerator::Unload() throw() {

  // This ONLY uses high bit of m_state[0]
  u32 p = m_state[0];
  // Fix low bits of m_state[0] and compute y[-1]
  int kk = N;              MT_REVSTEP(kk - N, kk - 1, kk + M - 1 - N); --kk;
  for (; kk > N - M; --kk) MT_REVSTEP(kk, kk - 1, kk + M - 1 - N);
  for (; kk > 0    ; --kk) MT_REVSTEP(kk, kk - 1, kk + M - 1);
  kk = 0;                  MT_REVSTEP(kk, kk - 1 + N, kk + M - 1);
  m_ptr = N;
  --m_rounds;
}
#undef MT_REVSTEP
