// RandomGenerator.hpp
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

#if !defined(RANDOMGENERATOR_H)
#define RANDOMGENERATOR_H

#define RCSID_RANDOMGENERATOR_H "$Id: RandomGenerator.hpp 6149 2006-04-27 20:11:56Z ckarney $"

/**
 * \file RandomGenerator.hpp
 * Header for RandomGenerator
 *****************************************************************************/

#include <iostream>
#include <stdexcept>
#include <vector>

#if defined(_MSC_VER)
#define WINDOWS 1
#pragma warning (disable: 4290)
#else
#define WINDOWS 0
#endif

#if defined(__sparc)
#define SUN 1
#else
#define SUN 0
#endif

// A simple compile-time error checker.
#define MT_STATIC_ASSERT(cond) { enum{ STATIC_ASSERT_ERROR = 1/int(cond) }; }

/**
 * \brief A generator of random bits.
 *
 * This provides an interface to Mersenne Twister random number
 * generator, MT19937.  See\n
 * Makoto Matsumoto <m-mat@math.sci.hiroshima-u.ac.jp> and Takuji
 * Nishimura,\n Mersenne Twister: A 623-Dimensionally Equidistributed
 * Uniform Pseudo-Random Number Generator,\n ACM TOMACS 8, 3-30 (1998)
 *
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html\n
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/MT2002/emt19937ar.html
 *
 * Interface routines written by Charles Karney <charles@karney.com>
 * licensed under the LGPL.  For documentation, see\n
 * http://charles.karney.info/random/
 ***********************************************************************/
class RandomGenerator {
protected:
  enum {
    /**
     * The number of random bits in a word.
     ***********************************************************************/
    W = 32
  };
  /**
   * A type large enough to hold W bits.
   ***********************************************************************/
  typedef unsigned long u32;
  /**
   * A type large enough to hold 2*W bits.
   ***********************************************************************/
  typedef unsigned long long u64;
  /**
   * A mask to give the low W bits in a word.
   ***********************************************************************/
  static const u32 LONG_MASK = 0xffffffffUL;

  // A "raw" random number with W bits in [0, 2^W)
  u32 Ran() throw();
  // A "raw" random number with 2*W bits in [0, 2^(2*W)).
  u64 Ran2() throw();

private:
  // Constants
  enum { N = 624,		// Long lag
	 M = 397,		// Short lag
	 longsperline = 72/11	// how many longs per line of output
  };
  static const u32 VERSION = 0x4d547231UL, // "MTr1"
    MATRIX_A   = 0x9908b0dfUL,
    UPPER_MASK = 0x80000000UL,
    LOWER_MASK = 0x7fffffffUL;

  // Data
  u32 m_state[N];		// the array for the state vector
  unsigned m_ptr;		// index of next rv
  long long m_rounds;		// how many times has Reload() been called
  std::vector<u32> m_seed;	// the seed vector

public:

  // Setting the seed via the constructor.

  // Set seed to [seed]
/**
 * Initialize with seed [n]
 ***********************************************************************/
  explicit RandomGenerator(unsigned long n);

  RandomGenerator();
  // Set seed to the vector v
  explicit RandomGenerator(const std::vector<unsigned long>& v);
  // Set seed from string
  explicit RandomGenerator(const std::string& s);

  // Resetting the seed via RandomGenerator::Reseed(...)
  // There are parallel routines to reseed an already instantiated RNG:

  // Set seed to [s]
  void Reseed(unsigned long n);
  // Set seed to [RandomGenerator::SeedWord()]
  void Reseed();
  // Set seed to the vector v
  void Reseed(const std::vector<unsigned long>& v);
  // Set seed from string
  void Reseed(const std::string& s);

  // Return a word of more or less random data suitable for seeding the
  // RandomGenerator.  This is obtained by reading /dev/urandom on Linux
  // and combining this with the value the microsoft clock.
  static unsigned long SeedWord();

/**
 * Return the seed vector (read-only)
 ***********************************************************************/
  const std::vector<unsigned long>& Seed() const throw() { return m_seed; }

  // A string representation of the seed vector
  std::string SeedString() const;

  static std::string VectorToString(const std::vector<unsigned long>& v);
  static std::vector<unsigned long> StringToVector(const std::string& s);

  // Comparing RandomGenerator's
  bool operator==(const RandomGenerator& r) const throw();
  bool operator!=(const RandomGenerator& r) const throw();

  // Writing and reading from a stream

  // Save the full state of the RNG to stream.
  void Save(std::ostream& os, bool bin = true) const throw(std::ios::failure);

  // Restore the full state of the RNG.
  void Restore(std::istream& is, bool bin = true) throw(std::ios::failure,
							std::out_of_range);

  // Or you can initialize the RNG from a stream.
  explicit RandomGenerator(std::istream& is, bool bin = true)
  throw(std::ios::failure, std::out_of_range);

  // Management

  // Return the number of random numbers used.
  long long Count() const throw();

  // Step generator forward (or backward) so that the new Count() is n.
  void SetCount(long long n) throw();

  // Step the generator forward (or backward) by n
  void StepCount(long long n) throw();

private:
  // Compute initial state from seed
  void Init() throw();
  // Advance state by N steps
  void Reload() throw();
  // Back up state by N steps
  void Unload() throw();
  // Original scalar seed routine
  void init_genrand(u32 s) throw();
  // Original vector seed routine
  void init_by_array(const std::vector<u32>& init_key) throw();
  // Helper for Integer(n).  A random unsigned in [0, n)
  template<typename T> T Unsigned(T n) throw();

  // One word write
  static void Write32(std::ostream& os, bool bin, int count, u32 x)
    throw(std::ios::failure);
  // One word read
  static u32 Read32(std::istream& is, bool bin) throw(std::ios::failure);

  static void syscheck();
};

/**
 * Return 32 bits of randomness
 ***********************************************************************/
inline RandomGenerator::u32 RandomGenerator::Ran() throw() {
  if (m_ptr >= N)
    Reload();

  u32 y = m_state[m_ptr++];

  // Tempering
  y ^= (y >> 11);
  y ^= (y <<  7) & 0x9d2c5680UL;
  y ^= (y << 15) & 0xefc60000UL;
  y ^= (y >> 18);

  return y;
}

/**
 * Return 64 bits of randomness
 ***********************************************************************/
inline RandomGenerator::u64 RandomGenerator::Ran2() throw() {
  u64 x = Ran();
  return (x << W) ^ static_cast<u64>(Ran());
}

/**
 * Return the number of random numbers used.  This needs to return a
 * long long result since it can reasonably exceed 2^31.
 ***********************************************************************/
inline long long RandomGenerator::Count() const throw() {
  return m_ptr == N + 1 ? 0 : m_rounds * N + m_ptr;
}

/**
 * Step the generator forwards of backwarks so that the value returned
 * by RandomGenerator::Count() is n
 ***********************************************************************/
inline void RandomGenerator::SetCount(long long n) throw() {
  StepCount(n - Count());
}

/**
 * \relates RandomGenerator
 * Write the state of the generator to stream os as text
 ***********************************************************************/
inline std::ostream& operator<<(std::ostream& os, const RandomGenerator& r) {
  r.Save(os, false);
  return os;
}

/**
 * \relates RandomGenerator
 * Read the state of the generator from stream is as text
 ***********************************************************************/
inline std::istream& operator>>(std::istream& is, RandomGenerator& r) {
  r.Restore(is, false);
  return is;
}

#endif // RANDOMGENERATOR_H
