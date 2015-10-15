// RandomSelect.hpp
//
// An implementation of the Walker algorithm for selecting from a finite
// set.
//
// http://charles.karney.info/random/
/*
 * Example:
 *
 *    // Weights for throwing a pair of dice
 *    real wt[] = { 0, 0, 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1 };
 *    std::vector<real> w(wt, wt + sizeof(wt)/sizeof(real));
 *
 *    // Initialize selection
 *    RandomSelect sel;
 *    sel.Init(w);
 *
 *    RandomNumber rand;   // Initialize random numbers
 *
 *    cout << "Throw a pair of dice 100 times:";
 *    for (size_t i = 0; i < 100; i++)
 *        cout << " " << sel(rand);
 *    cout << endl;
 *
 */

#ifndef RANDOMSELECT_H
#define RANDOMSELECT_H

#include "RandomNumber.hpp"
#include <vector>
#include <limits>

#define RCSID_RANDOMSELECT_H "$Id: RandomSelect.hpp 6149 2006-04-27 20:11:56Z ckarney $"

/**
 * \brief Random selection from a discrete set.
 *
 * An implementation of Walker algorithm for selecting from a finite set
 * (following Knuth, TAOCP, Vol 2, Sec 3.4.1.A).  This provides a rapid
 * way of selecting one of several choices depending on a discrete set
 * weights.
 *
 * Original citation is A. J. Walker, An Efficient Method for Generating
 * Discrete Random Variables and General Distributions, ACM TOMS 3,
 * 253-256 (1977).
 *
 * There are two changes here in the setup algorithm as given by Knuth:
 *
 * - The probabilities aren't sorted at the beginning of the setup; nor
 * are they maintained in a sorted order.  Instead they are just
 * partitioned on the mean.  This improves the setup time from O(k^2) to
 * O(k).
 *
 * - Input weights are of type T and intermediate calculations use type
 * S to control roundoff.  In particular if T and S are both integral
 * type, the returned distribution is exact (assuming that the
 * underlying RandomGenerator is exact.)
 *
 * Example:
 * \code

// Weights for throwing a pair of dice
real wt[] = { 0, 0, 1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1 };
std::vector<real> w(wt, wt + sizeof(wt)/sizeof(real));

// Initialize selection
RandomSelect sel;
sel.Init(w);

RandomNumber rand;   // Initialize random numbers

cout << "Throw a pair of dice 100 times:";
for (size_t i = 0; i < 100; i++)
    cout << " " << sel(rand);
cout << endl;

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
    const double x = r.RealS();	// r.RealS() is in the interval (-1/2, 1/2)
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

 * \endcode
 ***********************************************************************/
template<typename T, typename S> class RandomSelect {

public:
  /**
   * Initialize in a cleared state.
   ***********************************************************************/
  RandomSelect() : m_k(0), m_wsum(0), m_wmax(0) {};
  /**
   * Initialize with a weight vector of type T.
   ***********************************************************************/
  RandomSelect(const std::vector<T>& w) {
    Init(w);
  }

  void Init() throw();

  void Init(const std::vector<T>& w)
    throw(std::out_of_range);

  /**
   * Return an index into the weight vector with probability proportional
   * to the weight.  Return 0 if uninitialized.
   ***********************************************************************/
  size_t operator()(RandomNumber& r) const throw() {
    if (m_k == 0)
      return 0;
    size_t K = r.Integer<size_t>(m_k);
    if (std::numeric_limits<S>::is_integer)
      return r.Prob<S>(m_P[K], m_wsum) ? K : m_Y[K];
    else
      return r.Prob<S>(m_P[K]) ? K : m_Y[K];
  }

  /**
   * Return the sum of the weights.
   ***********************************************************************/
  S TotalWeight() const throw() { return m_wsum; }

  /**
   * Return the maximum weight.
   ***********************************************************************/
  T MaxWeight() const throw() { return m_wmax; }

  /**
   * Return the number of choices, i.e., the length of the weight vector.
   ***********************************************************************/
  size_t Choices() const throw() { return m_k; }

private:
  size_t m_k;
  std::vector<S> m_P;
  std::vector<size_t> m_Y;
  S m_wsum;
  T m_wmax;

};

/**
 * Clear the state.
 ***********************************************************************/
template<typename T, typename S>
inline void RandomSelect<T, S>::Init() throw() {
  m_k = 0;
  m_wsum = 0;
  m_wmax = 0;
  m_P.clear();
  m_Y.clear();
}

/**
 * Initialize with a weight vector of type T.  Internal calculations are
 * carried out with type S.  S needs to allow Choices() * MaxWeight() to
 * be represented.  Sensible combinations are
 *
 * - T integer, S integer with digits(S) >= digits(T)
 * - T integer, S real
 * - T real, S real with digits(S) >= digits(T)
 ***********************************************************************/
template<typename T, typename S>
inline void RandomSelect<T, S>::Init(const std::vector<T>& w)
  throw(std::out_of_range) {

  // Disallow T = real, S = integer
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer ||
		   !std::numeric_limits<S>::is_integer);

  // If T and S are the same type, S as precise as T
  MT_STATIC_ASSERT(std::numeric_limits<T>::is_integer !=
		   std::numeric_limits<S>::is_integer ||
		   std::numeric_limits<S>::digits >=
		   std::numeric_limits<T>::digits);
		   
  size_t N = w.size();
  m_k = 0;
  m_wsum = 0;
  m_wmax = 0;

  if (N == 0)
    return;

  // Use S for s and p to minimize (and in some cases eliminate) the
  // accumulation of roundoff errors.

  S s = 0;
  T tmx = 0;

  for (size_t i = 0; i < N; i++) {
    if (std::numeric_limits<T>::is_signed && w[i] < 0)
      throw std::out_of_range("RandomSelect: Negative weight");
    if (S(w[i]) > std::numeric_limits<S>::max() - s)
      throw std::out_of_range("RandomSelect: Overflow");
    s += S(w[i]);
    tmx = std::max(tmx, w[i]);
  }

  if (s <= 0)
    throw std::out_of_range("RandomSelect: Zero total weight");

  if (std::numeric_limits<S>::max()/S(N) < S(tmx))
    throw std::out_of_range("RandomSelect: Overflow");

  m_wsum = s;
  m_wmax = tmx;
  std::vector<S> p(N);
  std::vector<size_t> j(N);
  m_P.resize(N);
  m_Y.resize(N);
  m_k = N;

  // Pointers to the next empty low and high slots
  size_t a = 0;
  size_t b = N - 1;

  // Scale input and store in p and setup index array j.  Note s =
  // mean(p).  We could scale out s here, but the following is exact
  // when w[i] are low integers.
  for (size_t i = 0; i < N; i++) {
    p[i] = S(w[i]) * S(N);
    j[p[i] > s ? b-- : a++] = i;
#if !defined(NDEBUG)
    // Initialize m_P and m_Y with bogus values.  Not necessary, but
    // might be a helpful sanity check, in combination with Check().
    m_P[i] = -1;
    m_Y[i] = N;		// This value can be left if m_P[i] == 1
#endif
  }

  // Pointers to the next low and high slots to use.  Work towards the
  // middle.  This simplifies the loop exit test to a == b.
  a = 0;
  b = N - 1;

  while (true) {
    // A loop invariant here is mean(p[j[a..b]]) == s

    if (std::numeric_limits<S>::is_integer)
      // For integer S, store the unnormalized probability and select
      // using the exact Prob(m_P[k], m_wsum).
      m_P[j[a]] = p[j[a]];
    else
      // For real S, store the normalized probability and select using
      // Prob(m_P[k]).  There will be a round off error in performing
      // the division; but there is also the potential for round off
      // errors in performing the arithmetic on p.  There is therefore
      // no point in simulating the division exactly using the slower
      // Prob(real, real).
      m_P[j[a]] = p[j[a]] / m_wsum;
    

    // If all arithmetic were exact this assignment could be:
    //   if (p[j[a]] < s) Y[j[a]] = j[b];
    // But the following is safer:
    m_Y[j[a]] = j[p[j[a]] < s ? b : a];

    if (a == b) {
      // The following assertion may fail because of roundoff errors
      // assert( p[j[a]] == s );
      break;
    }

    // Update p, a, and b maintaining the loop invariant
    p[j[b]] = p[j[b]] - (s - p[j[a]]);
    if (p[j[b]] > s)
      a++;
    else
      j[a] = j[b--];
  }
  for (size_t i = 0; i < N; ++i)
    std::cout << i << " " << m_Y[i] << " " << m_P[i] << std::endl;
  return;
}

#endif // RANDOMSELECT_H
