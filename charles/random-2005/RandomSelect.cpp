// RandomSelect.hpp
//
// An implementation of the Walker algorithm for selecting from a finite
// set.
//
// http://charles.karney.info/random/

#include "RandomSelect.hpp"

namespace {
  char rcsid[] = "$Id: RandomSelect.cpp 6147 2006-04-27 18:03:36Z ckarney $";
  char RandomSelect_h_rcsid[] = RCSID_RANDOMSELECT_H;
}
// Setup state for Walker's selection algorithm using weights, w.

#if !defined(NDEBUG)
#include <iostream>

void RandomSelect::Check() {
  // create and print the list of probabilities from m_k, m_P, m_Y.

  assert(m_k == m_Y.size() && m_k == m_P.size());

  if (m_k == 0) return;

  std::vector<double> p(m_k, 0);

  for (size_t i = 0; i < m_k; i++) {

    // m_P is a probability.  This assert may be too "aggressive".
    assert( m_P[i] >= 0 && m_P[i] <= 1 );

    // m_Y needs to be in range if m_P[i] < 1.
    assert( m_P[i] >= 1 ||  m_Y[i] < m_k );

    // in Select(), m_P[i] > 1 counts as 1; m_P[i] < 0 counts as 0.
    p[i] += std::max(0.0, std::min(1.0, m_P[i]));
    if (m_P[i] < 1)
      p[m_Y[i]] += 1 - std::max(0.0, m_P[i]);

  }

  std::cout << "Selection probabilities\ni p(i) w(i)\n";
  for (size_t i = 0; i < m_k; i++)
    std::cout << i << " " << p[i]/m_k << " " << (p[i]/m_k) * m_wsum << "\n";
}
#endif
