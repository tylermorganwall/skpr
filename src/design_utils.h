#pragma once

namespace skpr {

enum class OptimizationSense {
  Maximize,
  Minimize
};

inline bool isImprovement(double candidateScore, double currentScore, OptimizationSense sense) {
  if (sense == OptimizationSense::Maximize) {
    return candidateScore > currentScore;
  }
  return candidateScore < currentScore;
}

template <typename EvaluateCandidate>
int findBestCandidate(int totalPoints, EvaluateCandidate evaluate, OptimizationSense sense,
                      double baselineScore, double& bestScore) {
  double currentBest = baselineScore;
  int bestIndex = -1;
  for (int candidateIndex = 0; candidateIndex < totalPoints; ++candidateIndex) {
    double candidateScore = baselineScore;
    if (!evaluate(candidateIndex, candidateScore)) {
      continue;
    }
    if (isImprovement(candidateScore, currentBest, sense)) {
      currentBest = candidateScore;
      bestIndex = candidateIndex;
    }
  }
  if (bestIndex != -1) {
    bestScore = currentBest;
  }
  return bestIndex;
}

} // namespace skpr

