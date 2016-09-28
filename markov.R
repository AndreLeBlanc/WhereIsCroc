# 0. We do not need to estimate the starting probability as we have the starting state.
# 1. Estimate the transition probability of the 3 parameters in each node given previous sampling.
# 2. Estimate the emission probability of the 3 parameters in each node given previous sampling.
# 3. Estimate what the crocodile prefers, what values of the 3 parameters does it tend towards.
# 4. With these generate a Markov chain and use the Viterbi algorithm to traverse the graph.