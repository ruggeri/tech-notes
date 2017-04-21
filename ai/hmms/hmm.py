from collections import namedtuple
import numpy as np
import pdb

NUM_HIDDEN_STATES = 100
NUM_EMISSION_VALUES = 100
NUM_TIME_STEPS = 100
MARKOV_BASE = 1.05

Model = namedtuple("Model", [
    "num_hidden_states",
    "num_emission_values",
    "initial_state_probabilities",
    "transition_matrix",
    "emission_matrix",
])

def sample_markov_matrix(num_in_states, num_out_states):
    result = np.full(
        (num_out_states, num_in_states),
        (1 - MARKOV_BIAS) / (num_out_states - 1)
    )
    for in_state in range(num_in_states):
        out_state = np.random.randint(num_out_states)
        result[out_state, in_state] = MARKOV_BIAS

    return result

def sample_markov_matrix(num_in_states, num_out_states):
    result = np.zeros((num_out_states, num_in_states))
    for in_state in range(num_in_states):
        out_probs = (
            MARKOV_BASE ** -np.arange(num_out_states, dtype = np.float32)
        )
        np.random.shuffle(out_probs)
        result[:, in_state] = out_probs
    result /= result.sum(axis = 0)

    return result

def almost_identity_matrix(num_states):
    emission_matrix = np.identity(num_states) + 0.01
    emission_matrix /= emission_matrix.sum(axis = 0)
    return emission_matrix

def default_model(num_hidden_states, num_emission_values):
    initial_state_probabilities = np.full(
        num_hidden_states, 1 / num_hidden_states
    )

    transition_matrix = sample_markov_matrix(
        num_hidden_states, num_hidden_states
    )

    emission_matrix = sample_markov_matrix(
        num_hidden_states, num_emission_values
    )

    return Model(
        num_hidden_states = num_hidden_states,
        num_emission_values = num_emission_values,
        initial_state_probabilities = initial_state_probabilities,
        transition_matrix = transition_matrix,
        emission_matrix = emission_matrix
    )

def run_model(model, num_time_steps):
    hidden_states = []
    observations = []
    for time_idx in range(num_time_steps):
        if time_idx > 0:
            prev_state = hidden_states[-1]
            next_state_probabilities = (
                model.transition_matrix[:, prev_state]
            )
        else:
            next_state_probabilities = model.initial_state_probabilities
        next_state = np.random.choice(
            model.num_hidden_states, p = next_state_probabilities
        )

        next_observation_probabilities = (
            model.emission_matrix[:, next_state]
        )
        next_observation = np.random.choice(
            model.num_emission_values,
            p = next_observation_probabilities
        )

        hidden_states.append(next_state)
        observations.append(next_observation)

    return (hidden_states, observations)

# Filtering
# Smoothing
# MLE

# This produces `Pr(X_t | e_{0,..., t - 1})`.
def forward_probabilities(model, observations):
    fs = [model.initial_state_probabilities]
    for time_idx, observation in enumerate(observations):
        prior_state_probs = fs[-1].copy()

        # Factor in prob of producing example.
        observation_emission_prob_by_states = (
            model.emission_matrix[observation, :]
        )
        prior_state_probs *= observation_emission_prob_by_states

        # Perform the mixing.
        next_state_probs = (
            model.transition_matrix.dot(prior_state_probs)
        )

        # Normalize
        next_state_probs /= next_state_probs.sum()

        fs.append(next_state_probs)

    return fs

# This produces `Pr(e_{t, ..., t_max} | X_t)`.
def backward_probabilities(model, observations):
    seqn_length = len(observations)
    bs = [None] * seqn_length

    # Reverse Markov matrix: find `Pr(X_{t-1} | X_t)`.
    reverse_transition_matrix = model.transition_matrix.T.copy()
    reverse_transition_matrix /= reverse_transition_matrix.sum(axis = 0)

    final_observation = observations[-1]
    bs[-1] = model.emission_matrix[final_observation, :]
    for time_idx in range(seqn_length - 2, -1, -1):
        next_state_probs = bs[time_idx + 1]
        # Disregarding the observation at time_idx, what is the
        # probability over X_{time_idx} given the observations
        # (time_idx + 1, ...)
        state_probs = reverse_transition_matrix.dot(next_state_probs)

        # Now add in the information of the observation at time_idx.
        observation = observations[time_idx]
        observation_emission_prob_by_states = (
            model.emission_matrix[observation, :]
        )
        state_probs *= observation_emission_prob_by_states
        # Normalize
        state_probs /= state_probs.sum()

        bs[time_idx] = state_probs

    return bs

def forward_backward(model, observations):
    seqn_length = len(observations)
    fs = forward_probabilities(model, observations)
    bs = backward_probabilities(model, observations)
    state_probs_by_time_idx = []

    for time_idx in range(seqn_length):
        state_probs = fs[time_idx] * bs[time_idx]
        state_probs /= state_probs.sum()
        state_probs_by_time_idx.append(state_probs)

    return state_probs_by_time_idx

def cross_entropy(true_hidden_states, hidden_state_estimates):
    seqn_length = len(true_hidden_states)

    num_bits = 0.0
    for time_idx in range(seqn_length):
        true_hidden_state = true_hidden_states[time_idx]
        estimated_prob_of_true_state = (
            hidden_state_estimates[time_idx][true_hidden_state]
        )
        num_bits += -np.log2(estimated_prob_of_true_state)

    return num_bits

# This tries to calculate the marginal probability of the hidden
# states after enough time for mixing.
def stationary_hidden_state_probabilities(model):
    probs = np.ones(model.num_hidden_states) / model.num_hidden_states
    for _ in range(1000):
        probs = model.transition_matrix.dot(probs)

    return probs

def entropy(probs):
    e = 0.0
    for prob in probs:
        e += -np.log2(prob) * prob
    return e

# TODOs:
# 1. EM.
# 2. Fixed lag smoothing.
# 3. Viterbi.

def main():
    # Build model; run model. Smooth estimates.
    model = default_model(NUM_HIDDEN_STATES, NUM_EMISSION_VALUES)
    (hidden_states, observations) = run_model(model, NUM_TIME_STEPS)
    smoothed_hidden_state_estimates = forward_backward(
        model, observations
    )

    # Evaluate model
    ce = cross_entropy(hidden_states, smoothed_hidden_state_estimates)
    hmm_ce = ce / NUM_TIME_STEPS
    mixed_ce = entropy(stationary_hidden_state_probabilities(model))
    uniform_ce = np.log2(NUM_HIDDEN_STATES)
    mixed_compression_ratio = hmm_ce / mixed_ce
    uniform_compression_ratio = hmm_ce / uniform_ce

    print(f"Bits per state: {hmm_ce}")
    print(f"Bits per state with mixed probs: {mixed_ce}")
    print(f"Mixed compression ratio: {mixed_compression_ratio}")
    print(f"Bits per state with uniform state probs: {uniform_ce}")
    print(f"Uniform compression ratio: {uniform_compression_ratio}")

if __name__ == "__main__":
    main()
