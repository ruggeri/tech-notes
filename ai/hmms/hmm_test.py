import hmm
import numpy as np

def make_test_model(transition_matrix, emission_matrix):
    return hmm.Model(
        num_hidden_states = 3,
        num_emission_values = 3,
        initial_state_probabilities = np.array([1.0, 0.0, 0.0]),
        transition_matrix = transition_matrix,
        emission_matrix = emission_matrix,
    )

def cycle_matrix():
    return np.array([
        [0, 0, 1],
        [1, 0, 0],
        [0, 1, 0],
    ], dtype = np.float32)

def test_basic_generation():
    model = make_test_model(
        cycle_matrix(),
        cycle_matrix()
    )

    (hidden_states, observations) = hmm.run_model(model, 5)
    assert((hidden_states == np.array([
        0, 1, 2, 0, 1
    ])).all())

    assert((observations == np.array([
        1, 2, 0, 1, 2
    ])).all())

def test_basic_forward():
    model = make_test_model(
        cycle_matrix(),
        cycle_matrix()
    )

    num_steps = 2
    (hidden_states, observations) = hmm.run_model(model, num_steps)
    filter_estimates = hmm.forward_probabilities(model, observations)
    assert(len(filter_estimates) == (num_steps + 1))

    print(filter_estimates[0])
    assert((
        filter_estimates[0] == np.array([1.0, 0.0, 0.0])
    ).all())

test_basic_generation()
test_basic_forward()
