defrecord NeuronInfo, ref: nil, pid: nil, activation: 0, predictivity: 0, spike: false, feedforward: [], dendrites: [], axons: []
defrecord Synapse, ref: nil, from: nil, to: nil, permanence: 0.0, connected: false, signal: false
defrecord Dendrite, ref: nil, to: nil, signals: 0, spike: false

defrecord PatchInfo, ref: nil, pid: nil, max: 256, min_ref: nil, max_ref: nil, neurons: [], patches: {nil, nil}