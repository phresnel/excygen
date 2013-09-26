import py_bindings

noise = py_bindings.RidgedMulti(octave_count=20)

def height(u, v):
    return noise(u*0.0015, v*0.0015)*200 - 350
