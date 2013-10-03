import py_bindings

noise = py_bindings.RidgedMulti(octave_count=23)

def height(u, v):
    return noise(u*0.0015, v*0.0015)*125 - 350
