import py_bindings

noise = py_bindings.RidgedMulti(seed=3)

def height(u, v):
    return noise(u*0.0015, v*0.0015)*200 - 350
