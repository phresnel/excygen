import py_bindings
rm = py_bindings.RidgedMulti()

def height(u, v):
    return rm(u*0.0015, v*0.0015)*200 - 350
