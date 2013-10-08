from excygen import *

preview = Renderer(width=512, height=512)

sunsky = SunSky(direction=direction(1,2,3),
                sunlight=RGB(1,1,1),
                skylight=RGB(1,1,1))

render(renderer=preview,
       sunsky=sunsky
       )
