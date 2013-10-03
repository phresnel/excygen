from excygen import *

preview = Renderer(width=512, height=512)

render(renderer=preview,
       sun_sky=SunSky(direction=direction(1,2,3))
      )
