from excygen import *

preview = Renderer(width=512, height=512)

sunsky = SunSky(direction=direction(1,2,3),
                sunlight=RGB(1,1,1),
                skylight=RGB(1,1,1))

#render(renderer=preview,
#       sunsky=sunsky
#       )


mapping = UVMapping2d(1,1,0,0)
tex = ImageTexture(mapping, "loose_gravel_9261459 (mayang.com).JPG")
