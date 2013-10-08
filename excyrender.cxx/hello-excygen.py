from excygen import *

preview = Renderer(width=512, height=512)

sunsky = SunSky(direction=direction(1,2,3),
                sunlight=RGB(1,1,1),
                skylight=RGB(1,1,1))

#render(renderer=preview,
#       sunsky=sunsky
#       )

foo = cross(Vector(1,0,0), Vector(0,1,0))
print foo

tex = ImageTexture(UVMapping2d(1,1,offset_u=2),
                   filename="loose_gravel_9261459 (mayang.com).JPG")
