from excygen import *

preview = Renderer(width=512, height=512, samples_per_pixel=1)

sunsky = SunSky(direction=direction(1,2,3),
                sunlight=RGB(1,1,1),
                skylight=RGB(1,1,1))

#render(renderer=preview,
#       sunsky=sunsky
#       )

s = cross(Vector(1,0,0), Vector(0,1,0))
print s

mat = Lambertian(ImageTexture(PlanarMapping2d(Vector(1,0,0), Vector(0,0,1), offset_v=2),
                              filename="loose_gravel_9261459 (mayang.com).JPG"))

m = Lambertian(ConstantTexture(RGB(1,2,3)))

def hoo(u,v):
    return 1.0


t = Terrain2dAlpha(Rectangle(Point2d(-1000,-1000),
                             Point2d(1000,1000)),
                   128,
                   hoo,
                   m)
#foo(lambda x, y: x*3.14159)
