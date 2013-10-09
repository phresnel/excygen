from excygen import *

preview = Renderer(width=128, height=128, samples_per_pixel=5)

sunsky = SunSky(direction=direction(1,2,3),
                sunlight=RGB(1,1,1),
                skylight=RGB(1,1,1))

s = cross(Vector(1,0,0), Vector(0,1,0))

mat = Lambertian(ImageTexture(PlanarMapping2d(Vector(1,0,0), Vector(0,0,1), offset_v=2),
                              filename="loose_gravel_9261459 (mayang.com).JPG"))

m = Lambertian(ConstantTexture(RGB(1,2,3)))


perlin = Perlin(frequency=0.01)

def hoo(u,v):
    return perlin(u,v) - 1


t = Terrain2dAlpha(Rectangle(Point2d(-1000,-1000),
                             Point2d(1000,1000)),
                   128,
                   hoo,
                   m)

render(renderer=preview, sunsky=sunsky, primitive=t)
