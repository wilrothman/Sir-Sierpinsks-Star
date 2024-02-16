from random import 
def r(lower, upper, n):
  for i in range(n):
    x, y = randrange(lower, upper), randrange(lower, upper)
    print(f"(pixel {x} {y} white)")
r(-75, 75, 201)
