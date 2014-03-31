* add an ID* wiring diagram which is polymorphic in box shape
* change input/output to inputs/outputs
* check no straight across wires (outY <- outX)
* mix w.d. and props in same file
* external first always
* unfinished propagators and propagators, similar syntax to wiring composition
* module system which supports require plus which can be filled by either prim plus or derived plus (say, from nands)
* change boxes to use boxy arrow [_:_, _:_, ... =[]= _:_]
* box exponential [_=>_] 
* box tensor [_[+]_]
* wiring diagram and propagator types use ->

box A := x:int, y:bool =[]= a:int, b:bool

wiring W := diagram with
  type i1:A, i2:A -> e:(x:int, y:bool =[]= a:int, b:bool)
  wire i1 inputs  x <- i2.a , y <- e.y
  wire i2 inputs  x <- e.x  , y <- i2.b
  wire e  outputs a <- i1.a , b <- i2.b
end

wiring C := compose W with
  type i11:A, i12:A, i21:A, i22:A -> A # optional
  i1 <- W rename i1 -> i11 , i2 -> i12
  i2 <- W rename i1 -> i21 , i2 -> i22
end

primitive propagator foo : A

propagator P := apply C with
  type A # optional
  i11 <- foo
  i12 <- foo
  i21 <- foo
  i22 <- foo
end

unfinished propagator UP := partially apply C with
  type i11:A, i21:A -> A
  i12 <- foo
  i22 <- foo
end

---

box A with
  inputs  x:int , y:bool
  outputs a:int , b:bool
end

wiring diagram W with
  external box e  with outputs a <- i1.a , b <- i2.b
  internal box i1 with inputs  x <- i2.a  , y <- e.y
  internal box i2 with inputs  x <- e.x   , y <- i2.b
end

wiring composition C with
  external w.d. W
  internal w.d. i1 <- W export internals i1 as i11 , i2 as i12
  internal w.d. i2 <- W export internals i1 as i21 , i2 as i22
end

primitive propagator foo : A

propagator P with
  external w.d. C
  internal box i11 <- foo
  internal box i22 <- foo
  internal box i21 <- foo
  internal box i22 <- foo
end

unfinished propagator UP with
  external w.d. C
  internal box i12 <- foo
  internal box i22 <- foo
end

