let

  foo = {
    a = 3;
    __functor = self: _arg: self.a;
  };

in {
  inherit foo;
  bar = foo 3;
}
