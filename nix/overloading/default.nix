let 

  a = {
    x = 3;
  };

  b = {
    x = 3;
  };

  # Overload the multiplication operator
  __mul = a: b: { foo = builtins.mul a.x b.x; bar = a.x + b.x; };


in a * b
