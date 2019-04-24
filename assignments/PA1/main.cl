Class Stack inherits IO { 
	car() : String { { abort(); new String; } };

	cdr() : Stack { { abort(); new Stack; } };

	isNil() : Bool { { abort(); true; } };

	cons(hd : String) : Cons {
    (new Cons).init(hd, self)
	};

  eval() : Stack { { abort(); new Stack; } };

	push(i : String) : Stack { (new Cons).init(i,self) };
	
	print_stack() : Object { abort() };
};

Class Nil inherits Stack {
	isNil() : Bool { true };

	push(i : String) : Stack { (new Cons).init(i,self) };

  eval() : Stack { self };

	print_stack() : Object { true };
};

Class Cons inherits Stack {
	xcar : String;
	xcdr : Stack;
  atoi : A2I <- new A2I;

	car() : String { xcar };

	cdr() : Stack { xcdr };

	isNil() : Bool { false };

	init(hd : String, tl : Stack) : Cons {
	  {
	    xcar <- hd;
	    xcdr <- tl;
	    self;
	  }
	};
	  
  eval() : Stack { 
    if xcar = "+" then {
      let result : Int <- atoi.a2i(xcdr.cdr().car()) + atoi.a2i(xcdr.car()),
          rest: Stack <- xcdr.cdr().cdr()
        in 
          (new Cons).init(atoi.i2a(result), rest);
    } else if xcar = "s" then {
      let son_car : String <- xcdr.car(),
          grandson_car: String <- xcdr.cdr().car(),
          rest: Stack <- xcdr.cdr().cdr()
        in 
          (new Cons).init(grandson_car, (new Cons).init(son_car, rest));
    } else self
    fi fi
  };

	push(i : String) : Stack { (new Cons).init(i,self) };

	print_stack() : Object {
		{
      out_string(xcar);
      out_string("\n");
      xcdr.print_stack();
		}
	};
};

class Main inherits IO {
  stack : Stack <- new Nil;
  terminate : Bool <- false;

  prompt() : String {
    {
      out_string(">");
      in_string();
    }
  };

  main() : Object {
    while not terminate loop ( 
      let s : String <- prompt() in
        if s = "x" then terminate <- true 
        else if s = "e" then stack <- stack.eval()
        else if s = "d" then stack.print_stack()
        else stack <- stack.push(s)
        fi fi fi
    ) pool
  };
};
