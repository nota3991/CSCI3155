object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   */

  /*
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */
  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => {
        return n.toDouble;
      }
      case B(true) => {
        return 1.0;
      }
      case B(false) => {
        return 0.0;
      }
      case Undefined => {
        return Double.NaN;
      }
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => {
        return b;
      }
      case N(n) => {
        if (n == 0) {
          return false;
        }
        return true;
      }
      case Undefined => {
        return false;
      }
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => {
        return s;
      }
      case N(n) => {
        return n.toString;
      }
      case B(b) => {
        return b.toString;
      }
      case Undefined => {
        return "undefined";
      }
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)

    e match {
      /* Base Cases */
      case _ if (isValue(e)) => {
        return e;
      }
      case ConstDecl(x, e1, e2) => {
        return eval(extend(env, x, eval(env, e1)), e2);
      }
      case Var(x) => {
        return get(env, x);
      }
      case Unary(Neg, e1) => {
        if (toBoolean(e1) == true) {
          return N(-1);
        } else if (toBoolean(e1) == false) {
          return N(0);
        } else {
          return N(toNumber(e1) * -1);
        }
      }
      case Unary(Not, e1) => {
        if (toBoolean(e1) == true) {
          return B(false);
        } else if (toBoolean(e1) == false) {
          return B(true);
        } else if (toNumber(e1) == 0) {
          return B(true);
        } else {
          return B(false);
        }
      }
      case Binary(Plus, e1, e2) => {
        return N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)));
      }
      case Binary(Minus, e1, e2) => {
        return N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)));
      }
      case Binary(Times, e1, e2) => {
        return N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)));
      }
      case Binary(Div, e1, e2) => {
        return N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)));
      }
      case Binary(Eq, e1, e2) => {
        return B(toNumber(eval(env, e1)) == toNumber(eval(env, e2)));
      }
      case Binary(Ne, e1, e2) => {
        return B(toNumber(eval(env, e1)) != toNumber(eval(env, e2)));
      }
      case Binary(Lt, e1, e2) => {
        return B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)));
      }
      case Binary(Le, e1, e2) => {
        return B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)));
      }
      case Binary(Gt, e1, e2) => {
        return B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)));
      }
      case Binary(Ge, e1, e2) => {
        return B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)));
      }
      case Binary(And, e1, e2) => {
        return B(toBoolean(eval(env, e1)) && toBoolean(eval(env, e2)));
      }
      case Binary(Or, e1, e2) => {
        return B(toBoolean(eval(env, e1)) || toBoolean(eval(env, e2)));
      }
      case Binary(Seq, e1, e2) => {
        return eval(env, e1); eval(env, e2);
      }
      /* Inductive Cases */
      case Print(e1) => println(pretty(eToVal(e1))); Undefined
      case If(e1, e2, e3) => {
        if (toBoolean(eval(env, e1)) == true) {
          return e2;
        } else {
          return e3;
        }
      }
      case _ => throw new UnsupportedOperationException
    }
  }

  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }
}