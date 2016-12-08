# metaclj.js

## Scope

### Lexical Scope

The `js` macro expands to code that captures the lexical environment at the
call site. Inside the body of a js form, symbols may refer to either JavaScript
locals or Clojure locals. When a Clojure local is used, the value is converted
to a JavaScript AST representation at each reference.

### Dynamic Scope

If a symbol in a `js` form is not resolved lexically as a local, it is
searched for in two separate dynamic scopes. First, the "JavaScript
namespace", then the current Clojure namespace, `clojure.core/*ns*`, as normal
Clojure code would. Be warned, this means that, JS definitions silently
shadow Clojure vars.

### No Hoisting

The expander and code generator only supports locals with proper lexical
scope.  That is, `let` is provided, but the hoisting `var` is not. Functions
may be named to allow for recursion, but normal JavaScript hoisting for
forward references is thwarted by replacing the recursive name with a gensym.
This strict adherence to lexical scope makes analysis and code generation much
simpler. If you need mutual recursion, use explicit forward declarations.
