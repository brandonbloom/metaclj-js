(ns metaclj.js.operators)

(def strict-infix
  '{+ "+"
    * "*"
    - "-"
    / "/"
    = "==="
    not= "!=="
    loose= "=="
    loose-not= "!="
    > ">"
    >= ">="
    < "<"
    <= "<="
    mod "%"
    pow "**"
    bit-and "&"
    bit-or "|"
    bit-xor "^"
    bit-shift-left "<<"
    bit-shift-right ">>"
    unsigned-bit-shift-right ">>>"})

(def short-circuiting
  '{and "&&"
    or "||"})

(def assignment
  '{+ "+="
    - "-="
    * "*="
    / "/="
    mod "%="
    pow "**="
    bit-shift-left "<<="
    bit-shift-right ">>="
    unsigned-bit-shift-right ">>>="
    bit-and "&="
    bit-xor "^="
    bit-or "|="})

(def strict-unary
  '{- "-"
    + "+"
    bit-not "~"
    not "!"})

;TODO: pre/post ++/--
