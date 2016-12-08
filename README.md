# MetaCLJ - JavaScript

Staged metaprogramming of JavaScript from Clojure.

Compare to [Terra][1]/C.


## Usage

Don't.

This library exists to power the JavaScript backend of another compiler I'm
working on. I've only published this for the curious.

Here's a small example without any of the context you'd need to understand it:

```clojure
(require '[metaclj.js :as js :refer [js]])

(js/require 'metaclj.js.core :refer :all)

(let [n 5]
  (js/eval
    (iife
      (let x 1)
      (for [(let y 1) (<= y n) (++ y)]
        (set! x (* x y)))
      (return x))))
```

This evaluates to `120` and is compiled as:

```javascript
(function(){
  let x = 1;
  for (let y = 1; y <= 5; ++y) {
    x = x * y;
  };
  return x;
})()
```

Lots more docs to write... eventually... maybe.


## Relationship To MetaCLJ

This project was forked from an early version of [MetaCLJ][2]. Although
unlikely to be worth the trouble, they could potentially be reunited someday.


## License

Copyright © 2016 Brandon Bloom

Distributed under the Eclipse Public License version 1.0.


[1]: http://terralang.org
[2]: https://github.com/brandonbloom/metaclj
