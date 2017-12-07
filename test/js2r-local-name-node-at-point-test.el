(require 'js2-refactor)
(require 'buttercup)

(describe
 "Find local name"

 (it "finds the local name in import"
     (with-js2-buffer "import { foo as bar } from 'lib'"
       (search-backward "bar")
       (expect (js2-name-node-name (js2r--local-name-node-at-point))
               :to-equal "bar")))

 (it "skips the remote name in named imports"
     (with-js2-buffer "import { foo as bar } from 'lib'"
       (search-backward "foo")
       (expect (js2r--local-name-node-at-point)
               :to-throw 'error)))

 (it "exported name becomes local in import"
     (with-js2-buffer "import { bar } from 'lib'"
       (search-backward "bar")
       (expect (js2-name-node-name (js2r--local-name-node-at-point))
               :to-equal "bar")))

 (it "the 'foo' in x.foo is not a local name"
     (with-js2-buffer "x.foo"
       (expect (js2r--local-name-node-at-point)
               :to-throw 'error)))

 (it "the 'foo' in { foo: 1 } is not a local name"
     (with-js2-buffer "x = { foo: 1 }"
       (search-backward "foo")
       (expect (js2r--local-name-node-at-point)
               :to-throw 'error)))

 (it "the 'foo' in x = { foo } is a local name"
     (with-js2-buffer "x = { foo }"
       (search-backward "foo")
       (expect (js2-name-node-name (js2r--local-name-node-at-point))
               :to-equal "foo"))))
