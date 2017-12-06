(ns paip.macsymar)

(def basic-rules
  '((x + 0 = x)
     (0 + x = x)
     (x + x = 2 * x)
     (x - 0 = x)
     (0 - x = - x)
     (x - x = 0)
     (- - x = x)
     (x * 1 = x)
     (1 * x = x)
     (x * 0 = 0)
     (0 * x = 0)
     (x * x = x expt 2)
     (x / 0 = undefined)
     (0 / x = 0)
     (x / 1 = x)
     (x / x = 1)
     (0 expt 0 = undefined)
     (x expt 0 = 1)
     (0 expt x = 0)
     (1 expt x = 1)
     (x expt 1 = x)
     (x expt -1 = 1 / x)
     (x * (y / x) = y)
     ((y / x) * x = y)
     ((y * x) / x = y)
     ((x * y) / x = y)
     (x + - x = 0)
     ((- x) + x = 0)
     (x + y - x = y)))

(def associativity-commutativity
  '((s * n = n * s)
     (n * (m * x) = (n * m) * x)
     (x * (n * y) = n * (x * y))
     ((n * x) * y = n * (x * y))
     (n + s = s + n)
     ((x + m) + n = x + n + m)
     (x + (y + n) = (x + y) + n)
     ((x + n) + y = (x + y) + n)))