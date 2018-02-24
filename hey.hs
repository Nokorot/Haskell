
factorial 0 = 1
factorial n = n * factorial (n - 1)

calc x n = sum [ x^(2*i + 1) / ((factorial i) * (2*i + 1)) | i <- [0..n] ]
