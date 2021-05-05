double [] = []
double (x:xs) = 2*x : double xs

wordLength [] = []
wordLength (x:xs) = length x : wordLength xs

rem7 [] = []
rem7 (x:xs) = rem x 7 : rem7 xs

mapper _fn [] = []
mapper fn (x:xs) = fn x : mapper fn xs

reduceR _fn value [] = value
reduceR  fn value (x:xs) = fn x $ reduce fn value xs

reduceL _fn value [] = value
reduceL  fn value (x:xs) = reduce fn (fn value x) xs
