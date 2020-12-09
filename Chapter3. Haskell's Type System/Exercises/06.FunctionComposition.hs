inc :: Num a => a -> a
inc x = x + 1

double :: Num a => a -> a
double x = x * 2

incTheDouble x = inc (double x)

incTheDouble' = inc . double