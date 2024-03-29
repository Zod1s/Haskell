main :: IO ()
main = print $ stddev sample

sample :: [Double]
sample =
  [ 95,
    82,
    46,
    72,
    3,
    21,
    41,
    15,
    95,
    50,
    35,
    18,
    4,
    60,
    29,
    34,
    58,
    53,
    46,
    57,
    52,
    51,
    22,
    75,
    61,
    73,
    76,
    12,
    81,
    93,
    79,
    70,
    91,
    98,
    98,
    3,
    15,
    49,
    65,
    56,
    73,
    48,
    0,
    6,
    85,
    76,
    62,
    0,
    38,
    38,
    79,
    50,
    57,
    46,
    6,
    81,
    79,
    7,
    82,
    69,
    43,
    14,
    1,
    48,
    71,
    93,
    19,
    31,
    27,
    24,
    78,
    24,
    68,
    44,
    3,
    67,
    62,
    0,
    37,
    54,
    64,
    41,
    70,
    70,
    29,
    97,
    33,
    79,
    16,
    85,
    15,
    52,
    13,
    93,
    11,
    63,
    55,
    65,
    46,
    60
  ]

stddev :: [Double] -> Double
stddev ls = sqrt $ (sum [(x - xm) ** 2 | x <- ls]) / fromIntegral (length ls)
  where
    xm = average ls

average :: [Double] -> Double
average ls = sum ls / fromIntegral (length ls)
