#lang plai

(require "a1.rkt")

(test (interpret "sample.txt")
      '(6
        -3
        15
        18
        7
        18
        -40
        12))

(test (interpret "descriptions.txt")
      '(9
        1
        12
        1
        -2
        -12
        -20
        -20
        -3072))

(test (interpret "arithmetic.txt")
      '(-3
        15
        2
        -54
        -15))

(test (interpret "name_lookup.txt")
      '(-20
        0
        18
        7
        18
        -40
        -120
        -12
        13))

(test (interpret "functions.txt")
      '(12
        16
        -20))

(interpret "test1.funshake")
(interpret "part1.txt")