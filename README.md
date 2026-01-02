What's the fastest way to say each number? 'Twenty-seven' is 4 syllables, but 'three cubed' is only 2!

This program calculates the least amount of syllables needed to say each number, up to a set limit. The process is as follows:

  Generate the original name and syllable count for each number in the range. This is in the form of 'one hundred twenty-three thousand four hundred fifty-six' for 123456.
  Find all numbers that can be said using 1 syllable.
  Find all numbers that can be said using 2 syllables.
  Find all numbers that can be said using 3 syllables.
  Continue until all numbers in the range have a name.

During the step of finding names with N syllables, we attempt to construct numbers using results from lower syllable counts. For example:

  (N-1 syllables) squared
  (N-1 syllables) cubed
  (N-2 syllables) plus (1 syllable)
  (N-3 syllables) plus (2 syllables)
  (N-4 syllables) plus (3 syllables)
  and so forth.

A lot of the complexity comes from order of operations. We assume that parentheses cannot be said, so "twelve times nine plus one" will evaluate to 109, not 120. For each number we store the shortest name at each level: original, exponent and fraction, multiplication, division, addition and subtraction. While attempting construction, we take the highest level allowed for the operation. "eight times nine" would not be allowed as the input for multiplication, while "seventy-two" would be allowed. 

There is additional complexity from the use of fractions. For example, we can say 32 as "four cubed halves". This use of "halves" or "thirds" makes division faster, but it can be ambiguous. With the term "one hundred twenty fifths", it's not clear whether this means 120/5 or 100/25, so this is not allowed. The code includes logic to check whether a fraction will have more than one interpretation, and should therefore by disallowed.
