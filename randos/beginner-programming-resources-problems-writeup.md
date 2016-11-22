**Add Digits**

Given an integer `num`, repeatedly add its digits until the result has
only one digit. For instance, if `num=38`, then `38 => 3 + 8 = 11 =>
1+1=2`. So the result would be 2.

**Move Zeroes**

Given an array of numbers, create a new array with the non-zero
numbers in their original order, followed by any zeroes in the
original array. For instance, if `array=[1,0,3,0,2,0]`, then the
result should be `result=[1,3,2,0,0,0]`.

You may either produce a new array, or modify the original; do
whatever you think is easiest.

**Count And Say**

Given an input string such as `aabaaabbc`, return an array describing
that string. The array lists a letter, then how many times it repeats
consecutively, then another letter, and how many time it repeats
consecutively, etc.

For instance, in the example, the result should be
`["a", 2, "b", 1, "a", 3, "b", 2, "c", 1]`.

**Reverse Vowels of a String**

Take in a string, and reverse the vowels in the string, leaving all
the original letters in the same position. You may assume 'y' is not a
vowel.

For instance, if you are given `"gizmolina"`, you should return
`"gazmiloni"`. It is probably easiest to produce a new string instead
of modifying the original.

**Swap Pairs in An Array**

Given an input array like `["a", "b", "c", "d", "e", "f"]`, swap the
first two elements, then the next two, then the next two, etc. In the
example, the result should be `["b", "a", "d", "c", "f", "e"]`. Write
two versions: one which modifies the original array, and a second that
creates a new array for the result, leaving the original unmodified.

You may assume the input has an even number of items.

**Reverse String**

Given an input string like `"gizmo"`, return a new string `"omzig"`
that consists of all the letters in reverse order.

**First Unique Character**

Given an input string, return the position of the first character that
does not repeat later in the string. For instanze, if the input is
`"curiecountsribbons"`, then the first character that doesn't repeat
is the 'e' at position 4. Therefore you should return 4.

If every character repeats, as in `"gizmogizmo"`, please return `nil`.

You may assume the string contains only lowercase letters.

**String To Integer**

Given a string like `"123"`, turn it into an integer: `123`. You are
only only allowed to use the `to_i` method on single-character
strings; otherwise the problem would be too easy. For instance:
`"1".to_i` is allowed, but `"12".to_i` is against the rules.

**Integer To String**

Given an input integer like `123`, turn it into a string: `"123"`. You
are only allowed to use the `to_s` method on one-digit numbers,
otherwise the problem would be too easy. For instance: `1.to_s` is
allowed, but `12.to_s` is forbidden.

Hint: you may wish to use the `%` remainder operation. It may also
help to remember that when dividing two integers in Ruby, Ruby always
rounds down. For instance: `9/4==2`, and `9%4==1`.

**Rotate Array**

Given an input array and a number `k`, shift all items other by `k`
positions, wrapping around as needed. For instance, if
`input=["a", "b", "c", "d", "e"]` and `k=2`, then you want
`output=["d", "e", "a", "b", "c"]`. You can assume `0 <= k <
input.length`.

**Remove Duplicates**

Given an array of numbers like `[1, 3, 4, 3, 1, 8, 5]`, return a new
array with all the duplicates removed. The items in the new array
should appear in the same order as they first occurred in the original
array. In our example, `result=[1, 3, 4, 8, 5]`.

When you have done that, write a new version where, instead of
creating a new array, we modify the input and remove the items from
it.

**Find Items in Common**

Given two input arrays of numbers, return an array of numbers found in
both arrays. There should be no repeats within the result, even if a
number occurs multiple times in both input arrays.

For instance, if the input is `nums1 = [3, 2, 4, 1, 5]` and `nums2 =
[5, 1, 7, 2, 6, 8]`, then the result should be `result =
[1, 2, 5]`. For us, it does not matter in what order the numbers
appear: `[1, 2, 5]` is as good as `[2, 1, 5]` or `[5, 1, 2]`, etc. We
only care that all numbers in both inputs appear exactly once in the
result, and no others.

**Bulls And Cows**

You are given two inputs, a string `secret` and a second string
`guess`. You may assume both strings are of four letters.

You must return two numbers. First, the number of correct letters at
correct positions. For instance, if `secret = "abcd"` and `guess =
"adcb"`, then the guess of "a" and "c" are both correct. This is
called the number of "bulls": in my example, there are two bulls.

You must also return the number of "cows". The number of cows is the
number of guessed letters which (1) are not bulls, but (2) do appear
somewhere else in the string. For instance, if `secret = "abcd"` and
`guess = "adcb"`, both "b" and "d" are incorrectly placed, and are not
bulls. However, they are "cows" because those letters appear elsewhere
in the string.

You must not "double count" cows. Each letter in the secret can count
toward only one bull or cow. For instance, if `secret = "aaab"` and
`guess = "bbba"`, there are only two cows. Only one "b" in the guess
gets to match with the "b" at the end of the secret; the other "b"s
are neither cows nor buls. Likewise, if `secret = "abbb"` and `guess =
"aaaa"`, there is only one bull and no cows. The first "a" in secret
counts as a bull, and the subsequent "a"s in the guess have no other
"a" in the secret to match with.

**Valid Parentheses**

You are given an input string; you should return true if every left
parenthesis matches with a subsequent right parenthesis; otherwise
return false. For this exercise, you may ignore any non-parentheses
characters.

For instance, `"a((bb)c(dd))(e)"` has every parenthesis matched. On
the other hand, `")a("` is not correct, because the initial right
parenthesis is not closing any left parenthesis; likewise the final
left parenthesis is not closed by any right parenthesis.

**Merge Sorted Arrays**

You are given two arrays of numbers, both of which are already sorted
in ascending order. For instance, the inputs could be `nums1 =
[1, 3, 5]` and `nums2 = [2, 6, 8]`. Your job is to create a new array
of the numbers in ascending order.

Your code should not use the `sort` method, that would be too
easy. For instance, you cannot write `result = (nums1 + nums2).sort`.

Instead, start with an empty array (`result = []`). Then, one-by-one,
remove an element from one of the two input arrays (`nums1` or
`nums2`), and add it to the result array. Your job is to figure out
which element to remove from which input array, and to add it at the
right place in the result array.

It may help to remember that `pop` removes an element at the end of an
array, while `shift` removes an element at the beginning. Likewise,
`push` adds an element to the end of an array, while `unshift` adds an
element to the beginning.

**Column Encoding**

Take as an input a string like `input = "gizmo_is_the_best"`. I want
you to produce a new string by writing the input in columns, and then
concatenating the rows thus formed. For instance:

```
gmitbt
ioshe
z__es
```

Then the result should be: `"gmitbt" + "ioshe" + "z__es"` which is
`"gmitbtioshez__es"`.

In the beginning, assume that there are always three rows. When you
have that working, write a new version that takes a second input:
`num_rows`. This should allow the user to choose how many rows to
write the string across.
