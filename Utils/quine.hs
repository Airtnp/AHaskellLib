-- Generialized Quine
-- ref: https://codegolf.stackexchange.com/questions/41375/generalized-quine-generator

quine = ((++)<*>show).('(':).(++")$(++)<*>show$")

{-

How it works

P $ "Q" = ((++)<*>show).('(':).(++")$(++)<*>show$") $ "Q" constructs "R" by

(++")$(++)<*>show$"): appending the string ")$(++)<*>show$",
('(':): prepending the character '(', and
(++)<*>show (= \x->x++show x): appending a quoted version of that,
resulting in "R" = "(Q)$(++)<*>show$\"(Q)$(++)<*>show$\"".

R = (Q)$(++)<*>show$"(Q)$(++)<*>show$" works by

taking the string "(Q)$(++)<*>show$",
(++)<*>show: appending a quoted version of that,
applying Q to that,
resulting in Q "(Q)$(++)<*>show$\"(Q)$(++)<*>show$\"" = Q "R".

(The parens around Q are necessary because Q might contain $ just as easily as R does, and $ is unfortunately right-associative.)

Demo

λ> putStrLn $ ((++)<*>show).('(':).(++")$(++)<*>show$") $ "id"
(id)$(++)<*>show$"(id)$(++)<*>show$"
λ> putStrLn $ (id)$(++)<*>show$"(id)$(++)<*>show$"
(id)$(++)<*>show$"(id)$(++)<*>show$"
λ> putStrLn $ ((++)<*>show).('(':).(++")$(++)<*>show$") $ "reverse"
(reverse)$(++)<*>show$"(reverse)$(++)<*>show$"
λ> putStrLn $ (reverse)$(++)<*>show$"(reverse)$(++)<*>show$"
"$wohs>*<)++($)esrever("$wohs>*<)++($)esrever(
λ> putStrLn $ ((++)<*>show).('(':).(++")$(++)<*>show$") $ "length"
(length)$(++)<*>show$"(length)$(++)<*>show$"
λ> print $ (length)$(++)<*>show$"(length)$(++)<*>show$"
44

-}