module Colors (
    d_ylw,
    b_ylw,
    b_cyan,
    b_red,
    b_green,
    b_black,
    alt,
    alt2,
    alt3,
    alt4,
    clr
 ) where

b_red = "\ESC[0;1;31m"

d_ylw  = "\ESC[2;1;33m"
b_ylw  = "\ESC[1;33m"
b_cyan = "\ESC[0;1;36m"

b_green = "\ESC[2;1;32m"
b_black = "\ESC[0;1;30m"

alt    = "\ESC[38;5;65m"
alt2    = "\ESC[38;5;69m"

-- cyan/teal
alt3    = "\ESC[38;5;37m"
-- 137 blue
alt4    = "\ESC[38;5;33m"

clr    = "\ESC[0m"
