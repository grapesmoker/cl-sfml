(in-package :sfml)

(defcenum sf-key-code
  (:sf-key-unknown -1)
  (:sf-key-a 0)
  (:sf-key-b 1)
  (:sf-key-c 2)
  (:sf-key-d 3)
  (:sf-key-e 4)
  (:sf-key-f 5)
  (:sf-key-g 6)
  (:sf-key-h 7)
  (:sf-key-i 8)
  (:sf-key-j 9)
  (:sf-key-k 10)
  (:sf-key-l 11)
  (:sf-key-m 12)
  (:sf-key-n 13)
  (:sf-key-o 14)
  (:sf-key-p 15)
  (:sf-key-q 16)
  (:sf-key-r 17)
  (:sf-key-s 18)
  (:sf-key-t 19)
  (:sf-key-u 20)
  (:sf-key-v 21)
  (:sf-key-w 22)
  (:sf-key-x 23)
  (:sf-key-y 24)
  (:sf-key-z 25)
  (:sf-key-num-0 26)
  (:sf-key-num-1 27)
  (:sf-key-num-2 28)
  (:sf-key-num-3 29)
  (:sf-key-num-4 30)
  (:sf-key-num-5 31)
  (:sf-key-num-6 32)
  (:sf-key-num-7 33)
  (:sf-key-num-8 34)
  (:sf-key-num-9 35)
  (:sf-key-escape 36)
  (:sf-key-l-control 37)
  (:sf-key-l-shift 38)
  (:sf-key-l-alt 39)
  (:sf-key-l-system 40)
  (:sf-key-r-control 41)
  (:sf-key-r-shift 42)
  (:sf-key-r-alt 43)
  (:sf-key-r-system 44)
  (:sf-key-menu 45)
  (:sf-key-l-bracket 46)
  (:sf-key-r-bracket 47)
  (:sf-key-semicolon 48)
  (:sf-key-comma 49)
  (:sf-key-period 50)
  (:sf-key-quote 51)
  (:sf-key-slash 52)
  (:sf-key-backslash 53)
  (:sf-key-tilde 54)
  (:sf-key-equal 55)
  (:sf-key-dash 56)
  (:sf-key-space 57)
  (:sf-key-return 58)
  (:sf-key-back 59)
  (:sf-key-tab 60)
  (:sf-key-page-up 61)
  (:sf-key-page-down 62)
  (:sf-key-end 63)
  (:sf-key-home 64)
  (:sf-key-insert 65)
  (:sf-key-delete 66)
  (:sf-key-add 67)
  (:sf-key-subtract 68)
  (:sf-key-multiply 69)
  (:sf-key-divide 70)
  (:sf-key-left 71)
  (:sf-key-right 72)
  (:sf-key-up 73)
  (:sf-key-down 74)
  (:sf-key-numpad-0 75)
  (:sf-key-numpad-1 76)
  (:sf-key-numpad-2 77)
  (:sf-key-numpad-3 78)
  (:sf-key-numpad-4 79)
  (:sf-key-numpad-5 80)
  (:sf-key-numpad-6 81)
  (:sf-key-numpad-7 82)
  (:sf-key-numpad-8 83)
  (:sf-key-numpad-9 84)
  (:sf-key-f1 85)
  (:sf-key-f2 86)
  (:sf-key-f3 87)
  (:sf-key-f4 88)
  (:sf-key-f5 89)
  (:sf-key-f6 90)
  (:sf-key-f7 91)
  (:sf-key-f8 92)
  (:sf-key-f9 93)
  (:sf-key-f10 94)
  (:sf-key-f11 95)
  (:sf-key-f12 96)
  (:sf-key-f13 97)
  (:sf-key-f14 98)
  (:sf-key-f15 99)
  (:sf-key-pause 100)
  (:sf-key-count 101))

(defparameter *keypress-mapping*
  '(8 :sf-key-backspace
    27 :sf-key-escape
    39 :sf-key-quote
    42 :sf-key-multiply
    43 :sf-key-add
    44 :sf-key-comma
    45 :sf-key-subtract
    46 :sf-key-period
    47 :sf-key-divide
    59 :sf-key-semicolon
    61 :sf-key-equal
    91 :sf-key-l-bracket
    92 :sf-key-backslash
    93 :sf-key-r-bracket
    96 :sf-key-tilde
    127 :sf-key-delete))
    

(defcfun ("sfKeyboard_isKeyPressed" sf-keyboard-is-key-pressed) :boolean
  (key sf-key-code))

(defun is-key-pressed? (key-code)
  (sf-keyboard-is-key-pressed key-code))

;;   (foreign-enum-keyword 'sf-key-code key-code)))
