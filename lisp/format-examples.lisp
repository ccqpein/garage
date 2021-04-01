;;; this file for making examples of CL format function

#|
link: http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm

22.3.1 FORMAT Basic Output

22.3.2 FORMAT Radix Control

22.3.3 FORMAT Floating-Point Printers

22.3.4 FORMAT Printer Operations

22.3.5 FORMAT Pretty Printer Operations

22.3.6 FORMAT Layout Control

22.3.7 FORMAT Control-Flow Operations

22.3.8 FORMAT Miscellaneous Operations

22.3.9 FORMAT Miscellaneous Pseudo-Operations

22.3.10 Additional Information about FORMAT Operations

22.3.11 Examples of FORMAT
|#

(defun 22-3-1 ()
  "http://www.lispworks.com/documentation/HyperSpec/Body/22_ca.htm"
  (format t "22.3.1.1: ~~C #\\A => ~C~%" #\A)
  (format t "22.3.1.1: ~~:C #\\Space VS ~~C #\\Space => ~:C VS ~C~%" #\Space #\Space)
  
  (format t "22.3.1.2: ~~2% => ~2%~%")
  (format t "22.3.1.3: ~~2& => ~2&~%")
  (format t "22.3.1.4: ~~2| => ~2|~%")
  (format t "22.3.1.5: ~~3~~ => ~3~~%"))

(defun 22-3-2 ()
  "http://www.lispworks.com/documentation/HyperSpec/Body/22_cb.htm"
  )
