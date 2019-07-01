;;; basic-csv: csv-list-based.lisp
;;; By A. Sebold (manney)
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage :org.invigorated.csv
  (:use :common-lisp)
  (:export :parse-file
           :save-file))

(in-package :org.invigorated.csv)

(defun get-text (filename)
  "Opens and copies a text file in to a list then returns the list"
  (let ((text nil)
        (line nil))
    (with-open-file (in filename :direction :input)
                    (loop while (setf line (read-line in nil nil)) do
                          (setf text (append text
                                             (list (string-trim '(#\Space #\Newline) line))))))
    text))

(defun parse-line (line col-end-char decimal-point)
  "Returns a list of each element of the string LINE."
  (let ((csv-line nil)
        (char-at nil)
        (in-string nil)
        (in-quote nil)
        (in-number nil))

    (dotimes (x (length line))
      (setf char-at (char line x))
      ;; Make sure we aren't already in a string or number
      (if (and (null in-string)
               (null in-number))
          (cond
           ;; Blank cell
           ((char= char-at col-end-char)
            (if (eol-p x line)
                (setf csv-line (append csv-line (list nil))))
            (setf csv-line (append csv-line (list nil))))
           ;; Number
           ((or (digit-char-p char-at)
                (char= char-at #\-)
                (char= char-at #\+)
                (and (not (char= decimal-point col-end-char))
                     (char= char-at decimal-point)))
            (setf in-number x))
           ;; String
           (t
            (if (and (null in-quote)
                     (char= char-at #\"))
                (setf in-quote x))
            (setf in-string x))))
      ;; Examine each character based on the cases below
      (cond
       ;; If we are in a number and hit a string character
       ;; make the number in to a string
       ((and (not (null in-number))
             (not (digit-char-p char-at))
             (not (and (eql x in-number)
                       (or (char= char-at #\-)
                           (char= char-at #\+))))
             (not (or (char= char-at col-end-char)
                      (eol-p x line)))
             (not (and (not (char= decimal-point col-end-char))
                       (char= char-at decimal-point))))
        (setf in-string in-number)
        (setf in-number nil))
       ;; If we are in a string and hit a separator or EOL,
       ;; copy the new element to the list CSV-LINE
       ((and (not (null in-string))
             (or (char= char-at col-end-char)
                 (eol-p x line))
             (null in-quote))
        (let ((string-to-copy nil))
          ;; Check for EOL
          (if (eol-p x line)
              (setf string-to-copy (subseq line in-string (length line)))
            (setf string-to-copy (subseq line in-string x)))
          (setf csv-line (append csv-line (list string-to-copy)))
          (setf in-string nil)))
       ;; If we are in a quoted string and hit a separator
       ;; or EOL, check that we had an end quote before
       ;; copying the new element to the list CSV-LINE
       ((and (not (null in-string))
             (not (null in-quote))
             (or (char= char-at col-end-char)
                 (eol-p x line)))
        ;; Create the element to put in to CSV-LINE
        (let ((string-to-copy nil))
          ;; Check for EOL
          (if (eol-p x line)
              (if (char= (char line x) #\")
                  (setf string-to-copy (subseq line
                                               (1+ in-quote)
                                               (1- (length line)))))
            (if (char= (char line (1- x)) #\")
                (setf string-to-copy (subseq line
                                             (1+ in-quote)
                                             (1- x)))))
          ;; Copy the element to CSV-LINE
          (if string-to-copy
              (progn
                (setf csv-line (append csv-line (list string-to-copy)))
                (setf in-quote nil)
                (setf in-string nil)))))
       ;; If we are in a number and hit a separator or EOL,
       ;; copy the new element to the list CSV-LINE
       ((and (not (null in-number))
             (or (char= char-at col-end-char)
                 (eol-p x line)))
        ;; Create the number to put in to the list
        (let ((num-to-copy 0)
              (string-to-num nil)
              (comma-in-num nil))
          ;; Check for EOL
          (if (eol-p x line)
              (setf string-to-num (subseq line in-number (length line)))
            (setf string-to-num (subseq line in-number x)))
          ;; Check for a #\, instead of a #\.
          (setf comma-in-num (position decimal-point string-to-num))
          (if comma-in-num
              (setf (char string-to-num comma-in-num) #\.))
          ;; Convert the string to a number "safely"
          (let ((*read-eval* nil))
            (setf num-to-copy (apply 'read-from-string string-to-num nil)))
          (setf csv-line (append csv-line (list num-to-copy))))
        (setf in-number nil))))
    csv-line))

(defun parse-file (filename &key (separator #\,) (decimal-point #\.))
        "Returns a list of a list of each parsed line of a .csv."
        (check-type filename string)
        (check-type separator character)
        (check-type decimal-point character)
        (let ((csv-file (get-text filename))
                                (csv-list nil))

                (dotimes (x (length csv-file))
                        (setf csv-list (append csv-list (list
                                                                                                                                                         (parse-line
                                                                                                                                                                (nth x csv-file)
                                                                                                                                                                separator
                                                                                                                                                                decimal-point)))))
                csv-list))

(defun line-to-string (l separator decimal-point)
  "Returns a string of a list whose elements are separated
   by the character SEPARATOR.  If there are numbers in
   the list, it will convert the decimal point to the
   character DECIMAL-POINT."
  (let ((output nil))
    (dotimes (x (length l))
      (cond
       ;; NIL
       ((null (nth x l))
        (setf output (concatenate 'string output (string separator))))
       ;; A number
       ((numberp (nth x l))
        (let* ((num-to-string (princ-to-string (nth x l)))
               ;; If we have a decimal point, flag it's position
               (period-in-string (position #\. num-to-string)))
          ;; Convert the decimal point if needed
          (if period-in-string
              (setf (char num-to-string period-in-string) decimal-point))
          ;; Dump it to our output string
          ;; Check if this is the first object
          (if (null output)
              (setf output num-to-string)
            (if (null (nth (1- x) l))
                ;; Check to see if the previous object was NIL
                (setf output (concatenate 'string output num-to-string))
              (setf output (concatenate 'string output (string separator)
                                        num-to-string))))))
       (t
        ;; Default (always string?)
        ;; Check the string for a SEPARATOR in it;
        ;;   if it has one, then quote it
        (let ((separator-check (position separator (nth x l))))
          (if separator-check
              (let ((*print-escape* nil))
                (setf (nth x l) (format nil "\"~A\"" (nth x l))))))

        ;; Check if this is the first object
        (if (null output)
            (setf output (princ-to-string (nth x l)))
          (if (null (nth (1- x) l))
              ;; Check to see if the previous object was NIL
              (setf output (concatenate 'string output
                                        (princ-to-string (nth x l))))
            (setf output (concatenate 'string output (string separator)
                                      (princ-to-string (nth x l)))))))))
    output))

(defun file-to-string (l separator decimal-point)
  "Returns a string of a list of lists that contain
   each element of a previously PARSEd-FILE."
  (let ((output nil))
    (dolist (x l)
      (if output
          (setf output
                (concatenate 'string output
                             (string #\Newline)
                             (line-to-string x
                                             separator
                                             decimal-point)))
        (setf output (line-to-string x separator decimal-point))))
    output))

(defun save-file (l filename &key (separator #\,) (decimal-point #\.))
  "Returns T if the list L has been converted back to
   text and saved to the file FILENAME.  Creates FILENAME
   if it doesn't exist."
  (check-type l list)
  (check-type filename string)
  (check-type separator character)
  (check-type decimal-point character)
  (let ((output nil))
    ;; Convert L to a string and place in OUTPUT
    (if l
        (setf output (file-to-string l separator decimal-point))
      (setf output nil))
    ;; Save OUTPUT to a (new?) file named FILENAME
    (if output
        (progn
          (with-open-file (out filename
                               :direction :output :if-exists :supersede)
                          (write-string output out))
          t)
      nil)))

(defun eol-p (x s)
  "Returns T if integer X is at the EOL for string S."
  (check-type x integer)
  (check-type s string)
  (if (= x (1- (length s)))
      t
    nil))

(in-package :cl-user)
