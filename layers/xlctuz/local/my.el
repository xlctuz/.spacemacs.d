(defun type->ctype (type)
  (cond ((string-equal type "char") "string")
        ((string-equal type "dec") "double")
        ((string-equal type "date") "Date")
        ((string-equal type "int") "int")
        ((string-equal type "NM number") "NM_number")
        ((string-equal type "projection") "Projection")
        ((string-equal type "exp") "double")
        ((string-equal type "latitude") "Latitude")
        ((string-equal type "longitude") "Longitude")
        (t "unknown")))

;; (reduce #'(lambda (l r)
;;             (concat l "_" r))
;;         lst)

(defun to-name (lst)
  (replace-regexp-in-string
   "[^a-z0-9A-Z_]" "_"
   (downcase (cond ((= (length lst) 0) "")
                   ((= (length lst) 1) (car lst))
                   (t (reduce #'(lambda (l r)
                                  (concat l "_" r))
                              lst))))))

(defun front-to-sublist (lst reserve)
  (let ((len (length lst)))
    (if (> len reserve)
        (apply #'list (subseq lst 0 (- (length lst) reserve)) (subseq lst (- (length lst) reserve)))
      lst)))

(defun buffer->lines (start end)
  (remove "" (split-string (buffer-substring-no-properties start end) "\n")))

(defun fix-type (words)
  (cond ((< (length words) 3) words)
        ((and (= (length words) 3)
              (string-equal "NM" (car words))
              (string-equal "number" (cadr words)))
         (cons "NM number" (cddr words)))
        ((and (= (length words) 4)
              (string-equal "char" (car words))
              (string-equal "0" (cadr words))
              (string-equal "-" (caddr words))
              (string-equal "500" (cadddr words)))
         '("char" "0-500"))
        (t (cons (car words) (fix-type (cdr words))))))

(defun line->words (line)
  (fix-type (split-string line)))

(defun preread-lines ()
  (mapcar #'line->words (buffer->lines (point-min) (point-max))))

(defun read-fields (lines)
  (mapcar #'(lambda (lst)
              (cons (to-name (car lst)) (cdr lst)))
          (mapcar #'(lambda (lst)
                      (front-to-sublist lst 2))
                  lines)))

(defun field-name (field)
  (car field))

(defun field-type (field)
  (cadr field))

(defun field-length (field)
  (nth 2 field))

(defun field->cfield (field)
  (concat (type->ctype (field-type field)) " "  (field-name field) ";"))

(defun field->read-statement (field)
  (if (= (length field) 3)
      (if (string-equal (field-length field) "0-500")
          (format "%s = hcrf_read_to_delimiter(reader, \"\\x0B\\x0D\\x0A\");"
                  (field-name field))
        (format "%s = hcrf_read_%s(reader, %s);"
                (field-name field)
                (to-name (split-string (field-type field)))
                (field-length field)))
    ""))

;; (defun format-lines (fields field-formater)
;;   (if (equal fields nil)
;;       "\n"
;;     (concat (funcall field-formater (car fields)) "\n" (format-lines (cdr fields) field-formater))))

;; (defun fields->cfields (fields)
;;   (format-lines fields #'field->cfield))

;; (defun fields->read-statements (fields)
;;   (format-lines fields #'field->read-statement))

(defun format-lines (lst)
  (cond ((= (length lst) 0) "\n")
        ((= (length lst) 1) (concat (car lst) "\n"))
        (t (reduce #'(lambda (l r)
                       (concat l "\n" r))
                   lst))))


(defun make-debug-type ()
  (let* ((temp (mapcar #'split-string (buffer->lines (region-beginning) (region-end))))
         (name (cadr (car temp)))
         (fields (mapcar
                  #'(lambda (field)
                      (subseq field 0 (- (length field) 1)))
                  (mapcar #'cadr (subseq temp 2 (- (length temp) 1))))))

    (concat
     (format "template<>\n")
     (format "ostream& debug_type(ostream&ostr, int indent, const %s& val)\n" name)
     (format "{\n")
     (format "    ostr << endl;\n")
     (reduce #'concat (mapcar #'(lambda (field)
                                  (concat (format "    write_space(ostr, indent) << \"%s: \";\n" field)
                                          (format "    debug_type(ostr, indent+2, val.%s);\n" field)
                                          (format "    ostr << endl;\n" field)))
                              fields))
     (format "    return ostr;\n")
     (format "}\n"))))

(defun debug-type ()
  (let ((str (make-debug-type)))
    (kill-new str)))

(defun hcrfdef->cdef ()
  (let* ((lines (preread-lines))
         (name (to-name (car lines)))
         (fields (remove nil (read-fields (cdr lines)))))
    (with-output-to-temp-buffer "*temp*"
      (let ((str (concat "\n"
                         (format "struct %s\n{\n"  (capitalize name))
                         (format-lines (mapcar #'(lambda (field)
                                                   (concat "    " (field->cfield field)))
                                               fields))
                         (format "\n};\n\n")
                         (format "%s parse_%s(Hcrf_reader reader)\n"  (capitalize name) name)
                         "{\n"
                         (format "    %s ret;\n" (capitalize name))
                         (format-lines (mapcar #'(lambda (field)
                                                   (concat "    ret." (field->read-statement field)))
                                               fields))
                         "\n    return ret;\n}\n")))
        (kill-new str)
        (princ str)))))
