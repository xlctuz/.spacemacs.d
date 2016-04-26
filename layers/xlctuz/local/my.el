(defun type->ctype (type)
  (cond ((string-equal type "char") "string")
        ((string-equal type "dec") "double")
        ((string-equal type "date") "Date")
        ((string-equal type "int") "int")
        ((string-equal type "NM number") "NM_number")
        ((string-equal type "projection") "Projection")
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

(defun buffer->lines ()
  (remove "" (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))

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
  (mapcar #'line->words (buffer->lines)))

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
          (format "%s = text_read_to_delimiter(reader, \"\\x0B\\x0D\\x0A\");"
                  (field-name field))
        (format "%s = text_read_%s(reader, %s);"
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

(defun hcrfdef->cdef ()
  (let* ((lines (preread-lines))
         (name (to-name (car lines)))
         (fields (remove nil (read-fields (cdr lines)))))
    (end-of-buffer)
    (insert "\n")
    (insert (format "struct %s\n{\n"  (capitalize name)))
    (insert (format-lines (mapcar #'(lambda (field)
                                      (concat "    " (field->cfield field)))
                                  fields)))
    (insert (format "\n};\n\n"))
    (insert (format "%s parse_%s(Text_reader reader)\n"  (capitalize name) name))
    (insert "{\n")
    (insert (format "    %s ret;\n" (capitalize name)))
    (insert (format-lines (mapcar #'(lambda (field)
                                      (concat "    ret." (field->read-statement field)))
                                  fields)))
    (insert "\n    return ret;\n}\n")))


(defun make-debug-type (defination)
  )

(defun debug-type ()
  (let ((str (buffer-substring-no-properties (region-beginning) (region-end))))
    (message str)
    (with-output-to-temp-buffer "*temp*"
      (princ str))))
