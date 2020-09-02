;;; gdscript-debugger.el --- Description -*- lexical-binding: t; -*-

;; /Applications/Godot.app/Contents/MacOS/Godot --path /Users/pepa/godot/platformer/ -d --remote-debug 127.0.0.1:6009 --breakpoints res://scenes/world/Player.gd:88

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Bindat-Examples.html#Bindat-Examples

;; https://godotforums.org/discussion/18533/understanding-godot-low-level-networking-protocol

;; decimal to hex.
;; (format "%x" 20)

;; hex to decimal.
;; (format "%d" #x000014)
;; (format "%b" #x7FFFFFFF)

;; parse-ssh-pubkey.el
;; https://gist.github.com/mogigoma/3174341

(require 'bindat)
(require 'generator)

;; (eval (if (eq (bindat-get-field struct 'data-type) 4) str byte))

;; gdscript-debugger

;; Overlay arrow markers
(defvar gdscript-debugger--thread-position nil)

(defvar gdscript-debugger--boolean-spec
  '((:boolean-data u32r)))

(defvar gdscript-debugger--integer-spec
  '((:integer-data u32r)))

;; Credit goes to https://github.com/skeeto/bitpack/blob/master/bitpack.el
(defsubst bitpack--load-f32 (b0 b1 b2 b3)
  (let* ((negp (= #x80 (logand b0 #x80)))
         (exp (logand (logior (ash b0 1) (ash b1 -7)) #xff))
         (mantissa (logior #x800000
                           (ash (logand #x7f b1) 16)
                           (ash b2 8)
                           b3))
         (result (if (= #xff exp)
                     (if (= #x800000 mantissa)
                         1.0e+INF
                       0.0e+NaN)
                   (ldexp (ldexp mantissa -24) (- exp 126)))))
    (if negp
        (- result)
      result)))

(defvar gdscript-debugger--float-spec
  '((:float-byte-a byte)
    (:float-byte-b byte)
    (:float-byte-c byte)
    (:float-byte-d byte)
    (:float-value eval (bitpack--load-f32
                        (bindat-get-field struct :float-byte-d)
                        (bindat-get-field struct :float-byte-c)
                        (bindat-get-field struct :float-byte-b)
                        (bindat-get-field struct :float-byte-a)))
    ;; (eval (progn
    ;;         (message "IN A FLOAT %s" last)
    ;;         ;; (gd-bytes last)
    ;;         )
    ;;       )
    ))

(defvar gdscript-debugger--string-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)))

(defvar gdscript-debugger--vector2-spec
  '((:float-byte-a byte)
    (:float-byte-b byte)
    (:float-byte-c byte)
    (:float-byte-d byte)
    (:x eval (bitpack--load-f32
                        (bindat-get-field struct :float-byte-d)
                        (bindat-get-field struct :float-byte-c)
                        (bindat-get-field struct :float-byte-b)
                        (bindat-get-field struct :float-byte-a)))
    (:float-byte-e byte)
    (:float-byte-f byte)
    (:float-byte-g byte)
    (:float-byte-h byte)
    (:y eval (bitpack--load-f32
                        (bindat-get-field struct :float-byte-h)
                        (bindat-get-field struct :float-byte-g)
                        (bindat-get-field struct :float-byte-f)
                        (bindat-get-field struct :float-byte-e)))))

(defvar gdscript-debugger--dictionary-spec
  '((:elements u32r)
    (:dictionary-length eval (* 2 last))
    (:items repeat (:dictionary-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debugger--array-spec
  '((:array-length u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    ;;(eval (message "ARRAY size: %s" last))
    (:items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))

(defvar gdscript-debugger--pool-vector-2-array-spec
  '((:array-length u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    ;;(eval (message "pool-vector-2-array-spec size: %s" last))
    (:items repeat (:array-length) (struct gdscript-debugger--vector2-spec))
    ;;(:items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))

(defvar gdscript-debugger--rid-spec nil) ;; (unsupported)

(defvar gdscript-debugger--object-as-id
  '((:object-as-id-a u32r)
    (:object-as-id-b u32r)
    (:long eval (let ((a (bindat-get-field struct :object-as-id-a))
                      (b (bindat-get-field struct :object-as-id-b)))
                  (logior (lsh b 32) a)))
    (eval (message "gdscript-debugger--object-as-id size: %s %s" (bindat-get-field struct :object-as-id-a) (bindat-get-field struct :object-as-id-b)))))

(defvar gdscript-debugger--unknown-spec-6 '((eval (message "IN A SPEC 6"))))
(defvar gdscript-debugger--unknown-spec-7 '((eval (message "IN A SPEC 7"))))
(defvar gdscript-debugger--unknown-spec-8 '((eval (message "IN A SPEC 8"))))
(defvar gdscript-debugger--unknown-spec-9 '((eval (message "IN A SPEC 9"))))
(defvar gdscript-debugger--unknown-spec-10 '((eval (message "IN A SPEC 10"))))
(defvar gdscript-debugger--unknown-spec-11 '((eval (message "IN A SPEC 11"))))
(defvar gdscript-debugger--unknown-spec-12 '((eval (message "IN A SPEC 12"))))
(defvar gdscript-debugger--unknown-spec-13 '((eval (message "IN A SPEC 13"))))
(defvar gdscript-debugger--unknown-spec-14 '((eval (message "IN A SPEC 14"))))
(defvar gdscript-debugger--unknown-spec-15 '((eval (message "IN A SPEC 15"))))
(defvar gdscript-debugger--unknown-spec-17 '((eval (message "IN A SPEC 17"))))
(defvar gdscript-debugger--unknown-spec-20 '((eval (message "IN A SPEC 20"))))
(defvar gdscript-debugger--unknown-spec-21 '((eval (message "IN A SPEC 21"))))
(defvar gdscript-debugger--unknown-spec-22 '((eval (message "IN A SPEC 22"))))
(defvar gdscript-debugger--unknown-spec-23 '((eval (message "IN A SPEC 23"))))
;;(defvar gdscript-debugger--unknown-spec-24 '((eval (message "IN A SPEC 24"))))
(defvar gdscript-debugger--unknown-spec-25 '((eval (message "IN A SPEC 25"))))
(defvar gdscript-debugger--unknown-spec-26 '((eval (message "IN A SPEC 26"))))
(defvar gdscript-debugger--unknown-spec-27 '((eval (message "IN A SPEC 27"))))

(defvar gdscript-debugger--my-spec
  '((eval (message "IN A SPEC %s" (bindat-get-field struct :data-type)))))

(defvar godot-data-bindat-spec
  `((:data-type     u32r)
    ;;(:masked-data-type eval (logand (bindat-get-field struct :data-type) #xff))
    (eval (message "---------------------------------------------------------------------------------------------------- %s %s" last (logand (bindat-get-field struct :data-type) #xff)))
    (union (:data-type)
           (1 (struct gdscript-debugger--boolean-spec))
           (2 (struct gdscript-debugger--integer-spec))
           (3 (struct gdscript-debugger--float-spec))
           (4 (struct gdscript-debugger--string-spec))
           (5 (struct gdscript-debugger--vector2-spec))
           (6 (struct gdscript-debugger--unknown-spec-6))
           (7 (struct gdscript-debugger--unknown-spec-7))
           (8 (struct gdscript-debugger--unknown-spec-8))
           (9 (struct gdscript-debugger--unknown-spec-9))
           (10 (struct gdscript-debugger--unknown-spec-10))
           (11 (struct gdscript-debugger--unknown-spec-11))
           (12 (struct gdscript-debugger--unknown-spec-12))
           (13 (struct gdscript-debugger--unknown-spec-13))
           (14 (struct gdscript-debugger--unknown-spec-14))
           (15 (struct gdscript-debugger--unknown-spec-15))
           (16 (struct gdscript-debugger--rid-spec))
           (17 (struct gdscript-debugger--unknown-spec-17))
           (,(+ 17 (lsh 1 16)) (struct gdscript-debugger--object-as-id))
           (18 (struct gdscript-debugger--dictionary-spec))
           (19 (struct gdscript-debugger--array-spec))
           (20 (struct gdscript-debugger--unknown-spec-20))
           (21 (struct gdscript-debugger--unknown-spec-21))
           (22 (struct gdscript-debugger--unknown-spec-22))
           (23 (struct gdscript-debugger--unknown-spec-23))
           (24 (struct gdscript-debugger--pool-vector-2-array-spec))
           (25 (struct gdscript-debugger--unknown-spec-25))
           (26 (struct gdscript-debugger--unknown-spec-26))
           (27 (struct gdscript-debugger--unknown-spec-27))
           (t (struct gdscript-debugger--my-spec)))))

(defvar gdscript-debugger--previous-packet-data nil)

(defvar gdscript-debugger--packet-length-bindat-spec
  '((:packet-length u32r)))


;; (defvar packet-bindat-spec
;;   '((eval (message "CALLED packet-bindat-spec"))
;;     (:packet-length u32r)
;;     (eval (message "Packet size %s, index %s" last bindat-idx))
;;     (eval
;;      ;; Let's check if we have enough data for current packet
;;      (if (> (+ bindat-idx last) (length bindat-raw))
;;          (progn
;;            (setq previous-packet-data (substring bindat-raw bindat-idx (length bindat-raw)))
;;            (message " ------------ Ups, we need next data AAAA:")
;;            nil)
;;        (progn
;;          (message "BEFORE Packet size %s, index %s" last bindat-idx)
;;          ;;(struct godot-data-bindat-spec)
;;          (bindat-unpack godot-data-bindat-spec bindat-raw bindat-idx)
;;          (message "AFTER  Packet size %s, index %s" last bindat-idx)
;;          (eval
;;           ;; Let's check if we have any data left
;;           (if (eq (+ last bindat-idx) (length bindat-raw))
;;               (progn (message " ----------- Ups, we need next data BBBB")
;;                      nil)
;;             (bindat-unpack packet-bindat-spec bindat-raw (+ last bindat-idx)))))))))

;; (defvar packet-type-bindat-spec
;;   '((:data-type     u32r)
;;     (eval (progn
;;             (let ((type (bindat-get-field struct :data-type)))
;;               (message "Data type: %s" type))))
;;     (union (:data-type)
;;            (4 (struct string-spec);; (progn
;;                 ;; ;;(setq string-size last)
;;                 ;; (struct string-spec))
;;               )
;;            (t (struct gdscript-debugger--my-spec)))
;;     (eval (message "STRING: %s" (bindat-get-field struct :string-data)))
;;     ;;(data vec (data-length) (eval (if t str str)))
;;     (align 4)))

(defun gdscript-debugger--current-packet (content offset)
  (bindat-unpack gdscript-debugger--packet-length-bindat-spec content offset))

(defun gdscript-debugger--process-packet (content offset)
  (bindat-unpack godot-data-bindat-spec content offset))

(iter-defun command-iter (content)
  (let* ((content (concat gdscript-debugger--previous-packet-data content))
         (content-length (length content))
         (offset 0))
    (message "(content received): %s" (length content))
    (while (< offset content-length)
      (let* ((packet-length-data (gdscript-debugger--current-packet content offset))
             (packet-length (bindat-get-field packet-length-data :packet-length))
             (next-packet-offset (+ 4 offset packet-length)))
        ;;(message "packet-length-data: %s" packet-length-data)
        (message "offset %s packet-length     : %s" offset packet-length)
        (if (< next-packet-offset content-length)
            (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
              (message "packet-data %s - %s       : %s %s %s" (+ 4 offset)  next-packet-offset (type-of packet-data) (type-of (car packet-data)) (car packet-data))
              (iter-yield packet-data)
              (setq offset next-packet-offset))
          (progn
            (setq gdscript-debugger--previous-packet-data (substring content offset content-length))

            (message "UPS, we need more data!!!!!!!!!!!!!!!!!!!!!!!!!!! %s %s" next-packet-offset content-length)
            (cond
             ((eq next-packet-offset content-length)
              (message "But since %s %s are equals we need to process last packet" next-packet-offset content-length)
              (message "Last packet %s - %s" (+ 4 offset) next-packet-offset)
              (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
                (iter-yield packet-data)
                (setq gdscript-debugger--previous-packet-data nil)
                (message "packet-data %s - %s     : %s" (+ 4 offset)  next-packet-offset packet-data))
              ))
            (setq offset next-packet-offset) ;; to break out of loop
            ))))))

(defsubst get-boolean (struct-data)
  (bindat-get-field struct-data :boolean-data))

(defsubst get-integer (struct-data)
  (bindat-get-field struct-data :integer-data))

(defsubst get-float (struct-data)
  (bindat-get-field struct-data :float-value))

(defsubst get-string (struct-data)
  (bindat-get-field struct-data :string-data))


(defsubst to-rid (struct-data)
  (rid-create))

(defsubst to-vector2 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y)))
    (vector2-create :x x :y y)))

(defsubst to-boolean (struct-data)
  (prim-bool-create :value (if (eq 1 (get-boolean struct-data))
                             t
                           nil)))

(defsubst to-integer (struct-data)
  (prim-integer-create :value (get-integer struct-data)))

(defsubst to-float (struct-data)
  (prim-float-create :value (get-float struct-data)))

(defsubst to-string (struct-data)
  (prim-string-create :value (get-string struct-data)))

(defsubst to-object-id (struct-data)
  (object-id-create :value (bindat-get-field struct-data :long)))

(defsubst to-dictionary (struct-data)
  (let* ((items (bindat-get-field struct-data :items)))
    (dictionary-create :elements (to-dic items))))

(defun to-dic (xs)
  (let ((variables))
    (loop for (k v) on xs by (function cddr)
          do (setq variables (cons (from-key-value k v) variables)))
    variables))

(defun from-key-value (key value)
  (let* ((var-name (bindat-get-field key :string-data))
         (var-type (bindat-get-field value :data-type))
         (var-val (pcase var-type
                    (1 (to-boolean value))
                    (2 (to-integer value))
                    (3 (to-float value))
                    (4 (to-string value))
                    (5 (to-vector2 value))
                    (16 (to-rid value))
                    ((pred (= (+ 17 (lsh 1 16)))) (to-object-id value))
                    (18 (to-dictionary value)))))
    `(,var-name . ,var-val)))

;; ((:items
;;((:string-data . file) (:data-length . 4) (:data-type . 4))
;;((:string-data . res://scenes/world/Player.gd) (:data-length . 28) (:data-type . 4))
;;((:string-data . line) (:data-length . 4) (:data-type . 4))
;;((:integer-data . 151) (:data-type . 2))) (:dictionary-length . 4) (:data-type . 18))

(defun stack-data-to-plist (stack-data)
  (pcase stack-data
    (`(,file-key, file-value, line-key, line-value, function-key, function-value, id-key, id-value)
     `(file ,(get-string file-value)
            line ,(get-integer line-value)
            function ,(get-string function-value)))))

(defun error-data-to-plist (error-data)
  (pcase error-data
    (`(,hr, min, sec, msec, source-func, source-file, source-line, error-msg, error-descr, warning)
     `(hr ,(get-integer hr)
          min ,(get-integer min)
          sec ,(get-integer sec)
          msec ,(get-integer msec)
          source-func ,(get-string source-func)
          source-file ,(get-string source-file)
          source-line ,(get-integer source-line)
          error-msg ,(get-string error-msg)
          error-descr ,(get-string error-descr)
          warning, (get-boolean warning)))))

(defun mk-error (iter)
  (let ((callstack-size (bindat-get-field (iter-next iter) :integer-data))
        (error-data (bindat-get-field (iter-next iter) :items))
        (error-callstack-size (bindat-get-field (iter-next iter) :integer-data))
        ;; TODO process call stack
        ;; (error-callstack-size (bindat-get-field (iter-next iter) :integer-data))
        )
    `(command "error" callstack-size ,callstack-size error-data ,(error-data-to-plist error-data) error-callstack-size, error-callstack-size)))

(defun mk-performance (iter)
  (let ((skip-this (iter-next iter))
        (performance-data (bindat-get-field (iter-next iter) :items)))
    `(command "performace" performance-data ,performance-data)))

(defun read-var-names (iter count)
  (let ((variables))
    (dotimes (i count)
      (let* ((var-name (bindat-get-field (iter-next iter) :string-data))
             (var-value (iter-next iter))
             (var-type (bindat-get-field var-value :data-type))
             (var-val (pcase var-type
                (1 (to-boolean var-value))
                (2 (to-integer var-value))
                (3 (to-float var-value))
                (4 (to-string var-value))
                (5 (to-vector2 var-value))
                (16 (to-rid var-value))
                ((pred (= (+ 17 (lsh 1 16)))) (to-object-id var-value))
                (18 (to-dictionary var-value)))))
        ;;(message "[read-var-names] VAR-VALUE: %s" var-value)
        ;;(message "[read-var-names] VAR-VALUE: type %s %s" var-type var-val)

        (setq variables (cons `(,var-name . ,var-val) variables))))
    variables))

(defun mk-stack-frame-vars (iter)
  (let* ((total-size (get-integer (iter-next iter)))
         (locals-size (get-integer (iter-next iter)))
         (var-names (read-var-names iter locals-size))
         (members-size (get-integer (iter-next iter)))
         (members-names (read-var-names iter members-size))
         (globals-size (get-integer (iter-next iter)))
         (globals-names (read-var-names iter globals-size)))

    `(command "stack_frame_vars"
              total-size ,total-size
              locals-size ,locals-size
              var-names, var-names
              members-size ,members-size
              members-names ,members-names
              globals-size ,globals-size
              globals-names ,globals-names)))

(defun mk-stack-dump (iter)
  (let ((stack-level-count (get-integer (iter-next iter)))
        (stack-data (bindat-get-field (iter-next iter) :items)))
    `(command "stack_dump" stack-dump , (stack-data-to-plist stack-data))))

(defun mk-output (iter)
  (let ((output-count (bindat-get-field (iter-next iter) :integer-data))
        (outputs))
    (message "output-count: %s %s" output-count (type-of output-count))
    (dotimes (i output-count)
      (let ((output (bindat-get-field (iter-next iter) :string-data)))
        (setq outputs (cons output outputs))))
    `(command "output" outputs, outputs)))

(defun mk-debug-enter (iter)
  (let ((skip-this (iter-next iter))
        (can-continue (bindat-get-field (iter-next iter) :boolean-data))
        (reason (bindat-get-field (iter-next iter) :string-data)))
    `(command "debug_enter" can-continue ,can-continue reason, reason)))

(defun mk-debug-exit (iter)
  (let ((skip-this (iter-next iter)))
    '(command "debug_exit")))

(defun line-posns (line)
  "Return a pair of LINE beginning and end positions."
  (let ((offset (1+ (- line (line-number-at-pos)))))
    (cons
     (line-beginning-position offset)
     (line-end-position offset))))

(defsubst gdscript-debugger--drop-res (file-path)
  (substring file-path (length "res://")))

(defun gdscript-debugger--on-stack-dump (plist)
  (let* ((file (plist-get plist 'file))
         (line (plist-get plist 'line))
         (project-root (gdscript-util--find-project-configuration-file))
         (full-file-path (concat project-root (gdscript-debugger--drop-res file))))
    (with-current-buffer (find-file full-file-path)
      (let* ((posns (line-posns line))
             (start-posn (car posns)))
        (set-marker gdscript-debugger--thread-position start-posn (current-buffer))
        (goto-char gdscript-debugger--thread-position)))))

(defun gdscript-debugger--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  (message "(DATA received): %s" (length content))
  (message "(Old DATA): %s" (length gdscript-debugger--previous-packet-data))

  (condition-case x
      (let ((iter (command-iter content)))
        (while t
          (pcase (bindat-get-field (iter-next iter) :string-data)
            ("debug_enter" (let ((cmd (mk-debug-enter iter)))
                             (message "Debug_enter: %s" cmd)))
            ("debug_exit" (let ((cmd (mk-debug-exit iter)))
                            (message "Debug_exit: %s " cmd)))
            ("output" (let ((cmd (mk-output iter)))
                        (message "Output: %s" (plist-get cmd 'outputs))
                        ;; (dolist (element (plist-get cmd 'outputs))
                        ;;   (message "output: %s" element))
                        ))
            ("error" (let ((cmd (mk-error iter)))
                       (message "Error: %s" cmd)))
            ("performance" (let ((cmd (mk-performance iter)))
                             (message "Performace: %s" cmd)))
            ("stack_dump" (let ((cmd (mk-stack-dump iter)))
                            (message "Stack dump %s" cmd)
                            (gdscript-debugger--on-stack-dump (plist-get cmd 'stack-dump))))
            ("stack_frame_vars" (let ((cmd (mk-stack-frame-vars iter)))
                            (message "Stack frame vars %s" cmd))))))
    (iter-end-of-sequence (message "No more packets to process %s" x))))

  ;;(message "(stringp content): %s" (stringp content))
  ;;(message "(type-of content): %s" (type-of content))

  ;;(content-2 (concat previous-packet-data content))

  ;; (let* ((content (concat gdscript-debugger--previous-packet-data content))
  ;;        (content-length (length content))
  ;;        (offset 0)
  ;;        (plist))
  ;;   (message "(content received): %s" (length content))
  ;;   (while (< offset content-length)
  ;;     (let* ((packet-length-data (gdscript-debugger--current-packet content offset))
  ;;            (packet-length (bindat-get-field packet-length-data :packet-length))
  ;;            (next-packet-offset (+ 4 offset packet-length)))
  ;;       ;;(message "packet-length-data: %s" packet-length-data)
  ;;       (message "offset %s packet-length     : %s" offset packet-length)
  ;;       (if (< next-packet-offset content-length)
  ;;           (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
  ;;             (message "packet-data %s - %s       : %s" (+ 4 offset)  next-packet-offset packet-data)
  ;;             (setq offset next-packet-offset))
  ;;         (progn
  ;;           (setq gdscript-debugger--previous-packet-data (substring content offset content-length))
  ;;
  ;;           (message "UPS, we need more data!!!!!!!!!!!!!!!!!!!!!!!!!!! %s %s" next-packet-offset content-length)
  ;;           (cond
  ;;            ((eq next-packet-offset content-length)
  ;;             (message "But since %s %s are equals we need to process last packet" next-packet-offset content-length)
  ;;             (message "Last packet %s - %s" (+ 4 offset) next-packet-offset)
  ;;             (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
  ;;               (message "packet-data %s - %s     : %s" (+ 4 offset)  next-packet-offset packet-data))
  ;;             ))
  ;;           (setq offset next-packet-offset) ;; to break out of loop
  ;;           )))))
  ;;(message "(DATA processed): %s" (length content)))

;; (defun ignore-handler (packet-data plist name))
;;
;; (defmacro gd-string-handler (packet-data plist name)
;;   (let ((string (bindat-get-field ,packet-data :string-data)))
;;     (setq ,plist (plist-put ,plist ,name string))))
;;
;; (defmacro gd-boolean-handler (packet-data plist name)
;;   `(pcase (bindat-get-field ,packet-data :boolean-data)
;;   ;;`(pcase ,packet-data
;;     (0 (setq ,plist (plist-put ,plist ,name nil)))
;;     (1 (setq ,plist (plist-put ,plist ,name t)))))
;;
;; ;; (bindat-get-field '((stopped-threads . "all") (thread-id . "1") (reason . "end-stepping-range")) 'reason)
;;
;; (defun plist-test(x)
;;   (let ((plist))
;;     (gd-boolean-handler x plist 'hello)
;;     (message "HERE %s" plist)))
;;
;; ;; continuation handling http://web.mit.edu/Emacs/source/emacs/lisp/server.el
;;
;; (plist-get '(command "debug_enter" can-continue t reason "Breakpoint") 'reason)
;;
;; (defun command-dispatcher (packet-data)
;;   (pcase (bindat-get-field packet-data :string-data)
;;     ;; packet_peer_stream->put_var("debug_enter");
;;     ;; packet_peer_stream->put_var(2);
;;     ;; packet_peer_stream->put_var(p_can_continue);
;;     ;; packet_peer_stream->put_var(p_script->debug_get_error());
;;     ("debug_enter" '(ignore-handler gd-boolean-handler gd-string-handler))
;;     ("debug_exit")
;;     ("output")
;;     ("error")
;;     ("performance"))
;;   )

;;(defvar current-process nil)

(defvar server-clients '()
  "List with client processes")


(defun gdscript-debugger--sentinel-function (process event)
  "Gets called when the status of the network connection changes."
  (message "[sentinel] process: %s" process)
  (message "[sentinel] event  : %s" event)

  (cond
   ((string-match "open from .*\n" event)
    (push process server-clients))
   ((string= event "connection broken by remote peer\n")
    (message "Resetting server to accept data")
    (setq gdscript-debugger--previous-packet-data nil)
    (setq server-clients '()))
   ((eq (process-status process) 'closed)
    (message "EHHHH ???"))))

(defmacro gdscript-debugger--send-command (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase server-clients
     (`() (message "No game process is running."))
     (`(,server-process)
      (let ((command (progn ,@body)))
        (process-send-string server-process command)
        ;;(message "%s %s" server-process (process-status server-process))
        ))
     (_ (message "More than one game process running"))))

;;(print (macroexpand '(gdscript-debugger--send-command server-process (message "HIII %s" server-process))))

(defun gdscript-debugger-get-stack-dump()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "get_stack_dump")))

(defun gdscript-debugger-continue()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "continue")))

(defun gdscript-debugger-next()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "next")))

;;;###autoload
(defun gdscript-debugger-make-server()
  (interactive)

  (setq gdscript-debugger--thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdscript-debugger--thread-position)

  ;;(set-marker gdscript-debugger--thread-position (point) (current-buffer))


  ;; (make-network-process
  ;;  :name "echo-server"
  ;;  :buffer "*echo-server*"
  ;;  :family 'ipv4
  ;;  :service echo-server-port
  ;;  :sentinel 'gdscript-debugger--sentinel-function
  ;;  :filter 'gdscript-debugger--handle-server-reply
  ;;  :server 't)

  (let ((server-process
         (make-network-process
          :name "DEBUG"
          :buffer "*my-server22*"
          :server t
          :host "127.0.0.1"
          :service 6009
          :coding 'binary
          :family 'ipv4
          :filter #'gdscript-debugger--handle-server-reply
          :filter-multibyte nil
          :sentinel #'gdscript-debugger--sentinel-function)))
    (message "server-process: %s" server-process)
    (message "(type-of server-process): %s" (type-of server-process))))


;; (defun string-bindat-spec (len)
;;   '((string-length str ,len)))


;; (defun read-type (type size offset content data)
;;   (pcase type
;;     (4 (progn
;;          (message "Reading String of size %s %s" size data)
;;          ;; (let* ((str (bindat-unpack (string-bindat-spec size) content offset))
;;          ;;        (cmd (bindat-get-field str 'string-length)))
;;          ;;   (message "CMD: %s" cmd)
;;          ;;   )
;;          ))
;;     (_ (message "UNKNOWN TYPE %s" type))))

(defun gdscript-debugger--get-stack-frame-vars-definition (command-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:frame-type u32r)
    (:frame u32r)))

(defun gdscript-debugger--breakpoint-packet-definition (command-length file-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:file-type u32r)
    (:file-length u32r)
    (:file str ,file-length)
    (align 4)
    (:line-type u32r)
    (:line u32r)
    (:boolean-type u32r)
    (:boolean u32r)))


(defconst variant-bool 1 "bool")
(defconst variant-integer 2 "integer")
(defconst variant-float 3 "float")
(defconst variant-string 4 "string")
(defconst variant-array 19 "array")

(defun boolean-to-integer (b)
  (if (null b) 0 1))

(defun gdscript-debugger--get-stack-frame-vars (frame)
  (let* ((command "get_stack_frame_vars")
         (command-length (length command))
         (command-alength (align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debugger--get-stack-frame-vars-definition command-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 2)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:frame-type . ,variant-integer)
       (:frame . ,frame)))))

(defun gdscript-debugger--breakpoint-command (file line add-or-remove)
  (message "[ADDING BREAKPOINT] file %s , line: %s" file line)
  (let* ((command "breakpoint")
         (command-length (length command))
         (command-alength (align-length command))
         (file-length (length file))
         (packet-length (+ (* 10 4) command-alength file-length))
         (spec (gdscript-debugger--breakpoint-packet-definition command-length file-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 4)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:file-type . ,variant-string)
       (:file-length . ,file-length)
       (:file . ,file)
       (:line-type . ,variant-integer)
       (:line . ,line)
       (:boolean-type . ,variant-bool)
       (:boolean . ,(boolean-to-integer add-or-remove))))))

(defun gdscript-debugger--packet-definition (string-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:type u32r)
    (:string-length u32r)
    (:string-data str ,string-length)
    (align 4)))

(defun gdscript-debugger--command (command)
  (message "(gdscript-debugger--packet-definition (length command)): %s" (gdscript-debugger--packet-definition (length command)))
  (let* ((command-alength (align-length command))
         (packet-length (+ (* 4 4) command-alength)))
    (message "packet-length: %s" packet-length)
    (message "command-alength: %s" command-alength)
    (bindat-pack
     (gdscript-debugger--packet-definition (length command))
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 1)
       (:type . ,variant-string)
       (:string-length . ,(length command))
       (:string-data . ,command)))))

(defun align-length (string)
  (let ((len (length string)))
    (while (/= (% len 4) 0)
      (setq len (1+ len)))
    len))

;; (let ((s (gdscript-debugger--command)))
;;        (list (multibyte-string-p s)
;;              (mapconcat (lambda (byte)
;;                           (format "%02x" byte))
;;                         s " ")
;;              (current-time-string)))

(defun gdscript-debugger--add-fringe(pos &rest sprops)
  (interactive)
  (let* ((string (make-string 1 ?x))
         (buffer (current-buffer))
         (prop '(left-fringe breakpoint breakpoint-enabled))
         (overlay (make-overlay pos pos buffer)))
    (put-text-property 0 1 'display prop string)
    (if sprops
        (add-text-properties 0 1 sprops string))
    (overlay-put overlay 'put-break t)
    (overlay-put overlay 'before-string string)))

(defun gdscript-debugger--remove-strings (start end &optional buffer)
  "Remove strings between START and END in BUFFER.
Remove only strings that were put in BUFFER with calls to `gdscript-debugger--add-fringe'.
BUFFER nil or omitted means use the current buffer."
  (unless buffer
    (setq buffer (current-buffer)))
  (dolist (overlay (overlays-in start end))
    (when (overlay-get overlay 'put-break)
      (delete-overlay overlay))))

;;(make-overlay (line-beginning-position) (line-beginning-position) 'before-string)

(defun gdscript-debugger--current-file ()
  (concat "res://"
          (gdscript-util--get-godot-project-file-path-relative buffer-file-name)
          "." (file-name-extension buffer-file-name)))

(defun gdscript-debugger--remove-breakpoint ()
  (interactive)
  (gdscript-debugger--send-command
    (let ((start (line-beginning-position))
          (end (line-end-position))
          (file (gdscript-debugger--current-file))
          (line (line-number-at-pos)))
      (gdscript-debugger--remove-strings start end)
      (gdscript-debugger--breakpoint-command file line nil))))

(defun gdscript-debugger--add-breakpoint ()
  (interactive)
  (gdscript-debugger--send-command
    (gdscript-debugger--add-fringe (line-beginning-position) 'gdb-bptno 1)
    (let ((file (gdscript-debugger--current-file))
          (line (line-number-at-pos)))
      (gdscript-debugger--breakpoint-command file line t))))

(defun gdscript-debugger-get-stack-frame-vars ()
  (interactive)
  (gdscript-debugger--send-command
    (gdscript-debugger--get-stack-frame-vars 0)))

(provide 'gdscript-debugger)


;; (set-marker gud-overlay-arrow-position (point) (current-buffer))

;;(object-id-create :value "abc")

;;(vector2-create :x 12 :y 34)

;; (rid-create)

(cl-defstruct (object-id (:constructor object-id-create)
                         (:copier nil)
                         (:conc-name object-id->))
  value)

(cl-defstruct (prim-bool (:constructor prim-bool-create)
                       (:copier nil)
                       (:conc-name boolean->))
  value)

(cl-defstruct (prim-integer (:constructor prim-integer-create)
                       (:copier nil)
                       (:conc-name integer->))
  value)

(cl-defstruct (prim-float (:constructor prim-float-create)
                     (:copier nil)
                     (:conc-name float->))
  value)

(cl-defstruct (prim-string (:constructor prim-string-create)
                      (:copier nil)
                      (:conc-name string->))
  value)

(cl-defstruct (rid (:constructor rid-create)
                   (:copier nil)))

(cl-defstruct (pool-vector2-array (:constructor pool-vector2-array-create)
                                  (:copier nil)
                                  (:conc-name pool-vector2-array->))
  elements)

(cl-defstruct (dictionary (:constructor dictionary-create)
                          (:copier nil)
                          (:conc-name dictionary->))
  elements)

(cl-defstruct (vector2 (:constructor vector2-create)
                       (:copier nil)
                       (:conc-name vector2->))
  x y)



