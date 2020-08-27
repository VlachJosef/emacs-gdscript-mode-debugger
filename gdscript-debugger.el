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

;; (eval (if (eq (bindat-get-field struct 'data-type) 4) str byte))

;; gdscript-debugger

(defvar gdscript-debugger--boolean-spec
  '((:boolean-data u32r)
    ;;(eval (message "IN A BOOLEAN %s" last))
    ))

(defvar gdscript-debugger--integer-spec
  '((:integer-data u32r)
    ;;(eval (message "IN A INTEGER %s" last))
    ))

(defvar gdscript-debugger--float-spec
  '((:float-data u32r) ;; How to read float?
    ;;(eval (message "IN A FLOAT %s" last))
    ))

(defvar gdscript-debugger--string-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)
    ;; (eval (message "IN A STRING %s %s" last bindat-idx))
    ))

(defvar gdscript-debugger--dictionary-spec
  '((:dictionary-length   u32r)
    ;;(eval (message "DICTIONARY size: %s" last))
    (:items repeat (:dictionary-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debugger--array-spec
  '((:array-length   u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    ;;(eval (message "ARRAY size: %s" last))
    (:items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))

(defvar gdscript-debugger--pool-vector-2-array-spec
  '((:array-length   u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    (eval (message "pool-vector-2-array-spec size: %s" last))
    ;;(:items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))

(defvar gdscript-debugger--unknown-spec-5 '((eval (message "IN A SPEC 5"))))
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
(defvar gdscript-debugger--unknown-spec-16 '((eval (message "IN A SPEC 16"))))
(defvar gdscript-debugger--unknown-spec-17 '((eval (message "IN A SPEC 17"))))
(defvar gdscript-debugger--unknown-spec-18 '((eval (message "IN A SPEC 18"))))
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
  '((:data-type     u32r)
    ;;(eval (message "2222:data-type %s" last))
    (union (:data-type)
           (1 (struct gdscript-debugger--boolean-spec))
           (2 (struct gdscript-debugger--integer-spec))
           (3 (struct gdscript-debugger--float-spec))
           (4 (struct gdscript-debugger--string-spec))
           (5 (struct gdscript-debugger--unknown-spec-5))
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
           (16 (struct gdscript-debugger--unknown-spec-16))
           (17 (struct gdscript-debugger--unknown-spec-17))
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

(defun gdscript-debugger--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  (message "(DATA received): %s" (length content))
  (message "(Old DATA): %s" (length gdscript-debugger--previous-packet-data))

  ;;(message "(stringp content): %s" (stringp content))
  ;;(message "(type-of content): %s" (type-of content))

  ;;(content-2 (concat previous-packet-data content))

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
              (message "packet-data %s - %s       : %s" (+ 4 offset)  next-packet-offset packet-data)
              (setq offset next-packet-offset))
          (progn
            (setq gdscript-debugger--previous-packet-data (substring content offset content-length))

            (message "UPS, we need more data!!!!!!!!!!!!!!!!!!!!!!!!!!! %s %s" next-packet-offset content-length)
            (cond
             ((eq next-packet-offset content-length)
              (message "But since %s %s are equals we need to process last packet" next-packet-offset content-length)
              (message "Last packet %s - %s" (+ 4 offset) next-packet-offset)
              (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
                (message "packet-data %s - %s     : %s" (+ 4 offset)  next-packet-offset packet-data))
              ))
            (setq offset next-packet-offset) ;; to break out of loop
            )))))
  (message "(DATA processed): %s" (length content)))

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

(defun gdscript-debugger-get-stack-dump()
  (let ((server-process (get-process (car server-clients))))
    (message "%s %s" server-process (process-status server-process))
    (process-send-string server-process (gdscript-debugger--command "get_stack_dump"))))

(defun gdscript-debugger-continue()
  (let ((server-process (get-process (car server-clients))))
    (message "%s %s" server-process (process-status server-process))
    (process-send-string server-process (gdscript-debugger--command "continue"))))

;;;###autoload
(defun gdscript-debugger-make-server()
  (interactive)
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

(defun gdscript-debugger--breakpoint-command (file line)
  (message "[ADDING BREAKPOINT] file %s , line: %s" file line)
  (let* ((command "breakpoint")
         (command-length (length command))
         (file-length (length file))
         (packet-length (+ 2 (* 10 4) command-length file-length)) ;; 2 is for alignment - it needs to be dynamic
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
       (:boolean . 1)))))

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

(defun gdscript-debugger--remove-breakpoint ()
  (interactive)
  (let((start (line-beginning-position))
       (end (line-end-position)))
    (gdscript-debugger--remove-strings start end)))

(defun gdscript-debugger--add-breakpoint ()
  (interactive)
  (gdscript-debugger--add-fringe (line-beginning-position) 'gdb-bptno 1)
  (let ((server-process (get-process (car server-clients)))
        (file (concat "res://"
                 (gdscript-util--get-godot-project-file-path-relative buffer-file-name)
                 "." (file-name-extension buffer-file-name)))
        (line (line-number-at-pos)))
    (process-send-string server-process (gdscript-debugger--breakpoint-command file line))))

(provide 'gdscript-debugger)
