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

(defvar gdscript-debugger--string-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)
    ;; (eval (message "IN A STRING %s %s" last bindat-idx))
    ))

(defvar gdscript-debugger--array-spec
  '((:array-length   u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    ;;(eval (message "ARRAY size: %s" last))
    (:items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))



(defvar gdscript-debugger--my-spec
  '((eval (message "IN A SPEC %s" last))))

(defvar godot-data-bindat-spec
  '((:data-type     u32r)
    ;;(eval (message "2222:data-type %s" last))
    (union (:data-type)
           (1 (struct gdscript-debugger--boolean-spec))
           (2 (struct gdscript-debugger--integer-spec))
           (4 (struct gdscript-debugger--string-spec))
           (19 (struct gdscript-debugger--array-spec))
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

(defun gdscript-debugger--next-packet (content offset)
  (bindat-unpack gdscript-debugger--packet-length-bindat-spec content offset))

(defun gdscript-debugger--process-packet (content offset)
  (bindat-unpack godot-data-bindat-spec content offset))

(defun gdscript-debugger--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  (message "(DATA received): %s" (length content))

  ;;(message "(stringp content): %s" (stringp content))
  ;;(message "(type-of content): %s" (type-of content))

  ;;(content-2 (concat previous-packet-data content))

  (let* ((content (concat gdscript-debugger--previous-packet-data content))
         (content-length (length content))
         (offset 0))
    (message "(content received): %s" (length content))
    (while (< offset content-length)
      (let* ((packet-length-data (gdscript-debugger--next-packet content offset))
             (packet-length (bindat-get-field packet-length-data :packet-length))
             (next-packet-offset (+ 4 offset packet-length)))
        (if (< next-packet-offset content-length)
            (let ((packet-data (gdscript-debugger--process-packet content (+ 4 offset))))
              (setq offset next-packet-offset)
              (message "packet-length-data: %s" packet-length-data)
              (message "packet-length     : %s" packet-length)
              (message "offset            : %s" offset)
              (message "packet-data       : %s" packet-data))
          (progn
            (setq gdscript-debugger--previous-packet-data (substring content offset content-length))
            (setq offset next-packet-offset)
            (message "UPS, we need more data!!!!!!!!!!!!!!!!!!!!!!!!!!! %s %s" next-packet-offset content-length)
            )))))
  (message "(DATA proces sed): %s" (length content)))

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

(defun gdscript-debugger--packet-definition (string-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-amount u32r)
    (:type u32r)
    (:string-length u32r)
    (:string-data str ,string-length)
    (align 4)))

(defun gdscript-debugger--command (command)
  (message "(gdscript-debugger--packet-definition (length command)): %s" (gdscript-debugger--packet-definition (length command)))
  (bindat-pack
   (gdscript-debugger--packet-definition (length command))
   `((:packet-length . 24)
     (:array-type . 19)
     (:elements-amount . 1)
     (:type . 4)
     (:string-length . ,(length command))
     (:string-data . ,command))))

;; (let ((s (gdscript-debugger--command)))
;;        (list (multibyte-string-p s)
;;              (mapconcat (lambda (byte)
;;                           (format "%02x" byte))
;;                         s " ")
;;              (current-time-string)))

(provide 'gdscript-debugger)
