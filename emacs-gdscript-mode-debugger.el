;;; emacs-gdscript-mode-debugger.el --- Description -*- lexical-binding: t; -*-

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

(defvar first-only t)

(defvar boolean-spec
  '((:boolean-data u32r)
    (eval (message "IN A BOOLEAN %s" last))))

(defvar integer-spec
  '((:integer-data u32r)
    (eval (message "IN A INTEGER %s" last))))

(defvar string-spec
  '((:data-length   u32r)
    (:string-data str (:data-length))
    (align 4)
    (eval (message "IN A STRING %s %s" last bindat-idx))))

(defvar array-spec
  '((:array-length   u32r)
    ;;(:string-data str (:data-length))
    ;;(align 4)
    ;;(logand (bindat-get-field struct :array-length) #x7FFFFFFF)
    (eval (message "ARRAY size: %s" last))
    (items repeat (:array-length) (struct godot-data-bindat-spec))
    ;;(eval (message "AFTER  IN A ARRAY %s" last))
    ))



(defvar my-spec
  '((eval (message "IN A SPEC %s" last))))

(defvar godot-data-bindat-spec
  '((:data-type     u32r)
    (eval (message "2222:data-type %s" last))
    (union (:data-type)
           (1 (struct boolean-spec))
           (2 (struct integer-spec))
           (4 (struct string-spec))
           (19 (struct array-spec))
           (t (struct my-spec)))))

(defvar previous-packet-data nil)

(defvar packet-bindat-spec
  '((eval (message "CALLED packet-bindat-spec"))
    (:packet-length u32r)
    (eval (message "Packet size %s, index %s" last bindat-idx))
    (eval
     ;; Let's check if we have enough data for current packet
     (if (> (+ bindat-idx last) (length bindat-raw))
         (progn
           (setq previous-packet-data (substring bindat-raw bindat-idx (length bindat-raw)))
           (message " ------------ Ups, we need next data AAAA:")
           nil)
       (progn
         (message "BEFORE Packet size %s, index %s" last bindat-idx)
         ;;(struct godot-data-bindat-spec)
         (bindat-unpack godot-data-bindat-spec bindat-raw bindat-idx)
         (message "AFTER  Packet size %s, index %s" last bindat-idx)
         (eval
          ;; Let's check if we have any data left
          (if (eq (+ last bindat-idx) (length bindat-raw))
              (progn (message " ----------- Ups, we need next data BBBB")
                     nil)
            (bindat-unpack packet-bindat-spec bindat-raw (+ last bindat-idx)))))))))

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
;;            (t (struct my-spec)))
;;     (eval (message "STRING: %s" (bindat-get-field struct :string-data)))
;;     ;;(data vec (data-length) (eval (if t str str)))
;;     (align 4)))

(defun handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  (message "(DATA received): %s" (length content))

  ;;(message "(stringp content): %s" (stringp content))
  ;;(message "(type-of content): %s" (type-of content))

  (let ((content-2 (concat previous-packet-data content)))
    (bindat-unpack packet-bindat-spec content-2))



  ;; (when first-only
  ;;   (setq first-only nil)
  ;;   (message "(received): %s" (length content))
  ;;   (let* (
  ;;

  ;;        ;;(packet-x (bindat-unpack packet-type-bindat-spec (bindat-get-field packet :packet-data)))
  ;;        ;;(length (bindat-get-field what 'data-length))
  ;;        ;;(data (bindat-get-field packet-x :string-data))
  ;;        ;;(offset 12)
  ;;          )
  ;;     ;;(eval (message "DXXXXX %s" bindat-idx))
  ;;   ;;(message "DATA: %s" data)
  ;;   ;;(message "(bindat-get-field what :data-type): %s" (bindat-get-field what 'data-type))
  ;;   ;;(read-type type length offset content data)
  ;;   ;;(message "process: %s" process)
  ;;   ;;(message "packet-x %s" packet-x)
  ;;   ;;(message "what2 %s" what2)
  ;;     ))
  (message "(DATA processed): %s" (length content))
  )

(defun sentinel-function (process event)
  "Gets called when the status of the network connection changes."
  (message "[sentinel] process: %s" process)
  (message "[sentinel] event  : %s" event)
  (when (equal event "connection broken by remote peer\n")
    (message "Resetting server to accept data")
    (setq previous-packet-data nil)
    (setq first-only t)))


(defun make_server()
  (interactive)
  ;; (make-network-process
  ;;  :name "echo-server"
  ;;  :buffer "*echo-server*"
  ;;  :family 'ipv4
  ;;  :service echo-server-port
  ;;  :sentinel 'sentinel-function
  ;;  :filter 'handle-server-reply
  ;;  :server 't)

  (make-network-process
   :name "BEBUG"
   :buffer "*my-server22*"
   :server 't
   :host "127.0.0.1"
   :service 6009
   :coding 'binary
   :family 'ipv4
   :filter #'handle-server-reply
   :filter-multibyte nil
   :sentinel #'sentinel-function))


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



;; (defun rfc868-payload ()
;;        (bindat-pack
;;         '((now-hi u16)
;;           (now-lo u16))
;;         ;; Emacs uses Unix epoch, while RFC868 epoch
;;         ;; is 1900-01-01 00:00:00, which is 2208988800
;;         ;; (or #x83aa7e80) seconds more.
;;         (let ((now (time-add nil '(#x83aa #x7e80))))
;;           `((now-hi . ,(car now))
;;             (now-lo . ,(cadr now))))))

;; (let ((s (rfc868-payload)))
;;        (list (multibyte-string-p s)
;;              (mapconcat (lambda (byte)
;;                           (format "%02x" byte))
;;                         s " ")
;;              (current-time-string)))
