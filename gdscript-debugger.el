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

;; Overlay arrow markers
(defvar gdscript-debugger--thread-position nil)

(defvar gdscript-debugger--null-spec)

(defvar gdscript-debugger--boolean-spec
  '((:boolean-data u32r)))

(defvar gdscript-debugger--integer-spec
  '((:integer-data u32r)))

(defvar gdscript-debugger--integer-64-spec
  '((:data-a u32r)
    (:data-b u32r)
    (:integer-data eval (let ((a (bindat-get-field struct :data-a))
                              (b (bindat-get-field struct :data-b)))
                          (logior (lsh b 32) a)))))

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

(defsubst bitpack--load-f64 (b0 b1 b2 b3 b4 b5 b6 b7)
  (let* ((negp (= #x80 (logand b0 #x80)))
         (exp (logand (logior (ash b0 4) (ash b1 -4)) #x7ff))
         (mantissa (logior #x10000000000000
                           (ash (logand #xf b1) 48)
                           (ash b2 40)
                           (ash b3 32)
                           (ash b4 24)
                           (ash b5 16)
                           (ash b6  8)
                           b7))
         (result (if (= #x7ff exp)
                     (if (= #x10000000000000 mantissa)
                         1.0e+INF
                       0.0e+NaN)
                   (ldexp (ldexp mantissa -53) (- exp 1022)))))
    (if negp
        (- result)
      result)))

(defsubst to-symbol (symbol-name &optional suffix)
  (intern (concat (symbol-name symbol-name) suffix)))

(defmacro capture-float-spec (symbol-name)
  (let ((symbol (to-symbol symbol-name)))
    `(quote ((:vect vec 4 byte)
             (,symbol eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                             (apply 'bitpack--load-f32 alist)))))))

(defvar gdscript-debugger--float-spec
  '((:vect vec 4 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'bitpack--load-f32 alist)))))

(defvar gdscript-debugger--float-64-spec
  '((:vect vec 8 byte)
    (:float-value eval (let ((alist (reverse (append (bindat-get-field struct :vect) nil))))
                         (apply 'bitpack--load-f64 alist)))))

(defvar gdscript-debugger--string-spec
  '((:data-length u32r)
    (:string-data str (:data-length))
    (align 4)))

(defvar gdscript-debugger--string-z-spec
  '((:data-length u32r)
    (:string-data strz (:data-length))
    (align 4)))

(defvar gdscript-debugger--vector2-spec
  `(,@(capture-float-spec :x)
    ,@(capture-float-spec :y)))

(defvar gdscript-debugger--vector3-spec
  `(,@(capture-float-spec :x)
    ,@(capture-float-spec :y)
    ,@(capture-float-spec :z)))

(defvar gdscript-debugger--transform2d-spec
  `(,@(capture-float-spec :xx)
    ,@(capture-float-spec :yx)
    ,@(capture-float-spec :xy)
    ,@(capture-float-spec :yy)
    ,@(capture-float-spec :x-origin)
    ,@(capture-float-spec :y-origin)))

(defvar gdscript-debugger--dictionary-spec
  '((:data u32r)
    (:shared   eval (logand (bindat-get-field struct :data) #x80000000))
    (:elements eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:dictionary-length eval (* 2 last))
    (:items repeat (:dictionary-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debugger--array-spec
  '((:data u32r)
    (:shared       eval (logand (bindat-get-field struct :data) #x80000000))
    (:array-length eval (logand (bindat-get-field struct :data) #x7fffffff))
    (:items repeat (:array-length) (struct godot-data-bindat-spec))))

(defvar gdscript-debugger--pool-int-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debugger--integer-spec))))

(defvar gdscript-debugger--pool-string-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debugger--string-z-spec))))

(defvar gdscript-debugger--pool-vector-2-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debugger--vector2-spec))))

(defvar gdscript-debugger--pool-color-array-spec
  '((:array-length u32r)
    (:items repeat (:array-length) (struct gdscript-debugger--color-spec))))

;;(print (macroexpand '(capture-float-spec :hi)))

(defvar gdscript-debugger--plane-spec
  `(,@(capture-float-spec :normal-x)
    ,@(capture-float-spec :normal-y)
    ,@(capture-float-spec :normal-z)
    ,@(capture-float-spec :distance)))

(defvar gdscript-debugger--aabb-spec
  `(,@(capture-float-spec :x-coordinate)
    ,@(capture-float-spec :y-coordinate)
    ,@(capture-float-spec :z-coordinate)
    ,@(capture-float-spec :x-size)
    ,@(capture-float-spec :y-size)
    ,@(capture-float-spec :z-size)))

(defvar gdscript-debugger--basis-spec
  `(,@(capture-float-spec :xx)
    ,@(capture-float-spec :yx)
    ,@(capture-float-spec :zx)
    ,@(capture-float-spec :xy)
    ,@(capture-float-spec :yy)
    ,@(capture-float-spec :zy)
    ,@(capture-float-spec :xz)
    ,@(capture-float-spec :yz)
    ,@(capture-float-spec :zz)))

(defvar gdscript-debugger--color-spec
  `(,@(capture-float-spec :red)
    ,@(capture-float-spec :green)
    ,@(capture-float-spec :blue)
    ,@(capture-float-spec :alpha)))

(defvar gdscript-debugger--node-path-spec
  '((:data-length u32r)
    (:new-format eval (logand (bindat-get-field struct :data-length) #x80000000))
    (:name-count eval (logand (bindat-get-field struct :data-length) #x7FFFFFFF))
    (:sub-name-count u32)
    (:total eval (+ (bindat-get-field struct :name-count) (bindat-get-field struct :sub-name-count) ))
    (:flags u32)
    (:absolute eval (not (eq 0 (logand (bindat-get-field struct :flags) #x1))))
    (:items repeat (:total) (struct gdscript-debugger--string-spec))))

(defvar gdscript-debugger--rid-spec nil) ;; unsupported

(defvar gdscript-debugger--object-as-id
  '((:object-as-id-a u32r)
    (:object-as-id-b u32r)
    (:long eval (let ((a (bindat-get-field struct :object-as-id-a))
                      (b (bindat-get-field struct :object-as-id-b)))
                  (logior (lsh b 32) a)))))

(defvar gdscript-debugger--unknown-spec-17 '((eval (message "IN A SPEC 17"))))

(defconst encode-mask #xff)
(defconst encode-flag-64 (lsh 1 16))

(defvar godot-data-bindat-spec
  '((:type-data    u32r)
    (:type         eval (logand last encode-mask))
    (:flag-64      eval (logand (bindat-get-field struct :type-data) encode-flag-64))
    (:object-as-id eval (logand (bindat-get-field struct :type-data) encode-flag-64))
    (union (:type)
           (0 nil)
           (1 (struct gdscript-debugger--boolean-spec))
           ((eval (and (eq 2 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debugger--integer-spec))
           (2 (struct gdscript-debugger--integer-64-spec))
           ((eval (and (eq 3 tag) (equal 0 (bindat-get-field struct :flag-64)))) (struct gdscript-debugger--float-spec))
           (3 (struct gdscript-debugger--float-64-spec))
           (4 (struct gdscript-debugger--string-spec))
           (5 (struct gdscript-debugger--vector2-spec))
           (7 (struct gdscript-debugger--vector3-spec))
           (8 (struct gdscript-debugger--transform2d-spec))
           (9 (struct gdscript-debugger--plane-spec))
           (11 (struct gdscript-debugger--aabb-spec))
           (12 (struct gdscript-debugger--basis-spec))
           (14 (struct gdscript-debugger--color-spec))
           (15 (struct gdscript-debugger--node-path-spec))
           (16 (struct gdscript-debugger--rid-spec))
           ((eval (and (eq 17 tag) (equal 0 (bindat-get-field struct :object-as-id)))) (error "[ObjectId] Not implemented yet"))
           (17 (struct gdscript-debugger--object-as-id))
           (18 (struct gdscript-debugger--dictionary-spec))
           (19 (struct gdscript-debugger--array-spec))
           (21 (struct gdscript-debugger--pool-int-array-spec))
           (23 (struct gdscript-debugger--pool-string-array-spec))
           (24 (struct gdscript-debugger--pool-vector-2-array-spec))
           (26 (struct gdscript-debugger--pool-color-array-spec))
           (t (eval (error "Unknown type: %s" tag))))))

(defvar gdscript-debugger--previous-packet-data nil)
(defvar gdscript-debugger--data-needed nil)
(defvar gdscript-debugger--offset 0)

(defvar gdscript-debugger--packet-length-bindat-spec
  '((:packet-length u32r)))

(defun gdscript-debugger--current-packet (content offset)
  (bindat-unpack gdscript-debugger--packet-length-bindat-spec content offset))

(defun gdscript-debugger--process-packet (content offset)
  (bindat-unpack godot-data-bindat-spec content offset))

(iter-defun command-iter ()
  (setq gdscript-debugger--data-needed
        (catch 'not-enough-data-to-process-packed
          (while (< gdscript-debugger--offset (length gdscript-debugger--previous-packet-data))
            (let* ((content gdscript-debugger--previous-packet-data)
                   (content-length (length content))
                   (packet-length-data (gdscript-debugger--current-packet content gdscript-debugger--offset))
                   (packet-length (bindat-get-field packet-length-data :packet-length))
                   (next-packet-offset (+ 4 gdscript-debugger--offset packet-length)))
              (if (<= next-packet-offset content-length)
                  (let ((packet-data (gdscript-debugger--process-packet content (+ 4 gdscript-debugger--offset))))
                    (setq gdscript-debugger--offset next-packet-offset)
                    (iter-yield packet-data))
                (throw 'not-enough-data-to-process-packed next-packet-offset))))))
  (setq gdscript-debugger--offset 0))

(defsubst get-boolean (struct-data)
  (bindat-get-field struct-data :boolean-data))

(defsubst get-integer (struct-data)
  (bindat-get-field struct-data :integer-data))

(defsubst get-float (struct-data)
  (bindat-get-field struct-data :float-value))

(defsubst get-string (struct-data)
  (bindat-get-field struct-data :string-data))

(defsubst get-array (struct-data)
  (bindat-get-field struct-data :items))

(defsubst to-plane (struct)
  (let ((normal-x (bindat-get-field struct :normal-x))
        (normal-y (bindat-get-field struct :normal-y))
        (normal-z (bindat-get-field struct :normal-z))
        (distance (bindat-get-field struct :distance)))
    (plane-create
     :normal-x normal-x
     :normal-y normal-y
     :normal-z normal-z
     :distance distance)))

(defsubst to-aabb (struct)
  (let ((x-coordinate (bindat-get-field struct :x-coordinate))
        (y-coordinate (bindat-get-field struct :y-coordinate))
        (z-coordinate (bindat-get-field struct :z-coordinate))
        (x-size (bindat-get-field struct :x-size))
        (y-size (bindat-get-field struct :y-size))
        (z-size (bindat-get-field struct :z-size)))
    (aabb-create
     :x-coordinate x-coordinate
     :y-coordinate y-coordinate
     :z-coordinate z-coordinate
     :x-size x-size
     :y-size y-size
     :z-size z-size)))

(defsubst to-basis (struct)
  (let ((xx (bindat-get-field struct :xx))
        (yx (bindat-get-field struct :yx))
        (zx (bindat-get-field struct :zx))
        (xy (bindat-get-field struct :xy))
        (yy (bindat-get-field struct :yy))
        (zy (bindat-get-field struct :zy))
        (xz (bindat-get-field struct :xz))
        (yz (bindat-get-field struct :yz))
        (zz (bindat-get-field struct :zz)))
    (basis-create
     :x (vector3-create :x xx :y yx :z zx)
     :y (vector3-create :x xy :y yy :z zy)
     :z (vector3-create :x xz :y yz :z zz))))

(defsubst to-color (struct)
  (let ((red (bindat-get-field struct :red))
        (green (bindat-get-field struct :green))
        (blue (bindat-get-field struct :blue))
        (alpha (bindat-get-field struct :alpha)))
    (color-create :red red :green green :blue blue :alpha alpha)))

(defsubst to-node-path (struct)
  (let ((path (mapcar 'to-string (bindat-get-field struct :items)))
        (subpath nil) ;; TODO what is subpath
        (absolute (bindat-get-field struct :absolute)))
    (node-path-create :path path :subpath subpath :absolute absolute)))

(defsubst to-rid (struct-data)
  (rid-create))

(defsubst to-vector2 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y)))
    (vector2-create :x x :y y)))

(defsubst to-vector3 (struct-data)
  (let ((x (bindat-get-field struct-data :x))
        (y (bindat-get-field struct-data :y))
        (z (bindat-get-field struct-data :z)))
    (vector3-create :x x :y y :z z)))

(defsubst to-transform2d (struct-data)
  (let ((xx (bindat-get-field struct-data :xx))
        (yx (bindat-get-field struct-data :yx))
        (xy (bindat-get-field struct-data :xy))
        (yy (bindat-get-field struct-data :yy))
        (x-origin (bindat-get-field struct-data :x-origin))
        (y-origin (bindat-get-field struct-data :y-origin)))
    (transform2d-create
     :x (vector2-create :x xx :y yx)
     :y (vector2-create :x xy :y yy)
     :origin (vector2-create :x :x-origin :y :y-origin))))

(defsubst to-null (struct-data)
  (prim-null-create))

(defsubst to-boolean (struct-data)
  (prim-bool-create :value (if (eq 1 (get-boolean struct-data))
                               t
                             nil)))

(defsubst shared-to-boolean (shared)
  (prim-bool-create :value (if (eq 0 shared) nil t)))

(defsubst to-integer (struct-data)
  (prim-integer-create :value (get-integer struct-data)))

(defsubst to-float (struct-data)
  (prim-float-create :value (get-float struct-data)))

(defsubst to-string (struct-data)
  (prim-string-create :value (get-string struct-data)))

(defsubst to-object-id (struct-data)
  (object-id-create :value (bindat-get-field struct-data :long)))

(defsubst to-dictionary (struct-data)
  (let* ((shared (bindat-get-field struct-data :shared))
         (items (bindat-get-field struct-data :items)))
    (dictionary-create :shared (shared-to-boolean shared) :elements (to-dic items))))

(defun to-dic (xs)
  (cl-loop for (key value) on xs by 'cddr
           collect (from-key-value key value)))

(defsubst to-array (struct-data)
  (let ((shared (bindat-get-field struct-data :shared))
        (items (bindat-get-field struct-data :items)))
    (prim-array-create :shared (shared-to-boolean shared) :elements (mapcar 'from-variant items))))

(defsubst to-pool-int-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-int-array-create :elements (mapcar 'to-integer items))))

(defsubst to-pool-string-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-string-array-create :elements (mapcar 'to-string items))))

(defsubst to-pool-vector2-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-vector2-array-create :elements (mapcar 'to-vector2 items))))

(defsubst to-pool-color-array (struct-data)
  (let ((items (bindat-get-field struct-data :items)))
    (pool-color-array-create :elements (mapcar 'to-color items))))

(defun from-key-value (key value)
  (let* ((var-name (bindat-get-field key :string-data))
         (var-val (from-variant value)))
    `(,var-name . ,var-val)))

(defun from-variant (struct)
  (let ((type (bindat-get-field struct :type))
        (object-as-id (bindat-get-field struct :object-as-id)))
    (pcase type
      (0 (to-null struct))
      (1 (to-boolean struct))
      (2 (to-integer struct))
      (3 (to-float struct))
      (4 (to-string struct))
      (5 (to-vector2 struct))
      (7 (to-vector3 struct))
      (8 (to-transform2d struct))
      (9 (to-plane struct))
      (11 (to-aabb struct))
      (12 (to-basis struct))
      (14 (to-color struct))
      (15 (to-node-path struct))
      (16 (to-rid struct))
      (17 (if (eq 0 object-as-id) (error "TODO object as not ID")
            (to-object-id struct)))
      (18 (to-dictionary struct))
      (19 (to-array struct))
      (21 (to-pool-int-array struct))
      (23 (to-pool-string-array struct))
      (24 (to-pool-vector2-array struct))
      (26 (to-pool-color-array struct))
      (_ (error "[from-variant] Unknown type %s" type)))))

(defun to-stack-dump (stack-data level)
  (pcase stack-data
    (`(,file-key, file-value, line-key, line-value, function-key, function-value, id-key, id-value)
     (stack-dump-create
      :file (get-string file-value)
      :line (get-integer line-value)
      :function-name (get-string function-value)
      :level level))))

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
             (var-val (from-variant var-value)))
        ;;(message "%s) [read-var-names] var-name var-val : %s %s" i var-name var-val)

        (setq variables (cons `(,var-name . ,var-val) variables))))
    variables))

(defun mk-stack-frame-vars (iter)
  (let* ((total-size (get-integer (iter-next iter)))
         (locals-size (get-integer (iter-next iter)))
         (locals (read-var-names iter locals-size))
         (members-size (get-integer (iter-next iter)))
         (members (read-var-names iter members-size))
         (globals-size (get-integer (iter-next iter)))
         (globals (read-var-names iter globals-size)))
    (stack-frame-vars-create :locals locals :members members :globals globals)))

(defun to-property-info (properties)
  (let ((property-info))
    (dolist (property properties)
      (cond ((eq 6 (bindat-get-field property :array-length))
             (let* ((data (bindat-get-field property :items))
                    (name (bindat-get-field (car data) :string-data))
                    (type (bindat-get-field (nth 1 data) :integer-data))
                    (hint (bindat-get-field (nth 2 data) :integer-data))
                    (hint-string (bindat-get-field (nth 3 data) :string-data))
                    (usage (bindat-get-field (nth 4 data) :integer-data))
                    (variant (from-variant (nth 5 data)))
                    (new-prop (property-info-create
                               :name name
                               :type type
                               :hint hint
                               :hint-string hint-string
                               :usage usage
                               :variant variant)))
               (push new-prop property-info)))
            (t (message "Ignoring property %s" property))))
    property-info))

(defun mk-inspect-object (iter)
  (let ((three (get-integer (iter-next iter)))
        (object-id (get-integer (iter-next iter)))
        (class (get-string (iter-next iter)))
        (properties (get-array (iter-next iter))))
    (inspect-object-create :object-id object-id :class class :properties (to-property-info properties))))

(defun mk-stack-dump (iter)
  (let ((stack-level-count (get-integer (iter-next iter)))
        (outputs))
    (dotimes (level stack-level-count)
      (let ((stack-data (bindat-get-field (iter-next iter) :items)))
        (push (to-stack-dump stack-data level) outputs)))
    (reverse outputs)))

(defun mk-output (iter)
  (let ((output-count (bindat-get-field (iter-next iter) :integer-data))
        (outputs))
    ;(message "output-count: %s %s" output-count (type-of output-count))
    (dotimes (i output-count)
      (let* ((data (iter-next iter))
             (output (bindat-get-field data :items 0 :string-data)))
        (setq outputs (cons output outputs))))
    `(command "output" outputs, outputs)))

(defun mk-debug-enter (iter)
  (let ((skip-this (iter-next iter))
        (can-continue (bindat-get-field (iter-next iter) :boolean-data))
        (reason (bindat-get-field (iter-next iter) :string-data)))
    (debug-enter-create :can-continue can-continue :reason reason)))

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

(defun gdscript-debugger--on-stack-dump (stuck-dump project-root)
  (let* ((file (stack-dump->file stuck-dump))
         (line (stack-dump->line stuck-dump))
         (full-file-path (concat project-root (gdscript-debugger--drop-res file))))
    (if (not project-root)
        (error "Project for file %s not found." file)
      (with-current-buffer (find-file full-file-path)
        (let* ((posns (line-posns line))
               (start-posn (car posns)))
          (set-marker gdscript-debugger--thread-position start-posn (current-buffer))
          (goto-char gdscript-debugger--thread-position))))))

(defmacro gdscript-debugger--command-handler (&rest body)
  `(progn
     ,@body
     (setq gdscript-debugger--previous-packet-data
           (substring gdscript-debugger--previous-packet-data gdscript-debugger--offset (length gdscript-debugger--previous-packet-data)))
     (setq gdscript-debugger--offset 0)))

(defun gdscript-debugger--handle-server-reply (process content)
  "Gets invoked whenever the server sends data to the client."
  ;;(message "(DATA received): %s" (length content))
  ;;(message "(Old DATA): %s" (length gdscript-debugger--previous-packet-data))
  (setq gdscript-debugger--previous-packet-data (concat gdscript-debugger--previous-packet-data content))
  (when (or (null gdscript-debugger--data-needed)
            (<= gdscript-debugger--data-needed (length gdscript-debugger--previous-packet-data)))
    (condition-case x
        (let ((iter (command-iter)))
          (while t
            (let* ((next-data (iter-next iter))
                   (str (bindat-get-field next-data :string-data)))
              (pcase str
                ("debug_enter"
                 (gdscript-debugger--command-handler
                  (message "Received 'debug_enter' command")
                  (let ((cmd (mk-debug-enter iter)))
                    (pcase (debug-enter->reason cmd)
                      ("Breakpoint" (gdscript-debugger-get-stack-dump))
                      (_ (error "Unknown reason %s" cmd))))))
                ("debug_exit"
                 (gdscript-debugger--command-handler
                  (message "Received 'debug_exit' command")
                  (let ((cmd (mk-debug-exit iter)))
                    (message "Debug_exit: %s " cmd))))
                ("output"
                 (gdscript-debugger--command-handler
                  (message "Received 'output' command")
                  (let ((cmd (mk-output iter)))
                    ;;(message "Output: %s" (plist-get cmd 'outputs))
                    ;; (dolist (element (plist-get cmd 'outputs))
                    ;;   (message "output: %s" element))
                    )))
                ("error"
                 (gdscript-debugger--command-handler
                  (message "Received 'error' command")
                  (let ((cmd (mk-error iter)))
                    ;;(message "Error: %s" cmd)
                    )))
                ("performance"
                 (gdscript-debugger--command-handler
                  (message "Received 'performance' command")
                  (let ((cmd (mk-performance iter)))
                    ;; (message "Performace: %s" cmd)
                    )))
                ("stack_dump"
                 (gdscript-debugger--command-handler
                  (message "Received 'stack_dump' command")
                  (let ((cmd (mk-stack-dump iter)))
                    ;;(message "[stack_dump] cmd: %s" cmd)
                    (gdscript-debugger--refresh-stack-frame-vars-buffer cmd)
                    (gdscript-debugger--on-stack-dump (car cmd) (process-get process 'project))
                    (gdscript-debugger-get-stack-frame-vars (stack-dump->level (car cmd))))))
                ("stack_frame_vars"
                 (gdscript-debugger--command-handler
                  (message "Received 'stack_frame_vars' command")
                  (let ((cmd (mk-stack-frame-vars iter)))
                    (with-current-buffer (gdscript-debugger--get-locals-buffer)
                      (let ((inhibit-read-only t))
                        (erase-buffer)
                        (insert "Locals:\n")
                        (dolist (local (stack-frame-vars->locals cmd))
                          (insert (gdscript-debugger--variable-name (car local)))
                          (insert (format ": %s\n" (gdscript-debugger--stringify (cdr local)))))
                        (insert "\nMembers:\n")
                        (dolist (member (stack-frame-vars->members cmd))
                          (insert (gdscript-debugger--variable-name (car member)))
                          (insert (format ": %s\n" (cdr member))))
                        (insert "\nGlobals:\n")
                        (dolist (global (stack-frame-vars->globals cmd))
                          (insert (gdscript-debugger--variable-name (car global)))
                          (insert (format ": %s\n" (cdr global)))))
                      (display-buffer (current-buffer)))
                    ;; (message "Stack frame vars %s" cmd)
                    )))
                ("message:inspect_object"
                 (gdscript-debugger--command-handler
                  (message "Received 'message:inspect_object' command")
                  (let ((cmd (mk-inspect-object iter)))
                    (gdscript-debugger--refresh-inspector-buffer cmd))))
                (_ (error "Unknown command %s data %s" str next-data))))))
      ;;(iter-end-of-sequence (message "No more packets to process %s" x))
      (iter-end-of-sequence nil))))

(defun gdscript-debugger--stringify (object)
  (cond ((object-id-p object)
         (let ((object-id (object-id->value object)))
           (propertize (format "Object ID: %s" (number-to-string object-id)) 'object-id object-id)))
        (t object)))

(defun gdscript-debugger--variable-name (var-name)
  (propertize (format "%25s" var-name) 'font-lock-face font-lock-variable-name-face))

(defvar server-clients '()
  "List with client processes")

(defun gdscript-debugger--sentinel-function (process event)
  "Gets called when the status of the network connection changes."
  (message "[sentinel] process: %s" process)
  (message "[sentinel] event  : %s" event)
  (cond
   ((string-match "open from .*\n" event)
    (push process server-clients))
   ((or
     (string= event "connection broken by remote peer\n")
     (string= event "deleted\n"))
    (message "Resetting server to accept data.")
    (set-marker gdscript-debugger--thread-position nil)
    (setq gdscript-debugger--previous-packet-data nil
          gdscript-debugger--offset 0
          gdscript-debugger--data-needed nil
          server-clients '()))
   ((eq (process-status process) 'closed)
    (message "EHHHH ???"))))

(defmacro gdscript-debugger--send-command (&rest body)
  "Todo"
  (declare (indent 0) (debug t))
  `(pcase server-clients
     (`() (message "No game process is running."))
     (`(,server-process)
      (let ((command (progn ,@body)))
        (process-send-string server-process command)))
     (_ (message "More than one game process running"))))

;;(print (macroexpand '(gdscript-debugger--send-command server-process (message "HIII %s" server-process))))

(defun gdscript-debugger-inspect-object(object-id)
  (gdscript-debugger--send-command
    (gdscript-debugger--inspect-object object-id)))

(defun gdscript-debugger-get-stack-dump()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "get_stack_dump")))

(defun gdscript-debugger-continue()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "continue")))

(defun gdscript-debugger-next()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "next")))

(defun gdscript-debugger-step()
  (interactive)
  (gdscript-debugger--send-command (gdscript-debugger--command "step")))

;;;###autoload
(defun gdscript-debugger-make-server()
  (interactive)

  (setq gdscript-debugger--thread-position (make-marker))
  (add-to-list 'overlay-arrow-variable-list 'gdscript-debugger--thread-position)

  ;;(set-marker gdscript-debugger--thread-position (point) (current-buffer))

  (let ((project-root (gdscript-util--find-project-configuration-file)))
    (if (not project-root)
        (error "Not in Godot project!")
      (let ((server-process
             (make-network-process
              :name "DEBUG"
              :buffer nil
              :server t
              :host "127.0.0.1"
              :service 6010
              :coding 'binary
              :family 'ipv4
              :filter #'gdscript-debugger--handle-server-reply
              :filter-multibyte nil
              :sentinel #'gdscript-debugger--sentinel-function)))
        (process-put server-process 'project project-root)
        (message "Debugger server started - project: %s" project-root)))))

(defun gdscript-debugger--inspect-object-definition (command-length)
  `((:packet-length u32r)
    (:array-type u32r)
    (:elements-count u32r)
    (:command-type u32r)
    (:command-length u32r)
    (:command str ,command-length)
    (align 4)
    (:object-id-type u32r)
    (:object-id u32r)))

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

;; (print (symbol-function 'gdscript-debugger--get-stack-frame-vars))

(defun gdscript-debugger--inspect-object (object-id)
  (let* ((command "inspect_object")
         (command-length (length command))
         (command-alength (align-length command))
         (packet-length (+ (* 6 4) command-alength))
         (spec (gdscript-debugger--inspect-object-definition command-length)))
    (bindat-pack spec
     `((:packet-length . ,packet-length)
       (:array-type . ,variant-array)
       (:elements-count . 2)
       (:command-type . ,variant-string)
       (:command-length . ,command-length)
       (:command . ,command)
       (:object-id-type . ,variant-integer)
       (:object-id . ,object-id)))))

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
  ;;(message "(gdscript-debugger--packet-definition (length command)): %s" (gdscript-debugger--packet-definition (length command)))
  (let* ((command-alength (align-length command))
         (packet-length (+ (* 4 4) command-alength)))
    ;; (message "packet-length: %s" packet-length)
    ;; (message "command-alength: %s" command-alength)
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

(defun gdscript-debugger--absolute-path ()
  "Return the absolute path of current gdscript file."
  (when (and buffer-file-name
             (file-exists-p buffer-file-name)
             (string= (file-name-extension buffer-file-name) "gd"))
    buffer-file-name))

(defun gdscript-debugger--current-file ()
  (let ((current-file (gdscript-util--get-godot-project-file-path-relative buffer-file-name)))
    (when current-file
      (let ((extension (file-name-extension buffer-file-name)))
        (when (string= extension "gd")
          (concat "res://" current-file ".gd"))))))

(defmacro gdscript-debugger--with-gdscript-file (file-info body)
  (declare (indent 1) (debug t))
  `(let ((,file-info (cons (gdscript-debugger--current-file) (gdscript-debugger--absolute-path))))
     (if (not (car ,file-info))
         (message "No GDScript file.")
       ,body)))

;; (print (macroexpand '(gdscript-debugger--with-gdscript-file gdscript-file
;;  (let ((line (line-number-at-pos)))
;;    (message "No breakpoint at %s:%s" line gdscript-file)))))

(defun gdscript-debugger-remove-breakpoint ()
  (interactive)
  (gdscript-debugger--with-gdscript-file file-info
    (let* ((start (line-beginning-position))
           (end (line-end-position))
           (line (line-number-at-pos))
           (file (car file-info))
           (file-absolute (cdr file-info))
           (breakpoint (breakpoint-create :file file :file-absolute file-absolute :line line)))
      (if (not (member breakpoint gdscript-debugger--breakpoints))
          (message "No breakpoint at %s:%s" file line)
        (gdscript-debugger--send-command
          (gdscript-debugger--remove-strings start end)
          (gdscript-debugger--remove-breakpoint-from-buffer breakpoint)
          (gdscript-debugger--breakpoint-command file line nil))))))

(defun gdscript-debugger-add-breakpoint ()
  (interactive)
  (gdscript-debugger--with-gdscript-file file-info
    (let* ((line (line-number-at-pos))
           (file (car file-info))
           (file-absolute (cdr file-info))
           (breakpoint (breakpoint-create :file file :file-absolute file-absolute :line line)))
      (if (member breakpoint gdscript-debugger--breakpoints)
          (message "Breakpoint already present at %s:%s" file line)
        (gdscript-debugger--send-command
          (gdscript-debugger--add-fringe (line-beginning-position) 'gdb-bptno 1)
          (gdscript-debugger--add-breakpoint-to-buffer breakpoint)
          (gdscript-debugger--breakpoint-command file line t))))))

(defun gdscript-debugger-get-stack-frame-vars (level)
  (gdscript-debugger--send-command
    (gdscript-debugger--get-stack-frame-vars level)))

(cl-defstruct (prim-null (:constructor prim-null-create)
                         (:copier nil)))

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

(cl-defstruct (plane (:constructor plane-create)
                     (:copier nil)
                     (:conc-name plane->))
  normal-x normal-y normal-z distance)

(cl-defstruct (aabb (:constructor aabb-create)
                    (:copier nil)
                    (:conc-name aabb->))
  x-coordinate y-coordinate z-coordinate x-size y-size z-size)

(cl-defstruct (basis (:constructor basis-create)
                     (:copier nil)
                     (:conc-name basis->))
  x y z)

(cl-defstruct (color (:constructor color-create)
                         (:copier nil)
                         (:conc-name color->))
  red green blue alpha)

(cl-defstruct (node-path (:constructor node-path-create)
                         (:copier nil)
                         (:conc-name node-path->))
  path subpath absolute)

(cl-defstruct (rid (:constructor rid-create)
                   (:copier nil)))

(cl-defstruct (object-id (:constructor object-id-create)
                         (:copier nil)
                         (:conc-name object-id->))
  value)

(cl-defstruct (dictionary (:constructor dictionary-create)
                          (:copier nil)
                          (:conc-name dictionary->))
  shared elements)

(cl-defstruct (vector2 (:constructor vector2-create)
                       (:copier nil)
                       (:conc-name vector2->))
  x y)

(cl-defstruct (vector3 (:constructor vector3-create)
                       (:copier nil)
                       (:conc-name vector3->))
  x y z)

(cl-defstruct (transform2d (:constructor transform2d-create)
                           (:copier nil)
                           (:conc-name transform2d->))
  x y origin)

(cl-defstruct (prim-array (:constructor prim-array-create)
                          (:copier nil)
                          (:conc-name prim-array->))
  shared elements)

(cl-defstruct (pool-int-array (:constructor pool-int-array-create)
                              (:copier nil)
                              (:conc-name pool-int-array->))
  elements)

(cl-defstruct (pool-string-array (:constructor pool-string-array-create)
                                 (:copier nil)
                                 (:conc-name pool-string-array->))
  elements)

(cl-defstruct (pool-vector2-array (:constructor pool-vector2-array-create)
                                  (:copier nil)
                                  (:conc-name pool-vector2-array->))
  elements)

(cl-defstruct (pool-color-array (:constructor pool-color-array-create)
                                (:copier nil)
                                (:conc-name pool-color-array->))
  elements)

(cl-defstruct (stack-frame-vars (:constructor stack-frame-vars-create)
                                (:copier nil)
                                (:conc-name stack-frame-vars->))
  locals members globals)

(cl-defstruct (stack-dump (:constructor stack-dump-create)
                          (:copier nil)
                          (:conc-name stack-dump->))
  file line function-name level)

(cl-defstruct (inspect-object (:constructor inspect-object-create)
                              (:copier nil)
                              (:conc-name inspect-object->))
  object-id class properties)

(cl-defstruct (property-info (:constructor property-info-create)
                             (:copier nil)
                             (:conc-name property-info->))
  name type hint hint-string usage variant)

(cl-defstruct (breakpoint (:constructor breakpoint-create)
                          (:copier nil)
                          (:conc-name breakpoint->))
  file file-absolute line)

(cl-defstruct (debug-enter (:constructor debug-enter-create)
                           (:copier nil)
                           (:conc-name debug-enter->))
  can-continue reason)

(defun gdscript-debugger--parent-mode ()
  "Generic mode to derive all other buffer modes from."
  (kill-all-local-variables)
  (setq buffer-read-only t)
  (buffer-disable-undo))

(defun gdscript-debugger-toggle-breakpoint ()
  (interactive)
  (message "TODO [gdscript-debugger-toggle-breakpoint]"))

(defun gdscript-debugger-delete-breakpoint ()
  (interactive)
  (message "TODO [gdscript-debugger-delete-breakpoint]"))

(defun gdscript-debugger-goto-breakpoint ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((breakpoint (get-text-property (point) 'gdscript-debugger--breakpoint)))
      (if breakpoint
          (let ((file (breakpoint->file-absolute breakpoint))
                (line (breakpoint->line breakpoint)))
            (save-selected-window
              (let* ((buffer (find-file-noselect file))
                     (window (display-buffer buffer)))
                (with-current-buffer buffer
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (set-window-point window (point))))))
        (error "Not recognized as break/watchpoint line")))))

(defun gdscript-debugger-inspect-object-id ()
  (interactive)
  (save-excursion
    (let ((object-id (get-text-property (point) 'object-id)))
      (if object-id
          (progn
            (gdscript-debugger-inspect-object object-id)
            (save-selected-window
              (let* ((buffer (gdscript-debugger--get-buffer-create 'inspector-buffer))
                     (window (display-buffer buffer)))
                (with-current-buffer buffer
                  (set-window-point window (point))))))
        (error "Not recognized as object-id line")))))

(defun gdscript-debugger-show-stack-frame-vars ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((stack (get-text-property (point) 'gdscript-debugger--stack-dump)))
      (if stack
          (gdscript-debugger-get-stack-frame-vars (stack-dump->level stack))
        (error "Not recognized as stack-frame line")))))

(defvar gdscript-debugger--stack-dump-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\r" 'gdscript-debugger-show-stack-frame-vars)
    map))

(defvar gdscript-debugger--breakpoints-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map " " 'gdscript-debugger-toggle-breakpoint)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "D" 'gdscript-debugger-delete-breakpoint)
    (define-key map "\r" 'gdscript-debugger-goto-breakpoint)
    map))

(defvar gdscript-debugger--stack-frame-vars-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "q" 'kill-current-buffer)
    (define-key map "\r" 'gdscript-debugger-inspect-object-id)
    map))

(defvar-local gdscript-debugger--buffer-type nil
  "One of the symbols bound in `gdscript-debugger--get-buffer-create'.")

(defun gdscript-debugger--get-buffer (buffer-type)
  "Get a specific buffer.

In that buffer, `gdscript-debugger--buffer-type' must be equal to BUFFER-TYPE."
  (catch 'found
    (dolist (buffer (buffer-list) nil)
      (with-current-buffer buffer
        (when (eq gdscript-debugger--buffer-type buffer-type)
          (throw 'found buffer))))))

(defun gdscript-debugger--get-locals-buffer ()
  (gdscript-debugger--get-buffer-create 'stack-frame-vars-buffer))

(defun gdscript-debugger-display-stack-dump-buffer ()
  "Display stack dump."
  (interactive)
  (display-buffer (gdscript-debugger--get-buffer-create 'stack-dump-buffer)))

(defun gdscript-debugger-display-stack-frame-vars-buffer ()
  "Display the variables of current stack."
  (interactive)
  (display-buffer (gdscript-debugger--get-buffer-create 'stack-frame-vars-buffer)))

(defun gdscript-debugger-display-breakpoint-buffer ()
  "Display the breakpoints."
  (interactive)
  (display-buffer (gdscript-debugger--get-buffer-create 'breakpoints-buffer)))

(defun gdscript-debugger-display-inspector-buffer ()
  "Display the inspector."
  (interactive)
  (display-buffer (gdscript-debugger--get-buffer-create 'inspector-buffer)))

(defun gdscript-debugger--remove-breakpoint-from-buffer (breakpoint)
  (setq gdscript-debugger--breakpoints (remove breakpoint gdscript-debugger--breakpoints))
  (refresh-breakpoints-buffer))

(defun gdscript-debugger--add-breakpoint-to-buffer (breakpoint)
  (unless (member breakpoint gdscript-debugger--breakpoints)
    (push breakpoint gdscript-debugger--breakpoints)
    (refresh-breakpoints-buffer)))

(defun gdscript-debugger--refresh-stack-frame-vars-buffer (stack-dump)
  (with-current-buffer (gdscript-debugger--get-buffer-create 'stack-dump-buffer)
    (let ((inhibit-read-only t)
          (longest-file-name 0))
      (dolist (stack stack-dump)
        (let* ((file (stack-dump->file stack))
               (line (stack-dump->line stack))
               (len (+ (length file) (length (number-to-string line)))))
          (when (< longest-file-name len)
            (setq longest-file-name len))))
      (erase-buffer)
      (dolist (stack stack-dump)
        (let ((ident (format "%s:%s" (stack-dump->file stack) (stack-dump->line stack))))
          (insert (propertize
                   (concat
                    (format (concat "%s - %-" (number-to-string (1+ longest-file-name)) "s - ") (stack-dump->level stack) ident)
                    (propertize
                     (format "%s\n" (stack-dump->function-name stack)) 'font-lock-face font-lock-function-name-face))
                   'gdscript-debugger--stack-dump stack)))))))

(defun gdscript-debugger--refresh-inspector-buffer (inspect-object)
  (with-current-buffer (gdscript-debugger--get-buffer-create 'inspector-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s %s\n" (inspect-object->object-id inspect-object) (inspect-object->class inspect-object)))
      (dolist (property (inspect-object->properties inspect-object))
        (insert (format "%s\n" property))))))

(defun refresh-breakpoints-buffer ()
 (with-current-buffer (gdscript-debugger--get-buffer-create 'breakpoints-buffer)
   (let ((inhibit-read-only t))
     (erase-buffer)
     (dolist (breakpoint gdscript-debugger--breakpoints)
       (insert (concat (propertize (format "%s:%s" (breakpoint->file breakpoint) (breakpoint->line breakpoint)) 'gdscript-debugger--breakpoint breakpoint) "\n"))))))

(defun gdscript-debugger--get-buffer-create (buffer-type)
  (or (gdscript-debugger--get-buffer buffer-type)
      (let ((rules (assoc buffer-type gdscript-debugger--buffer-rules))
            (new (generate-new-buffer "limbo")))
        (with-current-buffer new
          (let ((mode (gdscript-debugger--rules-buffer-mode rules)))
            (when mode (funcall mode))
            (setq gdscript-debugger--buffer-type buffer-type)
            (rename-buffer (funcall (gdscript-debugger--rules-name-maker rules)))
            (current-buffer))))))

(define-derived-mode gdscript-debugger--stack-dump-mode gdscript-debugger--parent-mode "Stack Dump"
  "Major mode for stack dump."
  (setq header-line-format "Stack dump"))

(define-derived-mode gdscript-debugger--stack-frame-vars-mode gdscript-debugger--parent-mode "Stack Frame Vars"
  "Major mode for stack frame variables."
  (setq header-line-format "Stack frame vars"))

(define-derived-mode gdscript-debugger--breakpoints-mode gdscript-debugger--parent-mode "Breakpoints"
  "Major mode for breakpoints management."
  (setq header-line-format "Breakpoints"))

(define-derived-mode gdscript-debugger--inspector-mode gdscript-debugger--parent-mode "Inspector"
  "Major mode for inspector management."
  (setq header-line-format "Inspector"))

(defun gdscript-debugger--stack-dump-buffer-name ()
  (concat "* Stack dump *"))

(defun gdscript-debugger--stack-frame-vars-buffer-name ()
  (concat "* Stack frame vars *"))

(defun gdscript-debugger--breakpoints-buffer-name ()
  (concat "* Breakpoints *"))

(defun gdscript-debugger--inspector-buffer-name ()
  (concat "* Inspector *"))

(defvar gdscript-debugger--buffer-rules '())
(defvar gdscript-debugger--breakpoints '())

(defun gdscript-debugger--rules-name-maker (rules-entry)
  (cadr rules-entry))
(defun gdscript-debugger--rules-buffer-mode (rules-entry)
  (nth 2 rules-entry))

(defun gdscript-debugger--set-buffer-rules (buffer-type &rest rules)
  (let ((binding (assoc buffer-type gdscript-debugger--buffer-rules)))
    (if binding
	(setcdr binding rules)
      (push (cons buffer-type rules)
	    gdscript-debugger--buffer-rules))))

(gdscript-debugger--set-buffer-rules
 'stack-frame-vars-buffer
 'gdscript-debugger--stack-frame-vars-buffer-name
 'gdscript-debugger--stack-frame-vars-mode)

(gdscript-debugger--set-buffer-rules
 'stack-dump-buffer
 'gdscript-debugger--stack-dump-buffer-name
 'gdscript-debugger--stack-dump-mode)

(gdscript-debugger--set-buffer-rules
 'breakpoints-buffer
 'gdscript-debugger--breakpoints-buffer-name
 'gdscript-debugger--breakpoints-mode)

(gdscript-debugger--set-buffer-rules
 'inspector-buffer
 'gdscript-debugger--inspector-buffer-name
 'gdscript-debugger--inspector-mode)

(define-key gdscript-mode-map (kbd "C-c C-d C-d s") 'gdscript-debugger-display-stack-frame-vars-buffer)
(define-key gdscript-mode-map (kbd "C-c C-d C-d d") 'gdscript-debugger-display-stack-dump-buffer)
(define-key gdscript-mode-map (kbd "C-c C-d C-d b") 'gdscript-debugger-display-breakpoint-buffer)
(define-key gdscript-mode-map (kbd "C-c C-d C-d i") 'gdscript-debugger-display-inspector-buffer)
(define-key gdscript-mode-map (kbd "C-c C-d b") 'gdscript-debugger-add-breakpoint)
(define-key gdscript-mode-map (kbd "C-c C-d r") 'gdscript-debugger-remove-breakpoint)
(define-key gdscript-mode-map (kbd "C-c C-d q") 'gdscript-debugger-make-server)
(define-key gdscript-mode-map (kbd "C-c C-d n") 'gdscript-debugger-next)
(define-key gdscript-mode-map (kbd "C-c C-d c") 'gdscript-debugger-continue)
(define-key gdscript-mode-map (kbd "C-c C-d s") 'gdscript-debugger-step)

(provide 'gdscript-debugger)
