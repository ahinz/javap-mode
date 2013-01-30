(defvar javap-mode-map nil "Keymap for javap")

(defun javap-toggle-complete ()
  (interactive)
  (setq javap-enabled-complete (if javap-enabled-complete nil 1))
  (update-buffer-with-javap))

;; This function from Magnar
(defun javap-trim-right (s)
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun javap-parse-classlike (classlike)
  (if (search "/" classlike)
      (javap-parse-class-as-path classlike)
    (javap-parse-class-as-dotted classlike)))

(defun javap-parse-class-as-path (class)
  (let* ((path-and-method (split-string (substring class 0 -1) "\\."))
         (path (first path-and-method))
         (meth (second path-and-method)))
    (list (concat (javap-trim-right path) ".class") meth)))

(defun javap-jump-to-method (method-name)
  (search-forward (concat method-name "("))
  (backward-char))

(defun javap-open-file (class-name method-name)
  (let ((base-path (first (parse-java-class-name buffer-file-name)))) 
    (find-file (concat base-path class-name))
    (when method-name
      (javap-jump-to-method method-name))))

(defun javap-open-file-at-point ()
  (interactive)
  (save-excursion
    (let ((start (re-search-backward "\s"))
          (end (progn (forward-char) (re-search-forward ":\\|\s"))))
      (apply 'javap-open-file
             (javap-parse-classlike (buffer-substring-no-properties (+ 1 start) end))))))

(setq javap-mode-map (make-keymap))
(suppress-keymap javap-mode-map t)
(define-key javap-mode-map (kbd "r") 'javap-toggle-complete)
(define-key javap-mode-map (kbd "RET") 'javap-open-file-at-point)

(setq java-ident-re "[a-zA-Z_$][a-zA-Z0-9_$]*")
(setq java-ident-dotted-re (concat java-ident-re "\\(\\." java-ident-re "\\)*"))
(setq java-ident-slashed-re (concat java-ident-re "\\(/" java-ident-re "\\)+"))
(setq java-bytecodes
  (concat "\\([scadiflb]a?"
          "\\(\\(load\\)\\|\\(store\\)\\|\\(const\\)\\|\\(return\\)\\)\\(_[0123]\\)?\\)"
          "\\|"
          "\\([scadiflb]2[scadiflb]\\)"
          "\\|"
          "\\(invoke\\(dynamic\\|interface\\|special\\|virtual\\|static\\)\\)"
          "\\|"
          "\\(return\\|new\\|pop2\\|pop\\|nop\\|checkcast\\|dup\\|getstatic\\)"
          "\\|"
          "\\(getfield\\|putfield\\)"))

(setq javap-font-lock-keywords
  `(
    ("\\(BoxesRunTime.*?\\):"
     1 font-lock-warning-face)

    (,(concat "\\(" java-ident-dotted-re "\\)(")
     1 font-lock-function-name-face)

    ("[0-9]+\\:"
     . font-lock-preprocessor-face)

    (,java-bytecodes
     . font-lock-constant-face)

    ("\\(public\\)\\|\\(private\\)\\|\\(final\\)\\|\\(static\\)\\|\\(class\\)\\|\\(extends\\)"
     . font-lock-keyword-face)

    ("\s\\(\\(void\s\\)\\|\\(long\\)\\|\\(int\\)\\|\\(byte\\)\\|\\(short\\)\\)"
     . font-lock-type-face)

    ("\\(#[0-9]+\\)"
     1 font-lock-preprocessor-face)

    (,(concat "\\(InterfaceMethod\\|Method\\|Field\\)\s" java-ident-slashed-re "\\(\\." java-ident-re "\\)?")
     . font-lock-constant-face)

    ;; (,java-ident-slashed-re
    ;;  . font-lock-function-name-face)

    (,java-ident-dotted-re
     . font-lock-type-face)))



(define-derived-mode javap-mode fundamental-mode "Java Decompile"
  "javap-mode is for viewing decompiled class files"
  (set (make-local-variable 'font-lock-defaults) '(javap-font-lock-keywords))
  (set (make-local-variable 'javap-enabled-complete) 1)

  (use-local-map javap-mode-map)
  (setq comment-start "//")
  (setq commend-end "")
)

(defun parse-java-class-name (apath)
  (let* ((parts (split-string apath "classes"))
         (pathp (first parts))
         (classp (file-name-sans-extension (second parts)))
         (classdot 
          (replace-regexp-in-string "\\$" "\\\\$" (replace-regexp-in-string "/" "." classp))))
    (list 
     (concat pathp "classes/")
     (if (string-equal "." (substring classdot 0 1))
         (substring classdot 1)
       classdot)
     )))

(defun call-javap (cwd class)
  (let* ((args (concat (if javap-enabled-complete "-c") " "))
         (wrapped-command (concat "pushd " cwd " && javap " args class " && popd")))
    (shell-command-to-string wrapped-command)))

(parse-java-class-name "/home/ahinz/src/azavea/geotrellis/target/scala-2.10/classes/geotrellis/raster/op/zonal/Histogram$$anonfun$createTileResults$1.class")

(defun update-buffer-with-javap ()
  (let ((buf (current-buffer))
        (p (point))
        (path-and-class (parse-java-class-name buffer-file-name)))
    (when (string-equal major-mode "javap-mode")
      (erase-buffer)
      (insert (call-javap (first path-and-class) (second path-and-class)))
      (goto-char p)
      ;(toggle-read-only 1)
      (set-buffer-modified-p nil))))


(add-hook 'javap-mode-hook (lambda ()
    (update-buffer-with-javap)))


(provide 'javap-mode)
