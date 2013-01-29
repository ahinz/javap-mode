(defvar java-ident-re "[a-zA-Z_$][a-zA-Z0-9_$]*")
(defvar java-ident-dotted-re (concat java-ident-re "\\(\\." java-ident-re "\\)*"))
(defvar java-ident-slashed-re (concat java-ident-re "\\(/" java-ident-re "\\)+"))
(defvar java-bytecodes
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

(defvar javap-font-lock-keywords
  `(
    ("\\(BoxesRunTime.*?\\):"
     1 font-lock-warning-face)

    ("[0-9]+\\:"
     . font-lock-preprocessor-face)

    (,java-bytecodes
     . font-lock-constant-face)

    ("\\(public\\)\\|\\(private\\)\\|\\(final\\)\\|\\(static\\)\\|\\(class\\)"
     . font-lock-keyword-face)

    ("\\(\svoid\s\\)\\|long\\|int\\|byte\\|short"
     . font-lock-type-face)

    ("\\(#[0-9]+\\)"
     1 font-lock-preprocessor-face)

    ("Method\\|Field"
     . font-lock-string-face)

    (,java-ident-slashed-re
     . font-lock-function-name-face)

    (,java-ident-dotted-re
     . font-lock-type-face)))



(define-derived-mode javap-mode fundamental-mode "Java Decompile"
  "javap-mode is for viewing decompiled class files"
  (set (make-local-variable 'font-lock-defaults) '(javap-font-lock-keywords))
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
  (let ((wrapped-command (concat "pushd " cwd " && javap -c " class " && popd")))
    (shell-command-to-string wrapped-command)))

(parse-java-class-name "/home/ahinz/src/azavea/geotrellis/target/scala-2.10/classes/geotrellis/raster/op/zonal/Histogram$$anonfun$createTileResults$1.class")

(defun update-buffer-with-javap ()
  (let ((buf (current-buffer))
        (path-and-class (parse-java-class-name buffer-file-name)))
    (when (string-equal major-mode "javap-mode")
      (erase-buffer)
      (insert (call-javap (first path-and-class) (second path-and-class)))
      (toggle-read-only 1)
      (set-buffer-modified-p nil))))


(add-hook 'javap-mode-hook (lambda ()
    (update-buffer-with-javap)))


(provide 'javap-mode)
