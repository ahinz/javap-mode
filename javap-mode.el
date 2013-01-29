(define-derived-mode javap-mode fundamental-mode "Java Decompile"
  "javap-mode is for viewing decompiled class files"

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
