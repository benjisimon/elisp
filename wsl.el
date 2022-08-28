;;
;; Scripts to work with wsl stuff
;;

(defun wsl-reset-interop ()
  (interactive)
  "Reset the env variable WSL_INTEROP to a sane value so stuff like opening URLs work again"
  (let* ((cmd "ps auxwwww|grep init |grep -v grep | tail -1 | awk '{print $2}'")
         (pid (string-trim-right (shell-command-to-string  cmd))))
    (unless pid
      (error "Can't determine init pid"))
    (setenv "WSL_INTEROP" (format "/var/run/WSL/%s_interop" pid))))

                
