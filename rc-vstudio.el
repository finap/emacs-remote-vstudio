;;; rc-vstudio.el -- Remote controll Visual Studio

;; Copyright (C) 2011-2012 FINAP

;; Version: 0.2a
;; Author: FINAP

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defvar vstudio-remote-exe-name "visual_studio_hidemaru.exe"
  "Remote controll program to visual studio file name")
(defvar current-vstudio-proj-pid nil
  "Current editing visual studio project process id")
(defvar current-vstudio-proj-absolute-path ""
  "Current editing visual studio project absolute path")
(defvar not-open-vstudio-string "<not open vstudio project>"
  "Visual studio not open solution file string")

;;
;; utility
;;
(defun replace-file-path-space (filename)
  ""
  (concat "\"" filename "\"")
  )



;;
;; command
;;
(defun cmd-vstudio (vsfunc)
  "command"
  (shell-command-to-string (concat vstudio-remote-exe-name " " vsfunc "&"))
)

(defun cmd-vstudio-return-number (vsfunc)
  "command"
  (shell-command (concat vstudio-remote-exe-name " " vsfunc))
)

;;
;; visual studio list
;;
(defun vstudio-list ()
  "list visual studio"
  (split-string (cmd-vstudio "cmd_dte_list") "\n")
  )


(defun vstudio-list-project (pid)
  ""
  (split-string (cmd-vstudio (concat "cmd_project_list" " " pid)) "\n")
  )

(defun running-vstudio-project-number ()
  "runnning visual studio project"
  (string-to-number (car (vstudio-list)))
  )



;;
;; project file list
;;
(defun vstudio-proj-list-pid (pid)
  "solution "
  (cmd-vstudio (concat "cmd_project_list" " " pid))
  )


;;
;; project file list
;;
(defun vstudio-proj-file-list-pid (pid)
  "solution "
  (cmd-vstudio (concat "cmd_project_file_list" " " pid " " (vstudio-project-absolute-path-pid pid)))
  )


;; 
;; pid
;;
(defun set-current-vstudio-pid (num)
  "set current pid string"
  (setq current-vstudio-proj-pid (vstudio-project-pid num))
  )

(defun vstudio-project-pid (num)
  "get running visual studio pid"
  (if (and (>= (running-vstudio-project-number) (+ num 1))
	   (>= num 0))
      (nth (* 2 num) (cdr (vstudio-list)))
    "false"
    )
  )
(defun vstudio-project-pid-number (num)
  (setq pid (vstudio-project-pid num))
  (if (not (string-equal pid "false"))
      (setq pid (string-to-number (pid)))
      )
  pid
)

(defun vstudio-porject-pid-list ()
  ""
  (let* ((pidnum 1) (pidlist (vstudio-project-pid (- (running-vstudio-project-number) pidnum))))
    (while (> (running-vstudio-project-number) pidnum)
      (setq pidnum (1+ pidnum))
      (setq pidlist (cons (vstudio-project-pid (- (running-vstudio-project-number) pidnum)) pidlist))
      )
    pidlist
    )
)


;;;
;;; absolute-path
;;;
(defun set-current-vstudio-absolute-path (num)
  "set current absolute-path string"
  (setq current-vstudio-proj-absolute-path (vstudio-project-absolute-path num))
  )

(defun vstudio-project-absolute-path (num)
  "get running visual studio absolute path"
  (cond
   ((and (>= (running-vstudio-project-number) (+ num 1))
	(>= num 0))
      (progn (setq f (nth (* 2 num) (cdr (cdr (vstudio-list)))))
        (if (string-equal f "")
	     (setq f not-open-vstudio-string)
	  )
	f)
      )
   (t
    nil
    ))
  )

(defun vstudio-project-absolute-path-pid (pid)
  "get running visual studio absolute path"
  (let ((num 0) (f nil) (abs-path nil))
    (while (> (running-vstudio-project-number) (+ num 1))
       (progn (setq num-pid (nth (* 2 num) (cdr (vstudio-list))))
	      (setq f (vstudio-project-absolute-path num))
	      (if (string-equal num-pid pid)
		  (setq abs-path f)
		(setq num (running-vstudio-project-number))
		)
	      )
       (setq num (1+ num))
      )
    f)
  )


;;
;; check
;;
(defun vstudio-proj-check-for-build (pid)
  "solution debug stop"
  (cmd-vstudio-return-number (concat "_check_for_build" " " pid))
  )




(defun string-vstudio-pid-output-console (pid)
  "output string"
;  (set-buffer (get-vstudio-buffer))
  (cmd-vstudio (concat "cmd_output_console" " " pid))
  )







(defun open-vsproj ()
  "test"
  )


;;;
;;;
;;; interactive
;;;
;;;

;; output
;;

;; 
(defun string-current-vstudio-output-console ()
  "output string"
  (interactive)
  (string-vstudio-pid-output-console current-vstudio-proj-pid)
  )

;;
;; don't use.
;;
;;(defun vstudio-current-output-console ()
;;  "output string"
;;  (interactive)
;;   (setq buf (get-buffer-create "*compilation*"))
;;      (setq timer-temp-vs
;; 	   (run-with-timer 1 1
;; 			   (lambda ()
;; 			     (with-output-to-temp-buffer "*compilation*"
;; 			       (princ (string-current-vstudio-output-console))
;; 			       )
;; 			     )
;; 			   )
;; 	   )
;;      (while (eq 1 (vstudio-proj-check-for-build current-vstudio-proj-pid))
;;        (sit-for 1)
;;        )
;;      (cancel-timer timer-temp-vs)
;;      (princ (string-current-vstudio-output-console))
;;  )




;;
;; compile
;; 
(defun compile-vstudio-file (filename)
  "compile source code vc-compiler"
    (cmd-vstudio (concat "cmd_te_file_compile" " " (replace-file-path-space filename) " " "True" " " "True"))
  )

(defun vstudio-compile ()
  "visual studio compile solution current file"
  (interactive)
  (compile-vstudio-file (buffer-file-name))
  )

(defun vstudio-pid-openfile-compile ()
  "visual studio compile solution current file (no wait)"
  (interactive)
   (cmd-vstudio (concat "cmd_file_compile" " " current-vstudio-proj-pid " " (replace-file-path-space buffer-file-name) " " "True"))
  )

;; flymake compile command string
(defun vstudio-compile-command ()
  "compile-command string"
  (concat vstudio-remote-exe-name " " (replace-file-path-space "cmd_te_file_compile") " " (replace-file-path-space buffer-file-name) " " (replace-file-path-space "True") " " (replace-file-path-space "True"))
  )


;;
;; build
;;
(defun build-vstudio-proj (filename)
  "solution build"
 (with-output-to-temp-buffer "*compilation*"
   (princ (cmd-vstudio (concat "cmd_te_solution_build" " " (replace-file-path-space filename) " " "True" " " "True")))
   )
 )

(defun build-vstudio-pid-proj (pid)
  "pid solution build"
 (with-output-to-temp-buffer "*compilation*"
   (princ (cmd-vstudio (concat "cmd_solution_build" " " pid)))
   )
 )

(defun vstudio-build ()
  "visual studio build solution" 
  (interactive)
  (build-vstudio-proj buffer-file-name)
  )

(defun vstudio-pid-build ()
  "visual studio build solution" 
  (interactive)
  (build-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; rebuild
;;
(defun rebuild-vstudio-proj (filename)
  "solution rebuild"
  (with-output-to-temp-buffer "*compilation*"
    (princ (cmd-vstudio (concat "cmd_te_solution_rebuild" " " (replace-file-path-space filename) " " "True" " " "True")))
    )
  )

(defun rebuild-vstudio-pid-proj (pid)
  "pid solution rebuild"
  (with-output-to-temp-buffer "*compilation*"
    (princ (cmd-vstudio (concat "cmd_solution_rebuild" " " pid)))
    )
  )

(defun vstudio-rebuild ()
  "visual studio rebuild solution"
  (interactive)
  (rebuild-vstudio-proj (buffer-file-name))
;  (vstudio-current-output-console)
  )

(defun vstudio-pid-rebuild ()
  "visual studio rebuild pid solution"
  (interactive)
  (rebuild-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; clean
;;
(defun clean-vstudio-proj (filename)
  "solution clean"
  (cmd-vstudio (concat "cmd_te_solution_clear" " " (replace-file-path-space filename)))
  )

(defun clean-vstudio-pid-proj (pid)
  "solution clean"
  (cmd-vstudio (concat "cmd_solution_clean" " " pid))
  )

(defun vstudio-clean ()
  "visual studio clean solution"
  (interactive)
  (clean-vstudio-proj (buffer-file-name))
  )

(defun vstudio-pid-clean ()
  "visual studio clean pid solution"
  (interactive)
  (clean-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; debug
;;
(defun start-debug-vstudio-pid-proj (pid)
  "solution debug"
  (cmd-vstudio (concat "cmd_debug" " " pid))
  )

(defun start-debug-vstudio-proj (filename)
  "solution debug"
  (cmd-vstudio (concat "cmd_te_debug" " " (replace-file-path-space filename)))
  )

(defun vstudio-debug ()
  "visual studio debug solution"
  (interactive)
  (start-debug-vstudio-proj (buffer-file-name))
  )

(defun vstudio-pid-debug ()
  "visual studio debug pid solution"
  (interactive)
  (start-debug-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; stop
;;
(defun stop-debug-vstudio-pid-proj (pid)
  "solution debug stop"
  (cmd-vstudio (concat "cmd_debug_stop" " " pid))
  )

(defun stop-debug-vstudio-proj (filename)
  "solution debug stop"
  (cmd-vstudio (concat "cmd_te_debug_stop" " " (replace-file-path-space filename)))
  )

(defun vstudio-stop ()
  "visual studio stop debug solution"
  (interactive)
  (stop-debug-vstudio-proj (buffer-file-name))
  )

(defun vstudio-pid-stop ()
  "visual studio stop debug pid solution"
  (interactive)
  (stop-debug-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; run
;;
(defun run-vstudio-proj (filename)
  "solution no debug"
  (cmd-vstudio (concat "cmd_te_run_without_debug" " " (replace-file-path-space filename)))
  )

(defun run-vstudio-pid-proj (pid)
  "pid solution no debug"
  (cmd-vstudio (concat "cmd_run" " " pid))
  )

(defun vstudio-run ()
  "visual studio no debug solution"
  (interactive)
  (run-vstudio-proj (buffer-file-name))
  )

(defun vstudio-pid-run ()
  "visual studio no debug solution"
  (interactive)
  (run-vstudio-pid-proj current-vstudio-proj-pid)
  )

;;
;; cancel
;;
(defun cancel-build-vstudio-proj (filename)
  "solution no debug"
  (cmd-vstudio (concat "cmd_te_cancel" " " (replace-file-path-space filename)))
  )

(defun cancel-build-vstudio-pid-proj (pid)
  "pid solution no debug"
  (cmd-vstudio (concat "cmd_cancel" " " pid))
  )

(defun vstudio-build-cancel ()
  "visual studio cancel build solution"
  (interactive)
  (cancel-build-vstudio-proj (buffer-file-name))
  )

(defun vstudio-pid-build-cancel ()
  "visual studio cancel build solution"
  (interactive)
  (cancel-build-vstudio-pid-proj current-vstudio-proj-pid)
  )


;;
;; active
;;
(defun active-vstudio-pid-window (pid)
  "active current pid project window"
  (cmd-vstudio (concat "cmd_activate" " " pid))
  )

(defun active-vstudio-window-with-openfile (filename)
  "active project window with active file"
  (let ((current-col-tab-number (buffer-substring (point-at-bol) (point))))
    (setq current-col-tab-number (- (length (split-string current-col-tab-number "\t")) 1)) ; 1 is not tab string number
    (cmd-vstudio (concat "cmd_te_switch" " " (replace-file-path-space filename) " "
			 (number-to-string (line-number-at-pos)) " "
			 (number-to-string (+ (- (current-column) (* current-col-tab-number (- tab-width 1))) 1)))
		 )
    )
  )

(defun vstudio-pid-activate ()
  "activate visual studio window"
  (interactive)
  (active-vstudio-pid-window current-vstudio-proj-pid)
  )

(defun vstudio-activate-with-openfile ()
  "activate visual studio window"
  (interactive)
  (active-vstudio-window-with-openfile (buffer-file-name))
  )



;;;
;;; pid func
;;;
(defun vstudio-proj-pid-file-list (pid)
  "solution debug stop"
  (cmd-vstudio (concat "cmd_project_file_list" " " pid))
  )

(defun vstudio-project-absolute-path-list ()
  ""
  (interactive)
  (let* ((pathnum 0) (pathlist nil) f)
    (while (> (running-vstudio-project-number) pathnum)
      (setq f (vstudio-project-absolute-path pathnum))
      (setq pathlist (cons f pathlist))
      (setq pathnum (1+ pathnum))
      )
    pathlist
    )
)

(defun vstudio-project-pid-list ()
  (let* ((pidnum 0) (pidlist (vstudio-project-pid pidnum)))
    (while (> (running-vstudio-project-number) (+ 1 pidnum))
      (setq pidnum (1+ pidnum))
      (setq pidlist (cons (vstudio-project-pid pidnum) pidlist))
      )
    pidlist
    )
)


;;
;; select vstudio
;;
(defvar vstudio-mode-map (make-sparse-keymap) "vstudioのキーマップ")
(define-key vstudio-mode-map "C-n" 'vstudio-select-current-pid-next-line)
(define-key vstudio-mode-map (kbd "p") 'vstudio-select-current-pid-previous-line)
(define-key vstudio-mode-map (kbd "<RET>") 'vstudio-select-current-pid-count-line)

(defun vstudio-select-current-pid-next-line (arg)
  (interactive "p")
  (forward-line arg)
  (goto-char (point-min))
)

(defun vstudio-select-current-pid-previous-line (arg)
  (interactive "p")
  (vstudio-select-current-pid-next-line (- arg))
)

;(defun vstudio-select-current-pid-show
 ; (interactive)
  ;(

(defun vstudio-select-current-pid-count-line ()
  (interactive)
  (setq current-cursol-line (count-lines 1 (point)))
  (set-current-vstudio-pid current-cursol-line)
  (kill-buffer "*vstudio list*")
  (message (concat "set current project: " (vstudio-project-absolute-path current-cursol-line) "(pid:" (vstudio-project-pid current-cursol-line) ")"))
)

(defun vstudio-select ()
  "select visual studio solution"
  (interactive)
  (if (<= (running-vstudio-project-number) 0)
      (error "visual studio is not running"))
  (let ((num 0))
    (switch-to-buffer "*vstudio list*")
    (setq truncate-line t)
    (erase-buffer)
    (while (> (running-vstudio-project-number) num)
      (setq f (vstudio-project-absolute-path num))
      (insert f)	;path
      (move-to-column (+ (length f) 2) t)
      (setq pid (vstudio-project-pid num))
      (cond 
       ((equal pid current-vstudio-proj-pid)
	(insert "* current project *"))
       (t
	(insert "<non current>"))
       )				;cond
      (setq num (1+ num))
      (if (> (running-vstudio-project-number) num)
	  (insert "\n")
	  )
      )
    (goto-char (point-min))
    (use-local-map vstudio-mode-map)
    (setq major-mode 'vstudio-mode
	  mode-name "vstudio")
    )
)


(provide 'rc-vstudio)

;;;
;;; end of file
;;;