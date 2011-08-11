;;; remote-vstudio.el -- Remote controll Visual Studio

;; Copyright (C) 2011 FINAP

;; Version: 0.1
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
  "Remote controll visual studio program filename")
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
  (shell-command-to-string (concat vstudio-remote-exe-name " " vsfunc))
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
;; compile
;; 
(defun compile-vstudio-file (filename)
  "compile source code vc-compiler"
  (cmd-vstudio (concat "cmd_te_file_compile" " " (replace-file-path-space filename)))
  )


(defun build-vstudio-proj (pid)
  "solution build"
  (cmd-vstudio (concat "cmd_solution_build" " " pid))
  )

(defun rebuild-vstudio-proj (pid)
  "solution rebuild"
  (cmd-vstudio (concat "cmd_solution_rebuild" " " pid))
  )


(defun clean-vstudio-proj (pid)
  "solution clean"
  (cmd-vstudio (concat "cmd_clean" " " pid))
  )


(defun active-vstudio-window-pid (pid)
  "test"
  (cmd-vstudio (concat "cmd_activate" " " pid))
  )

(defun string-vstudio-output-console (pid)
  "output string"
  (set-buffer (get-vstudio-buffer))
  (insert (cmd-vstudio (concat "cmd_output_console" " " pid)))
  )

(defun start-debug-vstudio-proj (pid)
  "solution debug"
  (cmd-vstudio (concat "cmd_debug" " " pid))
  )

(defun stop-debug-vstudio-proj (pid)
  "solution debug stop"
  (cmd-vstudio (concat "cmd_debug_stop" " " pid))
  )

(defun run-vstudio-proj (pid)
  "solution no debug"
  (cmd-vstudio (concat "cmd_run" " " pid))
  )

(defun vstudio-proj-file-list (pid)
  "solution debug stop"
  (cmd-vstudio (concat "cmd_project_file_list" " " pid))
  )


(defun open-vsproj ()
  "test"
  )


;;;
;;; interactive
;;;
(defun vstudio-compile ()
  "current"
  (interactive)
  (compile-vstudio-file (buffer-file-name))
  )


(defun vstudio-build ()
  "current"
  (interactive)
  (build-vstudio-proj current-vstudio-proj-pid)
  )


(defun vstudio-debug ()
  "debug"
  (interactive)
  (start-debug-vstudio-proj current-vstudio-proj-pid)
  )

(defun string-current-vstudio-output-console ()
  "output string"
  (interactive)
  (string-vstudio-output-console current-vstudio-proj-pid)
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
  ""
  (interactive)
  (if (<= (running-vstudio-project-number) 0)
      (error "visual studio not running"))
  (let ((num 0))
    (switch-to-buffer "*vstudio list*")
    (setq truncate-line t)
    (erase-buffer)
    (while (> (running-vstudio-project-number) num)
      (setq f (vstudio-project-absolute-path num))
      (insert f)	;path
      (move-to-column 128 t)
      (setq pid (vstudio-project-pid num))
      (cond 
       ((equal pid current-vstudio-proj-pid)
	(insert "<current project>"))
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


(provide 'remote-vstudio)