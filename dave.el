(setenv "PATH" (concat (getenv "PATH") ":/home/dave/.gem/ruby/1.8/bin/"))

(desktop-save-mode 1)

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;(require 'rinari)
;;(setq rinari-tags-file-name "TAGS")

(require 'rspec-mode)
(require 'autotest)



(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-fullscreen)



(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines 1))
    (next-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (next-line)
      (transpose-lines -1))
    (move-to-column col)))



(defadvice find-file-at-point (around goto-line compile activate)
  (let ((line (and (looking-at ".*:\\([0-9]+\\)")
                   (string-to-number (match-string 1)))))
    ad-do-it
    (and line (goto-line line))))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))



;; yasnippet
(setq yas/root-directory "~/.emacs.d/dave/snippets")
(yas/load-directory yas/root-directory)
(load-library (concat yas/root-directory "/yasnippets-rspec/setup.el"))



(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end)
      (delete-trailing-whitespace) )
    (message "Ah, much better!"))



;; jekyll
(require 'jekyll)
(setq jekyll-directory "/home/dave/dev/kapoq-www")



;; M-( may insert a space before the open parenthesis, depending on
;; the syntax class or the preceding character. Set
;; parens-dont-require-spaces to a non-nil value if you wish to
;; inhibit this.
(setq parens-dont-require-spaces t)



;; DiredOmitMode
;; lets you hide uninteresting files, such as backup files and AutoSave files, from the DiredMode view.
;; It is defined in the Dired X package, so you must also arrange to load that
;; ‘M-o’ in Dired buffers will toggle hiding of files. Variables ‘dired-omit-files’ (note: no ‘-p’ at end) and ‘dired-omit-extensions’ together define the uninteresting files.
;; With most recent Emacs versions (e.g. 23.1.92.1) no hooks are needed and the following lines in your .emacs enable omit mode (in this example “dot” files are omitted):
(require 'dired-x)
(setq dired-omit-files-p t)



;; My own custom keys
(global-set-key (kbd "<f9>") 'ack)

(global-set-key (kbd "C-c C-d") 'duplicate-line)

(global-set-key [(control shift down)] 'move-line-down)
(global-set-key [(control shift up)] 'move-line-up)

(global-set-key (kbd "C-c b n") 'jekyll-draft-post)
(global-set-key (kbd "C-c b P") 'jekyll-publish-post)
(global-set-key (kbd "C-c b p") (lambda () 
                                  (interactive)
                                  (find-file "~/Sources/blog/_posts/")))
(global-set-key (kbd "C-c b d") (lambda () 
                                  (interactive)
                                  (find-file "~/Sources/blog/_drafts/")))


;; rcirc
(load "/home/dave/.emacs.d/dave/rcirc.el")

;; rdebug
(require 'rdebug)

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

