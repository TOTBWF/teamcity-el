;;; teamcity.el --- description -*- lexical-binding: t; -*-

;;; Commentary:


;; Utilities for interacting with teamcity builds.

;;; Code:

(require 'url)
(require 'let-alist)


(defgroup teamcity nil
  "Teamcity integration for Emacs."
  :group 'programming
  )

(defcustom teamcity-url
  "http://localhost:8081"
  "The address of the teamcity instance."
  :type 'string
  :group 'teamcity
  )

(defface teamcity-face
  '((t . (:inherit 'mode-line)))
  "Faces for teamcity status indicators"
  :group 'teamcity
  )

(defface teamcity-running-face
  '((t . (:inherit 'warning 'teamcity-face)))
  "Face for teamcity running indicator"
  )

(defface teamcity-failure-face
  '((t . (:inherit 'error 'teamcity-face)))
  "Face for teamcity failure indicator"
  )

(defface teamcity-success-face
  '((t . (:inherit 'success 'teamcity-face)))
  "Face for teamcity success indicator"
  )

(defvar jiralib-token nil
  "Teamcity token used for authentication."
  )

(defun teamcity-login ()
  "Logs in to teamcity by fetching the token from .authinfo."
  (interactive)
  (let* ((url (url-generic-parse-url teamcity-url))
         (found (nth 0 (auth-source-search :max 1
                                           :host (url-host url)
                                           :port (number-to-string (url-port url))
                                           :require '(:secret)
                                           )))
         secret)
    (when found
      (setq secret (let ((sec (plist-get found :secret)))
                     (if (functionp sec)
                         (funcall sec)
                       sec))
            jiralib-token `("Authorization" . ,(format "Bearer %s" secret))))))

(defun teamcity--propertize-modeline (build)
  "Builds the propertized string for BUILD to display on the modeline."
  (print (cadr build))
  (let-alist (cadr build)
    (cond ((string= "running" .state) (propertize (format "[%s%%%%]" .percentageComplete) 'face 'teamcity-running-face))
          ((string= "FAILURE" .status) (propertize "[✘]" 'face 'teamcity-failure-face))
          ((string= "SUCCESS" .status) (propertize "[✔]" 'face 'teamcity-success-face))
          (t .status))))

(defun teamcity-branch (branch)
  "Displays the status of the latest teamcity build on BRANCH."
  (let ((url-request-extra-headers (list (teamcity-login))))
    (setq mode-line-process (with-current-buffer (url-retrieve-synchronously (format "%s/app/rest/builds?locator=running:any,branch:name:%s" teamcity-url branch))
       (let-alist (xml-parse-region url-http-end-of-headers (point-max))
         (teamcity--propertize-modeline (nth 1 .builds))
         )))))

(defun teamcity-status ()
  "Displays the status of the latest teamcity build for the current branch."
  (interactive)
  (teamcity-branch (magit-get-current-branch)))

(provide 'teamcity)
;;; teamcity.el ends here
