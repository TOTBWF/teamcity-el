;;; teamcity.el --- description -*- lexical-binding: t; -*-

;;; Commentary:

;; Utilities for interacting with teamcity builds.
;; Docs for the rest API: https://www.jetbrains.com/help/teamcity/rest-api.html

;;; Code:

(require 'url)
(require 'let-alist)
(require 'ansi-color)


(defgroup teamcity nil
  "Teamcity integration for Emacs."
  :group 'programming)

(defcustom teamcity-url
  "http://localhost:8081"
  "The address of the teamcity instance."
  :type 'string
  :group 'teamcity)

(defface teamcity-face
  '((t . (:inherit 'mode-line)))
  "Faces for teamcity status indicators"
  :group 'teamcity)

(defface teamcity-running-face
  '((t . (:inherit 'warning 'teamcity-face)))
  "Face for teamcity running indicator")

(defface teamcity-failure-face
  '((t . (:inherit 'error 'teamcity-face)))
  "Face for teamcity failure indicator")

(defface teamcity-success-face
  '((t . (:inherit 'success 'teamcity-face)))
  "Face for teamcity success indicator")

(defvar teamcity-token nil
  "Teamcity token used for authentication.")

(defvar teamcity-project nil
  "Currently focused teamcity project.")

(defvar teamcity-status--timer nil
  "Teamcity build status refresh timer.")

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
            teamcity-token `("Authorization" . ,(format "Bearer %s" secret))))))

(defun teamcity--propertize-status (build)
  "Builds the propertized string for BUILD to display on the modeline."
  (let-alist (cadr build)
    (cond ((string= "running" .state) (propertize (format "[%s%%%%]" .percentageComplete) 'face 'teamcity-running-face))
          ((string= "FAILURE" .status) (propertize "[✘]" 'face 'teamcity-failure-face))
          ((string= "SUCCESS" .status) (propertize "[✔]" 'face 'teamcity-success-face))
          (t .status))))

(defun teamcity--request (resource method data callback)
  "Perform a METHOD request to a teamcity rest RESOURCE with DATA, and call CALLBACK on the parsed xml."
  (let ((url-request-extra-headers (list (teamcity-login) `("Origin" . ,teamcity-url) '("Content-Type" . "application/xml")))
        (url-request-method method)
        (url-request-data data)
        (buffer (current-buffer)))
    (url-retrieve (format "%s/app/rest/%s" teamcity-url resource)
                  (lambda (_)
                    (let ((xml (xml-parse-region url-http-end-of-headers (point-max))))
                      (with-current-buffer buffer (funcall callback xml)))))))

(defun teamcity--request-synchronously (resource method data)
  "Perform a METHOD request synchronously to a teamcity rest RESOURCE with DATA and parse to xml."
  (let ((url-request-extra-headers (list (teamcity-login) `("Origin" . ,teamcity-url) '("Content-Type" . "application/xml")))
        (url-request-method method)
        (url-request-data data))
    (with-current-buffer (url-retrieve-synchronously (format "%s/app/rest/%s" teamcity-url resource))
      (xml-parse-region url-http-end-of-headers (point-max)))))

(defun teamcity-status--update (branch)
  "Update the status of the latest teamcity build on BRANCH."
  (teamcity--request (format "builds?locator=running:any,branch:name:%s,buildType:project:%s,count:1" branch teamcity-project)
                     "GET"
                     nil
                     (lambda (xml)
                       (let-alist xml
                         (setq mode-line-process (teamcity--propertize-status (nth 1 .builds))
                               teamcity-status--timer (run-at-time 30 nil (lambda () (teamcity-status--update branch))))
                         ))
                     ))

(defun teamcity-browse-branch (branch)
  "Open the BRANCH in a web browser."
  (teamcity--request (format "builds?locator=running:any,branch:name:%s" branch)
                     "GET"
                     nil
                     (lambda (xml)
                       (let-alist xml
                         (let-alist (cadr (nth 1 .builds))
                           (browse-url .webUrl))))))

(defun teamcity-trigger-build (build-conf branch)
  "Trigger a build using BUILD-CONF on the given BRANCH."
  (teamcity--request "buildQueue"
                     "POST"
                     (xmlgen `(build :branchName ,branch (buildType :id ,build-conf)))
                     (lambda (xml) (print xml))))

;; (defun teamcity-queue--list)

(defun teamcity-queue-list ()
  "List all teamcity builds in the queue."
  (let-alist (teamcity--request-synchronously "buildQueue" "GET" nil)
    (mapcar (lambda (build)
              (let-alist (cadr build)
                (cons (concat .branchName " [" .id "]") .webUrl))) (cdr .builds))))

(defun teamcity-project-list ()
  "List all teamcity projects."
  (let-alist (teamcity--request-synchronously "projects" "GET" nil)
    (-keep (lambda (proj)
             (let-alist (cadr proj)
               (when (not (or (string= .parentProjectId "_Root")
                              (string= .id "_Root")))
                 (cons (concat .name  " [" .parentProjectId "]") .id)))) (cdr .projects))))

(defun teamcity--branch-status (branch)
  "Get the status of the last build on BRANCH."
  (let-alist (teamcity--request-synchronously (format "builds?locator=branch:name:%s,buildType:project:%s,count:1" branch teamcity-project) "GET" nil)
    (nth 1 .builds)))

(defun teamcity-branch-list ()
  "List all of the teamcity branches, along with the status of the last build."
  (let-alist (teamcity--request-synchronously (format "projects/id:%s/branches" teamcity-project) "GET" nil)
    (mapcar (lambda (branch)
              (let-alist (cadr branch)
                (let ((status (teamcity--branch-status .name)))
                  (cons (concat (teamcity--propertize-status status) " " .name) (cadr status)))))(cdr .branches))))

(defun teamcity-configuration-list ()
  "List all build configurations for the selected branch."
  (let-alist (teamcity--request-synchronously (format "buildTypes?locator=project:id:%s" teamcity-project) "GET" nil)
    (mapcar (lambda (type)
              (let-alist (cadr type)
                (cons .name .id))) (cdr .buildTypes))))

(defun teamcity-build-log (build-id)
  "Get the build log for BUILD-ID."
  (let ((log-buffer-name (format "*build-log-%s*" build-id)))
    (unless (get-buffer log-buffer-name)
      (display-buffer (with-current-buffer (url-retrieve-synchronously (format "%s/httpAuth/downloadBuildLog.html?buildId=%s" teamcity-url build-id))
                        (delete-region (point-min) url-http-end-of-headers)
                        (delete-trailing-whitespace (point-min) (point-max))
                        (while (re-search-forward "\\[[0-9,?]+[a-z]" nil t)
                          (replace-match (concat "" (match-string 0))))
                        (ansi-color-apply-on-region (point-min) (point-max))
                        (rename-buffer log-buffer-name))))))

(defun teamcity-queue-transformer (build)
  "Ivy transformer for a given BUILD."
  (put-text-property (string-match-p " \\[" build) (length build) 'face 'font-lock-doc-face build)
  build)

(defun teamcity-queue ()
  "Browse the teamcity build queue."
  (interactive)
  (ivy-read "Open Build: " (teamcity-queue-list)
            :action (lambda (build) (browse-url (cdr build)))
            :caller 'teamcity-queue))

(defun teamcity-project-transformer (project)
  "Ivy transformer for a given PROJECT."
  (put-text-property (string-match-p " \\[" project) (length project) 'face 'font-lock-doc-face project)
  project)

(defun teamcity-projects ()
  "Browse teamcity projects."
  (interactive)
  (ivy-read "Select Project: " (teamcity-project-list)
            :action (lambda (project) (setq teamcity-project (cdr project)))
            :caller 'teamcity-projects))

(defun teamcity-branches ()
  "Browse teamcity branches."
  (interactive)
  (ivy-read "Select Branch: " (teamcity-branch-list)
            :action (lambda (branch) (let-alist (cdr branch) (browse-url .webUrl)))
            :caller 'teamcity-branches))

(defun teamcity-branches-build (branch)
  "Select a build configuration on a given BRANCH."
  (ivy-read "Build Configuration: " (teamcity-configuration-list)
            :action (lambda (build-conf) (teamcity-trigger-build (cdr build-conf) branch))
            :caller 'teamcity-branches-build))

(ivy-set-display-transformer 'teamcity-queue 'teamcity-queue-transformer)
(ivy-set-display-transformer 'teamcity-projects 'teamcity-project-transformer)

(ivy-set-actions
 'teamcity-branches
 '(("r" (lambda (branch) (let-alist (cdr branch) (teamcity-branches-build .branchName))) "rerun")
   ("l" (lambda (branch) (let-alist (cdr branch) (teamcity-build-log .id))) "log")))

(defun teamcity-browse ()
  "Opens the latest teamcity build for the current branch."
  (interactive)
  (teamcity-browse-branch (magit-get-current-branch)))

(defun teamcity-status--toggle-mode (enable)
  "Toggle the teamcity mode based off of ENABLE."
  (if enable
      (cancel-timer teamcity-status--timer)
    (teamcity-status--update (magit-get-current-branch))
    ))

(define-minor-mode teamcity-status-mode
  "Monitor the build status of the current branch."
  :global t
  :variable (t . teamcity-status--toggle-mode)
  )

(provide 'teamcity)
;;; teamcity.el ends here
