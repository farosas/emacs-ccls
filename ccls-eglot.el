;;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Fabiano Rosas

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and-or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'eglot)
(require 'ccls-call-hierarchy)

(defvar lsp-current-server)

(defun eglot-call-hierarchy--read-node (data &optional parent)
  (mapcan
   (eglot--lambda ((CallHierarchy) id name location callType numChildren children)
     (eglot--dbind ((Location) uri range) location
       (eglot--dbind ((Range) start end) range
	 (make-ccls-tree-node
	  :location (cons (eglot--uri-to-path uri) start)
	  :has-children (< 0 numChildren)
	  :parent parent
	  :expanded nil
	  :children nil
	  :data (make-ccls-call-hierarchy-node
		 :id id
		 :name name
		 :call-type callType))
	 )))
   (if (vectorp data) data (list data))))

(defun eglot-call-hierarchy--request-children (callee node)
  "."
  (let ((id (ccls-call-hierarchy-node-id (ccls-tree-node-data node))))
    (mapcan
     (eglot--lambda ((CallHierarchy) id name location callType numChildren children)
       (--map (ccls-eglot-call-hierarchy--read-node it node) children))
     (list (jsonrpc-request lsp-current-server :$ccls/call
			    `(:id ,id
				  :callee ,callee
				  :callType 3
				  :levels ,ccls-tree-initial-levels
				  :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
				  :hierarchy t))))))

(defun eglot-call-hierarchy--request-init (callee)
  "."
  (defvar lsp-current-server (eglot--current-server-or-lose))

  (jsonrpc-request lsp-current-server :$ccls/call
		   `(:textDocument (:uri ,(concat lsp--uri-file-prefix buffer-file-name))
				   :position ,(eglot--pos-to-lsp-position)
				   :callee ,callee
				   :callType 3
				   :qualified ,(if ccls-call-hierarchy-qualified t :json-false)
				   :hierarchy t)))

(defun eglot-call-hierarchy--make-string (node depth)
  "Propertize the name of NODE with the correct properties"
  (let ((data (ccls-tree-node-data node)))
    (if (= depth 0)
        (ccls-call-hierarchy-node-name data)
      (concat
       (propertize (ccls-call-hierarchy-node-name data)
                   'face (pcase (ccls-call-hierarchy-node-call-type data)
                           ('0 'ccls-call-hierarchy-node-normal-face)
                           ('1 'ccls-call-hierarchy-node-base-face)
                           ('2 'ccls-call-hierarchy-node-derived-face)))
       (propertize (format " (%s:%s)"
                           (file-name-nondirectory (car (ccls-tree-node-location node)))
                           (cdr (ccls-tree-node-location node)))
                   'face 'ccls-tree-mode-line-face)))))

(defun eglot-call-hierarchy (callee)
  (interactive "P")
  (setq callee (if callee t :json-false))
  (ccls-tree--open
   (make-ccls-tree-client
    :name "call hierarchy"
    :mode-line-format (format " %s %s %s %s"
			      (propertize (if (eq callee t) "Callee types:" "Caller types:") 'face 'ccls-tree-mode-line-face)
			      (propertize "Normal" 'face 'ccls-call-hierarchy-node-normal-face)
			      (propertize "Base" 'face 'ccls-call-hierarchy-node-base-face)
			      (propertize "Derived" 'face 'ccls-call-hierarchy-node-derived-face))
    :top-line-f (lambda () (propertize (if (eq callee t) "Callees of " "Callers of") 'face 'ccls-tree-mode-line-face))
    :make-string-f 'eglot-call-hierarchy--make-string
    :read-node-f 'eglot-call-hierarchy--read-node
    :request-children-f (apply-partially #'eglot-call-hierarchy--request-children callee)
    :request-init-f (lambda () (eglot-call-hierarchy--request-init callee)))))
