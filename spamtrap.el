;;; spamtrap.el --- Check message recipients for spam traps

;; Copyright (C) 1999 by Free Software Foundation, Inc.

;; Author: Lars Clausen <lrclause@shasta.cs.uiuc.edu>
;; Keywords: mail

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This code is intended to be run when a message is being sent.  It will
;; check if any receiver host is a spam trap (by matching `spam-trap-regexp'
;; against the hostname, do a DNS lookup if it is, and ask the user for
;; the real hostname if the host doesn't exist.
;; Should be put into `message-send-mail-hook'

(require 'message)

(defvar spam-trap-regexp
  "[Ss][Pp][Aa][Mm]"
  "The regexp for addresses that could be spam traps")

;;; Code:

(defun spam-trap-check ()
  (save-restriction
      (message-narrow-to-headers)
      (spam-trap-replace "BCC")
      (spam-trap-replace "CC")
      (spam-trap-replace "Mail-Copies-To")
      (spam-trap-replace "To")
      )
  )

(defun spam-trap-replace (header)
  (let ((addrs (mail-fetch-field header nil nil t))
        (fixed-addrs nil))
    (while addrs
      (let ((addr (car addrs)))
        (setq addrs (cdr addrs))
        (message (prin1-to-string `("Addr: " ,addr)))
        (if (string-match spam-trap-regexp addr)
            (let ((fixed (read-from-minibuffer "Possible spam trap: " addr)))
              (setq fixed-addrs (cons fixed fixed-addrs)))
          (setq fixed-addres (cons addr fixed-addrs)))))
    (if fixed-addrs
        (progn
          (message (prin1-to-string fixed-addrs))
          (message-remove-header header)
          (message-add-header
           (concat header ": "
                   (car fixed-addrs)
                   (if (cdr fixed-addrs)
                       (map (lambda (a) (concat ", " a)) (cdr fixed-addrs))
                     "")))))))

;;; spamtrap.el ends here
