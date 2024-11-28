;; -*- lexical-binding: t; -*-

(require 'org-srs-item)
(require 'org-srs-review)

(defconst org-srs-item-cloze-regexp
  (rx "{{" (group (*? not-newline))
      "}{" (group (*? not-newline))
      (or "}}" (and "}{" (group (*? not-newline)) "}}"))))

(provide 'org-srs-item-cloze)
