;;; nibbles-mode.el --- A Nibbles editing mode       -*- lexical-binding: t; -*-

;; Copyright (C) 2023  kaki

;; Author: kaki
;; Package-Requires: ((cl-lib "1.0") (rx) (dash "2.18.1"))
;; Created: 15 July 2023
;; Version: 0.0.0
;; Keywords: languages, Nibbles

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'rx)
(require 'dash)

(defconst nibbles-mode-version "0.0.0")

(defgroup nibbles-mode nil
  "Major mode for editing Nibbles code."
  :group 'languages)

(defface nibbles-mode-operator-face
  '((t :inherit default))
  "Face used to highlight builtin operators."
  :group 'nibbles-mode)

(eval-and-compile
  ;; generate from http://golfscript.com/nibbles/quickref.html
  (defconst nibbles-mode--command-alist
    '((":" . ": : 7 :: any* any* ~[] \"append\"")
      ("]" . "] : 8 :: num >num ~0 \"max\"")
      ("+" . "+ : 8 :: num vec ~1 ~1 \"add\" | 8 :: [int] \"sum\" | 8 :: [[*]] \"concat\"")
      ("[" . "[ : a :: int >num \"min\"")
      ("*" . "* : a :: int vec ~-1 ~2 \"multiply\" | 8 :: str [*] \"join\"")
      ("-" . "- : 9 :: num num ~1 ~1 \"subtract\" | f :: [a] [a] \"diff\"")
      ("/" . "/ : b :: num num ~2 \"divide\" | a :: [*] reqfn \"foldr1\" | a :: [*] const fn ~~tuple \"foldr\"")
      ("%" . "% : c :: num num ~2 \"modulus\" | 8 :: str str|chr ~words \"split (remove empties)\"")
      ("^" . "^ : e :: int int ~10 ~2 \"pow (minus is nth root)\" | e :: int [*]|chr ~∞ \"replicate\"")
      ("." . ". : c :: [*] fn \"map\"")
      ("|" . "| : 9 :: [*] reqfn ~not∘ \"filter\" | 9 :: [*] ~ ~ fn ~not∘ \"partition\"")
      ("!" . "! : 9 :: [*] const zipop \"zip with\"")
      ("\\" . "\\ : b :: [*] \"reverse\" | a :: chr chclass \"char class?\"")
      ("," . ", : d :: [*] \"length\" | d :: num ~∞ \"range 1.. ( `, is 0...)\"")
      ("<" . "< : b :: num [*] ~while \"take ( `< also drop)\"")
      (">" . "> : c :: int [*] ~while \"drop ( `> also take)\"")
      ("=" . "= : 9 :: num [*] ~~nowrap \"subscript (wrapped)\"")
      ("?" . "? : f :: [a] a ~by \"index (or 0 if not found)\" | f :: num fn fn ~default \"if/else\"")
      ("&" . "& : 8 :: str int vec ~~center \"justify\"")
      ("o" . "o : e :: chr \"ord\"")
      ("`/" . "`/ : 9 d :: num num ~2 \"divmod\" | be :: num [*] ~2 \"chunks of\" | e b :: [*] foldop \"special folds\"")
      ("`%" . "`% : b d :: num num ~2 \"moddiv\" | bc :: num [*] ~2 \"step\" | f1 :: [*] any ~default \"split list (keep empties)\"")
      ("`r" . "`r : f 1 :: str \"read int\"")
      ("?," . "?, : fd :: [*] fn fn ~default \"if/else (lazy list)\"")
      (">>" . ">> : c0 :: [*] \"tail\"")
      ("<<" . "<< : b0 :: [*] \"init\"")
      ("`(" . "`( : e 1 :: [*] \"uncons\" | c 7 :: chr \"to lowercase\"")
      ("`)" . "`) : e 2 :: [*] \"swapped uncons\" | c 2 :: chr \"to uppercase\"")
      ("`\\" . "`\\ : de :: int [*] ~2 \"n chunks\" | e c :: [*] foldop \"special scans\"")
      ("`?" . "`? : e a :: [*] fn|const ~not∘ \"find indices [by]\"")
      ("`=" . "`= : b9 :: [*] reqfn \"chunk by\"")
      ("=~" . "=~ : bc :: [*] reqfn ~~nosort \"group by (also sorts)\"")
      ("or" . "or : bc :: [*] const ~~tbd \"or\"")
      ("`<" . "`< : e d :: [*] \"sort\"")
      ("`'" . "`' : e 3 :: [*] \"transpose\"")
      ("=\\" . "=\\ : dc :: [*] reqfn \"scanl1\" | dc :: [*] const fn ~~tuple \"scanl\"")
      ("`*" . "`* : 90 :: [int] \"product\" | 90 :: [[*]] \"nary cartesian product\"")
      ("`_" . "`_ : cc :: int [*] ~2 \"subsequences\"")
      ("``p" . "``p : b80 :: [*] \"permutations\"")
      ("`:" . "`: : e 8 :: [a] [a] \"list of 2 lists\"")
      ("`-" . "`- : e 8 :: [*] fn? any ~~uniq \"list difference [by]\"")
      ("`&" . "`& : e 5 :: [*] fn? any ~~uniq \"list intersection [by]\" | c0 :: num num ~-2 \"bit intersection\"")
      ("`|" . "`| : e 6 :: [*] fn? any ~~uniq \"list union [by]\" | b0 :: num >num ~1 \"bit union\"")
      ("`^" . "`^ : e 7 :: [*] fn? any ~~uniq \"list xor [by]\" | b0 :: num num ~1 \"bit xor\"")
      ("`$" . "`$ : e 4 :: [*] \"uniq\" | d7 :: num \"signum\"")
      ("!=" . "!= : c0 :: num >num ~0 \"abs diff\"")
      ("%~" . "%~ : e 0 :: [*] fn ~not∘ \"split by\"")
      ("-~" . "-~ : 90 :: str \"strip\"")
      ("ch" . "ch : dd :: int ~256 \"chr\"")
      ("`p" . "`p : f1 :: num \"int to str\"")
      ("``@" . "``@ : d70 :: num \"to bits\"")
      ("hex" . "hex : b02 :: any \"to/from hex\"")
      ("`D" . "`D : 800 :: {int} \"to base from data\"")
      ("`@" . "`@ : 87 :: int num ~10 ~tbd \"to base\" | b0 :: num [*] ~10 \"from base\"")
      ("`." . "`. : b2 :: any* fn ~~inf \"iterate while uniq\"")
      (".~~" . ".~~ : 900 :: any* fn \"append until null\"")
      (";~" . ";~ : 60 :: any* reqfn \"save fn\"")
      ("==" . "== : 60 :: any* const \"equal?\"")
      ("`;" . "`; : 66 :: any* fn \"recursion (alt name ``; )\"")
      ("`#" . "`# : f0 :: any {int} ~~nosalt \"hashmod\"")
      ("error" . "error : fd02 :: any \"error\"")))

  (defconst nibbles-mode--literate-only-command-alist
    '(("fsb" . "fsb : (literate only) :: [*] int int fn ~∞ ~not∘ \"find salt by\"")
      ("fs" . "fs : (literate only) :: [*] int int [*] ~∞ \"find salt\"")
      ("pt" . "pt : (literate only) :: any \"debug arg type\"")
      ("p" . "p : (literate only) :: any \"show\"")
      ("ct" . "ct : (literate only) :: () \"debug context types\"")))

  (defconst nibbles-mode--syntax-alist
    '((";" . "; : save")
      ("let" . "let : let statement (literate only)")
      ("sets" . "sets : name extras")))

  (defconst nibbles-mode--command-length
    (apply #'max
           (mapcar (lambda (pair)
                     (length (car pair)))
                   nibbles-mode--command-alist)))

  (rx-define nibbles-mode-debruijn-index
    (seq (0+ ?\;)
         (in "$@_")))

  (defconst nibbles-mode--command-regexps
    (cl-destructuring-bind (command-alpha command-symbol)
        (-separate (lambda (s)
                     (string-match-p (rx string-start (in "0-9A-Za-z"))
                                     s))
                   (mapcar #'car nibbles-mode--command-alist))
      (cl-destructuring-bind (command-sympha command-symbol)
          (-separate (lambda (s)
                       (string-match-p (rx (in "0-9A-Za-z") string-end)
                                       s))
                     command-symbol)
        (mapcar #'regexp-opt
                (list command-alpha command-symbol command-sympha)))))

  (defconst nibbles-mode--command-alpha-regexp
    (nth 0 nibbles-mode--command-regexps))

  (defconst nibbles-mode--command-symbol-regexp
    (nth 1 nibbles-mode--command-regexps))

  (defconst nibbles-mode--command-sympha-regexp
    (nth 2 nibbles-mode--command-regexps))

  (defconst nibbles-mode--command-regexp
    (rx (or (seq symbol-start
                 (regexp nibbles-mode--command-alpha-regexp)
                 symbol-end)
            (regexp nibbles-mode--command-symbol-regexp)
            (regexp nibbles-mode--command-sympha-regexp))))

  (defconst nibbles-mode--literate-only-command-regexp
    (regexp-opt
     (mapcar #'car nibbles-mode--literate-only-command-alist))))

(defvar nibbles-mode--builtins-regexp
  (cl-destructuring-bind (syntax-alpha syntax-symbol)
      (-separate (lambda (s)
                   (string-match-p (rx string-start symbol-start)
                                   s))
                 (mapcar #'car nibbles-mode--syntax-alist))
    (rx-to-string
     `(or (submatch nibbles-mode-debruijn-index)
          (submatch (or (regexp
                         ,(regexp-opt syntax-symbol))
                        (seq symbol-start
                             (regexp
                              ,(regexp-opt syntax-alpha))
                             symbol-end)))
          (submatch (regexp
                     ,nibbles-mode--command-regexp))
          (submatch (regexp
                     ,nibbles-mode--literate-only-command-regexp))))))

(defun nibbles-mode-debruijn-index-string-to-number (str)
  "Convert argument reference identifier STR to DeBruijn index.

STR must match (rx nibbles-mode-debruijn-index)."
  (let ((i (string-match-p "[$@_]" str)))
    (+ 1
       (* i 3)
       (seq-position "$@_" (aref str i)))))

(defun nibbles-mode-info-at-point ()
  "Meant for `eldoc-documentation-function'."
  (save-excursion
    (when (looking-at (rx (or (syntax whitespace)
                              buffer-end)))
      (backward-char))
    (let ((pos (point))
          match?)
      (cl-loop for p from (max (point-min) (- pos nibbles-mode--command-length)) to pos
               do (goto-char p)
               until (setq match? (and (looking-at nibbles-mode--builtins-regexp)
                                       (> (match-end 0) pos))))
      (when match?
        (cond ((match-beginning 1)
               (let ((s (match-string 1)))
                 (format "%s : DeBruijn index %d"
                         s
                         (nibbles-mode-debruijn-index-string-to-number s))))
              ((match-beginning 2)
               (cdr
                (assoc (match-string 2)
                       nibbles-mode--syntax-alist)))
              ((match-beginning 3)
               (cdr
                (assoc (match-string 3)
                       nibbles-mode--command-alist)))
              ((match-beginning 4)
               (cdr
                (assoc (match-string 4)
                       nibbles-mode--literate-only-command-alist))))))))

(rx-define nibbles-mode-string (delim)
  (seq delim (0+ (or (seq ?\\ anychar)
                     (not (in ?\\ delim))))
       delim))

(defvar nibbles-mode-font-lock-keywords
  `((,(rx nibbles-mode-debruijn-index)
     . font-lock-variable-name-face)
    (,(rx symbol-start
          (1+ (in "0-9")))
     . font-lock-constant-face)
    (,(rx (or ";"
              (seq symbol-start
                   "sets"
                   symbol-end)))
     . font-lock-keyword-face)
    (,(rx symbol-start
          (submatch "let")
          (optional
           (1+ (syntax whitespace))
           (submatch (1+ (or (syntax word)
                             (syntax symbol))))))
     (1 font-lock-keyword-face)
     (2 font-lock-variable-name-face nil t))
    (,(rx symbol-start
          (regexp nibbles-mode--literate-only-command-regexp)
          symbol-end)
     . font-lock-warning-face)
    (,nibbles-mode--command-regexp
     . 'nibbles-mode-operator-face)))

(defvar nibbles-mode-syntax-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (modify-syntax-entry ?! "." table)
    (cl-loop for c from ?# to ?/
             do (modify-syntax-entry c "." table))
    (cl-loop for c from ?: to ?@
             do (modify-syntax-entry c "." table))
    (modify-syntax-entry ?\[ "." table)
    (cl-loop for c from ?\] to ?`
             do (modify-syntax-entry c "." table))
    (modify-syntax-entry ?\| "." table)
    (modify-syntax-entry ?\~ "." table)
    (modify-syntax-entry ?\' "\"" table)
    table))

(defalias 'nibbles-mode-syntax-propertize
  (syntax-propertize-rules
   ((rx ?\\)
    (0 "_"))
   (nibbles-mode--command-sympha-regexp
    (0 "."))
   ((rx "`'")
    (0 "."))
   ((rx (or (nibbles-mode-string ?\")
            (nibbles-mode-string ?\')))
    (0 nil))))

(defvar nibbles-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map prog-mode-map)
    map))

;;;###autoload
(define-derived-mode nibbles-mode prog-mode "Nibbles"
  "Major mode for editing Nibbles code."
  (setq font-lock-defaults
        `((nibbles-mode-font-lock-keywords)
          nil nil nil beginning-of-defun
          (font-lock-mark-block-function . mark-paragraph)))
  (setq-local syntax-propertize-function
              #'nibbles-mode-syntax-propertize)

  (setq-local require-final-newline nil)

  (setq-local eldoc-documentation-function
              #'nibbles-mode-info-at-point)
  (eldoc-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.nbl\\'" . nibbles-mode))

(provide 'nibbles-mode)

;;; nibbles-mode.el ends here
