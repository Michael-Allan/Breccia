;; The implementation of Breccia mode, a major mode for editing Breccian text.
;;
;; USAGE
;; ─────
;;   In your initialization file:
;;
;;      (require 'Breccia_mode)
;;      (add-to-list 'auto-mode-alist '("\\.brec\\'" . breccia-mode))
;;
;;   Working example:
;;
;;       http://reluk.ca/.emacs
;;       http://reluk.ca/.Xresources
;;       http://reluk.ca/project/Breccia/Emacs/mock-up.svg  [pending a screen shot]
;;
;;
;; DEFINITION OF TERMS  (additional to those of `http://reluk.ca/project/Breccia/`)
;; ───────────────────
;;   fontification chunk
;;       A document descriptor or fontification principal.
;;
;;   fontification principal
;;       A principal construct with one exception: in the case of a divider, each perfectly indented
;;       line starts a new fontification principal, such that a multi-line divider, while a single
;;       principal construct, may nevertheless comprise several contiguous fontification principals.
;;
;;
;; NOTES  (see at bottom)
;; ─────


(define-derived-mode breccia-mode text-mode
  "Breccia"
  "A major mode for editing Breccian text"
  (set 'font-lock-multiline t) ; For sake of aside and command-point bodies.
  (add-hook 'font-lock-extend-region-functions 'brecExtendSearch) ; [FLE]
 ;(set (make-local-variable 'font-lock-defaults)… )
 ;;; ‘It automatically becomes buffer-local when set.’ [FLB]
  (set 'font-lock-defaults '(brecKeywords))
  )



;; ═════════════════════════════════════════════════════════════════════════════════════════════════════
;; D e c l a r a t i o n s   i n   l e x i c a l   o r d e r
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════


(defface brecBulletFace
  `((default . (:inherit bold)))
  "The face for the bullet of a point.")



(defun brecChunkEndFromMidChunk()
  "Returns the end position of the present fontification chunk, provided that point is *not*
at the beginning of the chunk.  If point is at the beginning, then the result is undefined."
  (save-excursion
    (if (re-search-forward brecPrincipalLeaderPattern nil t) ; Cf. `brecExtendSearchDown`.
        (end-of-line 0) ; Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



(defface brecAsideBulletFace
  `((default . (:inherit (brecBulletFace brecAsidePointFace))))
  "The face for the bullet of an aside point.")



(defface brecAsidePointFace
  `((default . (:inherit font-lock-doc-face)))
  "The face for the descriptor of an aside point.")



(defface brecCommandBulletFace
  `((default . (:inherit (brecBulletFace brecCommandPointFace))))
  "The face for the bullet of a command point.")



(defface brecCommandPointFace
  `((default . (:inherit font-lock-builtin-face)))
  "The face for the descriptor of a command point.")



(defface brecCommentBlockLabelFace
  `((default . (:inherit font-lock-doc-face)))
  "The face for a comment block label.")



(defface brecDividerFace
  `((default . (:inherit font-lock-doc-face)))
  "The face for a divider.")



(defface brecDividerInverseLabelFace
  `((default . (:inherit (bold brecDividerFace)))
    (t :inverse-video t))
  "The face for an inverse label in a divider.")



(defface brecDividerLabelFace
  `((default . (:inherit (bold brecDividerFace))))
  "The face for an ordinary label in a divider.")



(defun brecExtendSearch()
  "Ensures that the font-lock search region extends to cover the whole of its fontification chunks,
bisecting none of them.  Returns nil if already it does, non-nil otherwise."
  (save-excursion
    (let ((is-changed (brecExtendSearchUp)))
      (or (brecExtendSearchDown) is-changed))))



(defun brecExtendSearchDown()
  "Ensures that `font-lock-end` bisects no fontification chunk, moving it forward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-end)
  (when (not (or (bolp)(eolp))) ; When the prior extenders such as `font-lock-extend-region-wholelines`
    ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
    ;; region bisects the text of the line, which means the text of a fontification chunk;
    ;; a Breccian document comprises at most a document descriptor and principal constructs [D],
    ;; both in turn comprising fontification chunks, and each covers the whole of its lines.
    (end-of-line)) ; Thus far at least the present construct must extend; extend it now,
                 ;;; that `re-search-forward` (below) must miss its leader.
  (let (is-changed)
    (if (re-search-forward brecPrincipalLeaderPattern nil t) ; Cf. `brecChunkEndFromMidChunk`.
        (end-of-line 0) ; Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (set 'font-lock-end (point))
      (set 'is-changed t))
    is-changed))



(defun brecExtendSearchUp()
  "Ensures that `font-lock-beg` bisects no fontification chunk, moving it backward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line) ; That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brecPrincipalLeaderPattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (set 'font-lock-beg (point))
      (set 'is-changed t))
    is-changed))



(defconst brecKeywords
  (let* ((drawingChar "[\u2500-\u2587\u2589-\u258f\u2591-\u259f]")
           ;;; The union of `drawingChar ∪ inversionMark` is `\u2500-\u259f`, namely a ‘division mark’.
         (drawingI (concat "\\(" drawingChar "+\\(?: +" drawingChar "+\\)*\\)"))
           ;;; Capturing (I) a divider drawing sequence exclusive of embedded newlines.
         (labelingChar "[^[:space:]\u2500-\u259f]") ; Exclusive of whitespace.
         (labeling (concat labelingChar "+\\(?: +" labelingChar "+\\)*"))
           ;;; A divider labeling sequence exclusive of embedded newlines.
         (labelingI (concat "\\(" labeling "\\)"))
           ;;; Capturing (I) a `labeling` sequence.
         (inversionMark "[\u2588\u2590]")
         (inverseLabelingIII
          (concat "\\(" inversionMark "\\)\\( *\\(?:" labeling " *\\)?\\)\\(\u2588\\)?")))
           ;;; Capturing (I) an inversion mark, (II) any sequence of labeling and space characters,
           ;;; and (III) any full block character.
    (list

     ;; ════════════
     ;; Plain bullet, with inadvertent exceptions as noted.
     ;; ════════════
     ;; First the case of a non-alphanumeric singleton.  Such a bullet comprises a single perfectly
     ;; indented (│␢⇥) character — neither alphanumeric nor whitespace, neither a backslash (\),
     ;; division mark (D) nor specialized bullet symbol (S) — terminated by a space or newline.
     ;;
     ;;         │␢⇥
     ;;     ┌────────┐  ┌──────────────────────────────────────────┐
     (cons "^ \\{4\\}*\\([^[:alnum:][:space:]\\\u2500-\u259f/:⋱+]\\)\\(?: \\|$\\)"
           ;;                                └─┴───────────┴────┘
           ;;                               ¬ \      D        S
           '(1 'brecPlainBulletFace))

     ;; Next the case of a bullet comprising a non-alphanumeric sequence without an initial backslash.
     ;; It begins with perfect indentation (│␢⇥) and a character (C) that is neither alphanumeric nor
     ;; whitespace, neither a backslash (\) nor division mark (D).
     ;;
     ;;            │␢⇥                     C
     (cons ;   ┌────────┐  ┌───────────────────────────────────┐
      (concat "^ \\{4\\}*\\([^[:alnum:][:space:]\\\u2500-\u259f]"
        ;;;                                     └─┴────────────┘
        ;;;                                    ¬ \      D
        ;;;
        ;;; It continues with one or more non-alphanumeric, non-whitespace characters,
        ;;; the last of which (Cn) is not a specialized terminator (T).
        ;;;
        ;;;                                    Cn
        ;;;    ┌──────┈     ┈──────┐ ┌────────────────────┐
              "[^[:alnum:][:space:]]*[^[:alnum:][:space:]+]\\)")
        ;;;                                              │
        ;;;                                            ¬ T
        ;;;
        ;;; Note: this matcher inadvertently fontifies exception and non-singleton jointer bullets,
        ;;; requiring they be refontified further below.

      '(1 'brecPlainBulletFace))

     ;; Finally the bullets that begin with backslashes, or contain alphanumeric characters, or both.
     ;; These are handled by a single matcher.  A positive match begins with perfect indentation (│␢⇥)
     ;; and a character sequence (C⋯) comprising either one or more backslashes and a non-alphanumeric
     ;; character, or zero or more backslashes and an alphanumeric character.
     ;;
     ;;            │␢⇥
     (cons ;   ┌────────┐     ┌─────────────────────┈     ┈──────────────────────────┐
      (concat "^ \\{4\\}*\\(\\(?:\\\\+[^[:alnum:][:space:]\\]\\|\\\\*[[:alnum:]]+ *\\)"

        ;;; It continues with any number of character sequences.
        ;;;
        ;;;      ┌─────────────────┈     ┈───────────────────┐

              "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]]+\\)*\\)")
        ;;;
        ;;; It ends either with a non-alphanumeric character followed by a space, or with a newline.
        ;;;
        ;;; Note: this matcher inadvertently fontifies exception and non-singleton task bullets,
        ;;; requiring they be refontified further below

      '(1 'brecPlainBulletFace))


   ;;; ──  S p e c i a l i z e d   c o n s t r u c t s  ────────────────────────────────────────────────


     ;; ═══════════
     ;; Aside point
     ;; ═══════════
     ;; An aside point starts with a perfectly indented (│␢⇥) bullet comprising one slash (/).
     (list "^ \\{4\\}*\\(/\\)\\(?: +\\|$\\)"
           ;; ┈──────┘  └───┘
           ;;   │␢⇥       /

           '(1 'brecAsideBulletFace)
           (list                        ; Usually a descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)"     ; extending thence to the end of the point.
            '(brecChunkEndFromMidChunk) ; Before seeking to fontify it, bring in the whole of it. [PSE]
            nil '(1 'brecAsidePointFace)))


     ;; ═════════════
     ;; Command point
     ;; ═════════════
     ;; A command point starts with a perfectly indented (│␢⇥) bullet comprising one colon (:).
     (list "^ \\{4\\}*\\(:\\)\\(?: +\\|$\\)"
           ;; ┈──────┘  └───┘
           ;;   │␢⇥       :

           '(1 'brecCommandBulletFace)
           (list                        ; Usually a command descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)"     ; extending thence to the end of the point.
            '(brecChunkEndFromMidChunk) ; Before seeking to fontify it, bring in the whole of it. [PSE]
            nil '(1 'brecCommandPointFace)))


     ;; ═══════
     ;; Divider
     ;; ═══════
     ;; A divider starts with a perfectly indented (│␢⇥) sequence of drawing or inverse labeling.
     (list
      (concat "^ \\{4\\}*\\(?:" drawingI "\\|" inverseLabelingIII "\\)")
      ;;       └────────┘
      ;;           │␢⇥

      '(1 'brecDividerFace nil t) ; `drawingI`
      '(2 'brecDividerFace nil t)             ; I,
      '(3 'brecDividerInverseLabelFace nil t) ; II and
      '(4 'brecDividerFace nil t)             ; III of `inverseLabelingIII`.

      ;; Thence it may include any mix of drawing, labeling and inverse labeling sequences.
      (list (concat drawingI "\\|" labelingI "\\|" inverseLabelingIII)
            '(brecChunkEndFromMidChunk) ; Before seeking to fontify these, ensure the search region
            nil                         ; extends far enough to include all of them. [PSE]
            '(1 'brecDividerFace nil t) ; `drawingI`
            '(2 'brecDividerLabelFace nil t) ; `labelingI`
            '(3 'brecDividerFace nil t)             ; I,
            '(4 'brecDividerInverseLabelFace nil t) ; II and
            '(5 'brecDividerFace nil t)))           ; III of `inverseLabelingIII`.


   ;;; ──  L a t e   f o n t i f i c a t i o n  ────────────────────────────────────────────────────────


     ;; ══════════
     ;; Commentary
     ;; ══════════
     ;; Commentary is delimited per line by one or more backslashes (\⋯) together isolated
     ;; in whitespace (␢).  Usually the delimiter is followed by content (C) too.
     (list "\\(?:^\\| \\)\\(\\\\+\\)\\( +.*\\)?$" ; [CIL, SPC]
           ;; └─────────┘  └───────┘  └──────┘
           ;;      ␢           \⋯         C

           '(1 'font-lock-comment-delimiter-face t) '(2 'font-lock-comment-face t t))
             ;;; `⋯face t`: Override any pre-applied face of a fontification chunk. [OCF]

     ;; Moreover where a line of pure commentary is delimited by two or more backslashes (\\⋯),
     ;; any content is taken to be a block label (L).
     (cons "^ *\\\\\\{2,\\}\\( +.+\\)$" '(1 'brecCommentBlockLabelFace t)))) ; [CIL, SPC]
       ;;;  └─┘└──────────┘  └──────┘   `⋯face t`: Override any pre-applied face. [OCF]
       ;;;   ␢     \\⋯           L


  "The value of `font-lock-keywords` for the search-based fontification of Breccian text.")



(defface brecPlainBulletFace
  `((default . (:inherit (brecBulletFace font-lock-keyword-face))))
  "The face for the bullet of a plain point, as opposed to a specialized type of point.")



(defconst brecPrincipalLeaderPattern ; Perfect indentation (│␢⇥),          [SPC]
  "^ \\{4\\}*\\\\*[^[:space:]\\]"   ; zero or more backslashes (\⋯)
  ;; ┈──────┘└───┘└────────────┘   ; and a character (C) that is neither
  ;;   │␢⇥     \⋯       C         ; whitespace nor a backslash.

  "The regexp pattern of the sequence marking the start of a fontification principal.")



;;;;;;;;;;;;;;;;;;;;

(provide 'Breccia_mode)


;; NOTES
;; ─────
;;   CIL  Commentary may appear within inverse labeling.  That nevertheless it appears there
;;        entirely in plain video, even its spaces must be fontified.
;;        http://reluk.ca/project/Breccia/documentation.task § divider
;;
;;   D ·· http://reluk.ca/project/Breccia/documentation.task § document
;;
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   FLE  Font lock extension.  The alternative to `font-lock-extend-region-functions`, namely the
;;        little used `font-lock-extend-after-change-region-function`, appears to be a design error.
;;        https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-03/msg00818.html
;;
;;   OCF  Overrides in comment fontification.  The fontification of a comment must override that
;;        of any containing fontification principal, such as a point descriptor, and must therefore
;;        follow it in `brecKeywords`.
;;
;;        We might have tried fontifying the commentary using the syntax system, which runs earlier.
;;        Mere syntax tabulation would have been inadequate here, unable to grasp the form of Breccian
;;        commentary; instead we could probably have relied on the macro `syntax-propertize-rules` to
;;        set syntax properties on the comment delimiters.  But then could the `subexp-highlighters` for
;;        the containing construct have worked around the comments, e.g. with `override` at nil?  [SBF]
;;
;;   PSE  `pre-form` search extension: extending the end boundary of the search region
;;        for multi-line anchoring.  The manual warns, ‘It is generally a bad idea to return a position
;;        greater than the end of the line’ [SBF].  But this appears to be a bug in the manual.
;;        https://stackoverflow.com/a/9456757/2402790
;;
;;   SBF  Search-based fontification.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html
;;
;;   SPC  Synchronized pattern of commentary.  Marking an instance of a pattern or anti-pattern,
;;        one of several that together are maintained in synchrony.


                                      ;;; Copyright © 2019 Michael Allan and contributors.  Licence MIT.
