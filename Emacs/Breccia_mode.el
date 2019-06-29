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
;;   fontification surface
;;       A whole nodal surface, with one exception as follows.  In the case of a divider surface,
;;       each perfectly indented line outside of a comment block starts a new fontification surface,
;;       such that a multi-line divider surface, while it is a single nodal surface, may nevertheless
;;       comprise several contiguous fontification surfaces.
;;
;;
;; NOTES  (see at bottom)
;; ─────


(define-derived-mode breccia-mode text-mode
  "Breccia"
  "A major mode for editing Breccian text"
  (set 'font-lock-multiline t); For sake of aside and command-point bodies.
  (add-hook 'font-lock-extend-region-functions 'brecExtendSearch) ; [FLE]
 ;(set (make-local-variable 'font-lock-defaults)… )
 ;;; ‘It automatically becomes buffer-local when set.’ [FLB]
  (set 'font-lock-defaults '(brecKeywords)))



;; ═════════════════════════════════════════════════════════════════════════════════════════════════════
;; D e c l a r a t i o n s   i n   l e x i c a l   o r d e r
;; ═════════════════════════════════════════════════════════════════════════════════════════════════════


(defface brecAsideBulletFace
  `((default . (:inherit (brecBulletFace brecAsidePointFace))))
  "The face for the bullet of an aside point.")



(defface brecAsidePointFace
  `((default . (:inherit font-lock-doc-face)))
  "The face for the descriptor of an aside point.")



(defface brecBulletFace
  `((default . (:inherit bold)))
  "The face for the bullet of a point.")



(defun brecSurfaceEndFromMid()
  "Returns the end position of the present fontification surface, provided that point is *not*
at the beginning of the surface.  If point is at the beginning, then the result is undefined."
  (save-excursion
    (if (re-search-forward brecPrincipalLeaderPattern nil t); Cf. `brecExtendSearchDown`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (point)))



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



(defface brecExceptionBulletFace
  `((default . (:inherit (brecBulletFace font-lock-warning-face))))
  "The face for the bullet body of an exception point.")



(defface brecExceptionBulletTerminatorFace
  `((default . (:inherit brecExceptionBulletFace))
    (t :weight normal))
  "The face for the bullet terminator ‘!!’ of an exception point.")



(defun brecExtendSearch()
  "Ensures that the font-lock search region extends to cover the whole of its fontification surfaces,
bisecting none of them.  Returns nil if already it does, non-nil otherwise."
  (save-excursion
    (let ((is-changed (brecExtendSearchUp)))
      (or (brecExtendSearchDown) is-changed))))



(defun brecExtendSearchDown()
  "Ensures that `font-lock-end` bisects no fontification surface, moving it forward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-end)
  (when (not (or (bolp)(eolp))) ; When the prior extenders such as `font-lock-extend-region-wholelines`
    ;; do not leave `font-lock-end` at a line terminus, as usually they do, then the search
    ;; region bisects the text of the line, which means the text of a fontification surface
    ;; (a Breccian document contains nothing else), and each surface covers the whole of its lines.
    (end-of-line)) ; Thus far at least the present surface must extend; extend it now,
                 ;;; that `re-search-forward` (below) must miss its leader.
  (let (is-changed)
    (if (re-search-forward brecPrincipalLeaderPattern nil t); Cf. `brecSurfaceEndFromMid`.
        (end-of-line 0); Moving to the end of the previous line.
      (goto-char (point-max)))
    (when (< font-lock-end (point))
      (set 'font-lock-end (point))
      (set 'is-changed t))
    is-changed))



(defun brecExtendSearchUp()
  "Ensures that `font-lock-beg` bisects no fontification surface, moving it backward in the buffer
as necessary.  Returns nil if no change was required, non-nil otherwise."
  (goto-char font-lock-beg)
  (end-of-line); That `re-search-backward` (below) finds any leader on the present line.
  (let (is-changed)
    (if (re-search-backward brecPrincipalLeaderPattern nil t)
        (beginning-of-line)
      (goto-char (point-min)))
    (when (> font-lock-beg (point))
      (set 'font-lock-beg (point))
      (set 'is-changed t))
    is-changed))



(defface brecForbiddenWhitespaceFace
  `((default . (:inherit (font-lock-warning-face)))
    (t :inverse-video t))
  "The face for disallowed, horizontal whitespace characters.")



(defface brecGenericBulletFace
  `((default . (:inherit (brecBulletFace font-lock-keyword-face))))
  "The face for the bullet of a generic point.")



(defface brecJointerBulletFace `()
  "The face for the bullet of a jointer.")



(defface brecJointerFace
  `((default . (:inherit brecCommandPointFace)))
  "The face for the reference part of a jointer.")



(defconst brecKeywords
  (let* ((drawingChar "[\u2500-\u2587\u2589-\u258F\u2591-\u259F]")
         (drawingI (concat "\\(" drawingChar "+\\(?: +" drawingChar "+\\)*\\)"))
           ;;; Capturing (I) a sequence of `drawingChar` inclusive of embedded spaces,
           ;;; yet exclusive of embedded newlines.

         (labelingChar "[^[:space:]\u2500-\u259F]"); Yet exclusive of whitespace.
         (labeling (concat labelingChar "+\\(?: +" labelingChar "+\\)*"))
           ;;; A sequence of `labelingChar` inclusive of embedded spaces,
           ;;; yet exclusive of embedded newlines.
         (labelingI (concat "\\(" labeling "\\)")); Capturing (I) an instance of `labeling`.

         (inversionMark "[\u2588\u2590]")
         (inverseLabelingIII
          (concat "\\(" inversionMark "\\)\\( *\\(?:" labeling " *\\)?\\)\\(\u2588\\)?")))
           ;;; Capturing (I) an inversion mark, (II) any `labeling` together with any space characters
           ;;; around it, and (III) any full block character.
    (list

     ;; ═══════════
     ;; Aside point
     ;; ═══════════
     ;; An aside point starts with a perfectly indented (│␢⇥) bullet comprising one slash (/).
     (list "^ \\{4\\}*\\(/\\)\\(?: +\\|$\\)"
           ;; ┈──────┘  └───┘
           ;;   │␢⇥       /

           '(1 'brecAsideBulletFace)
           (list                    ; Usually a descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)" ; extending thence to the end of the point.
            '(brecSurfaceEndFromMid); Before seeking to fontify it, bring in the whole of it. [PSE]
            nil '(1 'brecAsidePointFace)))


     ;; ══════
     ;; Bullet of a point type that has an unfontified descriptor
     ;; ══════
     (list
      (lambda( limit ); Seek the next such bullet.
        (catch 'result
          (while (re-search-forward; Start with a naive search, the best a regexp can do here.
                  (concat
                   "^ \\{4\\}*\\(\\\\*"; Perfectly indented (│␢⇥), the bullet starts with
                   ;; ┈──────┘   └───┘   zero or more backslashes (\⋯) and a character
                   ;;   │␢⇥        \⋯    that is neither whitespace nor a backslash.
                   ;;
                   "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]\\]\\)"

                   ;; Thence it continues.  It ends before a space that comes immediately after
                   ;; a non-alphanumeric character, or before a newline.
                   "\\(?:[[:alnum:]]+ *\\|[^[:alnum:][:space:]]+\\)*\\)")

                  limit t)
            (let ((m1Beg (match-beginning 1)); Now ensure the resulting (naive) match is correct.
                  (m1End (match-end 1))
                  m2Beg m2End m3Beg m3End m4Beg m4End m5Beg m5End isMatchChanged)

              (let ((end m1End)); Trim from the match any unwanted end boundary missed above.
                 ;; It is either a delimiter of inline commentary (regexp pattern ‘ +\\+’)
                 ;; or a sequence of trailing space at the line end (‘ +$’).  Trim it thus:
                (while (char-equal (char-before end) ?\\); For any trailing backslashes captured,
                  (set 'end (1- end)))                   ; scan backward past them.
                (while (char-equal (char-before end) ?\s); For any trailing space characters,
                  (set 'end (1- end))                    ; scan backward past them, and trim
                  (set 'm1End end)                       ; the whole from the captive group.
                  (set 'isMatchChanged t)))
              (when
                  (catch 'isMatched
                    (let ((charLast (char-before m1End)))
                      (when (char-equal ?+ charLast); If a task bullet is captured,
                        (set 'm2Beg m1Beg)          ; then recapture it as follows.
                        (if (= 1 (- m1End m1Beg))   ; If it comprises ‘+’ alone,
                            (set 'm2End m1End)      ; then recapture it as group 2.
                          (set 'm2End (- m1End 1))  ; Else it has a ‘body’, too.
                          (set 'm3Beg m2End)        ; Recapture its body as group 2
                          (set 'm3End m1End))       ; and its ‘+’ terminator as group 3.
                        (set 'm1Beg nil)
                        (set 'm1End nil)
                        (set 'isMatchChanged t)
                        (throw 'isMatched t))

                      (let ((length (- m1End m1Beg)))
                        (when (and (> length 1)     ; If an exception bullet is captured,
                                   (char-equal ?! charLast)
                                   (char-equal ?! (char-before (1- m1End))))
                          (set 'm4Beg m1Beg)        ; then recapture it as follows.
                          (if (= 2 (- m1End m1Beg)) ; If it comprises ‘!!’ alone,
                              (set 'm4End m1End)    ; then recapture it as group 4.
                            (set 'm4End (- m1End 2)); Else it has a ‘body’, too.
                            (set 'm5Beg m4End)      ; Recapture its body as group 4
                            (set 'm5End m1End))     ; and its ‘!!’ terminator as group 5.
                          (set 'm1Beg nil)
                          (set 'm1End nil)
                          (set 'isMatchChanged t)
                          (throw 'isMatched t))

                        (let ((charFirst (char-after m1Beg))); Abandon any unwanted match:
                          ;; either a non-bullet (division sequence), or a bullet of a point type
                          ;; with a fontified descriptor (aside, command or jointer), as follows.
                          (cond
                           ((= 1 length); If exactly one character is captured and it is
                            (when (or (char-equal ?/ charFirst) ; either an aside bullet,
                                      (char-equal ?: charFirst) ; command bullet
                                      (char-equal ?⋱ charFirst)); or jointer bullet,
                              (throw 'isMatched nil))); then abandon the match and continue seeking.

                           ((= 2 length); If exactly two characters are captured and each is
                            (when (and (char-equal ?⋱ charFirst); a ‘⋱’,  so that together they
                                       (char-equal ?⋱ charLast)); comprise a jointer bullet,
                              (throw 'isMatched nil)))); then abandon the match and continue seeking.

                          (when (and (>= charFirst ?\u2500) ; If a division mark leads the match,
                                     (<= charFirst ?\u259F)); then abandon it and continue seeking.
                            (throw 'isMatched nil)))))
                    t)
                (when isMatchChanged
                  (set-match-data
                   (append
                    (list (match-beginning 0) (match-end 0) m1Beg m1End
                          m2Beg m2End m3Beg m3End
                          m4Beg m4End m5Beg m5End)
                    (list (current-buffer)))))
                (throw 'result t))))
          (throw 'result nil)))
      '(1 'brecGenericBulletFace nil t)
      '(2 'brecTaskBulletFace nil t) '(3 'brecTaskBulletTerminatorFace nil t)
      '(4 'brecExceptionBulletFace nil t) '(5 'brecExceptionBulletTerminatorFace nil t))


     ;; ═════════════
     ;; Command point
     ;; ═════════════
     ;; A command point starts with a perfectly indented (│␢⇥) bullet comprising one colon (:).
     (list "^ \\{4\\}*\\(:\\)\\(?: +\\|$\\)"
           ;; ┈──────┘  └───┘
           ;;   │␢⇥       :

           '(1 'brecCommandBulletFace)
           (list                    ; Usually a command descriptor follows the bullet,
            "\\(\\(?:.\\|\n\\)+\\)" ; extending thence to the end of the point.
            '(brecSurfaceEndFromMid); Before seeking to fontify it, bring in the whole of it. [PSE]
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
      '(2 'brecDividerFace nil t)            ; I,
      '(3 'brecDividerInverseLabelFace nil t); II and
      '(4 'brecDividerFace nil t)            ; III of `inverseLabelingIII`.

      ;; Thence it may include any mix of drawing, labeling and inverse labeling sequences.
      (list (concat drawingI "\\|" labelingI "\\|" inverseLabelingIII)
            '(brecSurfaceEndFromMid); Before seeking to fontify these, ensure the search region
            nil                     ; extends far enough to include all of them. [PSE]
            '(1 'brecDividerFace nil t); `drawingI`
            '(2 'brecDividerLabelFace nil t) ; `labelingI`
            '(3 'brecDividerFace nil t)            ; I,
            '(4 'brecDividerInverseLabelFace nil t); II and
            '(5 'brecDividerFace nil t)))          ; III of `inverseLabelingIII`.


     ;; ═══════
     ;; Jointer
     ;; ═══════
     ;; A jointer starts with a perfectly indented (│␢⇥) bullet comprising one or two
     ;; diagonal ellipses (⋱⋯).
     (list "^ \\{4\\}*\\(⋱⋱?\\)\\(?: +\\|$\\)"
           ;; ┈──────┘  └─────┘
           ;;   │␢⇥        ⋱⋯

           '(1 'brecJointerBulletFace)
           (list                    ; A reference may follow the bullet,
            "\\(\\(?:.\\|\n\\)+\\)" ; extending thence to the end of the jointer.
            '(brecSurfaceEndFromMid); Before seeking to fontify it, bring in the whole of it. [PSE]
            nil '(1 'brecJointerFace)))


   ;;; ──  D e f e r r e d   f o n t i f i c a t i o n  ────────────────────────────────────────────────

     ;; ══════════
     ;; Commentary
     ;; ══════════
     ;; Commentary is delimited per line by one or more backslashes (\⋯) together isolated
     ;; in whitespace (␢).  Usually the delimiter is followed by content (C) too.
     (list "\\(?:^\\| \\)\\(\\\\+\\)\\( +.*\\)?$"; [CIL, SPC]
           ;; └─────────┘  └───────┘  └──────┘
           ;;      ␢           \⋯         C

           '(1 'font-lock-comment-delimiter-face t) '(2 'font-lock-comment-face t t))
             ;;; `⋯face t`: Override any pre-applied face of a fontification surface. [OCF]

     ;; Moreover where a line of pure commentary is delimited by two or more backslashes (\\⋯),
     ;; any content is taken to be a block label (L).
     (cons "^ *\\\\\\{2,\\}\\( +.+\\)$" '(1 'brecCommentBlockLabelFace t)); [CIL, SPC]
       ;;;  └─┘└──────────┘  └──────┘   `⋯face t`: Override any pre-applied face. [OCF]
       ;;;   ␢     \\⋯           L


     ;; ════════════════════
     ;; Forbidden whitespace
     ;; ════════════════════
     (cons "[\t\u00A0\u2000-\u200A\u202F\u205F\u3000]" '(0 'brecForbiddenWhitespaceFace t))))
       ;;;    9,   A0, 2000 - 200A, 202F, 205F, 3000


  "The value of `font-lock-keywords` for the search-based fontification of Breccian text.")



(defconst brecPrincipalLeaderPattern; Perfect indentation (│␢⇥),          [SPC]
  "^ \\{4\\}*\\\\*[^[:space:]\\]"  ; zero or more backslashes (\⋯)
  ;; ┈──────┘└───┘└────────────┘  ; and a character (C) that is neither
  ;;   │␢⇥     \⋯       C        ; whitespace nor a backslash.

  "The regexp pattern of the sequence marking the start of a fontification surface.")



(defface brecTaskBulletFace
  `((default . (:inherit (brecBulletFace font-lock-function-name-face))))
  "The face for the bullet body of a task point.")



(defface brecTaskBulletTerminatorFace
  `((default . (:inherit font-lock-comment-face)))
  "The face for the bullet terminator ‘!!’ of a task point.")



;;;;;;;;;;;;;;;;;;;;

(provide 'Breccia_mode)


;; NOTES
;; ─────
;;   CIL  Commentary may appear within inverse labeling.  That nevertheless it appears there
;;        entirely in plain video, even its spaces must be fontified.
;;        http://reluk.ca/project/Breccia/documentation.task § divider
;;
;;   FLB  Font lock basics.
;;        https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Basics.html
;;
;;   FLE  Font lock extension.  The alternative to `font-lock-extend-region-functions`, namely the
;;        little used `font-lock-extend-after-change-region-function`, appears to be a design error.
;;        https://lists.gnu.org/archive/html/bug-gnu-emacs/2015-03/msg00818.html
;;
;;   OCF  Overrides in comment fontification.  The fontification of a comment must override that
;;        of any containing fontification surface, such as a point descriptor, and must therefore
;;        follow it in `brecKeywords`.
;;
;;        We might have tried fontifying the commentary using the syntax system, which runs earlier.
;;        Mere syntax tabulation would have been inadequate here, unable to grasp the form of Breccian
;;        commentary; instead we could probably have relied on the macro `syntax-propertize-rules` to
;;        set syntax properties on the comment delimiters.  But then could the `subexp-highlighters` for
;;        the containing node have worked around the comments, e.g. with `override` at nil?  [SBF]
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
