;;; doom-sandstorm-theme.el --- Sandstorm theme -*- no-byte-compile: t; -*-

(require 'doom-themes)

(defgroup doom-sandstorm-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(def-doom-theme doom-sandstorm
  "A dark Sandstorm theme."

  ;; name        default   256       16
  ((bg         '("#334047" "#334047" "brightblack"  ))
   (bg-alt     '("#2F3B41" "#2F3B41" "brightblack"  ))
   (bg-alt-2   '("#2B383D" "#2B383D" "brightblack"  ))
   (base0      '("#3E4E57" "#1d2127" "brightblack"  ))
   (base1      '("#1d2127" "#2E3B40" "brightblack"  ))
   (base2      '("#272727" "#3D494F" "brightblack"  ))
   (base3      '("#515F66" "#4A575F" "brightblack"  ))
   (base4      '("#494952" "#3F3F3F" "brightblack"  ))
   (base5      '("#62686E" "#74969F" "white"        ))
   (base6      '("#757B80" "#2D353A" "brightblack"  ))
   (base7      '("#9ca0a4" "#8990A5" "white"        ))
   (base8      '("#faf4c6" "#DFDFDF" "white"        ))
   (fg         '("#c2c2b0" "#C2C5C8" "white"        ))
   (fg-alt     '("#5D656B" "#A0A3A8" "white"        ))

   (grey       '("#686858"                          ))
   (red        '("#aa4450" "#AD341B" "red"          ))
;  (orange     '("#ff9800" "#D39735" "brightred"    ))
   (orange     '("#D39735" "#D39735" "brightred"    ))
   (green      '("#87875f" "#6D9158" "green"        ))
   (green-br   '("#6D9158"                          ))
;  (green-br   '("#719611"                          ))
   (teal       '("#578F8F" "#8EBCBB" "brightgreen"  ))
   (dark-teal  '("#467373" "#8EBCBB" "brightgreen"  ))
   (yellow     '("#cc8800" "#CBC03F" "yellow"       ))
   (yellow-2   '("#CBC03F" "#CBC03F" "yellow"       ))
   (light-grey '("#D1D1BE" "#A0A3A8" "white"        ))
   (dark-grey  '("#212A2E" "#212A2E" "brightblack"  ))
   (dark-blue  '("#6688aa" "#5C748E" "blue"         ))
   (blue       '("#6783A0" "#6783A0" "blue"         ))
   (magenta    '("#8787AF" "#7979B4" "magenta"      ))
   (violet     '("#8181a6" "#8D73AD" "brightmagenta"))
   (cyan       '("#6C9BA8" "#6C9BA8" "brightcyan"   ))
   (dark-cyan  '("#528b8b" "#5090A3" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight                 light-grey            )
   (vertical-bar              dark-grey             )
   (selection                 base3                 )
   (builtin                   teal                  )
   (comments                  base7                 )
   (doc-comments              base7                 )
   (constants                 orange                )
   (functions                 magenta               )
   (keywords                  light-grey            )
   (methods                   teal                  )
   (operators                 green-br              )
   (type                      blue                  )
   (strings                   green                 )
   (variables                 violet                )
   (numbers                   light-grey            )
   (region                    base0                 )
   (error                     red                   )
   (warning                   yellow                )
   (success                   green                 )
   (vc-modified               orange                )
   (vc-added                  green                 )
   (vc-deleted                red                   )

   ;; custom categories
   (modeline-fg               nil                   )
   (modeline-fg-alt           base7                 )
   (modeline-bg               bg-alt                )
   (modeline-bg-l             bg-alt                )
   (modeline-bg-inactive      bg-alt-2              )
   (modeline-bg-inactive-l    bg-alt-2              ))

  ;; --- extra faces ------------------------
  (((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l)

   (cursor                            :background green                                      )
   (fringe                            :background nil        :foreground fg-alt              )
   (help-key-binding                  :background nil        :foreground nil    :bold bold   )
   (nav-flash-face                    :background base0      :foreground nil                 )
   (hl-line                           :background nil                                        )
   (treemacs-fringe-indicator-face    :background green                                      )
   (eros-result-overlay-face          :background nil        :foreground green  :box t       )
   (evil-mc-cursor-default-face       :background teal                                       )
   (bookmark-face                     :background nil                                        )
;  (Man-overstrike                    :background nil                                        )

   (Info-quoted                       :family "Hack"         :foreground fg     :bold bold   )
   (info-node                         :foreground fg         :bold nil          :underline t )
   (info-xref-visited                 :foreground violet     :bold bold         :underline t )
   (info-menu-star                    :foreground fg                                         )
   (dired-directory                   :bold bold             :foreground fg                  )
   (link-visited                      :foreground violet                                     )
   (mu4e-highlight-face               :foreground fg         :background nil    :underline t )

   (lsp-face-highlight-read           :background nil        :foreground fg     :underline t )
   (lsp-face-highlight-write          :background nil        :foreground yellow :underline t )
   (tooltip                           :background bg-alt     :foreground fg                  )
   (company-tooltip-selection         :background fg         :foreground bg-alt              )
   (company-tooltip-annotation        :foreground yellow                                     )
   (popup-tip-face                    :background bg-alt     :foreground fg                  )

   (swiper-line-face                  :background base3                                      )
   (swiper-match-face-3               :background green      :foreground fg                  )
   (swiper-match-face-2               :background green      :foreground fg                  )
   (swiper-match-face-1               :background green      :foreground fg                  )
   (swiper-background-match-face-3    :background green      :foreground fg                  )
   (swiper-background-match-face-2    :background green      :foreground fg                  )
   (swiper-background-match-face-1    :background green      :foreground fg                  )
   (evil-ex-lazy-highlight            :background green      :foreground fg                  )
   (ivy-minibuffer-match-face-2       :background nil        :foreground dark-blue           )
   (ivy-minibuffer-match-face-3       :background nil        :foreground magenta             )
   (ivy-cursor                        :background fg                                         )
   (shadow                                                   :foreground base7)
   (variable-pitch                    :background nil        :foreground fg :height 120      )
   (doom-modeline-info                :bold nil                                              )
   (doom-modeline-warning             :bold nil                                              )
   (doom-modeline-debug               :bold nil                                              )
   (doom-modeline-urgent              :bold nil              :foreground red                 )
   (doom-modeline-evil-emacs-state    :bold nil              :foreground red                 )
   (doom-modeline-evil-normal-state   :bold nil              :foreground green               )
   (doom-modeline-evil-motion-state   :bold nil                                              )
   (doom-modeline-evil-visual-state   :bold nil              :foreground yellow-2            )
   (doom-modeline-evil-insert-state   :bold nil              :foreground teal                )
   (doom-modeline-evil-replace-state  :bold nil              :foreground yellow              )
   (doom-modeline-evil-operator-state :bold nil                                              )
   (compilation-mode-line-fail        :bold nil              :foreground red                 )

   (org-block-end-line                :background nil        :foreground base3               )
   (org-block-begin-line              :background nil        :foreground base3               )
   (org-block                         :background nil                                        )
   (org-document-info-keyword         :foreground base3                                      )
   (org-document-title                :foreground base3                                      )
   (org-drawer                        :foreground base3                                      )
   (org-level-1                       :foreground nil        :bold bold                      )
   (org-level-2                       :foreground nil        :bold bold                      )
   (org-level-3                       :foreground nil        :bold bold                      )
   (org-level-4                       :foreground nil        :bold bold                      )
   (org-level-5                       :foreground nil        :bold bold                      )
   (org-level-6                       :foreground nil        :bold bold                      )
   (org-level-7                       :foreground nil        :bold bold                      )
   (org-level-8                       :foreground nil        :bold bold                      )
   (org-quote                         :background nil                                        )
   (org-agenda-date                   :foreground green      :bold bold                      )
   (org-agenda-date-today             :foreground fg         :bold bold                      )
   (org-agenda-date-weekend           :foreground light-grey :bold bold                      )
   (org-agenda-date-weekend           :foreground light-grey :bold bold                      )
   (org-habit-clear-face              :background dark-teal                                  )

   (magit-header-line                 :background bg-alt     :foreground fg     :box t       )
   (magit-diff-removed-highlight      :background nil        :foreground red                 )
   (magit-diff-refine-removed         :background nil        :foreground red                 )
   (magit-diff-added-highlight        :background nil        :foreground teal                )
   (magit-diff-refine-added           :background nil        :foreground teal                )
   (magit-diff-context                :background nil                                        )
   (magit-diff-removed                :background nil        :foreground red                 )
   (magit-diff-added                  :background nil        :foreground teal                )
   (diff-refine-removed               :background nil        :foreground red                 )
   (diff-refine-added                 :background nil        :foreground teal                )

   (corfu-bar                         :background green)
   (corfu-border                      :background base0)
   (corfu-background                  :background base0)
   (corfu-current                     :background base0 :bold bold)

   ((paren-face-match &override)      :foreground fg                    :background fg-alt)
   ((paren-face-mismatch &override)   :foreground (doom-darken red 0.4) :background cyan))
  ;; --- extra variables ---------------------
  ())

;;; doom-sandstorm-theme.el ends here
