;;; rune-window-nav-hydra.el -*- lexical-binding: t; -*-

(defhydra +hydra/window-nav (:hint nil)
  "
Resize: _h_: left  _j_: down  _k_: up  _l_: right "
  ("h" hydra-move-splitter-left       )
  ("j" hydra-move-splitter-down       )
  ("k" hydra-move-splitter-up         )
  ("l" hydra-move-splitter-right      )
  ("q" nil                           ))

(provide 'rune-window-nav-hydra)
