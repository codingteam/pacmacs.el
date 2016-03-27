(require 'pacmacs-xpm)
(require 'pacmacs-im-enum)

(let* ((sprite-sheet-path "sprites/Blinking-Terrified-Ghost.txt")
       (sprite-sheet (pacmacs--read-im-enum-image sprite-sheet-path)))
  (benchmark-run 10
    (pacmacs--palette-from-image sprite-sheet)))
