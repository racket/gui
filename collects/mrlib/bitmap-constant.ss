(module bitmap-constant mzscheme
  (require "include-bitmap.ss")

  (provide (rename include-bitmap bitmap-constant)
           (rename include-bitmap/relative-to bitmap-constant/relative-to)))