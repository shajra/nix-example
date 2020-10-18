;;; -*- lexical-binding: t; -*-


(when (featurep! :tools direnv)
  (error! "you can't use ':tools direnv' with ':tools emacs-direnv'"))
