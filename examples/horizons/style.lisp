;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; STYLE.LISP --- CSS generation using LASS
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package :datastar-cl/horizons-demo)

(defun generate-css ()
  "Generate CSS for the minimal Horizons visualization"
  (concatenate
   'string
   (lass:compile-and-write
    '(*
      :margin 0
      :padding 0
      :box-sizing border-box)
    
    '(body
      :background "#000"
      :color "#0f0"
      :font-family monospace
      :height 100vh
      :overflow hidden)
    
    '(a
      :color "#fff")
    '((:and a :visited)
      :color "#ddd")
    
    '(.container
      :display flex
      :flex-direction column
      :height 100vh
      :padding 20px
      :gap 20px)
    
    '(.main-view
      :flex 1
      :display flex
      :flex-direction column
      :gap 10px
      :min-height 0)
    
    '(.details-pane
      :flex 1
      :border "1px solid #0f0"
      :padding 20px
      :overflow-y auto
      :background "#001100"
      :white-space pre
      :scrollbar-width none
      :font-size 12px
      :max-height 50vh)
    
    '(.canvas-container
      :flex 1
      :position relative
      :border "1px solid #0f0"
      :overflow hidden
      :min-height 250px)
    
    '(.stream-overlay
      :position absolute
      :top 10px
      :left 40px
      :width 100%
      :height 100%
      :z-index 30
      :font-family monospace
      :font-size 0.75rem
      :color "rgba(0, 255, 0, 0.30)"
      :white-space pre-wrap
      :overflow hidden
      :pointer-events none)
    
    '(planets-canvas
      :position relative
      :z-index 1
      :display block
      :width 100%
      :height 100%)
    
    '(canvas
      :cursor pointer
      :display block)
    
    '(.stats
      :padding 10px
      :border "1px solid #0f0"
      :font-size 14px
      :flex-shrink 0
      (h2
       :margin 0 0 5px 0
       :text-transform uppercase))
    
    '(.highlight
      :color "#FFF")

    '(.details-pane
      (h2
       :color "#0ff"
       :margin-bottom 15px
       :font-size 20px)
      
      (h3
       :color "#0f0"
       :margin-top 15px
       :margin-bottom 10px
       :font-size 16px)
      
      (p
       :margin-bottom 8px
       :line-height 1.6
       :white-space pre)
      
      (.metric
       :color "#0ff"
       :font-weight bold))
    
    '(.no-selection
      :color "#666"
      :font-style italic)
    
    '(::-webkit-scrollbar
      :width 0px
      :background transparent))
   
   ;; Additional raw CSS for webkit scrollbar
""))
