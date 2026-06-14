;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; STYLE.LISP --- CSS generation
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/glitch-demo)

;;; Font URLs -- defparameters so they can be overridden for local hosting.

(defparameter *google-fonts-url*
  (concatenate 'string
               "https://fonts.googleapis.com/css2"
               "?family=Cousine:ital,wght@0,400;0,700;1,400;1,700"
               "&family=Saira+Extra+Condensed:wght@100;200;300;400;500;600;700;800;900"
               "&family=Saira+Semi+Condensed:wght@100;200;300;400;500;600;700;800;900"
               "&family=Saira:ital,wght@0,100..900;1,100..900"
               "&display=swap"))

(defparameter *dejavu-fonts-url*
  "https://fonts.cdnfonts.com/css/dejavu-sans?styles=253,247")

;;; CSS compiled at load time

(defparameter *app-css*
  (lass:compile-and-write
   '(body
     :background-color "#0a0803"
     :color "#ffb000"
     :font-family "'_Saira Extra Condensed','_Saira Semi Condensed','Cousine','Courier New'" courier monospace
     :margin 0
     :padding "40px 20px"
     :display flex
     :justify-content center
     :align-items center
     :min-height "80vh"
     :overflow-x hidden)

   '("body::before"
     :content " "
     :display block
     :position fixed
     :top 0 :left 0 :bottom 0 :right 0
     :background "linear-gradient(rgba(18, 16, 16, 0) 50%, rgba(0, 0, 0, 0.25) 50%), linear-gradient(90deg, rgba(255, 0, 0, 0.06), rgba(0, 255, 0, 0.02), rgba(0, 0, 255, 0.06))"
     :z-index 2
     :background-size "100% 3px, 3px 100%"
     :pointer-events none)

   '(.terminal
     :max-width 900px
     :width "100%"
     :border "1px solid #996a00"
     :padding 30px
     :box-shadow "0 0 15px rgba(255, 176, 0, 0.15)"
     :background-color "rgba(10, 8, 3, 0.9)"
     )

   '(h1
     :gap "0.4rem"
     :font-family "'Saira Extra Condensed','Saira Semi Condensed','Courier New'" courier monospace
     :z-index 5
     :text-shadow "0 0 2px rgba(255,176,0,0.55)"
     :font-size 2rem
     :margin "0 0 10px 0"
     :text-transform uppercase
     :font-weight 500     
     :border-bottom "1px dashed #996a00"
     :padding-bottom 10px)

   '("h1 a"
    :display "grid"
    :grid-template-columns "auto 1fr"
    :column-gap "0.6rem"     
     :_display "flex"
     :_align-items "flex-start"
     :color inherit
     :text-decoration none)

   '("h1 a:hover"
     :opacity 0.7)

   '(h2
     :font-size 1.3rem
     :font-family "'Saira','Saira Semi Condensed','Courier New'" courier monospace     
     :margin "20px 0 8px 0"
     :text-transform uppercase
     :letter-spacing 1px)
   
   '(.acronym
     :display "flex"
     :align-items "center"
     :line-height 0.9
     :font-family "'DejaVu Sans'" sans-serif
     :text-transform uppercase
     :font-weight 700
     :font-size "3.6rem"
     :color "#ffcc00"     
     :margin "0 10px 0 0"     
     :letter-spacing -11px)
   
   '(.wordmark
     :font-family "'Saira Extra Condensed','Saira Semi Condensed','Courier New'" courier monospace     
    :display "flex"
     :flex-direction "column"
     :justify-content "center"
     :line-height 0.79
     :letter-spacing 12px
     )
   '(.combine
     :letter-spacing 9px
     )
   
   '(.meta
     :font-family "'_Saira Extra Condensed','Saira Semi Condensed','Courier New'" courier monospace          
     :color "#996a00"
     :font-size 0.85rem
     :margin-bottom 30px
     :text-transform uppercase)

   '(p
     :line-height 1.6
     :margin-bottom 20px)

   '(.manifesto
     :border-left "3px solid #ffb000"
     :padding-left 15px
     :margin "10px 0"
     :color "#ffcc00"          
     :font-style italic)
   
   '(.authors
     :margin-left 30px
     :font-style regular
     :color "#996a00"     
     )
   '(".authors::before"
     :content "- "
     :color "#996a00")
   '(.projects-list
     :list-style none
     :padding 0
     :margin "30px 0")

   '(".projects-list li"
     :margin-bottom 12px)

   '(".projects-list a"
     :color "#ffb000"
     :_text-decoration none
     :font-weight bold)
   '("a"
     :color "#ffb000"
     :_text-decoration none
     :font-weight bold)   

   '(".projects-list a:hover"
     :background-color "#ffb000"
     :color "#0a0803")

   '(button
     :background transparent
     :border "1px solid #996a00"
     :color "#ffb000"
     :font-family inherit
     :font-size 0.85rem
     :padding "4px 12px"
     :cursor pointer
     :letter-spacing 1px
     :margin-bottom 20px)

   '("button:hover"
     :background-color "#ffb000"
     :color "#0a0803")
   
   '("button.on"
     :background-color "#ffb000"
     :color "#0a0803")

   '(".prompt::before"
     :content "CL-Λ-COMBINE> "
     :color "#996a00")

   '(.cursor
     :display inline-block
     :width 10px
     :height 1.2rem
     :background-color "#ffb000"
     :animation "blink 1s step-end infinite"
     :vertical-align middle
     :margin-left 4px)

   '(:keyframes blink
     (from :background-color transparent)
     (to   :background-color transparent)
     (50%  :background-color "#ffb000"))

   ;;; CRT-like effect.
   '(:keyframes glitch
     (0%   :transform "translate(0, 0)"
           :text-shadow "0 0 2px rgba(255,176,0,0.55)")
     (20%  :transform "translate(-2px, 0)"
           :text-shadow "2px 0 0 rgba(255,0,0,0.55), -2px 0 0 rgba(0,255,255,0.55)")
     (40%  :transform "translate(4px, 0)"
           :text-shadow "-1px 0 0 rgba(255,0,0,0.55), 1px 0 0 rgba(0,255,255,0.55)")
     (60%  :transform "translate(-4px, 0)"
           :text-shadow "1px 0 0 rgba(255,0,0,0.45), -1px 0 0 rgba(0,255,255,0.45)")
     (100% :transform "translate(0, 0)"
           :text-shadow "0 0 2px rgba(255,176,0,0.55)"))

   ;;   '("body:has(#play-btn.on) .terminal.glitching"
   '("body .terminal.glitching"   
     :animation "glitch 0.22s ease-out")))
