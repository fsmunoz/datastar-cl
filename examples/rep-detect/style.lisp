;;;; -*- Mode: LISP; fill-column: 80; coding: utf-8 -*-

;;;; STYLE.LISP --- LASS CSS generation
;;;;
;;;; Copyright (C) 2025, 2026 Frederico Muñoz / ΛↃ lambda combine
;;;;
;;;; This file is part of datastar-cl, the Common Lisp SDK for Datastar
;;;;
;;;; License: MIT

(in-package #:datastar-cl/rep-detect-demo)

;;; Design tokens (lifted from ~/src/lisp/lambda-combine/style.lisp)
;;; --bg      #0a0803   deep amber-black background
;;; --fg      #ffb000   primary amber
;;; --bright  #ffcc00   bright amber / headings
;;; --dim     #996a00   dim amber / borders
;;; --dimmer  #664d00   very dim amber / retired rows, redacted
;;; --scanln  rgba(0,0,0,.25)  CRT scanline overlay

(defparameter *google-fonts-url*
  (concatenate 'string
               "https://fonts.googleapis.com/css2"
               "?family=Cousine:ital,wght@0,400;0,700;1,400;1,700"
               "&family=Saira+Extra+Condensed:wght@400;500;700"
               "&family=Saira+Semi+Condensed:wght@400"
               "&display=swap")
  "Google Fonts URL for terminal and headline typefaces.")

(defparameter *dejavu-fonts-url*
  "https://fonts.cdnfonts.com/css/dejavu-sans?styles=253,247"
  "CDN URL for the DejaVu Sans font (acronym ligatures).")

(defparameter *app-css*
  (concatenate 'string
   (lass:compile-and-write

   ;;; Page chrome

   '(body
     :background "#0a0803"
     :color "#ffb000"
     :font-family "'Cousine','Courier New'" courier monospace
     :margin 0
     :padding "40px 20px"
     :display flex
     :justify-content center)

   '("body::before"
     :content "''"
     :position fixed
     :inset 0
     :background "linear-gradient(rgba(18,16,16,0) 50%,rgba(0,0,0,.25) 50%)"
     :background-size "100% 3px"
     :pointer-events none
     :z-index 100)

   '(.terminal
     :max-width 1000px
     :width "100%"
     :border "1px solid #996a00"
     :padding 30px
     :box-shadow "0 0 15px rgba(255,176,0,.15)")

   ;;; Typography

   '(h1
     :font-family "'Saira Extra Condensed','Saira Semi Condensed','Courier New'" courier monospace
     :display grid
     :grid-template-columns "auto 1fr"
     :column-gap ".6rem"
     :font-size 2rem
     :margin "0 0 5px"
     :text-transform uppercase
     :font-weight 500
     :border-bottom "1px dashed #996a00"
     :padding-bottom 10px)

   '(.acronym
     :font-family "'DejaVu Sans'" sans-serif
     :font-size 3.6rem
     :font-weight 700
     :color "#ffcc00"
     :line-height .9
     :letter-spacing -11px
     :margin-right 10px)

   '(.wordmark
     :font-family "'Saira Extra Condensed','Courier New'" courier monospace
     :display flex
     :flex-direction column
     :justify-content center
     :line-height .73
     :letter-spacing 12px)

   '(.combine
     :letter-spacing 9px)

   '(.meta
     :color "#a87d00"
     :font-size .85rem
     :text-transform uppercase
     :letter-spacing 1px
     :margin ".3em 0")

   '(h2
     :font-family "'Saira Extra Condensed','Courier New'" courier monospace
     :font-size 1.5rem
     :text-transform uppercase
     :letter-spacing 2px
     :margin "1em 0 .3em"
     :color "#ffcc00")

   ;;; Registry table

   '(table
     :border-collapse collapse
     :width "100%"
     :margin ".5em 0")

   ;; Scroll container for the registry table so it doesn't grow unbounded.
   ;; The sticky th rule keeps column headers visible while scrolling.
   ;; scrollbar-color / scrollbar-width are the Firefox/standard properties;
   ;; WebKit pseudo-elements are appended as raw CSS below (concatenate).
   '(.registry-scroll
     :max-height 340px
     :overflow-y auto
     :scrollbar-color "#664d00 #0a0803"
     :scrollbar-width thin)

   '(th
     :color "#ffcc00"
     :border-bottom "1px solid #996a00"
     :padding ".3em .6em"
     :text-align left
     :text-transform uppercase
     :letter-spacing 1px
     :position sticky
     :top 0
     :background "#0a0803"
     :z-index 1)

   '(td
     :padding ".2em .6em"
     :border-bottom "1px solid #1a1000")

   '(.at-large
     :color "#ffb000")

   '(".retired td"
     :color "#664d00")

   '(.stats
     :color "#f89d00"
     :font-size .85rem
     :text-transform uppercase
     :letter-spacing 1px
     :margin ".5em 0"
     :padding 1rem)

   ;;; Links

   '(a
     :color "#ffb000"
     :font-weight bold
     :text-decoration underline)

   '("a:hover"
     :background-color "#ffb000"
     :color "#0a0803"
     :text-decoration none)

   '("a:focus-visible"
     :outline "2px solid #ffb000"
     :outline-offset "2px")

   ;;; Form controls

   '(button
     :background transparent
     :border "1px solid #996a00"
     :color "#ffb000"
     :font-family inherit
     :font-size .65rem
     :padding "6px 12px"
     :cursor pointer
     :letter-spacing 1px
     :text-transform uppercase
     :margin-right ".3em")

   '("button:hover"
     :background "#ffb000"
     :color "#0a0803")

   '("input,select"
     :background transparent
     :border "1px solid #996a00"
     :color "#ffb000"
     :font-family inherit
     :font-size .85rem
     :padding "6px 8px"
     :margin-right ".3em")

   ;; Opt the select into the new customisable-select API (Chrome 135+).
   ;; Without this, the open dropdown popup is OS-rendered and ignores CSS.
   ;; border-radius: 0 overrides the default rounded corners the base style adds.
   '(select :appearance base-select :border-radius 0)

   '("input:focus,select:focus"
     :outline none
     :border-color "#ffb000")

   ;; The dropdown list renders its own popup surface; <option> needs a solid
   ;; dark background (the select's transparent bg doesn't propagate to it).
   '(option
     :background-color "#0a0803"
     :color "#ffb000")

   ;; Reverse-video the highlighted/selected option (amber on black), matching
   ;; button:hover. The box-shadow inset forces the fill in WebKit, which
   ;; otherwise ignores background-color on the active option.
   '("option:checked,option:hover"
     :background-color "#ffb000"
     :color "#0a0803"
     :box-shadow "0 0 10px 100px #ffb000 inset")

   ;;; Messaging

   '(.footer
     :margin-top 20px
     :padding-top 10px
     :border-top "1px dashed #996a00"
     :color "#a87d00"
     :font-size .72rem
     :letter-spacing 1px
     :text-transform uppercase)

   '(.msg
     :color "#ff4400"
     :min-height 1.2em
     :margin ".3em 0")

   '(.processing
     :color "#ffb000"
     :min-height 1.2em
     :margin ".3em 0"
     :letter-spacing 1px
     :animation "blink 1s steps(2) infinite")

   '(hr
     :border none
     :border-top "1px dashed #996a00"
     :margin "1em 0")

   '(:keyframes blink
     (50% :opacity .3))

   ;;; Radar / geo map

   '("svg#radar"
     :display block
     :width "100%"
     :border "1px solid #1a1000"
     :margin ".3em 0")

   ;; Per-kind colour inherited by child circles/text via currentColor
   '(.ping-spawn  :color "#00ffff")   ; cyan
   '(.ping-sight  :color "#ffb000")   ; amber
   '(.ping-retire :color "#ff00ff")   ; magenta

   ;; Dot: solid contact marker, fades out over 8 s
   '(.ping-dot
     :animation "ping-dot 8s ease-out forwards")

   ;; Ring: expands from contact and fades over 6 s
   '(.ping-ring
     :animation "ping-ring 6s ease-out forwards"
     :transform-box fill-box
     :transform-origin center)

   ;; Label: monospace city tag, outlasts the ring
   '(.ping-label
     :font-size 8px
     :font-family monospace
     :fill "#996a00"
     :animation "ping-dot 12s ease-out forwards")

   '(:keyframes ping-dot
     (0%   :opacity 1)
     (60%  :opacity .7)
     (100% :opacity 0))

   '(:keyframes ping-ring
     (from :opacity .8 :transform "scale(1)")
     (to   :opacity 0  :transform "scale(7)"))

   ;;; Action log

   '("#action-log"
     :font-size ".82rem"
     :color "#a87d00"
     :margin ".5em 0")

   '("#action-log ul"
     :list-style none
     :margin 0
     :padding 0)

   '("#action-log li"
     :padding ".15em 0"
     :border-bottom "1px solid #1a1000"
     :letter-spacing "0.5px")

   )
   "
.registry-scroll::-webkit-scrollbar { width: 6px; }
.registry-scroll::-webkit-scrollbar-track { background: #0a0803; }
.registry-scroll::-webkit-scrollbar-thumb { background: #664d00; }
.registry-scroll::-webkit-scrollbar-thumb:hover { background: #996a00; }
/* Customisable-select popup surface (Chrome 135+) */
select::picker(select) {
  appearance: base-select;
  background: #0a0803;
  border: 1px solid #996a00;
  border-radius: 0;
  color: #ffb000;
}
select::picker(select) option:hover {
  background-color: #ffb000;
  color: #0a0803;
}
")
  "Compiled CSS string for the rep-detect demo page.")
