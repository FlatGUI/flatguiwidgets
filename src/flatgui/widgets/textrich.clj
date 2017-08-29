; Copyright (c) 2015 Denys Lebediev and contributors. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file LICENSE at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Rich Text widget"
      :author "Denys Lebediev"}
flatgui.widgets.textrich
  (:require [flatgui.awt :as awt]
            [flatgui.base :as fg]
            [flatgui.widgets.component]
            [flatgui.widgets.scrollpanel]
            [flatgui.inputchannels.keyboard :as keyboard]
            [flatgui.inputchannels.mouse :as mouse]
            [flatgui.inputchannels.mousewheel :as mousewheel]
            [flatgui.inputchannels.timer :as timer]
            [flatgui.inputchannels.clipboard :as clipboard]
            [flatgui.inputchannels.awtbase :as inputbase]
            [flatgui.util.matrix :as m]
            [flatgui.comlogic :as fgc]
            [flatgui.widgets.textcommons :as textcommons])
  (:import [java.awt.event KeyEvent]
           (flatgui.core.engine.ui FGTransferable)))


(def default-style
  {:font nil
   :foreground :prime-6
   :background :prime-4})

(defn glyph [type data style]
  {:type type
   :data data
   :style style})

(defn char-glyph
  ([c style]
   (condp = c
     \space (glyph :whitespace nil style)
     \newline (glyph :linebreak nil style)
     (glyph :char c style)))
  ([c] (char-glyph c default-style)))

(def linebreak-glyph (char-glyph \newline))

(def whitespace-glyph (char-glyph \space))

(defn image-glyph [image-url size] {:type :image :data image-url :style {:size size}})

(defn video-glyph [video-url size] {:type :video :data video-url :style {:size size}})

(def empty-rendition
  {:glyphs []
   :lines nil
   :caret-line 0
   :caret-pos 0
   :selection-mark 0
   :caret-coords [0 0 0]
   :w 0
   :rendition nil})

(defmulti glyph-size (fn [g _interop] (:type g)))

(def empty-glyph-size {:w 0 :h 0})

(defn- text-size [interop text font]
  {:w (.getStringWidth interop text font)
   :h (.getFontHeight interop font)})

(defmethod glyph-size :char [g interop]
  (let [font (:font (:style g))
        text (str (:data g))]
    (text-size interop text font)))

(defmethod glyph-size :whitespace [g interop]
  (let [font (:font (:style g))]
    (text-size interop " " font)))

(defmethod glyph-size :linebreak [_g _interop] empty-glyph-size)

(defmethod glyph-size :test [g _interop] {:w (:w (:style g)) :h (:h (:style g))})

(defmethod glyph-size :image [g _interop] (:size (:style g)))

(defmethod glyph-size :video [g _interop] (:size (:style g)))

(def delimiters #{:whitespace :linebreak})

;; We assume that a string's width is the sum of its characters' widths (due to web impl limitations)
(defn line-size [glyphs line-start line-size interop]
  (loop [w 0
         h 0
         c 0]
    (if (< c line-size)
      (let [s (glyph-size (nth glyphs (+ line-start c)) interop)]
        (recur
          (+ w (:w s))
          (max h (:h s))
          (inc c)))
      {:w w :h h})))

;; line: [<start> <len> <h> <w>]
(defn wrap-lines [glyphs w interop]
  (let [g-count (count glyphs)]
    (loop [line-start 0
           lines []
           last-delim-index -1
           line-w-to-last-delim -1
           line-h-to-last-delim -1
           g-index 0
           line-h 0]
      (if (>= g-index g-count)
        (if (and (> g-index line-start) (not (delimiters (:type (nth glyphs line-start)))))
          (conj lines [line-start (- g-index line-start) line-h (:w (line-size glyphs line-start (- g-index line-start) interop))])
          lines)
        (let [g (nth glyphs g-index)
              is-delim (delimiters (:type g))
              current-size (line-size glyphs line-start (- g-index line-start) interop)
              current-len (:w current-size)
              current-h (:h current-size)
              g-line-h (max current-h line-h)]
          (if (and is-delim (or (>= current-len w) (= (:type g) :linebreak)))
            (let [step-back (and (> current-len w) (not= last-delim-index -1))
                  next-line-start (if step-back (inc last-delim-index) (inc g-index))]
              (recur
                next-line-start
                (if (delimiters (:type (nth glyphs line-start)))
                  lines
                  (conj lines [line-start
                               (if step-back (- last-delim-index line-start) (- g-index line-start))
                               (if step-back line-h-to-last-delim g-line-h)
                               (if step-back line-w-to-last-delim current-len)]))
                -1
                -1
                -1
                next-line-start
                0))
            (recur
              (if (and is-delim (= g-index line-start)) (inc g-index) line-start)
              lines
              (if is-delim g-index last-delim-index)
              (if is-delim current-len line-w-to-last-delim)
              (if is-delim g-line-h line-h-to-last-delim)
              (inc g-index)
              g-line-h)
            ))))))

(defn render-glyph [lr glyph]
  (let [last-primitive (last lr)]
    (if (#{:char :whitespace} (:type glyph))
      (let [append-to-last (and last-primitive (= :string (:type last-primitive)) (= (:style last-primitive) (:style glyph)))
            addition (if (= :char (:type glyph)) (str (:data glyph)) " ")]
        (if append-to-last
          (assoc-in lr [(dec (count lr)) :data] (str (:data last-primitive) addition))
          (conj lr {:type :string :data addition :style (:style glyph)})))
      (conj lr glyph))))

(defn render-lines [glyphs lines]
  (loop [l 0
         line-rendition []]
    (if (< l (count lines))
      (let [line (nth lines l)
            line-start (first line)
            line-len (second line)]
        (recur
          (inc l)
          (conj
            line-rendition
            (loop [lr []
                   i 0]
              (if (< i line-len)
                (let [g-index (+ line-start i)
                      g (nth glyphs g-index)]
                  (recur
                    (render-glyph lr g)
                    (inc i)))
                {:h (nth line 2) :primitives lr})))))
      line-rendition)))

(defn- jump-to-line [lines old-caret-pos old-caret-line jump-fn]
  (let [new-line-index (max (min (jump-fn old-caret-line) (dec (count lines))) 0)
        new-line (nth lines new-line-index)
        caret-line-pos (- old-caret-pos (first (nth lines old-caret-line)))
        new-caret-line-len (second (nth lines new-line-index))]
    (+ (first new-line) (min caret-line-pos new-caret-line-len))))

(defn dec-caretpos [lines old-caret-pos old-caret-line]
  (let [new-pos (dec old-caret-pos)]
    (if (< new-pos (first (nth lines old-caret-line)))
      (if (pos? old-caret-line)
        (+
          (first (nth lines (dec old-caret-line)))
          (second (nth lines (dec old-caret-line))))
        (first (nth lines old-caret-line)))
      new-pos)))

(defn inc-caretpos [lines old-caret-pos old-caret-line]
  (let [new-pos (inc old-caret-pos)]
    (if (> new-pos (+ (first (nth lines old-caret-line)) (second (nth lines old-caret-line))))
      (if (< old-caret-line (dec (count lines)))
        (first (nth lines (inc old-caret-line)))
        (second (nth lines old-caret-line)))
      new-pos)))

(defn inline-caretpos [lines caret-pos caret-line]
  (if (not (empty? lines))
    (let [line-start (first (nth lines caret-line))
          line-end (+ (first (nth lines caret-line)) (second (nth lines caret-line)))]
      (cond
        (< caret-pos line-start) line-start
        (> caret-pos line-end) line-end
        :else caret-pos))
    caret-pos))

(defn find-next-page [lines old-caret-line clip-h dir-fn]
  (loop [dh 0
         l old-caret-line]
    (let [in-range (and (>= l 0) (< l (count lines)))
          lh (if in-range (nth (nth lines l) 2))]
      (if (and in-range (< (+ dh lh) clip-h))
        (recur
          (+ dh lh)
          (dir-fn l 1))
        l))))

(fg/defaccessorfn evolve-caretpos [component lines old-caret-pos old-caret-line old-selection-mark old-glyph-count input-glyphs]
  (cond

    (pos? (count input-glyphs))
    (let [;; min here to take into account possible selection that is to be replaced with supplied text
          new-caret-pos (+ (min old-selection-mark old-caret-pos) (count input-glyphs))
          input-linebreak-count (count (filter (fn [g] (= (:type g) :linebreak)) input-glyphs))]
      (inline-caretpos lines new-caret-pos (min (+ old-caret-line input-linebreak-count) (dec (count lines)))))

    (keyboard/key-pressed? component)
    (let [key (keyboard/get-key component)]
      (condp = key
        KeyEvent/VK_LEFT (dec-caretpos lines old-caret-pos old-caret-line)
        KeyEvent/VK_BACK_SPACE (if (and (< old-caret-line (count lines)) (> old-caret-pos (first (nth lines old-caret-line))))
                                 (dec-caretpos lines old-caret-pos old-caret-line)
                                 (inline-caretpos lines (dec old-caret-pos) (dec old-caret-line)))
        KeyEvent/VK_RIGHT (inc-caretpos lines old-caret-pos old-caret-line)
        KeyEvent/VK_HOME (if (inputbase/with-ctrl? component)
                           (inline-caretpos lines 0 0)
                           (first (nth lines old-caret-line)))
        KeyEvent/VK_END (if (inputbase/with-ctrl? component)
                          (inline-caretpos lines old-glyph-count (dec (count lines)))
                          (+ (first (nth lines old-caret-line)) (second (nth lines old-caret-line))))
        KeyEvent/VK_UP (jump-to-line lines old-caret-pos old-caret-line (fn [l] (dec l)))
        KeyEvent/VK_DOWN (jump-to-line lines old-caret-pos old-caret-line (fn [l] (inc l)))
        KeyEvent/VK_PAGE_UP (let [ch (m/y (get-property [:this] :clip-size))]
                              (jump-to-line lines old-caret-pos old-caret-line (fn [l] (find-next-page lines l ch -))))
        KeyEvent/VK_PAGE_DOWN (let [ch (m/y (get-property [:this] :clip-size))]
                                (jump-to-line lines old-caret-pos old-caret-line (fn [l] (find-next-page lines l ch +))))
        (inline-caretpos lines old-caret-pos old-caret-line)))

    :else (inline-caretpos lines old-caret-pos old-caret-line)))

(fg/defaccessorfn calc-caret-line [caret-pos lines]
  (if (not (empty? lines))
    (loop [l 0]
      (if (< l (count lines))
        (let [line (nth lines l)
              line-start (first line)
              line-len (second line)]
          (if (and (>= caret-pos line-start) (<= caret-pos (+ line-start line-len)))
            l
            (recur (inc l))))
        (throw (IllegalStateException. (str "caret-pos=" caret-pos " is out of model")))))
    0))

(def empty-caret-coords [0 0 0])
;; Caret coords: [<x> <y> <h>]
(fg/defaccessorfn calc-caret-coords [glyphs caret-line caret-pos lines]
  (if (pos? (count lines))
    (loop [l 0
           y 0]
      (if (< l (count lines))
        (let [line (nth lines l)
              line-h (nth line 2)]
          (if (= l caret-line)
            (let [line-start (first line)
                  interop (get-property [:this] :interop)]
              [(:w (line-size glyphs line-start (- caret-pos line-start) interop)) y line-h])
            (recur
              (inc l)
              (+ y line-h))))
        (throw (IllegalStateException. (str "caret-line=" caret-line " is out of model")))))
    empty-caret-coords))

(fg/defaccessorfn input-data-reson? [component] (and (map? (get-reason)) (not (nil? (:type (get-reason))))))

(fg/defaccessorfn full-model-reinit [component old-model glyphs]
  (let [w (m/x (get-property [:this] :clip-size))
        interop (get-property component [:this] :interop)
        lines (wrap-lines glyphs w interop)
        rendition (render-lines glyphs lines)

        old-caret-pos (:caret-pos old-model)
        old-caret-line (:caret-line old-model)
        old-selection-mark (:selection-mark old-model)
        caret-pos (evolve-caretpos component lines old-caret-pos old-caret-line old-selection-mark (count glyphs) [])
        caret-line (calc-caret-line caret-pos lines)]
    {:glyphs glyphs
     :lines lines
     :caret-line caret-line
     :caret-pos caret-pos
     :selection-mark 0
     :caret-coords (calc-caret-coords glyphs caret-line caret-pos lines)
     :w w
     :rendition rendition}))

(fg/defaccessorfn glyphs-> [component old-model glyphs input-glyphs]
  (let [old-caret-pos (:caret-pos old-model)
        old-caret-line (:caret-line old-model)
        old-selection-mark (:selection-mark old-model)

        input-glyph-count (count input-glyphs)
        sstart (min old-caret-pos old-selection-mark)
        send (max old-selection-mark old-caret-pos)
        has-selection (not= sstart send)

        new-glyphs (cond

                     (and
                       (not has-selection)
                       (keyboard/key-pressed? component)
                       (= (keyboard/get-key component) KeyEvent/VK_BACK_SPACE))
                     (vec (concat (take (max 0 (dec sstart)) glyphs) (take-last (- (count glyphs) sstart) glyphs)))

                     (and
                       (not has-selection)
                       (keyboard/key-pressed? component)
                       (= (keyboard/get-key component) KeyEvent/VK_DELETE))
                     (vec (concat (take sstart glyphs) (take-last (- (count glyphs) sstart 1) glyphs)))

                     :else
                     (vec (concat (take sstart glyphs) input-glyphs (take-last (- (count glyphs) send) glyphs))))

        ;; TODO re-render starting from changed line, no need to re-render everything
        w (m/x (get-property [:this] :clip-size))
        interop (get-property component [:this] :interop)
        lines (wrap-lines new-glyphs w interop)
        rendition (render-lines new-glyphs lines)

        caret-pos (evolve-caretpos component lines old-caret-pos old-caret-line old-selection-mark (count glyphs) input-glyphs)
        selection-mark caret-pos
        caret-line (calc-caret-line caret-pos lines)
        caret-coords (calc-caret-coords new-glyphs caret-line caret-pos lines)]
    (assoc
      old-model
      :glyphs new-glyphs
      :lines lines
      :rendition rendition
      :caret-line caret-line
      :caret-pos caret-pos
      :selection-mark selection-mark
      :caret-coords caret-coords)))

(fg/defaccessorfn rendition-input-data-evolver [component old-rendition input-data]
  (condp = (:type input-data)
    :string (glyphs-> component old-rendition (:glyphs old-rendition) (map char-glyph (:data input-data)))
    :image (glyphs-> component old-rendition (:glyphs old-rendition) [(image-glyph (:data input-data) (:size input-data))])
    :video (glyphs-> component old-rendition (:glyphs old-rendition) [(video-glyph (:data input-data) (:size input-data))])))

(fg/defaccessorfn rendition-clipboard-paste-evolver [component old-rendition]
  (if-let [strdata (clipboard/get-plain-text component)]
    (rendition-input-data-evolver component old-rendition {:type :string :data strdata})
    (if-let [imagedata (clipboard/get-image component)]
      (let [media-server (get-property [:this] :media-server)]
        (if-let [image-file-name (.storeImage media-server (if-let [s (get-property [:this] :media-prefix)] s (name (:id component))) imagedata)]
          (rendition-input-data-evolver component old-rendition {:type :image
                                                                 :data image-file-name
                                                                 ;; TODO where to get unit size? interop?
                                                                 :size {:w (/ (.getWidth imagedata nil) 64.0) :h (/ (.getHeight imagedata nil) 64.0)}})
          old-rendition))
      old-rendition)))

(fg/defevolverfn :rendition
  (let [glyphs (:glyphs old-rendition)]
    (cond

      (nil? (:lines old-rendition))
      (full-model-reinit component old-rendition glyphs)

      (not= (:w old-rendition) (m/x (get-property [:this] :clip-size)))
      (full-model-reinit component old-rendition glyphs)  ;TODO optimize this one

      (keyboard/key-event? component)
      (let [enter (= (keyboard/get-key component) KeyEvent/VK_ENTER)]
        (if
          (keyboard/key-typed? component)
          (let [typed-text (textcommons/textfield-dflt-text-suplier component)]
            (glyphs-> component old-rendition (:glyphs old-rendition)
                      (if enter [linebreak-glyph] (map char-glyph typed-text))))
          (if (keyboard/key-pressed? component)
            (glyphs-> component old-rendition (:glyphs old-rendition) nil)
            old-rendition)))

      (input-data-reson? component)
      (rendition-input-data-evolver component old-rendition (get-reason))

      (clipboard/clipboard-paste? component)
      (rendition-clipboard-paste-evolver component old-rendition)

      :else old-rendition)))

(fg/defevolverfn :text
  (let [rendition (:rendition (get-property [:this] :rendition))]
    (apply str (mapcat (fn [r-line] (map #(if (= :string (:type %)) (:data %)) (:primitives r-line))) rendition))))

(fg/defevolverfn :content-size
  (let [cs (get-property component [:this] :clip-size)
        rendition (get-property [:this] :rendition)
        lines (:lines rendition)]
    (loop [l 0
           w 0
           h 0]
      (if (< l (count lines))
        (let [line (nth lines l)]
          (recur
            (inc l)
            (max w (nth line 3))
            (+ h (nth line 2))))
        (m/defpoint (max w (m/x cs)) (max h (m/y cs)))))))

(fg/defevolverfn :viewport-matrix
  (let [rendition (get-property [:this] :rendition)
        caret-coords (:caret-coords rendition)
        caret-x (nth caret-coords 0)
        caret-y (nth caret-coords 1)
        caret-h (nth caret-coords 2)
        clip-size (get-property [:this] :clip-size)
        content-size (get-property [:this] :content-size)
        vmx (- (m/mx-x old-viewport-matrix))
        vmy (- (m/mx-y old-viewport-matrix))]
    (if (not (and
               (>= caret-x vmx)
               (< caret-x (+ vmx (m/x clip-size)))
               (>= caret-y vmy)
               (< (+ caret-y caret-h) (+ vmy (m/y clip-size)))))
      (textcommons/keep-in-range
        (m/translation
          (cond
            (< caret-x vmx) (- caret-x)
            (>= caret-x (+ vmx (m/x clip-size))) (- (- caret-x (m/x clip-size)))
            :else (- vmx))
          (cond
            (< caret-y vmy) (- caret-y)
            (>= (+ caret-y caret-h) (m/y clip-size)) (- (- (+ caret-y caret-h) (m/y clip-size)))
            :else (- vmy)))
        clip-size
        content-size)
      old-viewport-matrix)))

(fg/defwidget "textrich"
              {
               :rendition empty-rendition
               :caret-visible true;false
               :->clipboard nil
               :first-visible-symbol 0
               :focusable true
               ;:cursor :text
               :skin-key [:textrich]
               :background :prime-4
               :foreground :prime-1
               :no-mouse-press-capturing true
               :editable true
               :evolvers {
                          :rendition rendition-evolver
                          :text text-evolver
                          :content-size content-size-evolver
                          :viewport-matrix viewport-matrix-evolver
                          ;:caret-visible caret-visible-evolver
                          ;:->clipboard ->clipboard-evolver
                          ;:cursor cursor-evolver
                          }}
              flatgui.widgets.component/component)