
(module splash mzscheme
  (require (lib "class100.ss")
           (lib "class.ss")
	   (lib "mred.ss" "mred"))
  
  (provide get-splash-bitmap get-splash-canvas get-splash-eventspace get-dropped-files 
           start-splash shutdown-splash close-splash)
  
  (define splash-filename #f)
  (define splash-bitmap #f)
  (define splash-eventspace (make-eventspace))
  (define dropped-files null)
  
  (define (get-splash-bitmap) splash-bitmap)
  (define (get-splash-canvas) splash-canvas)
  (define (get-splash-eventspace) splash-eventspace)
  (define (get-dropped-files) dropped-files)
  
  (define (start-splash _splash-filename _splash-title width-default)
    (set! splash-title _splash-title)
    (set! splash-filename _splash-filename)
    (set! splash-max-width (max 1 (splash-get-resource (get-splash-width-resource) width-default)))
    (send gauge set-range splash-max-width)
    (send splash-frame set-label splash-title)
    (let/ec k
      (define (no-splash)
        (set! splash-bitmap #f)
        (set! splash-canvas #f)
        (set! splash-eventspace #f)
        (set! dropped-files null)
        (k (void)))
      
      (unless splash-filename
        (no-splash))
      (unless (file-exists? splash-filename)
        (fprintf (current-error-port) "WARNING: bitmap path ~s not found~n" splash-filename)
        (no-splash))
      
      (set! splash-bitmap (make-object bitmap% splash-filename))
      (unless (send splash-bitmap ok?)
        (fprintf (current-error-port) "WARNING: bad bitmap ~s~n" splash-filename)
        (no-splash))
      
      (send splash-canvas min-width (send splash-bitmap get-width))
      (send splash-canvas min-height (send splash-bitmap get-height))
      (send splash-frame center 'both)
      (send splash-frame show #t)
      (flush-display) (yield) (sleep)
      (flush-display) (yield) (sleep)))
  
  (define splash-title "no title")
  
  (define splash-current-width 0)
  
  (define (get-splash-width-resource) (format "~a-splash-max-width" splash-title))
  (define splash-max-width 1)
  
  (define (close-splash)
    (unless (= splash-max-width splash-current-width)
      (set-resource (get-splash-width-resource) (max 1 splash-current-width)))
    (set! quit-on-close? #f)
    (when splash-frame
      (send splash-frame show #f)))
  
  (define (shutdown-splash)
    (set! splash-load-handler (lambda (old-load f) (old-load f))))
   
  (define funny?
    '(let ([date (seconds->date (current-seconds))])
      (and (= (date-day date) 25)
           (= (date-month date) 12))))
  
   (define (splash-load-handler old-load f)
     (let ([finalf (splitup-path f)])
       (set! splash-current-width (+ splash-current-width 1))
       (when (<= splash-current-width splash-max-width)
         (send gauge set-value splash-current-width))
       (old-load f)))
  
  (current-load
   (let ([old-load (current-load)])
     (lambda (f)
       (splash-load-handler old-load f))))
  
  (define funny-gauge%
    (class100 canvas% (parent)
      (inherit get-dc min-width min-height stretchable-width stretchable-height)
      (private-field
       [funny-value 0]
       [funny-bitmap (make-object bitmap%
                       (build-path (collection-path "icons") "touch.bmp"))]
       [max-value 1])
      (public
        [set-range (lambda (r) (set! max-value r))]
        [set-value
         (lambda (new-value)
           (let* ([before-x
                   (floor (* (send funny-bitmap get-width) (/ funny-value max-value)))]
                  [after-x
                   (ceiling (* (send funny-bitmap get-width) (/ new-value max-value)))]
                  [width (- after-x before-x)])
             (send (get-dc) draw-line
                   (+ before-x 2) 0
                   (+ width 2) 0)
             (send (get-dc) draw-line
                   (+ before-x 2) (+ (send funny-bitmap get-height) 4)
                   (+ width 2) (+ (send funny-bitmap get-height) 4))
             (send (get-dc) draw-bitmap-section funny-bitmap
                   (+ 2 before-x) 2
                   before-x 0
                   width (send funny-bitmap get-height)))
           (set! funny-value new-value))])
      (override
        [on-paint
         (lambda ()
           (let ([dc (get-dc)])
             (send dc clear)
             (send dc draw-rectangle 0 0
                   (+ (send funny-bitmap get-width) 4)
                   (+ (send funny-bitmap get-height) 4))
             (send dc draw-bitmap-section funny-bitmap
                   2 2 0 0
                   (* (send funny-bitmap get-width) (/ funny-value max-value))
                   (send funny-bitmap get-height))))])
      (sequence
        (super-init parent)
        (min-width (+ (send funny-bitmap get-width) 4))
        (min-height (+ (send funny-bitmap get-height) 4))
        (stretchable-width #f)
        (stretchable-height #f))))
  
  (define (splash-get-resource name default)
    (let ([b (box 0)])
      (if (get-resource "mred" name b #f)
          (unbox b)
          default)))
  (define (set-resource name value)
    (write-resource "mred" name value (find-graphical-system-path 'setup-file)))
  
  (define (splitup-path f)
    (let*-values ([(absf) (if (relative-path? f)
                              (build-path (current-directory) f)
                              f)]
                  [(base name _1) (split-path absf)])
      
      (if base
          (let-values ([(base2 name2 _2) (split-path base)])
            (if base2
                (let-values ([(base3 name3 _2) (split-path base2)])
                  (build-path name3 name2 name))
                (build-path name2 name)))
          name)))
  
  (define quit-on-close? #t)
  
  (define splash-frame%
    (class frame%
      (override on-drop-file on-close)
      (define (on-drop-file filename)
        (set! dropped-files (cons filename dropped-files)))
      (define (on-close)
        (when quit-on-close?
          (exit)))
      (super-instantiate ())))
  
  (define splash-canvas%
    (class100 canvas% args
      (inherit get-dc)
      (override
        [on-paint
         (lambda ()
           (if splash-bitmap
               (send (get-dc) draw-bitmap splash-bitmap 0 0)
               (send (get-dc) clear)))])
      (sequence
        (apply super-init args))))
  
  (define splash-frame
    (parameterize ([current-eventspace splash-eventspace])
      (instantiate splash-frame% ()
        (label splash-title)
        (style '(no-resize-border)))))
  (send splash-frame set-alignment 'center 'center)
  (send splash-frame accept-drop-files #t)
  
  (define panel (make-object vertical-pane% splash-frame))
  (define splash-canvas (make-object splash-canvas% panel))
  (define h-panel (make-object horizontal-pane% panel))
  (define gauge
    (if funny?
        (make-object funny-gauge% h-panel)
        (make-object gauge% #f splash-max-width h-panel '(horizontal))))
  (send panel stretchable-width #f)
  (send panel stretchable-height #f)
  (send h-panel set-alignment 'center 'top)
  (send splash-canvas stretchable-width #f)
  (send splash-canvas stretchable-height #f))
