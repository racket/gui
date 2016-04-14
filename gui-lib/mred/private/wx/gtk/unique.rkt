#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         racket/draw/unsafe/bstr
         net/base64
	 "../common/queue.rkt"
         "types.rkt"
         "utils.rkt")

(provide 
 (protect-out do-single-instance))

;; ----------------------------------------
;; Old-style -singleInstance support lith libunqiue

(define unique-lib-name "libunique-1.0")

(define unique-lib
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    (ffi-lib unique-lib-name '("0"))))

(define-ffi-definer define-unique unique-lib
  #:default-make-fail make-not-available)

(define _gsize _ulong)

(define UNIQUE_RESPONSE_OK 1)

(define _UniqueApp _GtkWidget) ; not a widget, but we want to connect a signal
(define _UniqueMessageData (_cpointer 'UniqueMessageData))

(define-unique unique_app_new (_fun _string _string -> _UniqueApp)
  #:fail (lambda () (lambda args
                      (unless unique-lib
                        (log-error "~s not found; single-instance mode disabled"
                                   unique-lib-name))
                      #f)))
(define-unique unique_app_add_command (_fun _UniqueApp _string _int -> _void))
(define-unique unique_app_is_running (_fun _UniqueApp -> _gboolean))
(define-unique unique_app_send_message (_fun _UniqueApp _int _UniqueMessageData -> _int))

(define-unique unique_message_data_new (_fun -> _UniqueMessageData))
(define-unique unique_message_data_free (_fun _UniqueMessageData -> _void))
(define-unique unique_message_data_set (_fun _UniqueMessageData _pointer _gsize -> _void))
(define-unique unique_message_data_get (_fun _UniqueMessageData (len : (_ptr o _gsize))
                                             -> (data : _bytes)
                                             -> (scheme_make_sized_byte_string
                                                 data
                                                 len
                                                 0)))

(define-signal-handler connect-message-received "message-received"
  (_fun _UniqueApp _int _UniqueMessageData _uint -> _int)
  (lambda (app cmd data time)
    (let ([d (unique_message_data_get data)])
      (with-handlers ([exn:fail? (lambda (exn)
				   (log-error 
				    (format "error handling single-instance message: ~s"
					    (exn-message exn))))])
	(let* ([p (open-input-bytes d)]
	       [vec (read p)])
	  (handle-argv vec))))
    UNIQUE_RESPONSE_OK))

(define (send-command-line app)
  (let ([msg (unique_message_data_new)]
        [b (let ([o (open-output-bytes)])
             (write (for/vector ([p (in-vector (current-command-line-arguments))])
                      (define cp (path->complete-path p))
                      (define s (path->string cp))
                      (if (equal? cp (string->path s))
                          s
                          ;; can't represent as string; use bytes
                          (path->bytes cp)))
                    o)
             (get-output-bytes o))])
    (unique_message_data_set msg b (bytes-length b))
    (unique_app_send_message app 42 msg)))

(define (do-single-instance/libunique)
  (let ([app (unique_app_new (build-app-name) #f)])
    (when app
      (unique_app_add_command app "startup" 42)
      (when (unique_app_is_running app)
        (when (= (send-command-line app)
                 UNIQUE_RESPONSE_OK)
          (exit 0)))
      (void (connect-message-received app)))))

;; ----------------------------------------
;; New-style -singleInstance support with Gtk

(define _GtkApplication _GtkWidget) ; (_cpointer/null 'GtkApplication)
(define _GApplicationCommandLine (_cpointer 'GApplicationCommandLine))

(define-gtk gtk_application_new (_fun _string _int -> _GtkApplication)
  #:fail (lambda () #f))

(define-gdk g_application_get_is_remote (_fun _GtkApplication -> _gboolean)
  #:make-fail make-not-available)
(define-gdk g_application_run (_fun _GtkApplication _int (_vector i _string) -> _gboolean)
  #:make-fail make-not-available)
(define-gdk g_application_command_line_get_arguments
  (_fun _GApplicationCommandLine (n : (_ptr o _int)) -> (p : _pointer) -> (values p n))
  #:make-fail make-not-available)
(define-gdk g_strfreev (_fun _pointer -> _void)
  #:make-fail make-not-available)

(define-signal-handler connect-activate "activate"
  (_fun _GtkApplication -> _void)
  (lambda (app)
    (void)))

(define-signal-handler connect-command-line "command-line"
  (_fun _GtkApplication _GApplicationCommandLine -> _void)
  (lambda (app cmdline)
    (define-values (args n) (g_application_command_line_get_arguments cmdline))
    (define argv (cast args _pointer (_vector o _string n)))
    (g_strfreev args)
    (handle-argv argv)))

(define APPLICATION_HANDLES_COMMAND_LINE 8)

(define (do-single-instance/gtk)
  (define app (gtk_application_new (build-app-name) APPLICATION_HANDLES_COMMAND_LINE))
  (when app
    (define args (for/vector ([i (current-command-line-arguments)])
		   (path->string (path->complete-path i))))
    (g_application_run app (vector-length args) args)
    (when (g_application_get_is_remote app)
      (exit 0))
    (connect-activate app)
    (connect-command-line app)))

;; ----------------------------------------

(define (do-single-instance)
  (if gtk_application_new
      (do-single-instance/gtk)
      (do-single-instance/libunique)))

(define (handle-argv vec)
  (for-each
   queue-file-event
   (map (lambda (s) (if (bytes? s)
			(bytes->path s)
			(string->path s)))
	(vector->list vec))))

(define-mz gethostname (_fun _pointer _long -> _int)
  #:fail (lambda () #f))

(define HOSTLEN 256)

(define (build-app-name)
  (let-values ([(path) (simplify-path
                        (path->complete-path
                         (or (find-executable-path (find-system-path 'run-file) #f)
                             (find-system-path 'run-file))
                         (current-directory)))]
               [(host) (or (and gethostname
                                (let ([b (make-bytes HOSTLEN)])
                                  (and (zero? (gethostname b HOSTLEN))
                                       (bytes->string/utf-8 (car (regexp-match #rx#"^[^\0]*" b)) #\?))))
                           "")])
    (string->bytes/utf-8
     (format "org.racket-lang.~a"
             (encode
              (format "~a~a~a" host path (version)))))))

(define (encode s)
  (regexp-replace* #rx"=|\r\n" (base64-encode (string->bytes/utf-8 s)) ""))
