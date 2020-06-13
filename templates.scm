(define doctype "HTML PUBLIC \"ISO/IEC 15445:2000//DTD HyperText Markup Language//EN\"")

(define (main-template title page #!optional class)
  `((doctype ,doctype)
    (html
     (head (title ,title)
           (meta (@ (content "text/html; charset=UTF-8") (http-equiv "Content-Type")))
           (meta (@ (name "viewport") (content "width=device-width, initial-scale=1.0")))
           (link (@ (rel "icon") (href "/static/favicon.ico") (type "image/png")))
           (link (@ (href "/static/styles/mona.css") (rel "stylesheet") (type "text/css"))))
     ,(if (default-object? class)
	  `(body ,page)
	  `(body (@ (class "thread")) ,page)))))

(define (make-board-list)
  `((p (@ (class "boardlist")) ,@(list-intersperse
				  (map (lambda (board) (list 'a `(@ (href ,(string-append "/" board "/") )) board))
				       *board-list*)
				  " | "))))
(define (make-title board)
  `(h1 (a (@ (href ,(string-append "/" board "/"))) ,board)))

(define (make-menu board context)
  `((p (@ (class "nav"))
  ,@(cond ((equal? context "frontpage")
	 `((a (@ (href ,(make-abs-path board "list"))) "All threads")
	 " "
	  (a (@ (href "#newthread")) "New thread")))
	((equal? context "thread")
	 `((a (@ (href ,(make-abs-path board))) "back")))))))

(define (make-post-form board thread frontpage #!optional content flash)
  (let ((form
	 `((form (@ (action ,(make-abs-path board thread "post")) (method "post"))
		 (p

		  " "
		  (label (@ (for "agnomen")) "Name: ")
		  (input (@ (type "text") (name "agnomen")))
		  " "
		  (label (@ (for "inscriptio")) "E-mail: ")
		  (input (@ (type "text") (name "inscriptio")))
		  " "
		  (input (@ (type "submit") (value "POST"))))
		  (p
		  (textarea
		     (@ (name "epistula") (rows "8") (cols "78"))
		     ,(if (default-object? content)
			  ""
			  content))
		    (input (@ (type "hidden") (name "frontpage") (value ,frontpage)))
		    (input (@ (type "hidden") (name "ornamentum") (value ,(get-form-hash)))))

		 (fieldset (@ (class "comment"))
			   (legend "do not edit these")
			   (p
                            (input (@ (type "text") (name "name") (class "name") (size "11")))
                            (br)
                            (textarea (@ (name "message") (class "message")(rows "1") (cols "11")))))))))
    (if (default-object? flash)
        form
        (append-element form `((p (@ (class "flash")) ,flash))))))


(define (make-thread-form board #!optional headline content flash)
  (let ((form
	 `((h2 (@ (id "newthread")) "New Thread")
	   (form (@ (action ,(make-abs-path board "post")) (method "post"))
		 (p (@ (class "newthread"))
		  (label (@ (for "agnomen")) "Name: ")
		  (input (@ (type "text") (name "agnomen")))
		  " "
		  (label (@ (for "inscriptio")) "E-mail: ")
		  (input (@ (type "text") (name "inscriptio")))
		  " "
		  (input (@ (type "submit") (value "POST")))
		  (br)
		  (label (@ (for "titulus")) "Headline: ")
		  (br)
		  (input (@ (type "text") (name "titulus") (id "titulus") (size "78") (maxlength "78")
			      (value ,(if (default-object? headline)
					  ""
					  headline))))
		    (br)		    
		    (label (@ (for "epistula")) "Message: ")
		    (br)
		    (textarea (@ (name "epistula") (id "epistula") (rows "12") (cols "77"))
			      ,(if (default-object? content)
				   ""
				   content)))
		 (input (@ (type "hidden") (name "ornamentum") (value ,(get-form-hash))))

		 (fieldset (@ (class "comment"))
			   (legend "do not edit these")
			   (p (input (@ (type "text") (name "name") (class "name") (size "11")))
			      (br)
			      (textarea (@ (name "message") (class "message")(rows "1") (cols "11")))))))))
    (if (default-object? flash)
        form
        (append-element form `((p (@ (class "flash")) ,flash))))))

(define (checked? value query-string-list)
  (if (equal? value (lookup-def 'css query-string-list ""))
      `((checked "checked"))
      `()))

(define (preferences-view board query-string-list)
  `(,(make-board-list)
    ,(make-title board) ,(make-menu board "preferences")
    (hr)
    (h2 "Settings")
    (dl (dt (b "Style Sheets"))
	(dd
	 (p "The CSS below will be stored in the URL as a query string. "
	    "This BBS doesn't set HTTP cookies and won't remember you next time you visit. "
	    "So the only way to store this setting is to save that URL.")
	 (form (@  (action ,(make-abs-path board "preferences")) (method "get"))
	       (p
		(input ,(append `(@ (type "radio") (name "css") (id "default") (value "default"))
				(if (null? query-string-list)
				    `((checked "checked"))
	  			    (checked? "default" query-string-list))))
		(label (@ (for "default")) "default")
		(br)
		(input ,(append `(@ (type "radio") (name "css") (id "mona") (value "mona"))
				(checked? "mona" query-string-list)))
		(label (@ (for "mona")) "mona")
		(br)
		(input ,(append `(@ (type "radio") (name "css") (id "no") (value "no"))
				(checked? "no" query-string-list)))
		(label (@ (for "no")) "no")
		(br)
		(input (@ (type "submit") (value "SET!")))))))
	(hr)
	,footer))

(define (thread-view board thread posts headline filter-func)
  `(,(make-title board)
    ,(make-menu board "thread")
    (hr)
    ,(format-thread board thread posts headline filter-func "false")
    ,footer))

(define (format-thread board thread posts headline filter-func frontpage #!optional truncated)
  (let ((next-post-number (+ 1 (car (last posts)))))
    `((h2 (a (@ (href ,(make-abs-path board thread))) ,headline))
      (dl ,(if (default-object? truncated)
	       (filter-map (lambda (p) (and (filter-func p) (format-post board thread p))) posts)
	       (list (format-post board thread (car posts))
		     (add-stub (dec (caadr posts)) board thread)
		     (map (lambda (p) (format-post board thread p)) (cdr posts))))
	  (dt (a (@ (class "postnum")
		  (href ,(string-append "#t" thread "p" (number->string next-post-number)))
		  (id ,(string-append "t" thread "p" (number->string next-post-number)))) ,next-post-number))
	  (dd ,(make-post-form board thread frontpage)))
      (hr))))

(define (format-post board thread post)
  `((dt
     (a (@ (class "postnum")
	 (href ,(string-append "/" board "/" thread "/" (number->string (car post))))
	 (id ,(string-append "t" thread "p" (number->string (car post)))))
	,(car post))
	" "
	,(let* ((e-mail (lookup-def 'e-mail (cdr post) #f))
		(name (lookup-def 'name (cdr post) ""))
		(tripcode (lookup-def 'tripcode (cdr post) #f)))
	   (cond
	    ((and (equal? name "") tripcode e-mail)
	     `((small (a (@ (href ,(string-append "mailto:" e-mail))) ,(string-append "!" tripcode)))))
	    ((and (equal? name "") tripcode (not e-mail))
	     `((small ,(string-append "!" tripcode))))
	    ((and (equal? name "") (not tripcode) e-mail)
	     `((a (@ (href ,(string-append "mailto:" e-mail))) ,*name*)))
	    ((and (string>? name "") e-mail tripcode)
	     `((a (@ (href ,(string-append "mailto:" e-mail))) ,name)
	       (small ,(string-append "!" tripcode))))
	    ((and (string>? name "") e-mail (not tripcode))
	     `((a (@ (href ,(string-append "mailto:" e-mail))) ,name)))
	    ((and (string>? name "") (not e-mail) tripcode)
	     `((,name) (small ,(string-append "!" tripcode))))
	    ((and (string>? name "") (not e-mail) (not tripcode))
	     `((,name)))
	    (else *name*)))
	" "
	(samp ,(lookup-def 'date (cdr post))))
    (dd ,(lookup-def 'content (cdr post)))))

(define (add-stub n board thread)
  `((dt (a (@ (class "postnum")
	    (href ,(string-append "/" board "/" thread "#t" thread "p2"))
	    (id ,(string-append "t" thread "p" "2"))) 2)
	" … "
	,(if (> n 2)
	     `(a (@ (class "postnum")
		  (href ,(string-append "/" board "/" thread "#t" thread "p" (number->string n)))
		  (id ,(string-append "t" thread "p" (number->string n))))
                 ,(number->string n))
	     ""))
    (dd (p ""))))

(define (list-view board threads)
  `(,(make-board-list)
    ,(make-title board)
    (hr)
    (table (@ (summary "Thread list"))
	   (thead (tr (th "#") (th "headline") (th "posts") (th "last update")))
	   (tbody ,(output-table threads)))
    (hr)
    ,footer))

(define (output-table threads)
  (map (lambda (thread)
         `(tr (td ,(car thread))
              (td (a (@ (href ,(car (cadr thread)))) ,(lookup-def 'headline (cdr (cadr thread)))))
              (td ,(lookup-def 'messages (cdr (cadr thread) )))
              (td (samp ,(lookup-def 'date (cdr (cadr thread) ))))))
       (zip (iota (+ (length threads) 1) 1) threads)))

(define (live-threads board)
  (let* ((path (make-path *sexp* board "list"))
	 (threads (call-with-input-file path read))
	 (threadlist
	  (if (> (length threads) *live-threads*)
	      (take (zip (iota (+ (length threads) 1) 1) threads) 40)
	      (zip (iota (+ (length threads) 1) 1) threads))))
    `(p (@ (class "livethreads"))
	,(list-intersperse
	  (map (lambda (thread)
		 (let ((num (car thread)))
		   `(a (@ (href ,(if (< *frontpage-threads* num)
				     (make-path board (number->string (car (cadr thread))))
				     (string-append board "\#d" (number->string num)))))
		    ,(string-append (number->string num)
				   ": "
				   (lookup-def 'headline (cdr (cadr thread)))
				   " ("
				   (number->string (lookup-def 'messages (cdr (cadr thread))))
				   ")"))))
	       threadlist) " "))))
	 
(define (frontpage-view board threads)
  `(,(make-board-list)
    ,(make-title board)
    (hr)
    ,(live-threads board)
    ,(make-menu board "frontpage")
    (hr)
    ,(let ((count 0))
       (map
	(lambda (t)
	  (let ((thread (number->string (car t)))
		(posts (lookup-def 'posts (cdr t)))
		(headline (lookup-def 'headline (cdr t)))
		(truncated (lookup-def 'truncated (cdr t))))
	    (set! count (inc count))
	    (cons 
	     (make-jump-links count)
	     (if truncated
		 (format-thread board thread (cons (car posts) (take-right posts 5)) headline identity "true" #t)
		 (format-thread board thread posts headline identity "true")))))
	threads))
    ,(make-thread-form board)
    (hr)
    ,footer))

(define (make-jump-links count)
  `((pre (@ (class "jump"))
         (a (@ (id ,(string-append "d" (number->string count)))
               (href ,(if (= count 10)
                          "#d1"
                          (string-append "#d" (number->string (inc count))))))
            "▼")
         (raw "&nbsp;")
         (a (@ (id ,(string-append "u" (number->string count)))
               (href ,(if (= count 1)
                          "#u10"
                          (string-append "#u" (number->string (dec count))))))
            "▲"))))

(define footer
  '(p (@ (class "footer"))
      "bbs.scm + "
      (a (@ (href "https://www.gnu.org/software/mit-scheme/")) "MIT Scheme") " + " 
      (a (@ (href "https://mitpress.mit.edu/sites/default/files/sicp/index.html")) "SICP")
      " + Satori Mode"))
