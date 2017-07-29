(in-package :cl-user)

(ql:quickload (list :cl-who :hunchentoot :parenscript :local-time))

(defpackage :site
  (:use :cl :cl-who :hunchentoot :parenscript :local-time))

(in-package :site)

;; Globals, use them wisely
(defvar *games* '())

(defvar *time-format* '((:year 4) #\- (:month 2) #\- (:day 2) #\T
                             (:hour 2) #\: (:min 2) #\: (:sec 2)))

;; Dispatchers (for files)
(push (create-prefix-dispatcher
       "/retro-games.htm"
       'retro-games)
      *dispatch-table*)
(push (create-folder-dispatcher-and-handler
       "/images/"
       "/Users/kdb/personal-site/img/")
      *dispatch-table*)
(push (create-folder-dispatcher-and-handler
       "/styles/"
       "/Users/kdb/personal-site/styles/")
      *dispatch-table*)
(push (create-prefix-dispatcher
       "/factorial.htm"
       'factorial)
      *dispatch-table*)

;; Class
(defclass game ()
  ((name  :reader   name
          :initarg  :name)
   (votes :accessor votes
          :initform 0)))

(defmethod vote-for (user-selected-game)
  (incf (votes user-selected-game)))

(defmethod print-object ((object game) stream)
  "Makes our objects readable when printed out!"
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

;; Functions
(defun game-from-name (name)
  "Using the `find` function, we search for NAME in our list of games. We test
(:test) it by using the `string-equal` and we are using the name reader function
from our class to extract the name of the game."
  (find name *games* :test #'string-equal
        :key  #'name))

(defun game-stored? (game-name)
  "To be clearer on our intent, we use this small function to check to see if
`game-name` is in our DB."
  (game-from-name game-name))

;; List all names of games in alphabetical order (mapcar #'name (games #'name #'string<))
;; List all names of games in decending vote order (mapcar #'name (games #'votes #'>))
(defun games (based-on func)
  "Sort our games in desending order based on number of votes.
BASED-ON is a one of the class defined functions and FUNC is the function you
  wish to compare with."
  (sort (copy-list *games*) func :key based-on))

(defun add-game (name)
  "Unless the game of name NAME already exists in our DB, make said game of name
NAME and push it into our list of games."
  (unless (game-stored? name)
    (push (make-instance 'game :name name) *games*)))
(defvar *links* '(("Home" . "/") ("About Me" . "/about") ("Practice Page" . "/retro-games") ("Factorial" . "/factorial")))

;; Macros
(defmacro standard-page ((&key title script) &body body)
  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/styles/retro.css")
             ,(when script
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "nav"
                   (:ul
                    (dolist (link *links*)
                      (htm
                       (:li (:a :href (cdr link)
                                (fmt "~A" (car link))))))))
             (:div :id "header"
                   (:span :class "strapline"
                          (:b ,title)))
             ,@body
             (:div :id "footer"
                   (:address :class "email"
                             "Kevin \"The Nuclear\" Bloom"
                             (:a :href "mailto:kdb4@openmailbox.org"
                                 "&lt;kdb4@openmailbox.org&gt;"))
                   (:text :class "email"
                       (fmt "Last time modified: ~A"
                            (local-time:format-timestring ;needs fixed
                             nil
                             :format *time-format*)))
                   (:a :href "http://www.common-lisp.net"
                       (:img :src "/images/made-with-lisp-logo.jpg"
                             :alt "The Power of Lisp!"
                             :class "footerLogo"))
                   (:a :href "http://www.anybrowser.org/campaign/"
                       (:img :src "/images/any-browser.png"
                             :alt "Any Browser"
                             :class "footerLogo"))
                   (:a :href "http://weitz.de/hunchentoot/"
                       (:img :src "/images/hunchentoot.gif"
                             :alt "Served by Hunchentoot"
                             :class "footerLogo")))))))

;; Endpoints
(define-easy-handler (home :uri "/") ()
  (standard-page
      (:title "Kevin \"The Nuclear\" Bloom's Secret Lair!")
    (:p "This site is pretty empty right now... I'm in the process of moving in. ")
    (:label "Stuff to do:"
    (:ol
     (:li "HTTPS")
     (:li "Timestamp fix")
     (:li "Better home page")
     (:li "About me page")
     (:li "Projects page")
     (:li "Blog page")
     (:li "Resume?")))))

(define-easy-handler (about :uri "/about") ()
  (standard-page
      (:title "About Me")
    (:p "I'm just an Internet kid who's dad left him not once, not twelve, but a dozen times. Ask me anytime.")))




(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page
      (:title "Top Retro Games")
    (:h1 "Vote on your all time favourite retro games!")
    (:p "Missing a game? Make it available for votes "
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games #'votes #'>))
             (htm
              (:li (:a :href (format nil "vote?name=~a"
                                     (url-encode ; avoid injection attacks
                                      (name game))) "Vote!")
                   (fmt "~A with ~d votes" (escape-string (name game))
                        (votes game)))))))))


(define-easy-handler (vote :uri "/vote") (name)
  (when (game-stored? name)
    (vote-for (game-from-name name)))
  (redirect "/retro-games"))

(defvar *output* 0)

(defun factorial-calc (x)
  (if (= 1 x)
      1
      (* x (factorial-calc (1- x)))))

(define-easy-handler (factorial :uri "/factorial") ()
  (standard-page (:title "Take factorial")
    (:h1 "Take factorial")
    (:form :action "/factorial/calculate" :method "post" :id "factor"
           (:p "Input number" (:br)
               (:input :type "number" :name "x" :class "txt"))
           (:p (:input :type "submit" :value "Submit" :class "btn"))
           (:p "Output" (:br)
               (:textarea :cols "200" :rows "40" (fmt "~d" *output*))))))

(define-easy-handler (calc :uri "/factorial/calculate") (x)
  (let ((reasonable-x (parse-integer x)))
    (if (or
         (or (null x) (zerop (length x)))
         (> reasonable-x 50001))
        (setf *output* "To prevent exhaustion, I limit this to 50,000!")
        (setf *output* (factorial-calc reasonable-x))))
  (redirect "/factorial"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title
                  "Add a new game"
                  :script (ps
                            (defvar add-form nil)
                            (defun validate-game-name (evt)
                              (when (= (@ add-form name value) "")
                                (chain evt (prevent-default))
                                (alert "Please enter a name.")))

                            (defun init ()
                              (setf add-form (chain document (get-element-by-id "addform")))
                              (chain add-form (add-event-listener "submit"
                                                                  validate-game-name false)))

                            (setf (chain window onload) init)))
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))


(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))
