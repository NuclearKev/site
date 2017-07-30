(in-package :cl-user)

(load "/home/pi/quicklisp/setup.lisp")

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
       "img/")
      *dispatch-table*)
(push (create-folder-dispatcher-and-handler
       "/styles/"
       "styles/")
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
                            (local-time:format-timestring
                             nil
                             (local-time:now)
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
    (:p "This site is pretty empty right now... I'm in the process of moving in. "
        (:br)
        "Parden the slowness, I'm running on a very old Raspberry Pi")
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
    (:p "I was homegrown in a small city around the center of the grand ol' state of PA. My childhod was pretty simple: I played with LEGOs, looked at bugs, and shot BB guns. Fast forward to high school, I was doing the same stuff just more mature: hacking with GNU/Linux, researching bugs, and shooting real guns! For most of my life, I wanted to be an "
	(:a :href "https://en.wikipedia.org/wiki/Entomology" "entomologist") ", but changed my mind senior year of high school because of my love for computers. I decided to attend the Pennsyvania College of Technology (PCT) for Electronics and Computer Technology (BEE). I graduated from PCT suma cum laude in 2017 with my bachelors in BEE with a minor in mathematics. My final overall GPA was 3.991. During my time at PCT, I made a lot of friends and learned a lot of things. Most importantly, I became more interested in "
	(:a :href "https://www.gnu.org/philosophy/free-sw.en.html" "Free Software") " which, in the end, guided me to where I am today. I'll talk about this more later though. While in college, I learned a lot (possible sarcasm) about analog circuits, digital design, and microcontrollers. We spent most of our time working with the "
	(:a :href "http://store.digilentinc.com/zybo-zynq-7000-arm-fpga-soc-trainer-board/" "Digilent ZYBO") " development board. The ZYBO is nonfree, thus, I don't reccomend it. However, check out "
	(:a :href "http://www.clifford.at/icestorm/" "Project IceStorm") "; it is a free toolchain for the Lattice ICE family of FPGAs. If you need a microcontroller, just use one of the copious AVR devices that can be programmed with totally Free Software (Arduino, Teensy, etc.). We mostly worked with VHDL for hardware design and C for software. We also did a small amount of work with C#, but who cares about C#? It was made by Microsoft, therefore, it sucks. :)")
    (:br)
    (:p "I got my first job at a cinema, when I was 16. I originally started in concessions, but was promoted to projectionist due to my computer knowledge. I was the head projectionist/technician for 4 years. In Feburary of 2017, I started working for a tech startup part time. Once I graduated in May, I started full time with them. I still help out the theater when I can. While I was in college, I also tutored students in math. Although this was probably my least favorite job, I still enjoyed it.")
    (:br)
    (:p "One job that never gets old is being a full time GNU/Linux user. I've been doing this since 2010, when I used my first GNU/Linux machine. It was " 
	(:a :href "https://www.ubuntu.com/desktop" "Ubuntu") " 10.04 of course! "
	(:a :href "https://www.gnome.org/" "GNOME") " 2.x was still a thing at that time. It ran on an old Gateway desktop which was named "
	(:i "Gertrude.") "She lasted me all the way until I graduated high school, although, she ran many distros over the years. Currently she isn't in use but still has ye 'ol "
	(:a :href "https://en.wikipedia.org/wiki/CrunchBang_Linux" "Crunchbang") " installed on her. I then upgraded to custom built desktop that consisted of a bunch of old parts from my dad. The only new piece in the thing was the motherboard and RAM. I still use this machine to this day! Round the same time, I was given a Late 2012 MacBook Pro for school. I now use this machine for work only. Around sophmore year of college, I purchased an old 2005 Dell Latitude for mobile hacking. The machine worked great until in 2016, the screen died on it. I still use the machine occasionally by plugging it into an external monitor. At some point I got a Raspberry Pi. I never used it for anything until this site. It's a first gen model B and it's very slow. My last piece of hardware is an ASUS C201 Chromebook. This is an interesting device because it can be ran with only Free Software. I currently use "
	(:a :href "https://github.com/dimkr/devsus" "Devsus") ", a "
	(:a :href "http://www.devuan.org/" "Devuan") " fork that uses only Free Software. It uses the "
	(:a :href "http://linux-libre.fsfla.org/" "Linux-libre") " deblob scripts to remove the blobs from ChromeOS kernel. Overall the machine sucks but it is nice for simple web browser on the go! If all you need is a little Emacs while you're at the coffee shop, it'll work too. Nothing too crazy though.")
    (:br)
    (:p "I have a few hobbies that I partake in: hacking, reading, going to the gym, bowling, shooting, and thinking. For hacking, I normally do stuff in a Lisp dialect (Common Lisp, Scheme, or Emacs Lisp). My favorite is Common Lisp, specifically SBCL. Also related to hacking is "
        (:a :href "http://dragora.org" "Dragora GNU/Linux") ", my favorite distro. There are many reason why it's my favorite, which I'll probably talk about some other time. I like to read stuff about hacking, Free Software, Lisp, etc. Very seldom do I read novels. My favorite book is " 
        (:i "The Catcher in the Rye") ". My friends and I go bowling every Wednesday night followed by some food. I love guns too! More about that somewhere else. ")))

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
  (cond
    ((zerop x)
     1)
    ((= 1 x)
     1)
    (t
     (* x (factorial-calc (1- x))))))

(define-easy-handler (factorial :uri "/factorial") ()
  (standard-page (:title "Take factorial")
    (:h1 "Take factorial")
    (:form :action "/factorial/calculate" :method "post" :id "factor"
           (:p "Input number" (:br)
               (:input :type "number" :name "x" :class "txt" :required t))
           (:p (:input :type "submit" :value "Submit" :class "btn"))
           (:p "Output" (:br)
               (:textarea :cols "200" :rows "40" (fmt "~d" *output*))))))

(define-easy-handler (calc :uri "/factorial/calculate") (x)
  (let ((reasonable-x (parse-integer x)))
    (if (or (null x)
            (zerop (length x))
            (< reasonable-x 0)
            (> reasonable-x 10001))        ;While on the slow pi, mabe 50,000 on the newer?
        (setf *output* "To prevent exhaustion, I've limited this to 10,000!")
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


(defun start-test-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defun start-server ()
  (start (make-instance 'easy-acceptor :port 80)))

