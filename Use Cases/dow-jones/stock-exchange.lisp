(in-package :cells)

#|

The deal is this: explanations of chunks of code appear /below/ them.

Now here are Ron's functional requirements: process a stream of messages from an 
imagined source of financial data. Actually, Ron has an intermediate process
reading a real source and producing a somewhat-digested stream in Lisp-friendly
format. Sample:

(:date 5123 :weekday 3)
(:index ((AA 29.30 7.3894672) (AIG 53.30 7.3894672)(AXP 53.00 7.3894672)
(BA 59.87 7.3894672) (C 46.80 7.3894672) (CAT 87.58 7.3894672) (DD 47.74 7.3894672)
(DIS 26.25 7.3894672) (GE 36.10 7.3894672) (GM 27.77 7.3894672) (HD 36.75 7.3894672)
(HON 35.30 7.3894672) (HPQ 21.00 7.3894672) (IBM 76.47 7.3894672)
(INTC 23.75 7.3894672) (JNJ 68.73 7.3894672) (JPM 35.50 7.3894672) (KO 43.76 7.3894672)
(MCD 29.80 7.3894672) (MMM 76.76 7.3894672) (MO 65.99 7.3894672) (MRK 34.42 7.3894672)
(MSFT 25.36 7.3894672) (PFE 27.5 7.3894672) (PG 54.90 7.3894672) (SBC 23.8 7.3894672)
(UTX 100.96 7.3894672) (VZ 36.75 7.3894672) (WMT 48.40 7.3894672) (XOM 56.50 7.3894672)))
(:trade INTC    0.001932 :last  23.75)
(:trade MSFT    0.001932 :last  25.36)
(:trade INTC    0.011931 :last  23.75)
(:trade MSFT    0.011931 :last  25.36)
(:trade MSFT    0.041965 :last  25.32)
(:trade UTX     0.067027 :last 101.39)
...etc...

Date messages encode date as (+ (* (- year 2000) 1000) julian-days). Weekday is dicey,
so the tutorial deduces the Lisp weekday and stores that.

Index messages define which tickers are in the index and their weights.
Entries are: (ticker-symbol initial-price index-weight)

Trade messages are (ticker-symbol ticker-minute :LAST price)
Ticker-minute is time since open, in minutes. Negative indicates pre-open trading.

To get the ball rolling, we just want to print out each trade as received, with the 
addition of an indicator as to which way the price moved: -1, 0, or 1 for down, unchanged, or up.

For the index, we want to track the minute of the last trade affecting the index, the 
weighted index value, and the last move of each index entry.

|#
(defparameter *trc-trades* t)

#+test
(run-trading-day)

(defun run-trading-day ()
  (cell-reset)
  (let ((*trc-trades* nil)
        (t-day (make-be 'trading-day)))
    
    ;; - always call CELLS-RESET when starting a test run
    ;; - (make-be ...) -> (to-be (make-instance ...))
    ;; - TO-BE jumpstarts a Cells instance into the flow. (FN to-be)
    #+(or)
    (with-open-file (t-data (make-pathname
                                   :directory '(:absolute "0dev" "cells" "Use Cases" "dow-jones")
                                   :name "trades0504" :type "txt"))
            (with-metrics (nil t "run-trading-day")
              (loop for message = (read t-data nil :eof)
                  until (eq message :eof)
                  do (count-it :dow-message)
                    (setf (message t-day) message)))
            )
    
    (with-open-file (t-data (make-pathname
                             :directory '(:absolute "0dev" "cells" "Use Cases" "dow-jones")
                             :name "stock-exchange" :type "lisp"))
      (with-metrics (nil t "run-trading-day")
        (loop with in-data = nil
            do (if (not in-data)
                   (setf in-data (msg-start (read-line t-data nil :eof)))
                 (let ((message (read t-data nil :eof)))
                   (count-it :dow-message)
                   (if (eql (car message) :close)
                       (loop-finish)
                     (setf (message t-day) message)))))))

    (trc "index value = " (value (car (indexes t-day))))))

;; --- trading day ---------------------------------
;;

(defmodel trading-day ()
  ((message :initarg :message :accessor message
     :initform (c-in nil) ;; c-in -> c-input, how data enters a model (see FN c-input)
     :cell :ephemeral) ;; handling transient phenomena in a steady-state paradigm (FN ephemeral)
   
   (date :initarg :date :accessor date
     :initform (c? (or .cache ;; advanced trick using prior value (see FN date/.cache)
                     (when (eql :date (car (^message)))
                       (destructuring-bind (&key date weekday)
                           (^message)
                         (declare (ignore weekday)) ;; derive from date
                         (encode-julian-date (+ 2000 (floor date 1000)) (mod date 1000)))))))
   
   (weekday :initarg :weekday :accessor weekday
     :initform (c? (when (^date)
                     (multiple-value-bind (second minute hour date month year day daylight-p zone)
                         (decode-universal-time (^date))
                       (declare (ignorable second minute hour date month year daylight-p zone))
                       day))))

   ;; not much new here, but astute readers will wonder if this cell gets optimized away
   ;; when (^date) on its second evaluation uses its .cache and gets optimized away.
   ;;
   ;; yes. Just checked to be sure.
   
   (trade :cell :ephemeral :initarg :trade :accessor trade
     :initform (c? (when (eql :trade (car (^message)))
                     (message-to-trade (^message)))))
   ;;
   ;; nothing new here, but note that again we use the :ephemeral option
   ;;
   (indexes :initarg :indexes :accessor indexes
     :initform (c? (with-c-cache ('cons)
                     (when (eql :index (car (^message)))
                       (make-be 'index
                               :trading-day self
                               :index-def (second (^message)))))))
   (tickers :cell nil :reader tickers :initform (make-hash-table :rehash-size 50))
   ))


(def-c-output trade ((self trading-day) trade) ;; FN def-c-output
  (when trade ;; FN trade setf optimization
    (count-it :raw-trades)
    (push trade (trades (ensure-ticker self (trade-ticker-sym trade))))))

(defun trading-day-ticker (day sym)
  (gethash sym (tickers day)))

(defun (setf trading-day-ticker) (ticker day sym)
  (setf (gethash sym (tickers day)) ticker))

(defun ensure-ticker (trading-day ticker-sym &optional price minute)
  (or (trading-day-ticker trading-day ticker-sym)
    (setf (trading-day-ticker trading-day ticker-sym)
      (make-be 'ticker :ticker-sym ticker-sym
        :trades (c-in (when price
                        (list (make-trade :ticker-sym ticker-sym
                                :minute minute :price price))))))))

(defmodel ticker (model)
  ((ticker-sym :cell nil :initarg :ticker-sym :reader ticker-sym)
   (trades :initarg :trades :accessor trades :initform (c-in nil))
   (last-trade-info :reader last-trade-info
     :initform (c? (bwhen (trade (first (^trades)))
                     (bif (penult-trade (and (trade-price trade)
                                          (find-if 'trade-price (rest (^trades)))))
                       (let* ((last (trade-price trade))
                              (penult (trade-price penult-trade))
                              (move (cond
                                     ((< last penult) -1)
                                     ((= last penult) 0)
                                     (t 1))))
                         (values
                          (cons penult-trade move)
                          (if (zerop move) :no-propagate :propagate)))
                       (values (cons trade 0) :propagate)))))))

(defun last-trade (ticker)
  (car (last-trade-info ticker)))
(defun last-move (ticker)
  (cdr (last-trade-info ticker)))

(defun ticker-price (ticker)
  (bwhen (trade (last-trade ticker))
    (trade-price trade)))

(defun ticker-trade-minute (ticker)
  (bwhen (trade (last-trade ticker))
    (trade-minute trade)))

(def-c-output trades ((self ticker)) ;; FN trades def-c-output
    (when *trc-trades*
      (loop for trade in (set-difference new-value old-value)
            do (format t "~&at ~a min, ~a at ~a, change ~a"
                 (trade-minute trade) (ticker-sym self) (trade-price trade)
                 (or (last-move self) "")))))

;; --- index ---------------------------------------------------

(defmodel index ()
  ((index-def :cell nil :initarg :index-def :initform nil :accessor index-def)
   (trading-day :cell nil :initarg :trading-day :initform nil :accessor trading-day)
   (ticker-weights :initarg :ticker-weights :accessor ticker-weights
     :initform (c? (loop for (ticker-sym price weight) in (index-def self)
                       collecting (cons (ensure-ticker (trading-day self) ticker-sym price -60)
                                    ;; whoa, a mid-rule to-be! (FN ticker-weights rule)
                                    weight))))

   (state :reader state
     :initform (let ((moves (make-hash-table :size 50))) 
                 (c-formula (:lazy nil) ;; do not re-compute on every trade (see FN lazy)
                   (count-it :index-state-calc)
                   (clrhash moves) ;; Re-use OK since fresh cons triggers dataflow (FN state rule)
                   (let ((minutes (loop for (ticker . nil) in (ticker-weights self)
                                      maximizing (ticker-trade-minute ticker))))
                     (without-c-dependency ;; dependency on trade minute suffices (see FN without-c-dependency)
                      (loop for (ticker . weight) in (ticker-weights self)
                          summing (* weight (ticker-price ticker)) into value
                          do (setf (gethash (ticker-sym ticker) moves) (last-move ticker))
                          finally (return (list minutes value moves))))))))

   (value :reader value :initform (c? (second (^state))))
   ;;
   ;; allows dependency on just value, which will not change on unchanged trades (FN value cell)
   ))


(defun index-minutes (index) (first (state index)))
(defun index-moves (index) (third (state index)))
(defun index-ticker-sym-move (index ticker-sym) (gethash ticker-sym (index-moves index)))
(defun index-ticker-move (index ticker) (index-ticker-sym-move index (ticker-sym ticker)))

(def-c-output value ((self index))
  (when *trc-trades*
    (trc "index time:" (index-minutes self) :value new-value :was old-value)))

;;; --- trade ---------------------------------------------------------------------

(defstruct trade minute ticker-sym price)

(defun message-to-trade (message)
  (destructuring-bind (ticker-sym ticker-min &key last) (rest message)
    (make-trade
     :ticker-sym ticker-sym
     :minute ticker-min
     :price last)))

;;; --- utilities ---------------------------------------------------------

(defun encode-julian-date (year julian)
  (+ (encode-universal-time 0 0 0 1 1 year )
    (* (1- julian) 86400))) ;; seconds in a day

;; I am sorry, that is all there is to tell. So we have a mindless main loop and a few declarations
;; and somehow we get all the functionality desired. [OK, granted, this is a pretty simple
;; batch process which would not be too complicated in non-Cells form. In that regard, it
;; is a good tutorial use case but does not show off Cells very much.] Anyway...
;;
;; It occurs to me that the above notes do not convey how the damn thing works. So let us walk 
;; thru a hand-execution of the above sample data.
;;
;; (make-be 'trading-day) -> (to-be (make-instance 'trading-day))
;;
;; Each ruled Cell gets evaluated. Each Cell slot -- constant, input, or ruled -- is output.
;; So with trading-day:
;;
;;    message is input, and has no associated output function
;;
;;    date is evaluated:
;;;    (or .cache
;;;     (when (eql :date (car (^message)))
;;;       (destructuring-bind (&key date weekday)
;;;           (^message)
;;;         (declare (ignore weekday)) ;; derive from date
;;;         (encode-julian-date (+ 2000 (floor date 1000)) (mod date 1000)))))
;;    
;;      .cache is nil, but so is (message self). NIL is returned, there is no output.
;;      date now has a dependency on message.
;;
;;   weekday is evaluated
;;;               (c? (when (^date)
;;;                     (multiple-value-bind (second minute hour date month year day daylight-p zone)
;;;                         (decode-universal-time (^date))
;;;                       (declare (ignorable second minute hour date month year daylight-p zone))
;;;                       day))))
;;      date is nil, so weekday is NIL but has a dependency on date. No output is defined.
;;
;;   trade is evaluated
;;;              (c? (when (eql :trade (car (^message)))
;;;                     (message-to-trade (^message)))))
;;      message is NIL, so NIL is returned. trade now has a dependency on message. The output
;;      method on trade is invoked, but has no interest in NIL new values.
;;
;;   indexes is evaluated:
;;;              (with-c-cache ('cons)
;;;                     (when (eql :index (car (^message)))
;;;                       (make-be 'index
;;;                               :trading-day self
;;;                               :index-def (second (^message)))))))
;;      message is NIL, so NIL is returned, a dependency on message created. No output defined.
;; 
;; (setf (message t-day) <the :date message>)
;;
;;  Many rules are dispatched: date, trade, and indexes. Only date processes :date messages.
;;  it returns a converted date, and still has a dependency on message. Weekday has a dependency
;;  on date, so that rule gets dispatched. It returns a weekday calculated off the date, and
;;  keeps the dependency on that. Other rules return
;;  NIL, which is the same value they had before. Nothing else is done (and in this case, that 
;;  would only have been to call the output method on trade.
;;
;; (setf (message t-day) <the :index message>)
;;
;;  The date rule runs and returns its .cache without accessing any cell. The Cell internals
;;  optimize away the fact that date ever had a rule or any kind of cell. It sees weekday 
;;  was a dependent on date and nothing else, so it optimizes that away, too. Slots end up
;;  with the last values calculated, and now look to other rules as if they were constant
;;  all along.
;;
;;  The trade rule runs and comes up empty again. The indexes rule runs and adds a new
;;  index list to its current contents, which happens to be NIL.
;;
;;;;  make-be is called on the index instance. Each slot gets processed in turn in a
;;;;  fashion similar to that for trading-day. When the ticker-weights rule runs, ticker
;;;;  instances for each ticker in the index are created and passed to TO-BE, in the
;;;;  function ensure-ticker. No dependencies are created since index-def is not a Cell,
;;;;  so the ticker-weights cell gets optimized away.
;;;;
;;;;  as each ticker is created and processed by TO-BE:
;;;;;;; 
;;;;  the state rule is evaluated and computes an initial index state off the data
;;;;  provided in the index-def. state ends up with dependencies on each ticker in the
;;;;  index.
;;  [rest under construction]
;;

;;; =============================================================================
;;; Footnotes
;;; =============================================================================
;
;; --- FN to-be --------------------------------------
;;   TO-BE jumpstarts a Cells instance into the flow. Literally, as in
;;   the dataflow. It evaluates ruled slots to establish dependencies (those
;;   get established during evaluation) and in turn arrange for state change 
;;   within the model to propagate to the instance's ruled Cells. It also
;;   DEF-C-OUTPUTs all cell slots so the outside world is consistent
;;   with the model state. More on def-c-output below.
;
;; --- FN c-input ------------------------------------
;;
;; c-in is short for c-input, which simply means imperative application code
;; can SETF this slot. (Note that this is just the initform for this slot,
;; which can be overridden by subclasses or at make-instance time, and if
;; the override is not another C-IN or C-INPUT, then all bets are off. ie, The
;; SETF ability depends on the type of Cell (if any) associated at run-time
;; with the slot of an instance. It
;; is not an attribute of the slot as with the :cell slot option discussed just below.
;;
;; Anyway, C-IN lets us make a lot of points about Cells. 
;;
;; First, no model is
;; an island; the dataflow has to start somewhere. Just as a VisiCalc spreadsheet
;; has cells where you can type, say, different interest rates to see how that
;; effects the rest of a financial model, a Cell-based application model needs
;; some way to interface with the outside world, if only the mouse and keyboard
;; of a GUI application.
;;
;; The way we do that is by having conventional application code feed (SETF) data into
;; the dataflow model at what we call cell inputs. In a typical GUI app, this means
;; having callbacks registered with the window manager. The callbacks then take their
;; arguments (window events such as mouse-downs and key-presses) and setf that
;; info to slots of a window or system instance modelling the window or operating
;; system, slots mediated by c-input Cells.
;;
;; In this simple use case we have just one stream of external inputs (messages
;; from some financial data service) being SETFed into one slot, the message
;; slot of an instance of the trading-day class.
;; 
;; Second, the Cells design enforces discipline. So in case you are
;; wondering, no, if you do not bind a C-INPUT to a slot of an instance, you cannot
;; SETF that slot from imperative code. (Aside: (SETF SLOT-VALUE) /is/ a back door
;; allowing you to wreak havoc on your dataflow model if you so choose (but it will
;; wreak havoc).)
;;
;; Third, you might wonder why slots meant as inputs cannot just have no Cell at all
;; associated with them at run-time, and then have the Cell internals accept that
;; as a SETF-able state. Well, it is a long story, but it turns out that a lot of
;; Cells overhead can be avoided if we distinguish a slot whose value will never
;; change from an input slot which will be SETF'ed. A simple example of a constant
;; slot would be the bounding rectangle of a push button. Those values have to be
;; Cells because in other graphical elements sharing the same superclass, the bounding 
;; rectangle changes. A good example is the win32-style scroll bar thumb, which changes
;; size to reflect how much of the total file is visible. Anyway, it turns out that
;; a significant performance boost comes from having Cells which happen to access
;; a constant value not record a dependency on that value and, where a rule evaluation
;; turns out not to access any non-constant other Cell slot, likewise convert the ruled 
;; slot into a constant slot. Sorry you asked?
;;
;; --- FN ephemeral -----------------------------------------------------------
;;
;; Whoa, here is an advanced topic. Ephemeral means "fleeting". Before getting into 
;; that, the other options for the :cell option are T and NIL. T is the default.
;; NIL means you get a normal slot having nothing to do with Cells. Now about
;; that :ephemeral option: Messages are
;; like events: they happen, then they are no more. This is a problem for
;; Cells, which like a VisiCalc spreadsheet model (say, your household budget)
;; is all about steady-state occasionally perturbed by inputs. That is vague.
;; Here is a concrete example: suppose you have some game where the user has
;; to press a key when two randomly moving shapes overlap. You will have a hit rule 
;; that says (abbreviated somewhat):
;;
;;     (and (eql (event *sys*) :keypress) (shapes-overlap-p *sys*))
;;
;; OK, the key is pressed but the shapes do not overlap. No cigar. Now a few
;; seconds later the shapes do overlap. The key is not being pressed, but the 
;; EVENT slot of the *sys* instance (modelling the computer system) still
;; says :keypress. bad news. Obviously we need to process an event and then
;; clear the value before processing any other model input. Now perhaps we could
;; simply have imperative code which says:
;;
;;    (setf (event *sys*) :keypress)
;;    (setf (event *sys*) nil)
;;
;; But that is different. That suggests an application semantic in which the
;; EVENT slot changes from :keypress to NIL. It will trigger all the usual
;; dataflow, to see if the model should react. But in fact what we /really/
;; need is /not/ to clear the EVENT slot. What we really need is
;; ephemeral SETF behavior from a mechanism designed for steady-state.
;; We need the EVENT slot to take on a value just long enough to perturb our
;; model and then cease to be without fanfare.
;;
;; So we extend the Cells model with the :ephemeral option on a slot, and have
;; Cell internals watch out for that and silently clear the slot once a value
;; has been propagated to other Cells and output (again, outputs
;; are discussed below.)
;;
;; A final newbie note: watch the bouncing options. Ephemerality is a slot option,
;; not something one tailors to the instance. Think about it. Think about the
;; slot names. "message", "event". We want to get ephemeral behavior for these
;; slots no matter what cell (input or ruled) we choose to associate with them.
;; So it is more convenient and reliable to endow the slot itself with ephemerality.
;; in other cases we see different instances enjoying different Cell-ish qualities
;; for the same slot, sometimes constant, sometimes computed, sometimes being
;; SETFed by imperative code outside the dataflow model. These variations are
;; then found in the type of runtime Cell associated with the Cell slot.
;;
;; --- FN date/.cache --------------------------------------------------
;;
;;
;; There is a lot going on here, too, including some premature optimization.
;;
;; First of all, .cache is just a local variable, bound by the expansion
;; of the C? macro to the latest value calculated for this rule. It starts out as NIL, so
;; the rule next reads the message slot of the same trading-day instance. How so?
;;
;; ^message is a macro written by the defmodel macro. It expands simply to:
;;
;;     (message self)
;;
;; It used to expand to more, including vital Cell plumbing. Now I keep it around just
;; because I love that self-documenting quality. And yes, I have adopted the 
;; Smalltalk "self" convention over the C++ "this" convention. There is no need
;; to use these (^macros), just code (<slot-name> self) and you will establish a 
;; dependency on the message slot. What does dependency mean?
;;
;; Simply that the next time the message slot changes (the default test between old and 
;; new values is EQL, but can be overridden), the Cells engine will immediately kick
;; the DATE rule to see if it wants to compute a different value. 
;;
;; A very important point is that dependencies are established automatically simply
;; by invoking the reader or accessor associated with a slot, and that this happens
;; dynamically at run-time, not by inspection of code. A second point is that the
;; dependency is established even if the read takes place in a called function.
;; 
;; There is a backdoor. No dependencies are established in code wrapped by
;; the macro WITHOUT-C-DEPENDENCY.
;;
;; Another important point is that dependencies are re-decided completely each time
;; a rule is invoked. So this particular rule is an oddball: it will produce only one value, when a :date 
;; message is received
;; and teh first non-NIL value is returned. On the next message (of any kind) .cache will be
;; non-NIL and the rule will simply return that value.
;; During this last evaluation the cell will not access, hence no longer
;; depend on, the message slot or any other slot and it will get optimized away. This
;; improves performance, since the message slot no longer bothers propagating to 
;; the date slot and Cell internals no longer have to invoke the rule. Otherwise, every
;; new message for the entire day (none of which would be :date messages) would kick
;; off this rule.
;;
;; --- FN with-c-cache ------------------------------------
;;
;; I am actually doing something new here. The idea is that again we deviate
;; slightly from the spreadsheet paradigm and want to accumulate data
;; from a stream of ephemeral values. Normally we calculate a slot value in
;; its entirety from data at hand, even if only ephemerally. Here we want 
;; to add a newly computed result to a list of prior such results.
;;
;; with-c-cache will accept any two-argument function, and when the enclosed
;; form returns a non-nil value, pass that and the .cache to the specified
;; function.
;;
;; --- FN def-c-output --------------------------------------------
;;
;; Above is another optimization, and the long-awaited discussion of Cell
;; output.
;;
;; Output reinforces the "no model is an island" theme. We create
;; models to obtain interesting outputs from inputs, where the model
;; provides the interest. For a RoboCup player simulation, the inputs are
;; sensory information about the game, provided in a stream from a server
;; application managing multiple client players and coaches. The outputs are
;; messages to the server indicating player choices about turning, running,
;; and kicking. In between, the game play model is supposed to compute
;; actions producing more or less capable soccer play.
;;
;; --- FN trade setf optimization ---------------------------------------
;
;; But this is strange "output". It actually changes internal model state.
;; It is no output at all, just feeding dataflow back into a different
;; model input. Whassup?
;;
;; Like I said, it is an optimization. A ticker instance could have a
;; rule which watched the message stream looking for trades on that ticker,
;; but then every ticker would be watching the message stream.
;;
;; Instead, we simply leverage an "output" method to procedurally decide which
;; ticker has been traded and directly add the trade to that ticker's list
;; of trades.
;;
;; --- FN trades def-c-output --------------------------------------
;;
;; Now the above is a proper output. Merely a print trace to standard output, but 
;; that happens to be all the output we want just now. In a real trading application,
;; there probably would not be an output on this slot. Some gui widget might "output"
;; by telling the OS to redraw it, or some trader instance might decide to output
;; a buy order to an exchange, but that is about it.
;;
;; --- FN ticker-weights rule --------------------------------------
;;
;; A curiosity here is that ensure-ticker will often be making and to-be-ing new model
;; instances while this rule is running. No problem, though it would be possible to 
;; get into trouble if such destructive (well, constructive) operations triggered
;; dataflow back to this same rule. Here we are safe; it does not. In fact...
;;
;; This rule runs once and then gets optimized away, because in this simple case
;; index-def is a constant, not even a cell. Should we someday want to handle
;; changes to an index during a trading-day, this would have to change.
;;
;; --- FN lazy ------------------------------------------------------
;;
;;     Lazy ruled cells do not get calculated until someone asks their value,
;;     and once they are evaluated and dependencies have been established,
;;     they merely will be flagged "obsolete" should any of those dependencies
;;     change in value.
;;
;; --- FN state rule ------------------------------------------------
;;
;; c? ends up wrapping its body in a lambda form which becomes the rule for this
;; slot, and here that lambda form will close over the MOVES hash-table. Neat, eh?
;; What is going on is that we do not anticipate in the application design that
;; any cell will depend in isolation on the move of one ticker in the index. So
;; we can allocate just one hashtable at make-instance time and reuse that each
;; time the rule gets evaluated. Cells depending on the state Cell will know
;; when that aggregate value gets recomputed because the finally clause conses
;; up a new list each time.
;;
;; --- FN without-c-dependency -------------------------------------
;;
;; Our application knowledge tells us the dependency on ticker minute will suffice
;; to keep index state up to date, so we save a lot of internal cells overhead
;; by taking a chance and disabling dependency creation within the wrapper
;; with-c-output. (The danger is that someone later adds a desired dependency reference
;; to the rule without noticing the wrapper.)
;;
;; --- FN value Cell --------------------------------------------------
;;
;; Weird, right? Well, we noticed that many trades came thru at the same price
;; sequentially. The rule above for STATE gets kicked off on each trade, and the
;; index gets recomputed. Because it is an aggregate, we get a new list for state
;; even if the trade was at an unchanged priced and the index value does not change.
;; 
;; Now suppose there was some BUY! rule which cared only about the index value, and not
;; the latest minute traded of that value, which /would/ change if a new trade at
;; an unchanged price were received. Because a new list gets consed up (never mind the 
;; new trade minute), The BUY! rule would get kicked off because of the new list in the
;; the STATE slot. Not even overriding the default EQL test with EQUAL would work,
;; because the trade minute would have changed. 
;;
;; What to do? The above. Let VALUE get recalculated unnecessarily and return unchanged,
;; then code the BUY! rule to use VALUE. VALUE will get kicked off, but not BUY!, which
;; would likely be computationally intense.
;;

#| TRADEDATA
(:date 5123 :weekday 3)
(:index ((AA 29.30 7.3894672) (AIG 53.30 7.3894672)(AXP 53.00 7.3894672)
(BA 59.87 7.3894672) (C 46.80 7.3894672) (CAT 87.58 7.3894672) (DD 47.74 7.3894672)
(DIS 26.25 7.3894672) (GE 36.10 7.3894672) (GM 27.77 7.3894672) (HD 36.75 7.3894672)
(HON 35.30 7.3894672) (HPQ 21.00 7.3894672) (IBM 76.47 7.3894672)
(INTC 23.75 7.3894672) (JNJ 68.73 7.3894672) (JPM 35.50 7.3894672) (KO 43.76 7.3894672)
(MCD 29.80 7.3894672) (MMM 76.76 7.3894672) (MO 65.99 7.3894672) (MRK 34.42 7.3894672)
(MSFT 25.36 7.3894672) (PFE 27.5 7.3894672) (PG 54.90 7.3894672) (SBC 23.8 7.3894672)
(UTX 100.96 7.3894672) (VZ 36.75 7.3894672) (WMT 48.40 7.3894672) (XOM 56.50 7.3894672)))
(:trade INTC    0.001932 :last  23.75)
(:trade MSFT    0.001932 :last  25.36)
(:trade INTC    0.011931 :last  23.75)
(:trade MSFT    0.011931 :last  25.36)
(:trade MSFT    0.041965 :last  25.32)
(:trade UTX     0.067027 :last 101.39)
(:trade INTC    0.067062 :last  23.82)
(:trade MSFT    0.070397 :last  25.37)
(:trade INTC    0.070397 :last  23.82)
(:trade MSFT    0.074167 :last  25.32)
(:trade INTC    0.081800 :last  23.83)
(:trade MSFT    0.097178 :last  25.33)
(:trade MSFT    0.106488 :last  25.32)
(:trade INTC    0.110410 :last  23.82)
(:trade INTC    0.124263 :last  23.83)
(:trade MSFT    0.130411 :last  25.33)
(:trade INTC    0.143792 :last  23.81)
(:trade MSFT    0.143792 :last  25.33)
(:trade DIS     0.150441 :last  26.25)
(:trade INTC    0.160480 :last  23.82)
(:trade MSFT    0.160480 :last  25.33)
(:trade HPQ     0.166767 :last  21.00)
(:trade INTC    0.178832 :last  23.82)
(:trade MSFT    0.183710 :last  25.33)
(:trade DIS     0.187167 :last  26.25)
(:trade AIG     0.193117 :last  53.60)
(:trade INTC    0.196399 :last  23.81)
(:trade PFE     0.200523 :last  27.51)
(:trade MSFT    0.200523 :last  25.33)
(:trade GE      0.202185 :last  36.11)
(:trade MSFT    0.207199 :last  25.37)
(:trade BA      0.209810 :last  59.75)
(:trade INTC    0.210524 :last  23.83)
(:trade MSFT    0.230556 :last  25.37)
(:trade INTC    0.230556 :last  23.83)
(:trade BA      0.234812 :last  59.76)
(:trade MSFT    0.240580 :last  25.37)
(:trade INTC    0.247233 :last  23.83)
(:trade MSFT    0.256892 :last  25.37)
(:trade UTX     0.257729 :last 101.33)
(:trade GE      0.261942 :last  36.11)
(:trade AIG     0.267072 :last  53.60)
(:trade MSFT    0.272956 :last  25.36)
(:trade INTC    0.275617 :last  23.83)
(:trade WMT     0.280660 :last  48.40)
(:trade SBC     0.284975 :last  23.78)
(:trade GE      0.289229 :last  36.10)
(:trade MSFT    0.292285 :last  25.35)
(:trade DIS     0.295646 :last  26.30)
(:trade HPQ     0.303630 :last  21.04)
(:trade IBM     0.305629 :last  76.60)
(:trade INTC    0.307321 :last  23.81)
(:trade INTC    0.310671 :last  23.81)
(:trade SBC     0.316331 :last  23.76)
(:trade AIG     0.322292 :last  53.60)
(:trade MSFT    0.324057 :last  25.36)
(:trade MCD     0.324057 :last  29.79)
(:trade UTX     0.325694 :last 101.15)
(:trade INTC    0.327348 :last  23.81)
(:trade IBM     0.336878 :last  76.60)
(:trade MSFT    0.342414 :last  25.37)
(:trade MSFT    0.345710 :last  25.37)
(:trade HD      0.346983 :last  36.82)
(:trade BA      0.347295 :last  59.80)
(:trade MCD     0.360765 :last  29.80)
(:trade HPQ     0.364067 :last  21.03)
(:trade MSFT    0.364067 :last  25.37)
(:trade SBC     0.367409 :last  23.79)
(:trade MSFT    0.392928 :last  25.36)
(:trade AIG     0.407453 :last  53.55)
(:trade HPQ     0.407533 :last  21.03)
(:trade SBC     0.407533 :last  23.79)
(:trade MSFT    0.407533 :last  25.36)
(:trade INTC    0.407533 :last  23.82)
(:trade HPQ     0.407533 :last  21.03)
(:trade HD      0.407545 :last  36.84)
(:trade BA      0.413185 :last  59.80)
(:trade INTC    0.414117 :last  23.81)
(:trade PFE     0.420796 :last  27.51)
(:trade DIS     0.424120 :last  26.30)
(:trade AIG     0.424654 :last  53.58)
(:trade INTC    0.427471 :last  23.81)
(:trade XOM     0.429865 :last  56.85)
(:trade IBM     0.431927 :last  76.65)
(:trade HPQ     0.432407 :last  21.04)
(:trade HD      0.432507 :last  36.84)
(:trade MCD     0.439207 :last  29.80)
(:trade MSFT    0.442518 :last  25.36)
(:trade DIS     0.442518 :last  26.30)
(:trade MSFT    0.453747 :last  25.36)
(:trade PFE     0.458821 :last  27.52)
(:trade IBM     0.459026 :last  76.66)
(:trade HON     0.467342 :last  35.36)
(:trade XOM     0.469083 :last  56.88)
(:trade INTC    0.470871 :last  23.80)
(:trade SBC     0.476712 :last  23.79)
(:trade BA      0.476730 :last  59.80)
(:trade MCD     0.479248 :last  29.80)
(:trade HPQ     0.479248 :last  21.03)
(:trade AIG     0.480883 :last  53.57)
(:trade MSFT    0.482567 :last  25.36)
(:trade INTC    0.482567 :last  23.80)
(:trade IBM     0.484223 :last  76.73)
(:trade MSFT    0.494243 :last  25.36)
(:trade AIG     0.497551 :last  53.57)
(:trade PFE     0.497569 :last  27.53)
(:trade INTC    0.504245 :last  23.80)
(:trade HD      0.504660 :last  36.84)
(:trade IBM     0.504849 :last  76.73)
(:trade GM      0.507621 :last  30.53)
(:trade SBC     0.511484 :last  23.79)
(:trade HPQ     0.514265 :last  21.04)
(:trade HD      0.514798 :last  36.85)
(:trade MSFT    0.517601 :last  25.32)
(:trade WMT     0.524286 :last  48.46)
(:trade IBM     0.524286 :last  76.74)
(:trade INTC    0.529220 :last  23.80)
(:trade HPQ     0.536813 :last  21.04)
(:trade PG      0.537627 :last  54.91)
(:trade PFE     0.540979 :last  27.54)
(:trade INTC    0.544290 :last  23.80)
(:trade PG      0.547549 :last  54.91)
(:trade XOM     0.547624 :last  56.85)
(:trade HON     0.547687 :last  35.40)
(:trade UTX     0.550986 :last 101.33)
(:trade HD      0.555694 :last  36.85)
(:trade MSFT    0.560792 :last  25.35)
(:trade INTC    0.564337 :last  23.80)
(:trade XOM     0.566779 :last  56.85)
(:trade BA      0.567359 :last  59.81)
(:trade HON     0.581023 :last  35.41)
(:trade INTC    0.589796 :last  23.80)
(:trade BA      0.596050 :last  59.80)
(:trade CAT     0.612134 :last  87.83)
(:trade WMT     0.618386 :last  48.44)
(:trade INTC    0.620474 :last  23.80)
(:trade MCD     0.624417 :last  29.80)
(:trade MSFT    0.627748 :last  25.35)
(:trade BA      0.630881 :last  59.83)
(:trade AIG     0.634410 :last  53.56)
(:trade MCD     0.637785 :last  29.79)
(:trade HON     0.637785 :last  35.40)
(:trade INTC    0.649577 :last  23.79)
(:trade BA      0.655889 :last  59.85)
(:trade HD      0.662287 :last  36.83)
(:trade AIG     0.669431 :last  53.53)
(:trade HON     0.671133 :last  35.44)
(:trade MCD     0.674457 :last  29.79)
(:trade MO      0.683443 :last  66.20)
(:trade INTC    0.687668 :last  23.79)
(:trade MSFT    0.691181 :last  25.35)
(:trade PFE     0.694477 :last  27.54)
(:trade MSFT    0.720936 :last  25.35)
(:trade GM      0.726237 :last  30.50)
(:trade WMT     0.730056 :last  48.40)
(:trade IBM     0.740544 :last  76.74)
(:trade PG      0.744569 :last  54.91)
(:trade HON     0.752103 :last  35.46)
(:trade CAT     0.753014 :last  87.85)
(:trade MO      0.763918 :last  66.20)
(:trade MSFT    0.764592 :last  25.35)
(:trade HON     0.771289 :last  35.46)
(:trade BA      0.772935 :last  59.75)
(:trade JPM     0.773229 :last  35.51)
(:trade MSFT    0.774612 :last  25.35)
(:trade PG      0.776267 :last  54.91)
(:trade AIG     0.781168 :last  53.54)
(:trade HD      0.782946 :last  36.87)
(:trade CAT     0.784614 :last  87.85)
(:trade XOM     0.786285 :last  56.88)
(:trade MSFT    0.792950 :last  25.36)
(:trade UTX     0.794689 :last 101.40)
(:trade INTC    0.797969 :last  23.78)
(:trade IBM     0.801301 :last  76.74)
(:trade HD      0.809652 :last  36.87)
(:trade JPM     0.809652 :last  35.51)
(:trade MSFT    0.811489 :last  25.37)
(:trade MO      0.812994 :last  66.20)
(:trade IBM     0.816563 :last  76.75)
(:trade MCD     0.828046 :last  29.77)
(:trade UTX     0.829055 :last 101.37)
(:trade MSFT    0.833420 :last  25.36)
(:trade GM      0.837650 :last  30.50)
(:trade IBM     0.838004 :last  76.75)
(:trade HON     0.838531 :last  35.47)
(:trade XOM     0.841372 :last  56.88)
(:trade MCD     0.841894 :last  29.78)
(:trade KO      0.853202 :last  43.98)
(:trade UTX     0.858235 :last 101.38)
(:trade INTC    0.864331 :last  23.82)
(:trade PFE     0.869104 :last  27.55)
(:trade HON     0.873063 :last  35.48)
(:trade IBM     0.873095 :last  76.77)
(:trade HD      0.873132 :last  36.87)
(:trade XOM     0.884796 :last  56.86)
(:trade UTX     0.884820 :last 101.38)
(:trade HON     0.888886 :last  35.48)
(:trade INTC    0.891420 :last  23.81)
(:trade CAT     0.895715 :last  87.86)
(:trade MO      0.898111 :last  nil) ;; 66.19)
(:trade XOM     0.898111 :last  56.87)
(:trade IBM     0.899775 :last  76.78)
(:trade BA      0.899775 :last  59.83)
(:trade MSFT    0.901469 :last  25.38)
(:trade HD      0.906673 :last  36.86)
(:trade HPQ     0.908113 :last  21.03)
(:trade CAT     0.916467 :last  87.85)
(:trade BA      0.916467 :last  59.83)
(:trade MSFT    0.918773 :last  25.38)
(:trade PFE     0.926271 :last  27.57)
(:trade MO      0.926288 :last  66.18)
(:trade WMT     0.929791 :last  48.40)
(:trade KO      0.932333 :last  43.98)
(:trade JNJ     0.933224 :last  68.15)
(:trade PG      0.936516 :last  54.91)
(:trade INTC    0.938989 :last  23.81)
(:trade IBM     0.942596 :last  76.78)
(:trade XOM     0.944052 :last  56.89)
(:trade INTC    0.944885 :last  23.81)
(:trade BA      0.946486 :last  59.85)
(:trade IBM     0.958178 :last  76.78)
(:trade INTC    0.959853 :last  23.81)
(:trade JPM     0.959897 :last  35.50)
(:trade WMT     0.961498 :last  48.40)
(:trade MCD     0.963195 :last  29.77)
(:trade HPQ     0.966525 :last  21.03)
(:trade AIG     0.968663 :last  53.54)
(:trade XOM     0.978210 :last  56.89)
(:trade AIG     0.979896 :last  53.55)
(:trade CAT     0.979896 :last  87.85)
(:trade MCD     0.984732 :last  29.77)
(:trade PG      0.985307 :last  54.90)
(:trade WMT     0.995716 :last  48.41)
(:trade MSFT    1.005256 :last  25.38)
(:trade PFE     1.005256 :last  27.55)
(:trade JPM     1.008448 :last  35.48)
(:trade CAT     1.011343 :last  87.86)
(:trade XOM     1.011825 :last  56.88)
(:trade INTC    1.012667 :last  23.79)
(:trade JNJ     1.018655 :last  68.15)
(:trade KO      1.021589 :last  43.99)
(:trade INTC    1.026597 :last  23.78)
(:trade HD      1.029577 :last  36.85)
(:trade MSFT    1.029936 :last  25.39)
(:trade JPM     1.033267 :last  35.49)
(:trade C       1.064996 :last  46.80)
(:trade CAT     1.065946 :last  87.85)
(:trade MCD     1.066687 :last  29.75)
(:trade MRK     1.066687 :last  34.33)
(:trade PFE     1.066687 :last  27.55)
(:trade INTC    1.066687 :last  23.79)
(:trade INTC    1.066687 :last  23.79)
(:trade XOM     1.068360 :last  56.88)
(:trade JPM     1.068360 :last  35.49)
(:trade XOM     1.068360 :last  56.89)
(:trade KO      1.068360 :last  43.99)
(:trade MRK     1.070274 :last  34.34)
(:trade HON     1.073312 :last  35.49)
(:trade PFE     1.080025 :last  27.55)
(:trade MCD     1.080025 :last  29.75)
(:trade INTC    1.080025 :last  23.79)
(:trade AIG     1.083337 :last  53.55)
(:trade GM      1.083420 :last  30.55)
(:trade XOM     1.086739 :last  56.89)
(:trade HON     1.093425 :last  35.49)
(:trade HPQ     1.093425 :last  21.03)
(:trade INTC    1.093425 :last  23.79)
(:trade MSFT    1.093425 :last  25.37)
(:trade JPM     1.098339 :last  35.49)
(:trade IBM     1.099113 :last  76.86)
(:trade XOM     1.104257 :last  56.89)
(:trade MCD     1.104268 :last  29.74)
(:trade GE      1.108379 :last  36.14)
(:trade MSFT    1.108408 :last  25.40)
(:trade XOM     1.115052 :last  56.89)
(:trade JPM     1.118397 :last  35.50)
(:trade GM      1.118397 :last  30.55)
(:trade C       1.125426 :last  46.78)
(:trade MCD     1.132390 :last  29.74)
(:trade WMT     1.133494 :last  48.40)
(:trade MRK     1.135099 :last  34.33)
(:trade MSFT    1.135099 :last  25.39)
(:trade INTC    1.135099 :last  23.78)
(:trade INTC    1.146096 :last  23.79)
(:trade KO      1.146108 :last  43.99)
(:trade WMT     1.155346 :last  48.41)
(:trade PG      1.158447 :last  54.90)
(:trade WMT     1.162645 :last  48.41)
(:trade HON     1.162660 :last  35.52)
(:trade KO      1.162672 :last  43.98)
(:trade JNJ     1.166783 :last  68.20)
(:trade DIS     1.166815 :last  26.34)
(:trade HD      1.166856 :last  36.90)
(:trade MCD     1.171129 :last  29.74)
(:trade INTC    1.175130 :last  23.79)
(:trade JPM     1.178485 :last  35.50)
(:trade KO      1.178485 :last  43.98)
(:trade MSFT    1.184447 :last  25.39)
(:trade AIG     1.191811 :last  53.56)
(:trade WMT     1.195138 :last  48.41)
(:trade MSFT    1.199050 :last  25.39)
(:trade MO      1.201440 :last  66.18)
(:trade INTC    1.201841 :last  23.80)
(:trade DIS     1.201841 :last  26.34)
(:trade JNJ     1.202292 :last  68.20)
(:trade C       1.205172 :last  46.79)
(:trade KO      1.205172 :last  43.98)
(:trade WMT     1.209557 :last  48.40)
(:trade INTC    1.209927 :last  23.79)
(:trade VZ      1.209962 :last  34.75)
(:trade MSFT    1.213558 :last  25.37)
(:trade C       1.220169 :last  46.79)
(:trade DIS     1.220225 :last  26.34)
(:trade PFE     1.220225 :last  27.55)
(:trade JNJ     1.220921 :last  68.20)
(:trade MMM     1.223614 :last  76.70)
(:trade INTC    1.226875 :last  23.79)
(:trade DIS     1.230230 :last  26.34)
(:trade HPQ     1.230230 :last  21.03)
(:trade HON     1.230230 :last  35.52)
(:trade PFE     1.230230 :last  27.56)
(:trade SBC     1.230230 :last  23.78)
(:trade C       1.236915 :last  46.79)
(:trade MSFT    1.240577 :last  25.40)
(:trade DIS     1.243960 :last  26.34)
(:trade SBC     1.250258 :last  23.78)
(:trade MCD     1.250258 :last  29.74)
(:trade MSFT    1.250258 :last  25.40)
(:trade INTC    1.253588 :last  23.79)
(:trade HON     1.253588 :last  35.53)
(:trade MCD     1.257704 :last  29.74)
(:trade MSFT    1.262803 :last  25.37)
(:trade KO      1.271926 :last  43.99)
(:trade JPM     1.271926 :last  35.51)
(:trade VZ      1.276339 :last  34.75)
(:trade MSFT    1.280283 :last  25.40)
(:trade HPQ     1.280283 :last  21.03)
(:trade DIS     1.288624 :last  26.34)
(:trade GE      1.288664 :last  36.14)
(:trade JPM     1.288664 :last  35.51)
(:trade AIG     1.290300 :last  53.59)
(:trade CAT     1.290300 :last  87.86)
(:trade IBM     1.290300 :last  76.85)
(:trade SBC     1.291940 :last  23.77)
(:trade XOM     1.301948 :last  56.88)
(:trade DIS     1.303625 :last  26.34)
(:trade AIG     1.304047 :last  53.60)
(:trade KO      1.305316 :last  43.99)
(:trade JPM     1.305316 :last  35.51)
(:trade C       1.305316 :last  46.79)
(:trade KO      1.314761 :last  43.99)
(:trade DIS     1.316972 :last  26.35)
(:trade HON     1.316972 :last  35.54)
(:trade CAT     1.317022 :last  87.86)
(:trade IBM     1.317022 :last  76.85)
(:trade GE      1.318640 :last  36.15)
(:trade WMT     1.320354 :last  48.41)
(:trade HPQ     1.322354 :last  21.04)
(:trade AIG     1.331152 :last  53.59)
(:close)
|#

(defun msg-start (m)
  (search "TRADEDATA" m))

