;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Investment Benchmark: shows investment with benchmarks
;;  
;; Copyright 2018 Liang Wang <netcasper@gmail.com>
;;
;; This report is based on category-barchart.scm
;; by Christian Stimming <stimming@tu-harburg.de>
;;
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; depends must be outside module scope -- and should eventually go away.
(define-module (gnucash report standard-reports investment-benchmark))
(use-modules (gnucash report report-system report-collectors))
(use-modules (gnucash report report-system collectors))
(use-modules (srfi srfi-1))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 regex))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gettext))

(use-modules (gnucash printf))

(gnc:module-load "gnucash/report/report-system" 0)

;; included since Bug726449
(use-modules (ice-9 regex)) ;; for regexp-substitute/global, used by jpqplot
(load-from-path "html-jqplot") ;; for jqplot-escape-string
(load-from-path "html-barlinechart")

(define reportname (N_ "Investment Benchmark"))

;; Option names
(define optname-from-date (N_ "Start Date"))
(define optname-to-date (N_ "End Date"))
(define optname-stepsize (N_ "Step Size"))
(define optname-report-currency (N_ "Report's currency"))
(define optname-price-source (N_ "Price Source"))

(define optname-accounts (N_ "Accounts"))
(define optname-levels (N_ "Show Accounts until level"))

(define optname-fullname (N_ "Show long account names"))
(define optname-stacked (N_ "Use Stacked Bars"))
(define optname-slices (N_ "Maximum Bars"))
(define optname-plot-width (N_ "Plot Width"))
(define optname-plot-height (N_ "Plot Height"))
(define optname-sort-method (N_ "Sort Method"))

(define optname-averaging (N_ "Show Average"))
(define opthelp-averaging (N_ "Select whether the amounts should be shown over the full time period or rather as the average e.g. per month."))

(define (investment-benchmark-options-generator)
  (let* ((options (gnc:new-options)) 
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))

    ;; General tab
    (gnc:options-add-date-interval!
     options gnc:pagename-general
     optname-from-date optname-to-date "a")

    (gnc:options-add-interval-choice! 
     options gnc:pagename-general optname-stepsize "b" 'MonthDelta)

    (gnc:options-add-currency! 
     options gnc:pagename-general optname-report-currency "c")

    (gnc:options-add-price-source! 
     options gnc:pagename-general
     optname-price-source "d" 'pricedb-nearest)

    (add-option
     (gnc:make-multichoice-option
      gnc:pagename-general optname-averaging
      "e" opthelp-averaging
      'None
      (list (vector 'None
		    (N_ "No Averaging")
		    (N_ "Just show the amounts, without any averaging."))
	    (vector 'MonthDelta
		    (N_ "Monthly")
		    (N_ "Show the average monthly amount during the reporting period."))
	    (vector 'WeekDelta
		    (N_ "Weekly")
		    (N_ "Show the average weekly amount during the reporting period."))
	    (vector 'DayDelta
		    (N_ "Daily")
		    (N_ "Show the average daily amount during the reporting period."))
	    )
      ))
    
    ;; Accounts tab
    (add-option
     (gnc:make-account-list-option
      gnc:pagename-accounts optname-accounts
      "a"
      (N_ "Report on these accounts, if chosen account level allows.")
      (lambda ()
        (gnc:filter-accountlist-type 
         (list ACCT-TYPE-ASSET ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)
         (gnc-account-get-descendants-sorted (gnc-get-current-root-account))))
      (lambda (accounts)
        (list #t
              (gnc:filter-accountlist-type (list ACCT-TYPE-ASSET ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL) accounts)))
      #t))

    (gnc:options-add-account-levels! 
     options gnc:pagename-accounts optname-levels "c" 
     (N_ "Show accounts to this depth and not further.") 
     3)

    ;; Display tab
    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-fullname
      "a" (N_ "Show the full account name in legend?") #f))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display optname-stacked
      "b" 
      (N_ "Show barchart as stacked barchart?") 
      #t))

    (add-option
     (gnc:make-number-range-option
      gnc:pagename-display optname-slices
      "c" (N_ "Maximum number of bars in the chart.") 8
      2 24 0 1))

    (add-option
     (gnc:make-simple-boolean-option
      gnc:pagename-display
      (N_ "Show table")
      "d" (N_ "Display a table of the selected data.")
      #f))

    (gnc:options-add-plot-size! 
     options gnc:pagename-display 
     optname-plot-width optname-plot-height "e" 400 400)

    (gnc:options-add-sort-method! 
     options gnc:pagename-display
     optname-sort-method "f" 'amount)

    (gnc:options-set-default-section options gnc:pagename-general)

    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.

;; FIXME: the exchange rate should change every time interval, of
;; course, but right now we assume the very last exchange rate to be
;; constant over the whole report period. Note that this might get
;; *really* complicated.

(define (investment-benchmark-renderer report-obj)
  ;; A helper functions for looking up option values.
  (define (get-option section name)
    (gnc:option-value 
     (gnc:lookup-option 
      (gnc:report-options report-obj) section name)))

  (gnc:report-starting reportname)
  (let* ((to-date-tp (gnc:timepair-end-day-time 
                     (gnc:date-option-absolute-time
                      (get-option gnc:pagename-general 
                                  optname-to-date))))
        (from-date-tp (gnc:timepair-start-day-time 
                       (gnc:date-option-absolute-time
                        (get-option gnc:pagename-general 
                                    optname-from-date))))
        (interval (get-option gnc:pagename-general optname-stepsize))
        (report-currency (get-option gnc:pagename-general
                                     optname-report-currency))
        (price-source (get-option gnc:pagename-general
                                  optname-price-source))
        (report-title (get-option gnc:pagename-general 
                                  gnc:optname-reportname))
        (averaging-selection (get-option gnc:pagename-general
					 optname-averaging))
        (accounts (get-option gnc:pagename-accounts optname-accounts))
        (account-levels (get-option gnc:pagename-accounts optname-levels))
        
        (stacked? (get-option gnc:pagename-display optname-stacked))
        (show-fullname? (get-option gnc:pagename-display optname-fullname))
        (max-slices (inexact->exact
		     (get-option gnc:pagename-display optname-slices)))
        (height (get-option gnc:pagename-display optname-plot-height))
        (width (get-option gnc:pagename-display optname-plot-width))
	(sort-method (get-option gnc:pagename-display optname-sort-method))

        (show-table? (get-option gnc:pagename-display (N_ "Show table")))
        (document (gnc:make-html-document))
        (chart (gnc:make-html-barlinechart))
        (table (gnc:make-html-table))
	(account-types(list ACCT-TYPE-ASSET ACCT-TYPE-STOCK ACCT-TYPE-MUTUAL)))

    (define (show-acct? a)
      (member a accounts))

    ;; (gnc:debug accounts)
    (if (not (null? accounts))
        
        ;; Define more helper variables.
        (let* ((commodity-list #f)
               (exchange-fn #f)
               (tree-depth (if (equal? account-levels 'all)
                               (gnc:get-current-account-tree-depth)
                               account-levels))
               (averaging-fraction-func (gnc:date-get-fraction-func averaging-selection))
               (interval-fraction-func (gnc:date-get-fraction-func interval))
               (averaging-multiplier
                 (if averaging-fraction-func
                     ;; Calculate the divisor of the amounts so that an
                     ;; average is shown
                     (let* ((start-frac-avg (averaging-fraction-func (gnc:timepair->secs from-date-tp)))
                             (end-frac-avg (averaging-fraction-func (+ 1 (gnc:timepair->secs to-date-tp))))
                             (diff-avg (- end-frac-avg start-frac-avg))
                             (start-frac-int (interval-fraction-func (gnc:timepair->secs from-date-tp)))
                             (end-frac-int (interval-fraction-func (+ 1 (gnc:timepair->secs to-date-tp))))
                             (diff-int (- end-frac-int start-frac-int))
                            )
                     ;; Extra sanity check to ensure a number smaller than 1
                     (if (> diff-avg diff-int)
                         (/ diff-int diff-avg)
                         1))
                     1))
               ;; If there is averaging, the report-title is extended
               ;; accordingly.
               (report-title
                 (case averaging-selection
                   ((MonthDelta) (string-append report-title " " (_ "Monthly Average")))
                   ((WeekDelta) (string-append report-title " " (_ "Weekly Average")))
                   ((DayDelta) (string-append report-title " " (_ "Daily Average")))
                   (else report-title)))
               ;; This is the list of date intervals to calculate.
               (dates-list (gnc:make-date-list
			    (gnc:timepair-end-day-time from-date-tp) 
			    (gnc:timepair-end-day-time to-date-tp)
			    (gnc:deltasym-to-delta interval)))
               ;; Here the date strings for the x-axis labels are
               ;; created.
               (date-string-list
                (map (lambda (date-list-item)
                       (gnc-print-date date-list-item))
                     dates-list))
               (other-anchor "")
               (all-data '()))
          
          ;; Converts a commodity-collector into one single double
          ;; number, depending on the report's currency and the
          ;; exchange-fn calculated above. Returns a double, multiplied
          ;; by the averaging-multiplies (smaller than one; multiplication
          ;; instead of division to avoid division-by-zero issues) in case
          ;; the user wants to see the amounts averaged over some value.
          (define (collector->double c date)
            ;; Future improvement: Let the user choose which kind of
            ;; currency combining she want to be done.
	    (if (not (gnc:timepair? date))
		(throw 'wrong))
            (*
              (gnc-numeric-to-double
              (gnc:gnc-monetary-amount
                (gnc:sum-collector-commodity
                c report-currency
                (lambda (a b) (exchange-fn a b date)))))
             averaging-multiplier))

	  (define (count-accounts current-depth accts)
	    (if (< current-depth tree-depth)
		(let ((sum 0))
		  (for-each
		   (lambda (a)
		     (set! sum (+ sum (+ 1 (count-accounts (+ 1 current-depth)
							   (gnc-account-get-children a))))))
		   accts)
		  sum)
		(length (filter show-acct? accts))))

          ;; Calculates all account's balances. Returns a list of pairs:
          ;; (<account> <balance-list>), like '((Earnings (10.0 11.2))
          ;; (Gifts (12.3 14.5))), where each element of <balance-list>
          ;; is the balance corresponding to one element in
          ;; <dates-list>.
          ;;
          ;; If current-depth >= tree-depth, then the balances are
          ;; calculated *with* subaccount's balances. Else only the
          ;; current account is regarded. Note: All accounts in accts
          ;; and all their subaccounts are processed, but a balances is
          ;; calculated and returned *only* for those accounts where
          ;; show-acct? is true. This is necessary because otherwise we
          ;; would forget an account that is selected but not its
          ;; parent.
	  (define (apply-sign account x)
	    x)
          (define (calculate-report accounts progress-range)
	    ;; TODO: check if the-acount-destination-alist is
	    ;; empty. Otherwise category-by-account-report-work below
	    ;; will crash.
	    (let* ((the-acount-destination-alist (account-destination-alist accounts
									    account-types
									    tree-depth))
		   (account-reformat
		    (lambda (account result)
		      (let ((commodity-collector (gnc:make-commodity-collector)))
			(collector-end (fold (lambda (next date list-collector)
					       (commodity-collector 'merge next #f)
					       (collector-add list-collector
							      (apply-sign account
									  (collector->double commodity-collector
											     date))))
					     (collector-into-list)
					     result dates-list)))))

		   (the-work (category-by-account-report-work #f
							      dates-list the-acount-destination-alist
							      (lambda (account date)
								(make-gnc-collector-collector))
							      account-reformat))
		   (the-report (category-by-account-report-do-work the-work progress-range)))
	      the-report))

          ;; The percentage done numbers here are a hack so that
          ;; something gets displayed. On my system the
          ;; gnc:case-exchange-time-fn takes about 20% of the time
          ;; building up a list of prices for later use. Either this
          ;; routine needs to send progress reports, or the price
          ;; lookup should be distributed and done when actually
          ;; needed so as to amortize the cpu time properly.
	  (gnc:report-percent-done 1)
	  (set! commodity-list (gnc:accounts-get-commodities 
                                (append 
                                 (gnc:acccounts-get-all-subaccounts accounts)
                                 accounts)
                                report-currency))
	  (set! exchange-fn (gnc:case-exchange-time-fn 
                             price-source report-currency 
                             commodity-list to-date-tp
			     5 15))
          ;; Sort the account list according to the account code field.
          (set! all-data (sort
                          (filter (lambda (l)
                                    (not (= 0.0 (apply + (cadr l)))))
                                  (calculate-report accounts (cons 0 90)))
			  (cond
			   ((eq? sort-method 'acct-code)
			    (lambda (a b) 
			      (string<? (xaccAccountGetCode (car a))
					(xaccAccountGetCode (car b)))))
			   ((eq? sort-method 'alphabetical)
			    (lambda (a b) 
			      (string<? ((if show-fullname?
					     gnc-account-get-full-name
					     xaccAccountGetName) (car a))
					((if show-fullname?
					     gnc-account-get-full-name
					     xaccAccountGetName) (car b)))))
			   (else
			    (lambda (a b)
			      (> (apply + (cadr a))
				 (apply + (cadr b))))))))
          ;; Or rather sort by total amount?
          ;;(< (apply + (cadr a)) 
          ;;   (apply + (cadr b))))))
          ;; Other sort criteria: max. amount, standard deviation of amount,
          ;; min. amount; ascending, descending. FIXME: Add user options to
          ;; choose sorting.
          
          
          ;; (gnc:warn "all-data" all-data)

          ;; Proceed if the data is non-zeros
          (if 
           (and (not (null? all-data))
                (gnc:not-all-zeros (map cadr all-data)))
           (begin
	     (let* ((benchmark-data
		     '(("Benchmark1" (1000 2000 3000 4000 5000))
		       ("Benchmark2" (1000 2000 4000 8000 16000))))
		    (benchmark-data-length (length (cadar benchmark-data)))
		    (data (append all-data benchmark-data)))
	       (for-each
		(lambda (row)
		  (let ((row-length (length (cadr row))))
		    (if (<  row-length benchmark-data-length)
			(begin
			  (list-set! row 1 (append (cadr row) (make-list (- benchmark-data-length row-length) 0.0)))))))
		data)
	       (gnc:html-barlinechart-set-title! chart report-title)
	       (gnc:html-barlinechart-set-subtitle!
		chart (sprintf #f (_ "%s to %s")
			       (jqplot-escape-string (gnc-print-date from-date-tp))
			       (jqplot-escape-string (gnc-print-date to-date-tp))))
	       (gnc:html-barlinechart-set-width! chart width)
	       (gnc:html-barlinechart-set-height! chart height)
	       (gnc:html-barlinechart-set-data! chart (apply zip (map cadr data)))
	       (gnc:html-barlinechart-set-row-labels! chart date-string-list)
	       (gnc:html-barlinechart-set-col-labels!
		chart (map (lambda (pair)
			     (if (string? (car pair))
				 (car pair)
				 ((if show-fullname?
				      gnc-account-get-full-name
				      xaccAccountGetName) (car pair))))
			   data))
	       (gnc:html-barlinechart-set-col-linerenderer?!
		chart
		(append (make-list (length all-data) #f)
			(make-list (- (length data) (length all-data)) #t)))
	       (gnc:html-barlinechart-set-stacked?! chart stacked?)
	       (gnc:html-barlinechart-set-y-axis-label! chart (gnc-commodity-get-mnemonic report-currency))
	       (gnc:html-document-add-object! document (gnc:make-html-object-internal gnc:html-barlinechart-render chart))
	       (gnc:report-finished)
	       document)
             )

           ;; else if empty data
           (gnc:html-document-add-object!
            document
            (gnc:html-make-empty-data-warning
	     report-title (gnc:report-id report-obj)))))
        
	;; else if no accounts selected
        (gnc:html-document-add-object! 
         document 
	 (gnc:html-make-no-account-warning 
	  report-title (gnc:report-id report-obj))))))

(gnc:define-report 
 'version 1
 'name reportname
 'report-guid "9aaca2bd2f2a470687ab2de18ddcf91f"
 'menu-path (list gnc:menuname-asset-liability)
 'options-generator investment-benchmark-options-generator
 'renderer investment-benchmark-renderer)
