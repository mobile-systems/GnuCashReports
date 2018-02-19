;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-barlinechart.scm : generate HTML programmatically, with
;; support for simple style elements.
;; Copyright 2018 Liang Wang <netcasper@gmail.com>
;;
;; This module is based on html-barchart.scm
;; by Bill Gribble <grib@gnumatic.com>
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-from-path "html-jqplot")

(define <html-barlinechart>
  (make-record-type "<html-barlinechart>"
                    '(width 
                      height 
                      title 
                      subtitle 
                      x-axis-label
                      y-axis-label
                      col-labels
                      row-labels 
                      col-colors
		      col-linerenderer?
		      legend-reversed?
                      row-labels-rotated?
		      stacked?
                      data
		      button-1-bar-urls
                      button-2-bar-urls 
		      button-3-bar-urls
		      button-1-legend-urls
                      button-2-legend-urls 
		      button-3-legend-urls)))

(define gnc:html-barlinechart?
  (record-predicate <html-barlinechart>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-barlinechart> class
;;  generate the <object> form for a barlinechart. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-barlinechart-internal
  (record-constructor <html-barlinechart>))

(define (gnc:make-html-barlinechart)
  (gnc:make-html-barlinechart-internal -1 -1 #f #f #f #f '() '() '() '()
				   #f #f #f '() #f #f #f #f #f #f))

(define gnc:html-barlinechart-data
  (record-accessor <html-barlinechart> 'data))

(define gnc:html-barlinechart-set-data!
  (record-modifier <html-barlinechart> 'data))

(define gnc:html-barlinechart-width
  (record-accessor <html-barlinechart> 'width))

(define gnc:html-barlinechart-set-width!
  (record-modifier <html-barlinechart> 'width))

(define gnc:html-barlinechart-height
  (record-accessor <html-barlinechart> 'height))

(define gnc:html-barlinechart-set-height!
  (record-modifier <html-barlinechart> 'height))

(define gnc:html-barlinechart-x-axis-label
  (record-accessor <html-barlinechart> 'x-axis-label))

(define gnc:html-barlinechart-set-x-axis-label!
  (record-modifier <html-barlinechart> 'x-axis-label))

(define gnc:html-barlinechart-y-axis-label
  (record-accessor <html-barlinechart> 'y-axis-label))

(define gnc:html-barlinechart-set-y-axis-label!
  (record-modifier <html-barlinechart> 'y-axis-label))

(define gnc:html-barlinechart-row-labels
  (record-accessor <html-barlinechart> 'row-labels))

(define gnc:html-barlinechart-set-row-labels!
  (record-modifier <html-barlinechart> 'row-labels))

(define gnc:html-barlinechart-row-labels-rotated?
  (record-accessor <html-barlinechart> 'row-labels-rotated?))

(define gnc:html-barlinechart-set-row-labels-rotated?!
  (record-modifier <html-barlinechart> 'row-labels-rotated?))

(define gnc:html-barlinechart-stacked?
  (record-accessor <html-barlinechart> 'stacked?))

(define gnc:html-barlinechart-set-stacked?!
  (record-modifier <html-barlinechart> 'stacked?))

(define gnc:html-barlinechart-col-labels
  (record-accessor <html-barlinechart> 'col-labels))

(define gnc:html-barlinechart-set-col-labels!
  (record-modifier <html-barlinechart> 'col-labels))

(define gnc:html-barlinechart-col-colors
  (record-accessor <html-barlinechart> 'col-colors))

(define gnc:html-barlinechart-set-col-colors!
  (record-modifier <html-barlinechart> 'col-colors))

(define gnc:html-barlinechart-col-linerenderer?
  (record-accessor <html-barlinechart> 'col-linerenderer?))

(define gnc:html-barlinechart-set-col-linerenderer?!
  (record-modifier <html-barlinechart> 'col-linerenderer?))

(define gnc:html-barlinechart-legend-reversed?
  (record-accessor <html-barlinechart> 'legend-reversed?))

(define gnc:html-barlinechart-set-legend-reversed?!
  (record-modifier <html-barlinechart> 'legend-reversed?))

(define gnc:html-barlinechart-title
  (record-accessor <html-barlinechart> 'title))

(define gnc:html-barlinechart-set-title!
  (record-modifier <html-barlinechart> 'title))

(define gnc:html-barlinechart-subtitle
  (record-accessor <html-barlinechart> 'subtitle))

;; Note: Due to Bug726449 the input string's non-printable control
;;       characters must translated to HTML format tags BEFORE
;;       or WHEN calling this function.
;;       AND:
;;       To ensure that the generated subtitle doesn't contain any
;;       unescaped quotes or backslashes, all strings must be freed
;;       from those by calling jqplot-escape-string.
;;       Otherwise we're opening the gates again for bug 721768.
;;
;;       Example: "\n" must be translated to "<br /> to introduce
;;                a line break into the chart subtitle.
;;
;;       Example call:
;;         (gnc:html-barlinechart-set-subtitle! chart
;;           (string-append "Bgt:"
;;                          (jqplot-escape-string (number->string bgt-sum))
;;                          "<br /> Act:" ;; line break in the chart sub-title
;;                          (jqplot-escape-string (number->string act-sum))))
(define gnc:html-barlinechart-set-subtitle!
  (record-modifier <html-barlinechart> 'subtitle))

;; Note: ATM you can specify one url per column, but this url will be
;; used for all of the rows. Otherwise we could have cols*rows urls
;; (quite a lot), but this first requires fixing
;; guppi_bar_1_callback() in gnome/gnc-html-guppi.c .
;; FIXME url's haven't been working since GnuCash 1.x
;;       GnuCash 2.x switched from guppy to goffice, which
;;       made it very hard to remain the url functionality
;;       At this point I (gjanssens) is in the process of
;;       moving from goffice to jqplot for our charts
;;       which perhaps may allow urls again in the charts
;;       I'm keeping the parameters below around to remind
;;       us this still has to be investigated again
(define gnc:html-barlinechart-button-1-bar-urls
  (record-accessor <html-barlinechart> 'button-1-bar-urls))

(define gnc:html-barlinechart-set-button-1-bar-urls!
  (record-modifier <html-barlinechart> 'button-1-bar-urls))

(define gnc:html-barlinechart-button-2-bar-urls
  (record-accessor <html-barlinechart> 'button-2-bar-urls))

(define gnc:html-barlinechart-set-button-2-bar-urls!
  (record-modifier <html-barlinechart> 'button-2-bar-urls))

(define gnc:html-barlinechart-button-3-bar-urls
  (record-accessor <html-barlinechart> 'button-3-bar-urls))

(define gnc:html-barlinechart-set-button-3-bar-urls!
  (record-modifier <html-barlinechart> 'button-3-bar-urls))

(define gnc:html-barlinechart-button-1-legend-urls
  (record-accessor <html-barlinechart> 'button-1-legend-urls))

(define gnc:html-barlinechart-set-button-1-legend-urls!
  (record-modifier <html-barlinechart> 'button-1-legend-urls))

(define gnc:html-barlinechart-button-2-legend-urls
  (record-accessor <html-barlinechart> 'button-2-legend-urls))

(define gnc:html-barlinechart-set-button-2-legend-urls!
  (record-modifier <html-barlinechart> 'button-2-legend-urls))

(define gnc:html-barlinechart-button-3-legend-urls
  (record-accessor <html-barlinechart> 'button-3-legend-urls))

(define gnc:html-barlinechart-set-button-3-legend-urls!
  (record-modifier <html-barlinechart> 'button-3-legend-urls))

(define (gnc:html-barlinechart-append-row! barlinechart newrow)
  (let ((dd (gnc:html-barlinechart-data barlinechart)))
    (set! dd (append dd (list newrow)))
    (gnc:html-barlinechart-set-data! barlinechart dd)))

(define (gnc:html-barlinechart-prepend-row! barlinechart newrow)
  (let ((dd (gnc:html-barlinechart-data barlinechart)))
    (set! dd (cons newrow dd))
    (gnc:html-barlinechart-set-data! barlinechart dd)))

(define (gnc:html-barlinechart-append-column! barlinechart newcol)
  (let ((colnum 0)
        (rownum 0)
        (rows (gnc:html-barlinechart-data barlinechart))
        (this-row #f)
        (new-row #f))
    ;; find out how many cols are already there in the deepest row
    (for-each 
     (lambda (row)
       (let ((l (length row)))
         (if (> l colnum)
             (set! colnum l))))
     rows)
    
    ;; append the elements of 'newrow' to the rowumns 
    (for-each
     (lambda (newelt)
       ;; find the row, or append one 
       (if (not (null? rows))
           (begin
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))
       
       ;; make sure the rowumn is long enough, then append the data 
       (let loop ((l (length this-row))
                  (r (reverse this-row)))
         (if (< l colnum)
             (loop (+ l 1) (cons #f r))
             (set! this-row 
                   (reverse (cons newelt r)))))
       (if new-row
           (gnc:html-barlinechart-append-row! barlinechart this-row)
           (list-set! (gnc:html-barlinechart-data barlinechart) rownum this-row))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-barlinechart-not-all-zeros data)
  (define (myor list)
    (begin 
      (if (null? list) #f
	  (or (car list) (myor (cdr list))))))

  (cond ((number? data) (not (= 0 data)))
	((list? data) (myor (map gnc:html-barlinechart-not-all-zeros data)))
	(else #f)))

(define (gnc:html-barlinechart-prepend-column! barlinechart newcol)
  (let ((rows (gnc:html-barlinechart-data barlinechart))
        (this-row #f)
        (new-row #f)
        (rownum 0))
    (for-each 
     (lambda (elt)
       (if (not (null? rows))
           (begin 
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))
       (if new-row
           (gnc:html-barlinechart-append-row! barlinechart (list elt))
           (list-set! (gnc:html-barlinechart-data barlinechart) rownum
                      (cons elt this-row)))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-barlinechart-render barlinechart doc)
  (define (ensure-numeric elt)
    (cond ((number? elt)
           (exact->inexact elt))
          ((string? elt)
           (with-input-from-string elt
             (lambda ()
               (let ((n (read)))
                 (if (number? n) n 0.0)))))
          ((gnc:gnc-numeric? elt)
           (gnc-numeric-to-double elt))
          (#t 
           0.0)))
  
  (define (catenate-escaped-strings nlist)
    (if (not (list? nlist))
        ""
        (with-output-to-string
          (lambda ()
            (for-each 
             (lambda (s)
               (let ((escaped 
                      (regexp-substitute/global 
                       #f " " 
                       (regexp-substitute/global 
                        #f "\\\\" s
                        'pre "\\\\" 'post)
                       'pre "\\ " 'post)))
                 (display escaped)
                 (display " ")))
             nlist)))))

  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (title (gnc:html-barlinechart-title barlinechart))
         (subtitle (gnc:html-barlinechart-subtitle barlinechart))
         (url-1
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-1-bar-urls barlinechart)))
         (url-2
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-2-bar-urls barlinechart)))
         (url-3
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-3-bar-urls barlinechart)))
         (legend-1
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-1-legend-urls barlinechart)))
         (legend-2
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-2-legend-urls barlinechart)))
         (legend-3
          (catenate-escaped-strings 
           (gnc:html-barlinechart-button-3-legend-urls barlinechart)))
         (x-label (gnc:html-barlinechart-x-axis-label barlinechart))
         (y-label (gnc:html-barlinechart-y-axis-label barlinechart))
         (data (gnc:html-barlinechart-data barlinechart))
         (row-labels (catenate-escaped-strings 
                      (gnc:html-barlinechart-row-labels barlinechart)))
         (col-labels (catenate-escaped-strings 
                      (gnc:html-barlinechart-col-labels barlinechart)))
         (col-colors (catenate-escaped-strings 
                      (gnc:html-barlinechart-col-colors barlinechart)))
         (series-data-start (lambda (series-index)
                         (push "var d")
                         (push series-index)
                         (push " = [];\n")))
         (series-data-add (lambda (series-index x y)
                         (push (string-append
                               "  d"
                               (number->string series-index)
                               ".push(["
                               (number->string x)
                               ", "
                               (number->string y)
                               "]);\n"))))
         (series-data-end (lambda (series-index label linerenderere)
			    (push "data.push(d")
			    (push series-index)
			    (push ");\n")
			    (push "series.push({ label: \"")
			    (push (jqplot-escape-string label))
			    (push "\"")
			    (if linerenderere
				(push ", renderer:$.jqplot.LineRenderer, disableStack:true"))
			    (push "});\n\n")))
         ; Use a unique chart-id for each chart. This prevents chart
         ; clashed on multi-column reports
         (chart-id (string-append "chart-" (number->string (random 999999)))))
    (if (and (list? data)
             (not (null? data))
             (gnc:html-barlinechart-not-all-zeros data))
        (begin
            (push (gnc:html-js-include "jqplot/jquery.min.js"))
            (push (gnc:html-js-include "jqplot/jquery.jqplot.js"))
            (push (gnc:html-js-include "jqplot/jqplot.barRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.cursor.js"))
            (push (gnc:html-js-include "jqplot/jqplot.categoryAxisRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.highlighter.js"))
            (push (gnc:html-js-include "jqplot/jqplot.canvasTextRenderer.js"))
            (push (gnc:html-js-include "jqplot/jqplot.canvasAxisTickRenderer.js"))

            (push (gnc:html-css-include "jqplot/jquery.jqplot.css"))

            (push "<div id=\"")(push chart-id)(push "\" style=\"width:")
            (push (gnc:html-barlinechart-width barlinechart))
            (push "px;height:")
            (push (gnc:html-barlinechart-height barlinechart))
            (push "px;\"></div>\n")
            (push "<script id=\"source\">\n$(function () {")

            (push "var data = [];")
            (push "var series = [];\n")

            (if (and data (list? data))
              (let ((rows (length data))
                    (cols 0))
                (let loop ((col 0) (rowcnt 1))
                  (series-data-start col)
                  (if (list? (car data))
                      (begin 
                        (set! cols (length (car data)))))    
                  (for-each
                    (lambda (row)
                      (if (<= rowcnt rows)
                        (series-data-add col rowcnt
                                       (ensure-numeric (list-ref-safe row col)))
                      )
                      (set! rowcnt (+ rowcnt 1)))
                    data)
                  (series-data-end col (list-ref-safe (gnc:html-barlinechart-col-labels barlinechart) col)
				   (list-ref-safe (gnc:html-barlinechart-col-linerenderer? barlinechart) col))
                  (if (< col (- cols 1))
                      (loop (+ 1 col) 1)))))

            (push "var all_ticks = [")
            (for-each
                (lambda (val)
                    (push "\"")
                    (push val)
                    (push "\","))
                (gnc:html-barlinechart-row-labels barlinechart))
            (push "];\n")
            (push "var options = {
                   shadowAlpha: 0.07,\n")
	    (push "
                   stackSeries: ")
	    (push (if (gnc:html-barlinechart-stacked? barlinechart)
		      "true,\n"
		      "false,\n"))
	    (push "
                   legend: {
                        show: true,
                        placement: \"outsideGrid\", },
                   seriesDefaults: {
                        renderer: $.jqplot.BarRenderer,
                        rendererOptions: {
                            shadowAlpha: 0.04,
                            shadowDepth: 3,
                        },
                        fillToZero: true,
                   },
                   series: series,
                   axesDefaults: {
                   },        
                   grid: {
                   },
                   axes: {
                       xaxis: {
                           renderer:$.jqplot.CategoryAxisRenderer,
                           tickRenderer: $.jqplot.CanvasAxisTickRenderer,
                           tickOptions: {
                               angle: -30,
                               fontSize: '10pt',
                           },
                       },
                       yaxis: {
                           autoscale: true,
                       },
                   },
                   highlighter: {
                       tooltipContentEditor: formatTooltip,
                   },
                   cursor:{
                       show: true,
                       showTooltip: false,
                       zoom: true,
                   },
                };\n")

            (if title
              (begin 
                (push "  options.title = \"")
                (push (jqplot-escape-string title))
                (push "\";\n")))

            (if subtitle
              (begin 
                (push "  options.title += \" <br />")
                (push subtitle)
                (push "\";\n")))

            (if (and (string? x-label) (> (string-length x-label) 0))
              (begin 
                (push "  options.axes.xaxis.label = \"")
                (push x-label)
                (push "\";\n")))
            (if (and (string? y-label) (> (string-length y-label) 0))
              (begin 
                (push "  options.axes.yaxis.label = \"")
                (push y-label)
                (push "\";\n")))
            (push "  options.axes.xaxis.ticks = all_ticks;\n")


            (push "$.jqplot.config.enablePlugins = true;\n")
            (push "$(document).ready(function() {
var plot = $.jqplot('")
	    (push chart-id)
	    (push"', data, options);
var int_chart_width = document.getElementById(\"")
	    (push chart-id)
	    (push"\").getElementsByClassName(\"jqplot-zoom-canvas\")[0].width;
plot.axes.xaxis.ticks = getVisualTicks(int_chart_width);
plot.replot();
});

function formatTooltip(str, seriesIndex, pointIndex) {
    if (options.axes.xaxis.ticks[pointIndex] !== undefined)
        x = options.axes.xaxis.ticks[pointIndex];
    else
        x = pointIndex;
    y = data[seriesIndex][pointIndex][1].toFixed(2);
    return options.series[seriesIndex].label + '<br/>' + x + '<br/><b>' + y + '</b>';
}

function getVisualTicks(chart_width) {
    var num_ticks = all_ticks.length;
    var label_width = 25;
    var num_labels = chart_width / label_width;
    var show_every_nth_label = Math.ceil (num_ticks / num_labels);
    var visual_ticks = [];

    if (show_every_nth_label == 0)
        show_every_nth_label = 1;
    for (counter = 0; counter < all_ticks.length; counter++) {
        if ((counter % show_every_nth_label) == 0)
            visual_ticks.push (all_ticks[counter]);
        else
            visual_ticks.push (' ');
    }
    return visual_ticks;
}\n")

            (push "});\n</script>")

            (gnc:msg (string-join (reverse (map (lambda (e) (if (number? e) (number->string e) e)) retval)) ""))
 
        )
        (begin 
          (gnc:warn "barlinechart has no non-zero data.")
            " "))
    retval))
