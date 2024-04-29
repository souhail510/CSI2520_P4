#lang scheme
; defininig query histogram and datapath
(define queryHistogramFilename "C:\\Users\\Documents\\csi2520\\q00.jpg.txt")
  (define data-path "C:\\Users\\Documents\\imageDataset2_15_20")


; pixel number of images
(define pixelNumber 172800)

(define (list-text-files-in-directory directory-path)
  (filter (lambda (file)
            (string-suffix? file ".txt"))
          (map path->string
               (directory-list directory-path))))


; read a histogram textfile and returns the values in a list
(define (read-hist-file filename) 
(cdr (call-with-input-file filename
  (lambda (p)
    (let f ((x (read p)))
      (if (eof-object? x) '() (cons x (f (read p)))))))))



 ;)normalize histograms
(define (normalize histo)
  (define normalized-histo 
    (map (lambda (histoelem) (/ (exact->inexact histoelem) pixelNumber)) histo))
  normalized-histo)


 ;)sum of elements in histograms
(define (sumHisto histo)
  (cond ((null? histo) 0)
        ((null? (cdr histo)) (car histo))
        (else (+ (car histo) (sumHisto (cdr histo))))))


; get list of dataset files
(define imageDatasetDirectory(list-text-files-in-directory data-path))


; main function
(define (similaritySearch queryHistogramFilename imageDatasetDirectory)

  
;compare query histogram with all data histograms
  (define (compare histo1 histo2)
    (cond
      ((null? histo1) '())
      ((null? histo2) '())

      
;construct the minimum element into a list - and call compare recursivly with the rest of histograms elements
      ((< (car histo1) (car histo2))
       (cons (car histo1) (compare (cdr histo1) (cdr histo2))))
      (else
       (cons (car histo2) (compare (cdr histo1) (cdr histo2))))))


  
  (define sorted-list
    (sort (map (lambda (datafile)
                       ; reads query histogram and normalizes it
                 (let* ((queryhisto (read-hist-file queryHistogramFilename)) 
                        (queryNormalized-histo (normalize queryhisto))
                        
                        ; reads data histogram and normalizes it
                        (datahisto (read-hist-file datafile)) 
                        (dataNormalized-histo (normalize datahisto))
                        
                        ;compare normalized query histogram with normalized data histogram
                        (histoCompared (compare queryNormalized-histo dataNormalized-histo))
                        
                        ; compute somme of compared histogram
                        (maSomme (sumHisto histoCompared)))
                   
                       ; create list with 2 pairs datafilename and sum
                        (list datafile maSomme)))
                        imageDatasetDirectory)

             
                       ; sort filenames( first elements of x and y) based on their sum( second elements of x and y)
                        (lambda (x y) (< (cadr x) (cadr y)))))


  
 ; print 5 similar images
  (display "5 Similar images :\n")
  (display (take sorted-list 5)))