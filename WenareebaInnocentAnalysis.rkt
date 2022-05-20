;;; WENAREEBA INNOCENT REG.NO : 2021/HD05/2315U

#lang racket
;;;import libraries
(require net/url)
(require data-science-master)
(require plot)
(require math)
(require csv-reading math math/matrix plot racket/hash)
(require (only-in srfi/19 date->string))
(require (only-in srfi/19 string->date))


;Read the dataset that will be used for sentimental analysis
(define dataset (read-csv "dataset_tweets.csv" #:->number? #f #:header? #t))

;Filter the data in the dataset
;Remove all the retweets : RT
(define data_segment
  (let ([tmp (map (λ (x)
                    (list (list-ref x 9))) dataset)])
    (filter (λ (x) (not (string-prefix? (first x) "RT"))) tmp)))
(define find_data_segment (flatten data_segment))

;Display all the information in the twitter data
;The data displayed is being extracted from the texts
(define twitter_data_display_all_texts
  (apply string-append find_data_segment))

;create a procedure , that loops through the dataset
;Removes the spaces from the dataset for accurate sentiment analysis
;Removes the punctuations from the data
;Iterate over the list of information, and transform each character to lowercase
(define twitter_text_information_clean (string-normalize-spaces
                      (remove-punctuation
                       (string-downcase twitter_data_display_all_texts) #:websafe? #t)))

;create a procedure word_occurances to handle the information from text
;it then sorts this information
(define word_occurances (document->tokens twitter_text_information_clean #:sort? #t))

; Convert the list of information into sentiments of word occurances
(define data_sentiment (list->sentiment word_occurances #:lexicon 'nrc))

;Handle the display of the sentiment
;Record the frequencies of of occurance in the twitter data
(aggregate sum ($ data_sentiment 'sentiment) ($ data_sentiment 'freq))

;create a variable to handle the couts of the different sentimens for display on graphs
(let ([counter (aggregate sum ($ data_sentiment 'sentiment) ($ data_sentiment 'freq))])
  (parameterize ((plot-width 800))
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counter (λ (x y) (> (second x) (second y))))
	    #:color "Green"
	    #:line-color "Blue"))
	    #:x-label "Affective Label"
	    #:y-label "Frequency")))

;Perform sentimental analysis , on the number of times the word is occuring
(define sentiment_analysis (list->sentiment word_occurances #:lexicon 'bing))

;create a plotted graph of height of 200
(parameterize ([plot-height 200])
  
  ;Plot the histogram showing the sentiment analysis
  (plot (discrete-histogram
	 (aggregate sum ($ sentiment_analysis 'sentiment) ($ sentiment_analysis 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "Orange"
	 #:line-color "Blue")
	#:x-label "Frequency"
	#:y-label "Sentiment Polarity"))
