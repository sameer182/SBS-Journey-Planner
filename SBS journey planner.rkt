#lang racket/gui

;Block 2 project
;Creating a simple journey planner
;This group project is done by Sameer Limbu M00837788 , Barun Gurung M00831005 & Saroj Rasaily M00830945

(define stations
  (set "paddington" "notting hill gate" "west kensington" "ealing broadway" "queens way"))

(define linked-stations
  '#((("paddington" "notting hill gate") "District line" 2.1 9)
     (("notting hill gate" "queens way") "Central line" 1 4)
     (("notting hill gate" "ealing broadway") "Central line" 5 20 )
     (("notting hill gate" "west kensington") "District line" 3.8 15)))

(define route
  (λ (start end station)
    (cond
      ((proper-subset? (set start end) stations)
       (cond
         ;when start and end stations is the same
         ((equal? start end) (display "You have entered the same stations. \n"))


         (#t (for ([i (in-range (vector-length station))])
                  
                       
               (cond
                 ((set-member? (list->set (first (vector-ref station i))) start)
                  
                 (if
                   (equal? (first (remove start (first (vector-ref station i)))) end)
                     (printf "~a ----> ~a (~a) | Distance = ~a km | Duration = ~a min \n" start end (second (vector-ref station i)) (third (vector-ref station i)) (last (vector-ref station i)))
                     (routing start (first (remove start (first (vector-ref station i)))) end linked-stations)
                   ))
                    (routing (first (remove start (first (vector-ref station i)))) end end station)
                 )
                 ) 
               )
             )
           )
      
      ;Invalid Inputs
      
      ((and (not (set-member? stations end)) (not (set-member?  stations start)))
       (printf "Invalid inputs. \nWe operate in following stations 'Paddington' 'Notting Hill Gate' 'West Kensington' 'Ealing Broadway' & 'Queens Way'. \nThanks for understanding :) \n"))

      ((not (set-member?  stations start))
        (printf "Your starting point is currently out of service. \n"))

      ((not (set-member? stations end))
        (printf "Your destination point is currently out of service. \n"))
       
       )
     )
  )

(define routing
  (λ (current next end station)
    (for ([i (vector-length station)]
          #:when (not (equal? current end)))

      
       (cond  ((subset? (set current next) (list->set (first (vector-ref station i))))
                 (printf "~a ----> ~a (~a) | Distance = ~a km | Duration = ~a min \n" current next (second (vector-ref station i)) (third (vector-ref station i)) (last (vector-ref station i)))
              )
        ) 
      )
    
      (cond
      ((equal? current end) #t)
      (#t (routing next end end station))
      )
  )
)

;GUI part

( define myframe ( new frame%
[ label " SBS Journey Planner " ]
[ width 250] [ height 250]))

(new canvas% (parent myframe)(paint-callback
                            (λ (canvas dc)
                              (send dc set-scale 2 2)
                              (send dc set-text-foreground "blue")
                              (send dc draw-text "London Underground" 0 0)
                              )))


(define choice (new choice%
[ label "From" ] [ parent myframe ]
[ choices ( list "paddington"
"notting hill gate" "queens way" "west kensington" "ealing broadway")]))

(new choice%
[ label "To" ] [ parent myframe ]
[ choices ( list "paddington"
"notting hill gate" "queens way" "west kensington" "ealing broadway")])



( define butt ( new button% [ parent myframe ]
[ label "Search" ]))

(define re (new text-field% (parent myframe)(label "Result")))


(send myframe show #t)


(printf "\nHere is the result of your destination                  (Distance)           (Time)\n")

;User interaction

;This SBS journey planner operates in Two London Undergrounds service which are ( District line and Central line)
;The name of the stations are

     ; DISTRICT LINE              CENTRAL LINE

     ; paddington                 ealing broadway
     ; notting hill gate          notting hill gate
     ; west kensington            queensway
 

;Enter your stations between " " this section

    ; First section is the " STARTING DESTINATION " and Second section is the " END OF DESTINATION " 

;For example enter ("west kensington" "ealing broadway")
;Now enter your stations down below 

(route "paddington" "ealing broadway" linked-stations)





(printf "\n P.S. Currently our GUI is under maintenance for some updates \n Sorry for your inconvenience :)\n Have a Safe Journey from our Team :)")


;Thankyou





