breed [ nodes node ]                       ;; these turtles make up the network
breed [ ents ent ]                         ;; these entities individually explore the network
directed-link-breed [pref-links pref-link] ;; directed connections betwen nodes, but could be undirected.
pref-links-own [
  pref-ind                                 ;; preference of link in learning phase for individual in current run.
  pref-est                                 ;; preferences from pref-ind, but only on established path; removes loops.
  pref-sum                                 ;; sum of pref-ind or pref-est over all runs - see use-est-prefs toggle
  pref-hist                                ;; list of lists containing pref-ind or pref-est of each run; first list is 0 for run 1
  id-hist                                  ;; the sim # for the pref-hist, also the location in the list pref-hist, stored at each node
  pref-coll                                ;; sum of pref-ind or pref-est for all sims
  pref-coll2                               ;; sum of pref-ind or pref-est - used in ensemble and sampling apps
  tried                                    ;; flag to determine if a node has been tried - used in the app rules
]
nodes-own [ node-id ]                      ;; node-id used during setup of maze

globals [
  start-node                 ; start node #, always start at this node
  end-node                   ; end node #, quit walk-about when this node is reached
  steps                      ; current number of steps taken, starts at 0
  my-node                    ; current ent number (not node), while count ents = 0 or 1, my-node increases for each path
  my-loc-node                ; node where ent is currently located, starts at start node
  my-loc-node-last           ; the node from the previous step - undefined at start of maze
  my-node-new                ; node number of new  location
  my-node-link               ; temp link turtle of selected outgoing link from ent node
  my-links-out               ; agentset of links from current ent node, used only in rules-from-1998
  path-nov                   ; list of nodes on the novice path for the last simulation, used in go and animate button
  path-est                   ; list of nodes on the path taken by established ind during the last simulation
  path-coll                  ; list of nodes on the path taken by collective - used in 2nd collective animation button
  path-coll2                 ; list of nodes on the path taken by collective using random groups - used in animation button
  steps-nov                  ; list of # of steps of novice for each run, used in stats monitors and plot
  steps-est                  ; list of # of steps using individual's application rules in one run, used in plot
  sd-est                     ; list of standard deviations of samping of one pref in app-ind
  range-est                  ; list of max-min of samping of one pref in app-ind
  steps-coll                 ; list of # of steps using aggregate of preferences. Added in go & used in interface monitor
  steps-coll2                ; list of # of steps using aggregate of preferences, used in collective solution
  sd-coll2                   ; standard deviation of current ensemble or mean of s.d. over sampling, set by stats-ensemble toggle
  range-coll2                ; max range of either current ensemble or mean of s.d. over sampling, set by stats-ensemble toggle
  coll-sample                ; temp list of members in a sample, used in "collective solutions"
  steps-samp                 ; temp list of steps for the sampling for individual app
  steps-sampc                ; temp list of steps for the sampling for collective
  stats-samp                 ; list of statistics for sampling or ensemble = [mean, standard deviation, range] for indiv app
  stats-sampc                ; list of statistics for sampling or ensemble = [mean, standard deviation, range] for coll app
  steps-ensemb               ; temp list of steps for one-sized ensemble, over all samplings
  sd-ensemb                  ; temp list of sd for one-sized ensemble, over all samplings
  range-ensemb               ; temp list of range for one-sized ensemble, over all samplings
  run-id                     ; list of the run number starting at 1
  size-coll2                 ; temp list of size of collective corresponding to steps-coll2 list
  list-ensemble              ; temp list of count of current ensemble number from 1 to ensemble
  runs-made                  ; number of runs of individual novice solutions made so far
  nodes-out                  ; agenset of all nodes connected to current ent node, used in novice learning rules
  step-size                  ; pen name in plot of collective steps vs size
  steps-1998                 ; list of steps from the 1998 reference sim, 50 realizations in ensemble for each sized group from 1 to 50
]
; The following are defined in input windows, toggles, etc. as global variables
  ;move-method               ; three options for rules in solving the maze
  ;load-file                 ; logical flag to generate maze from files versus from random generation
  ;link-chance               ; when 100% - all nodes connected in x and y direction;  when lower, causes some links to be missing
  ;grid-size                 ; the number of nodes along one side of square maze; also speed of animation later
  ;sample-est                ; the number of times app-ind is called to provide stats ==> stats for est-mean and est-sd
  ;max-coll-size             ; the max number of ind in collective in the random assembly of collectives - see collective-solution
  ;sampling-ind              ; # of times the applied rules is used in app-ind to get steps
  ;sampling                  ; # of times the applied rules is used on coll. preferences to get steps, used in collective-solution
  ;ensemble                  ; # of times a random group size is formed to get ensemble average
  ;load-file switch          ; true-false switch for loading maze from files (nodes and attributes) vs generated
  ;use-est-prefs             ; true-false switch for using the established preferences in the collective solution;
                             ; otherwise use the novice preferences that include loops in the path
  ;fix-random                ; when toggle is up, the random seed is set to "starting-seed" at setup; otherwise randomly generated.
                             ; this allows for mesh generation and simulations to be reproducible. See Info page.
  ;starting-seed             ; allows the random number generator to be reproducible for maze generation and runs.


;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;

to go
  if count ents = 0 [setup-ent]
  ask pref-links [ set pref-ind 0 ] ; reset novice individual preferences for new run
  set path-nov [ ]
  set path-nov lput start-node path-nov             ; initialize path-nov with start-node

  while [ not ( my-loc-node = end-node ) ] [ ;; repeat until end-node is reached - stop
     take-step
     set path-nov lput my-loc-node path-nov       ; store node in path-nov
   ]

  set runs-made runs-made + 1
  set run-id lput runs-made run-id                  ; store run number
  set steps-nov lput steps steps-nov                ; store steps by indiv using preference learning phase

  app-ind                                           ; find applied solution for ind, return stats-samp

  set steps-est lput item 0 stats-samp steps-est    ; store steps by indiv using preference application rules
  set sd-est    lput item 1 stats-samp sd-est       ; store sd by indiv using preference application rules
  set range-est lput item 2 stats-samp range-est    ; store range by indiv using preference application rules

  ifelse use-est-prefs
  [ ask pref-links [ set pref-coll  ( pref-coll + pref-est ) ] ] ; faster with pref-est / (last steps-est) ^ 2
  [ ask pref-links [ set pref-coll  ( pref-coll + pref-ind ) ] ]
                                           ; store preferences from this run for collective solution
                                           ; these are the novice indiv preferences include loops - including closed loops.
  ifelse use-est-prefs [                   ; store pref history for later aggregation by the collective
    ask pref-links [
      set pref-hist lput ( pref-est ) pref-hist            ; faster with pref-est / (last steps-est) ^ 2
      set id-hist lput ( runs-made - 1 ) id-hist
      ]
  ]
  ; else
  [ ask pref-links [
    set pref-hist lput (pref-ind) pref-hist
    set id-hist lput ( runs-made - 1 ) id-hist
    ]
  ]

  set steps-coll lput (length app-coll - 1) steps-coll  ; calc and store steps by collective using pref-coll

  ask ents [die]
  tick
end

to take-step
  if count ents = 0 [setup-ent]                  ;; create ent, set my-loc-node, steps->0
  ask my-loc-node [                              ;; location of ent in node-space
      set nodes-out out-link-neighbors           ; list of nodes agentset of all out links
      (ifelse
        move-method = "random-walk"             [ random-walk        ] ; random walk
        move-method = "random-no-backstep"      [ random-no-backstep ] ; end no backstep option
        move-method = "rules-from-1998"         [ rules-from-1998    ] ; optimized rules
      )
    ]  ; end ask of node where ent located

  ask my-node [                             ;; move ent and update ent state
      set my-loc-node-last my-loc-node            ; save old node
      move-to my-node-new                         ; move ent
      set my-loc-node my-node-new                 ; update node locaction for end
    ]
    set steps steps + 1
end

;;===============================================================================================
;;=========Three learning rules for solving the maze the first time =============================

to random-walk
  set my-node-new one-of nodes-out       ; get random node from nodes-out
  ask my-loc-node
   [ask out-link-to my-node-new ;increment preference (note: pref-link doesn't work
    [ set pref-ind pref-ind + 1 ]
   ]
end

to random-no-backstep
  ifelse ( count nodes-out ) = 1
    [ set my-node-new one-of nodes-out ]   ; only one node so just select it
  ;else
    [ loop [                               ; otherwise, repeat until back node NOT selected
        set my-node-new one-of nodes-out
        if not ( my-node-new = my-loc-node-last ) [
          ask out-link-to my-node-new [set pref-ind pref-ind + 1] ; as above
          stop                       ; exit loop
         ]
       ] ; end loop
    ]    ; end multi-node case

  ask out-link-to my-node-new [ set pref-ind pref-ind + 1 ]
end

to rules-from-1998            ; LEARNING rules: how an individual solves the maze the first time, see paper for word version
  ask my-loc-node [ set my-links-out my-out-links ]            ; get agentset of links out
  set my-node-link one-of my-links-out with [ pref-ind = 0.0 ] ; get any link with p=0
  let my-1-links          my-links-out with [ pref-ind = 1   ] ; temp set of links with p=1 for later (rarely occurs)
  ifelse ( is-link? my-node-link )                             ; is an untried node present, i.e., with p=0
  [ ask my-node-link
     [ set my-node-new end2   ; set my-node-new to end of link my-node-link
       set pref-ind 1         ; set preference to 1 to link going to new node my-node-new
     ]
    ask my-loc-node [ ask in-link-from my-node-new [set pref-ind 0.1] ] ; set reciprocal preference to 0.1
    ask my-1-links [ set pref-ind 0 ]                                 ; set any other links with preference = 1.0 to 0
  ]
  ;else - when p of all nodes > 0, pick link with max p
  [ ask my-links-out [ set my-node-new [ end2 ] of max-one-of my-links-out [ pref-ind ] ]  ] ; does this ask all links out?
end

;;========= multiple versions of application rules follow, applying different preferences =============================

to app-ind         ;; APPLICATION rules: how an individual solves the maze after the LEARNING stage
                   ;; based on the experiences from learning stage --> this gives pref-est for the indivdual
                   ;; see 1998 paper for word version of rules
                   ;; included option to sample multiple app rules to see if multiple paths occur <== not observed

set stats-samp []   ; output list of steps for sampling: [ mean standard-deviation range]
set steps-samp []   ; steps of each sample, applying est rules to same pref-est ==> stats-samp for sampling

repeat sampling-ind [          ;; loop over the number of samples for the same indiv preference

  ask ents [ die ]
  setup-ent                    ; locate ent at my-loc-node = start-node, sets: my-node & steps = 0
  set path-est []
  set path-est lput start-node path-est  ; initialize path-est with start-node (this gets overwritten each sample)

  ask pref-links [                 ; first rule - set all nodes to un-tried
      set pref-est 0
      set tried false
    ]

  while [ not ( my-loc-node = end-node ) ]     ;; same as app-coll but use pref-ind to find path without loops

  [ ask my-loc-node [
      let my-loc-link-set my-out-links with [ tried = false ]     ; set of untried outgoing links from my-loc-node
      ifelse any? my-loc-link-set [                               ; true - yes, there are untried nodes
        set my-node-link max-one-of my-loc-link-set [ pref-ind ]  ; pick untried link with max pref
                                                                  ; this will always be a link that has not been tried.
        set my-node-new [ end2 ] of my-node-link                  ; end2 is always the destiation node for a directed link

        ask my-node-link
        [ set tried true
          set pref-est pref-ind
        ]
        set my-loc-node my-node-new
      ]
      ; else all nodes tried, pick a random node out - basically starting over
      [ set my-node-new [ end2 ] of one-of my-out-links     ; end2 is always the destinaion node of a link
        ask ( pref-link [ who ] of my-loc-node [ who ] of my-node-new ) [ set pref-est pref-ind ]
        set my-loc-node my-node-new
;        type "steps: " print steps                  ; print a message when occurs - rare.
      ]
      set steps steps + 1
      set path-est lput my-node-new path-est        ; store new node in path-est -> save the last path
     ] ; end step taken
  ]   ; move to next node; end if at end-node

  set steps-samp lput steps steps-samp        ; store number of steps in app-ind solution

]      ; end loop over number of samples (sampling)

ifelse sampling-ind = 1
  [ set stats-samp (list (mean steps-samp) (0                            ) (max steps-samp - min steps-samp) ) ]
  ;else sampling > 1
  [ set stats-samp (list (mean steps-samp) (standard-deviation steps-samp) (max steps-samp - min steps-samp) ) ]
end

;==============================================================================================

to collective-solutions       ;; For sims of ind runs, now select ransom groups for collective
  if runs-made < max-coll-size
   [ type "ERROR max collective size must be less than current runs made: " print runs-made stop ]

  set steps-coll2 []                         ;; list accumulation over all samples and ensembles
  set sd-coll2    []
  set range-coll2 []

  set size-coll2    (range 1 (max-coll-size + 1) ) ;; used to loop over samplings of each ensemble with same preferences
  set list-ensemble (range 1 (ensemble      + 1) ) ;; used to loop over ensembles of different random groups

  foreach size-coll2 [ coll-size ->           ;; loop over collective sizes starting with 1
    set steps-ensemb []                       ; list accumulation over ensembles of same size
    set sd-ensemb    []
    set range-ensemb []

    foreach list-ensemble [ ensemb ->         ;; loop of ensemble of same size with different members in coll.

      set coll-sample []
      ask pref-links [set pref-coll2 0]

      ask one-of pref-links [ set coll-sample up-to-n-of coll-size id-hist ] ; select random members from any link (all same)
      ask pref-links [ foreach coll-sample [ n -> set pref-coll2 (pref-coll2 + item n pref-hist) ] ] ; sum prefs

;      type "coll-sample: " show coll-sample ;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

      set stats-sampc []   ; output list of steps for sampling: [ mean standard-deviation range]
      set steps-sampc []   ; output list of total steps of each sample, applying pref-coll2

      repeat sampling [ set steps-sampc lput (length app-coll2 - 1) steps-sampc ]

      ifelse sampling = 1
        [ set stats-sampc (list (mean steps-sampc) (0                             ) (max steps-sampc - min steps-sampc) ) ]
        ;else  sampling > 1
        [ set stats-sampc (list (mean steps-sampc) (standard-deviation steps-sampc) (max steps-sampc - min steps-sampc) ) ]

      set steps-ensemb lput (item 0 stats-sampc) steps-ensemb
      set sd-ensemb    lput (item 1 stats-sampc) sd-ensemb
      set range-ensemb lput (item 2 stats-sampc) range-ensemb

    ] ; end loop over realizations of a fixed size of ensemble

    set steps-coll2 lput ( precision (mean steps-ensemb) 2 ) steps-coll2
    ifelse ( stats-ensemb and ensemble > 1 )
    [ set sd-coll2    lput (precision (standard-deviation steps-ensemb) 2) sd-coll2     ; this gives sd of ensemble
      set range-coll2 lput (max steps-ensemb - min steps-ensemb ) range-coll2  ; this gives range of the ensemble
    ]
    [ set sd-coll2    lput  (precision (mean sd-ensemb) 2 )  sd-coll2    ; this gives the ave sampling sd, not ensemble ave
      set range-coll2 lput  mean range-ensemb range-coll2
    ]

  ] ; end loop over ensemble sizes

  ;; plot results
  clear-plot
  set-current-plot-pen "step-size"
  foreach size-coll2 [ n -> plotxy    n  ( item ( n - 1 ) steps-coll2 ) ]
  if ( plot-ref-data ) [
    set-current-plot-pen "step-1998"
    foreach size-coll2 [ n ->
      if n > length steps-1998 [stop]
      plotxy    n  ( item ( n - 1 ) steps-1998 )
    ]
  ]
  if plot-stats [
    set-current-plot-pen "step-sd"
    foreach size-coll2 [ n -> plotxy  n  ( item ( n - 1 ) sd-coll2    ) ]
    set-current-plot-pen "step-range"
    foreach size-coll2 [ n -> plotxy  n  ( item ( n - 1 ) range-coll2 ) ]
  ]
end

to-report app-coll ;[id-pref] ; applied rules using prefs - pref-ind, pref-est, pref-coll, pref-coll2
                              ; return path-list - list of nodes in the path
  let id-pref 2
                              ; following coding is identical to app-coll2
  ask ents [ die ]
  setup-ent ; move to start-node, sets my-loc-node, my-node,  steps = 0.
  set my-loc-node start-node
  let steps-taken 0
  let path-list []
  set path-list lput start-node path-list            ; initialize path-list with start-node
  ask pref-links [ set tried false ]

  while [ not ( my-loc-node = end-node ) ] [
    ask my-loc-node [
      let my-loc-link-set my-out-links with [ tried = false ]  ; note that the incoming link will always be true
      ifelse any? my-loc-link-set [
        set my-node-link max-one-of my-loc-link-set [
          (ifelse-value
              id-pref = 1     [ pref-ind   ] ; novice pref
              id-pref = 2     [ pref-coll  ] ; aggregated individual pref
              id-pref = 3     [ pref-coll2 ] ; ensemble pref
           )
         ]
        set my-loc-node [ end2 ] of my-node-link               ; end2 is always the destiation node for a directed link
        ask my-node-link [ set tried true ]
      ]
      ; else all nodes tried, pick a random node out
      [ set my-loc-node [ end2 ] of one-of my-out-links ]      ; any outgoing link: end2 is destination node

      set steps-taken steps-taken + 1
      set path-list lput my-loc-node path-list
    ]
  ]
 report path-list ; list of nodes in path
end

to-report app-coll2 ; [id-pref] ; applied rules using prefs - pref-ind, pref-est, pref-coll, pref-coll2
                                ; return path-list - list of nodes in the path
  let id-pref 3
                             ; following coding is identical to app-coll
  ask ents [ die ]
  setup-ent ; move to start-node, sets my-loc-node, my-node,  steps = 0.
  set my-loc-node start-node
  let steps-taken 0
  let path-list []
  set path-list lput start-node path-list            ; initialize path-list with start-node
  ask pref-links [ set tried false ]

  while [ not ( my-loc-node = end-node ) ] [
    ask my-loc-node [
      let my-loc-link-set my-out-links with [ tried = false ]  ; note that the incoming link will always be true
      ifelse any? my-loc-link-set [
        set my-node-link max-one-of my-loc-link-set [
          (ifelse-value
              id-pref = 1     [ pref-ind   ] ; novice pref
              id-pref = 2     [ pref-coll  ] ; aggregated individual pref
              id-pref = 3     [ pref-coll2 ] ; ensemble pref
           )
         ]
        set my-loc-node [ end2 ] of my-node-link               ; end2 is always the destiation node for a directed link
        ask my-node-link [ set tried true ]
      ]
      ; else all nodes tried, pick a random node out
      [ set my-loc-node [ end2 ] of one-of my-out-links ]      ; any outgoing link: end2 is destination node

      set steps-taken steps-taken + 1
      set path-list lput my-loc-node path-list

    ]
  ]
 report path-list ; list of nodes in path
end

;==============================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annimation Procedure  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to animate-path [path-listx]
  ask ents [ die ]
  setup-ent

  foreach path-listx
  [ node-pos ->
    ask ents [ move-to node-pos ]
    display
    wait 1 / grid-size
  ]
end
;==============================================================================================
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup Procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  if not fix-random [ set starting-seed new-seed ]
  random-seed starting-seed
  set-default-shape nodes      "circle"
  set-default-shape pref-links "small-arrow-link"
  set-default-shape ents       "person"

  set runs-made 0

  ifelse load-file     ; load maze from file? otherwise generate maze if keep-maze is false
    [ load-maze ]
    [ generate-maze ]

;; SET start and end nodes, all shapes and colors
  set start-node min-one-of nodes [xcor + ycor] ; assume start node is lower left of all nodes
  ask start-node [ set color green ]
  set end-node max-one-of nodes [xcor + ycor]
  ask end-node [ set color red ]

  ask pref-links [
    set pref-ind 0.0
    set pref-hist []
    set id-hist []
  ]

  set steps-nov   []
  set steps-est   []
  set sd-est      []
  set range-est   []
  set steps-coll  []
  set steps-coll2 []
  set path-est    []               ; list of nodes on the path taken by established ind during the last simulation
;  set path-coll   []               ; list of nodes on the path taken by collective
;  set path-coll2 []               ; list of nodes on the path taken by collective using random groups
  set run-id      []               ; list of the number of runs, equal to [ 1 2 3 ... # of runs]

  setup-ent                        ; agent that solves maze

  set-up-1998-data

  reset-ticks
end

to set-up-1998-data
  set steps-1998 [12.76 13.86 12 12.28 11.74 12.12 11.56 11.66 11.02 10.78 11.14 11.2 11.02 10.62 10.88 10.62 11.08 10.56 10.48 10.02 10.4 10.06 10.56 10.32 10.46 10.52 10.28 10.32 9.98 10.32 10.14 10.28 10.1 10.08 10 10.2 10.14 9.8 10.24 10.38 10.14 9.88 9.88 10.18 10.1 9.9 9.9 10.04 9.78 10.02]
end

to setup-ent           ; ADD one ent who moves on nodes. Locate at Start node at lower left.
  create-ents 1 [
    set color white
    move-to start-node            ; move ent to start node
    set my-loc-node start-node    ; only one member in set
    set my-node turtle who
    set steps 0
  ]
end

to load-maze
  file-open "base-maze-attributes.txt"
  ;; data on the line is in this order: node-id attribute1 attribute2
  while [not file-at-end?]  [
      let items read-from-string (word "[" file-read-line "]")
      create-nodes 1
      [
        set node-id item 0 items
        set size    item 1 items
        set color   item 2 items
        setxy                       ; NLJ Set up rows 7 by 5 = 35 nodes
                                    ; center maze between -9,-9 to 9,9
           (( remainder (node-id + 6) 7 )   * (18 / 6)) - 9
           (int ( ( node-id - 1.0 ) / 7.0 ) * (18 / 4)) - 9
      ]
  ]
  file-close

;; The direct link file "base-maze-links.txt" has 3 columns separated by spaces
;; 1: node-id of the node originating the link (end1)
;; 2: node-id of the node on the other end of the link (end2)
;; 3: strength of the link (not used)

  file-open "base-maze-links.txt"

  while [not file-at-end?]  [
    ;; this reads a single line into a 3-item list
    let items read-from-string (word "[" file-read-line "]")
    ask get-node (item 0 items) [
      create-pref-link-to get-node (item 1 items) [
          set pref-ind 0
          set pref-est 0
          set pref-coll 0
          set pref-coll2 0
      ]
    ]
  ]
  file-close
end

to-report get-node [id]
  report one-of nodes with [node-id = id]
end

to generate-maze
  ; CREATE the grid of nodes by sprout node in each patch.
  ; Therefore patches determine # of nodes, max patches is 20 in each direction.

  let node-counter 0

  ask patches with [abs pxcor < (grid-size / 2) and abs pycor < (grid-size / 2)]
    [
      set node-counter node-counter + 1
      sprout-nodes 1 [   ; Creates number new turtles on the current patch.
        set color blue
        set node-id node-counter
      ]
    ]

  ; CREATE a directed network such that each node has a LINK-CHANCE percent chance of
  ; having a link established from a given node to one of its neighbors,
  ; and make some active and others inactive
  let shape-size ( 1 - 0.03 * ( grid-size - 3 ) )

  ask nodes
  [
    set size shape-size
    ; get a list of all the neighbor turtles (the nodes)
    let neighbor-nodes turtle-set [turtles-here] of neighbors4
                        ; turtle-set reports an agentset containing neighboring turtles
                        ; neighbors4 reports an agentset containing the 4 x-y surrounding patches

    ; remove nodes from neighbornodes with chance 1-link-chance
    ask neighbor-nodes with [ random-float 100 > link-chance ] [ die ]

    ; create pref-links to all neighbor-nodes
    create-pref-links-to neighbor-nodes
    [                   ; for link command: create-<breed>-to turtle [ commands ]
                        ; where create breed links are pref-links)
      set pref-ind 0    ; set link's value to zero
      set pref-est 0
      set pref-coll 0
     ]                  ; end of create link command
  ]                     ; end of ask nodes

  ; spread the nodes out in space - now original patch location is lost.
  ask turtles
  [
    setxy (xcor * (max-pxcor - 1) / (grid-size / 2 - 0.5))
          (ycor * (max-pycor - 1) / (grid-size / 2 - 0.5))
  ]  ; end of ask turtles loop, note this includes nodes and ent

end
@#$#@#$#@
GRAPHICS-WINDOW
254
53
571
371
-1
-1
14.7143
1
10
1
1
1
0
0
0
1
-10
10
-10
10
1
1
0
steps
30.0

BUTTON
8
350
240
383
 2. SETUP maze and initialize the sim                   
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
163
472
245
505
Run nonstop 
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
12
191
240
224
link-chance
link-chance
90
100
97.0
1
1
%
HORIZONTAL

SLIDER
12
154
240
187
grid-size
grid-size
3
19
7.0
1
1
nodes per side
HORIZONTAL

PLOT
252
373
571
631
Steps taken for Novice, Established & Collective
individuals sampled or aggregated
Steps to end node
0.0
20.0
0.0
30.0
true
false
"" "if runs-made = 0 [stop]"
PENS
"steps-coll" 1.0 0 -1604481 true "" "plotxy runs-made last steps-coll"
"steps-nov" 1.0 2 -14439633 true "" "plotxy runs-made last steps-nov"
"steps-est" 1.0 2 -13345367 true "" "plotxy runs-made last steps-est"

MONITOR
515
389
570
430
# of runs
runs-made
0
1
10

SWITCH
14
96
134
129
load-file
load-file
0
1
-1000

MONITOR
634
299
728
344
ave learning
mean steps-nov
2
1
11

CHOOSER
12
228
240
273
move-method
move-method
"random-walk" "random-no-backstep" "rules-from-1998"
2

TEXTBOX
9
395
246
429
3. RUN simulation\nstep-by-step, run once, or non-stop
13
15.0
0

BUTTON
9
471
94
505
Take one step
ifelse my-loc-node = end-node \n [ask ents [die] \n  setup-ent\n ]\n [take-step]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
94
472
164
505
Run once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
14
136
252
154
Random maze parameters - size and links
11
62.0
1

TEXTBOX
15
59
255
92
1.  SELECT random or uploaded maze\n    SELECT individaul solution method\n
13
15.0
1

MONITOR
576
550
653
595
collective
last steps-coll
2
1
11

MONITOR
632
343
728
388
ave established
mean steps-est
2
1
11

MONITOR
728
299
786
336
stnd dev
standard-deviation steps-nov
1
1
9

MONITOR
727
343
785
380
stnd dev
standard-deviation steps-est
1
1
9

TEXTBOX
12
521
248
644
                     **PLOT LEGEND**\nGREEN dot: # of steps by individual using \"move-method\" rules => Novice solution\nBLUE dot: # of steps by individual using Applied Rules => Established solution\nRED line: # of steps by a collective using an aggregate of preferences for all runs completed => Collective solution
11
62.0
1

TEXTBOX
582
243
875
313
***** INDIVIDUAL STEPS STATISTICS ******\n- Left column: Steps taken in current run\n- Right three columns: mean, standard deviation, and max range of the steps by individuals over all runs
11
0.0
1

TEXTBOX
137
94
239
135
CHOOSE MAZE TYPE\nOff: random maze\nOn: load maze
10
0.0
1

BUTTON
662
205
767
238
Established path
animate-path path-est
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
581
145
860
201
ANIMATE: Click to animate the last solution path.  NOTE: for a very long path, increase \"grid-size\" at left to speed up animation or teminate by selecting HALT in the Tools menu. 
11
62.0
1

MONITOR
784
299
847
336
max range
max steps-nov - min steps-nov
0
1
9

MONITOR
785
343
848
380
max range
max steps-est - min steps-est
0
1
9

BUTTON
581
205
663
238
Novice path
animate-path path-nov
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
580
757
777
790
Plot for samples and ensembles
collective-solutions
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
9
432
134
465
use-est-prefs
use-est-prefs
0
1
-1000

MONITOR
577
299
634
344
novice
last steps-nov
17
1
11

MONITOR
577
343
634
388
estab.
last steps-est
2
1
11

TEXTBOX
654
555
909
595
<== Collective steps using an aggregate of preferences from all individual runs: Hence, the collective has same ablity as the individual but used super information.
9
103.0
1

TEXTBOX
21
10
868
52
MAZE SOLUTION BY AN INDIVIDUAL WITH ONLY A LOCAL PERSPECTIVE, USING ONE OF 3 LEARNING RULES. FIRST, NOVICE INDIVIDUALS SOLVE THE MAZE AND THEN OPTIMIZES THEIR NOVICE SOLUTION BY USING THE APPLIED RULES, ELIMINATING LOOPS. THEN, A COLLECTIVE PATH IS FOUND BY USING THE SAME APPLIED RULES ON AN AGGREGATION OF INDIVIDUAL PREFERENCES: COLLECTIVE HAS THE SAME ABILITY BUT USES MORE DIVERSE INFORMATION.  
11
0.0
1

TEXTBOX
136
431
250
469
Use Novice or Established preferences in Collective preferences
9
62.0
1

TEXTBOX
584
55
867
157
4. ANALYZE simulations\n- Animate the last paths\n- Examine steps vs # of runs in plot\n- Statistics of mean of steps over runs\n- Plot collective steps vs # in collective
14
15.0
1

SWITCH
107
307
239
340
fix-random
fix-random
0
1
-1000

INPUTBOX
10
281
108
341
starting-seed
-1.993727914E9
1
0
Number

TEXTBOX
111
284
246
306
To reproduce a maze and simulations, see Info
9
0.0
1

TEXTBOX
577
480
912
548
The COLLECTIVE solution is the path found by using the same APPLIED rules as for the individual Established path above, but using an aggregate of the individual preferences from the above runs.  The toggle  \"use-est-pref\" selects the Established preferences when up, Novice prefs when down (must be set before individual runs). 
10
0.0
1

SLIDER
579
725
777
758
max-coll-size
max-coll-size
1
400
53.0
1
1
individuals
HORIZONTAL

BUTTON
766
205
861
238
Collective
animate-path app-coll
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
577
864
668
897
Show last path
set path-coll2 app-coll2\nanimate-path path-coll2
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
10
638
570
897
Collective Steps vs Number in collective
Number in collective (random selection)
Steps by collective
1.0
10.0
9.0
15.0
true
true
"" "clear-plot"
PENS
"step-size" 1.0 0 -2674135 true "" ""
"step-sd" 1.0 2 -4079321 true "" ""
"step-range" 1.0 2 -13345367 true "" ""
"step-1998" 1.0 0 -8330359 true "plot-pen-down" ""

SLIDER
579
693
685
726
sampling
sampling
1
100
1.0
1
1
NIL
HORIZONTAL

MONITOR
577
793
639
830
ave step
mean steps-coll2
2
1
9

MONITOR
639
793
697
830
stnd dev
mean sd-coll2
2
1
9

TEXTBOX
577
638
897
691
Plot options below: \"sampling\" examines the variation in the collective steps for a fixed collective preference;  \"ensemble\" examines the variation due to groups of different random members: one ensemble is one random group of a certain size
10
62.0
1

SLIDER
684
693
776
726
ensemble
ensemble
1
200
50.0
1
1
NIL
HORIZONTAL

SLIDER
579
423
684
456
sampling-ind
sampling-ind
1
20
1.0
1
1
NIL
HORIZONTAL

MONITOR
684
423
754
460
ave samp sd
mean sd-est
2
1
9

MONITOR
753
423
835
460
ave samp range
mean range-est
1
1
9

TEXTBOX
851
339
908
383
stats from all ind applied runs
9
0.0
1

TEXTBOX
840
423
900
459
average stats from each sampling
9
0.0
1

TEXTBOX
849
301
898
340
stats over all learning 
9
0.0
1

MONITOR
696
793
753
830
range
mean range-coll2
2
1
9

TEXTBOX
756
793
906
826
<== sd and range stats change depending on \"stats-ensemb\" switch
9
0.0
1

MONITOR
577
828
639
865
last step
last steps-coll2
2
1
9

MONITOR
639
828
697
865
stnd dev
last sd-coll2
2
1
9

MONITOR
696
828
753
865
range
last range-coll2
2
1
9

TEXTBOX
753
828
903
861
<== Mean statistics for the last ensemble in the plot, incl sampling
9
0.0
1

TEXTBOX
778
728
913
761
<== Maximum collective size for the plot < \"# of runs\"
9
0.0
1

TEXTBOX
578
465
872
493
****** COLLECTIVE STEPS STATISTICS *******
11
0.0
1

BUTTON
576
595
778
633
Animate last collective path
set path-coll app-coll\nanimate-path path-coll
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
778
596
902
633
steps in animation
length path-coll - 1
0
1
9

MONITOR
669
862
766
899
steps in animation
length path-coll2 - 1
0
1
9

SWITCH
777
758
901
791
plot-stats
plot-stats
1
1
-1000

SWITCH
776
693
900
726
stats-ensemb
stats-ensemb
0
1
-1000

TEXTBOX
768
863
918
896
<== repeated use of \"Show...\" will illustrate different paths taken for the same preferences\n
9
0.0
1

SWITCH
448
897
572
930
plot-ref-data
plot-ref-data
0
1
-1000

TEXTBOX
582
392
889
434
\"sampling-ind\" repeats Applied rules on learning prefs - to see if multiple path lengths occur (set before running)
11
0.0
1

TEXTBOX
576
904
908
935
<== Plots the reference data for the 1998 sims using Mathematica for an ensemble of 50, for max-size of 50, and with no sampling
10
0.0
1

TEXTBOX
10
506
260
524
\"Take one step\" is for movement demonstration. 
9
0.0
1

@#$#@#$#@
## A "Wisdom-of-the-crowds” agent simulation that demonstrates emergent collective problem solving: How a diverse collective can have higher performance than an individual or expert by aggregating diverse information, rather than better rules. The collective has the same smartness as the individuals. There also is no cooperation between the individuals, and there is no selection by some metric (like performance) to participate in the collective. The collective performance correlates with a diversity of individual contributions, reproducing Norman L. Johnson's 1998 maze simulations and study (1998 citation at the end).

## WHAT IS IT?     WHY IS THIS MAZE PROBLEM IMPORTANT?
LEARNING BY INDEPENDENT INDIVIDUALS
An individual agent solves an "English-hedge” maze: the agent can only see the choices at an intersection, has knowledge (bread crumbs) of paths explored, and has no knowledge of how far to the end. The maze has multiple shortest paths (14 in the 1998 maze). The Novice solved the maze initially using one of three "learning" rules: random walk, random walk without back step, and an optimized rule set from the 1998 paper. The agent's “learning solution" of the path through the maze from start to end is independent of all other individuals: there is no cooperation or sharing of information between individuals.  

One simulation run is for one agent.  Two runs, two different agents, etc. All agents have the same capabilities: they use identical learning rules.  Only their experience differs as they explore the maze along different paths. 

THE MAZE OPTIONS
The maze can either be a randomly-generated square maze or a maze loaded from two files (the maze used in the 1998 paper below). A maze represents a sequential problem with multiple decision steps leading to a final goal, similar to a decision tree.  Most academic studies for testing agent-based models are single challenges or puzzles (e.g., Prisoners’ dilemma or the two-armed bandit), often solved repeatedly. The maze problem is more like a real-world problem with multiple decision points, path dependencies, and multiple solutions. 

NO GLOBAL PERSPECTIVE
The learning rules have only a local perspective at each node in the maze: how many paths out of the node and any preference history (the “breadcrumbs") where the agent has visited, similar to how an ant solves a foraging problem. At the end of one agent’s simulation, the agent retains the knowledge (history of breadcrumbs) of the links discovered during the learning stage.  Note that unlike the pheromones of ants foraging for food, the “breadcrumbs” do not lessen over time.  
When the endpoint is found, the individual does not know whether or not the path found is long or short relative to any other individual. Similar to the English hedge maze.

APPLIED STAGE
When an individual solves the maze a second time by applying their learned preferences (stored at the links between the nodes), she uses an Application rule set that eliminates any closed loops in their original path, thereby shortening her applied path. This "ESTABLISHED path” is the path you'd take the get to a restaurant after you eliminated the part of the path where you drove around the block needlessly to get to the same point. 

COLLECTIVE SOLUTION
The preferences from a group of independent individuals are aggregated without weighting. The same Applied rules used by the individual are used to find a “COLLECTIVE" path or solution. Hence, the collective has the same capability or skills as the individual but has access to more information. 

THE COLLECTIVE PATH USING THE PREFERENCES OF MANY INDIVIDUALS CONVERGES TO THE SHORTEST PATH IN THE MAZE. 
This collective path is an EMERGENT property because no rule selects a shorter or shortest path. An emergent property is a global property that cannot be determined from local, or in this case individual, rules and information.  Why does the shortest path occur? In the same way the individual can eliminate loops in their path, the collective information combines to eliminate all loops connecting the start and end nodes. 

ROLE OF DIVERSITY
As discussed in the paper below, IN THE MAZE STUDY, THE HIGHER THE DIVERSITY OF THE COLLECTIVE, THE MORE LIKELY AN OPTIMAL PATH IS FOUND. “Diversity" of a collective is defined as the extent of unique contributions by the individuals at a node to the group and is a property of the group (one individual cannot be "diverse") and the domain (the maze). In common understandings of the mechanisms that lead to increased performance, such as "survival of the fittest" in evolution or "hiring the best candidate" in organizations, THE PERFORMANCE OF THE COLLECTIVE IS INCREASED BY SELECTION FROM AND THE REDUCTION OF DIVERSITY. In this maze study, any selection of the individuals or their contributions, such as only using the highest performers in the collective preferences, causes lower diversity and the path to degrade! See examples of this in the 1998 paper.  

USE OF THIS MAZE STUDY IN THE BOOK: WISDOM OF THE CROWDS
The first technical example in James Surowiecki's 2004 book "Wisdom of the Crowds" is this maze problem based on the 1998 paper. 
Surowiecki interviewed Johnson for a New Yorker article in 2001 on collective performance to explain the inability of professional investors to repeatedly beat the stock market average, primarily made up of a diversity of amateur investors. This New Yorker article was expanded into a book called the Wisdom of the Crowds. 

* REFERENCE: Johnson, N. L. "Collective Problem Solving: Functionality beyond the individual," Los Alamos National Laboratory Report, Los Alamos, NM LA-UR-98-2227, Nov 1998. Download at http://collectivescience.com/publications.html

## HOW IT WORKS
STARTING
The individual begins at the starting node and at each node on the path examines how many options are available on where to go next. Each link leading out of a node is a possible choice. 

MOVING
The individual will pick and move to a new node depending on the chosen learning rule. The individual will mark the links connecting the nodes using different strategies for each set of learning rules - like leaving breadcrumbs to find your way in a maze.
* If the Random Walk rule is used, an exit node is randomly selected from all possible nodes, and the link used is marked (the preference of the linked used is incremented by 1 - all preferences start at zero). 
* If the Random Walk without backstepping is chosen, then the link used to arrive at the current node is excluded from the random choice if possible, and the new link is marked.
* The "rules-from-1998" most resemble using breadcrumbs to capture the last used path and mark unused paths. Paths are marked so that previously tried links tried are not tried again. 

APPLIED STAGE - SOLVING THE MAZE AGAIN
After the individual finds the endpoint using any of the three learning rules, the maze is solved again from the start point, using the Applied rules and the preferences from the learning stage. The Applied rules try to pick the link with the strongest preference (most breadcrumbs) but include additional logic to prevent infinite loops. 

HOW THE COLLECTIVE SOLVES THE MAZE
The preferences for all the links found by an individual for each run up to the current run are aggregated to create a Collective preference for each link.  As each run is completed, the individual preferences are added to the Collective preference. With 20 runs, 20 different individual preference histories are aggregated. Depending on the toggle setting, the learning or established preferences are passed to the collective. 

## HOW TO USE IT

1.CHOOSE the type of maze using the toggle switch: 1) randomly-generated maze or 2) load a maze from two files. The start node will always be on the lower left and the end node (red) on the upper right.  If a generated maze is selected, 

* CHOOSE the complexity of the network that you want to model using the GRID-SIZE slider and 
* CHOOSE the density of links in the network using the LINK-CHANCE slider. 

WARNING: you can generate a maze with no solution where the start and finish nodes are NOT connected by links, which causes the simulation to run forever. As an omniscient being, if this happens, regenerate the maze.

CHOOSE a “move-method” from one of the three Learning Rules for the individual to solve the maze. 

OPTION: If you wish to reproduce a set of runs, toggle the "fix-random" switch up and put in any integer.  You can also copy the "starting seed" with the switch down, enter it into the "starting seed” box, and reproduce a prior set of simulations with the switch up. This option is useful for the regeneration of identical mazes and results. 

CHOOSE if the collective uses the full Learning preferences (including loops) or the preferences from the path found with the applied rules (omitting all loops).  

2.PRESS SETUP to generate or load a maze and set up the simulation parameters.

3.RUN THE SIMULATION to make an individual solve the maze. Three options:

* TAKE ONE STEP using the select learning rules, node by node. For demonstration only: This option does not update the simulation nor  store the stats for analysis.
* RUN ONCE: The individual will solve the maze and stop at the endpoint. 
* RUN WITHOUT STOPPING - the simulation will repeat the solution with a new individual each run until the button is pressed again. 

The simulations can be continued after step #4 below, expanding the number of individual runs for analysis. 

4.ANALYZE THE SIMULATIONS SO FAR

ANIMATE THE CURRENT PATH.  There are three options:

- NOVICE PATH: an individual steps along the nodes as found during the learning stage. 
- ESTABLISHED PATH: the path taken by the individual when the applied rules are used - eliminating unnecessary closed loops. 
- COLLECTIVE PATH: the path found by using the applied rules on the aggregate of the preferences so far. If 20 simulations run, then 20 individual preference histories are aggregated. 

PLOTS AND STATISTICS
Simulation data analysis is divided into individual runs analysis and collective analysis. 
* Individual Step Statistics: for all the runs so far for the individual. The statistics for the learning and established rules are displayed for the last run made (left column) and for all the individual runs (right columns). 
OPTION: Using the “sampling-ind” toggle, multiple applications can be made of the Applied rules to each individual to see if multiple path steps occur (this is rare).  
*Collective Step Statistics:
1.The upper block is for a run with an aggregate of all the individual preferences so far. You can view the path of this solution (repeated use of this option will identify if there are multiple paths possible). 
2.The lower block of options examine the statistics of different collective sizes from one to the maximum choosen and plots the results. Samplling slider selects the number times the Application rules are applied to one selction of individuals.  The Ensemble slider selects a random grouping of individuals for each collective size. The total number of simulations for the plot is equal to sampling * ensemble * max-coll-size. You can view the last path in the generation of the plot (repeated use of this option will identify if there are multiple paths possible).

To compare the NetLogo results with the results from the 1998 Mathematica simulations, set ensemble=50 sampling=50 max-coll-size=50, and toggle one the "plot-ref-data" below the plot. Results will not be identical because of randomness in the solution. 

## THINGS TO NOTICE
Most of these observations are discussed in detail in the 1998 paper. 

1.Compare the three different learning rules for solving the maze.  How does the mean number of steps differ over many runs? How does the standard deviation differ? Why does the range of steps for the random walk method never converge with many runs?  

2.Compare the statistics for the learning (Novice) and applied (Established) rules over many runs.  How does the Novice solution with the applied rules differ from the Established path? Why? Why does the random walk do so poorly? (See #7 below.) Why does the elimination of the back step make such a difference? 

3.Compare the animations of the Novice and Established paths (easiest to see in a simple random maze). The loops that don't contribute to the shorter individual path are eliminated (usually) in the Established paths.  But also note the incomplete loops unclosed by one link are NOT eliminated because the individual does not have a global perspective that includes nearby nodes.  How does this compare to ant foraging solutions using pheromones? 

4. How does the use of Novice versus Established preferences affect the Collective solution, particularly the convergence to the shortest path (use the toggle "use-est-pref" to explore)? Why would including the closed loops make the collective solution slower to converge? 

5.As individual preferences are added to the collective preferences by each additional run, observe how the collective solution eliminates unclosed loops, leading to a shorter path. This is the “Wisdom of the Crowds” advantage.  Often the collective will outperform the best performer, illustrating how, for grand challenges where experts disagree, collectives can often find solutions. See the reference below on “Applied science of collective intelligence."

6.A quick consistency check: The steps in the collective solution are the same as the steps in the individual solution at the end of the first run because the Applied rules are the same for the individual and the collective, and the preferences used in the rules are identical. You can validate this by observing that the toggle "use-est-prefs” makes no difference: the preferences used in the Established path by the individual are given to the collective, but these applied preferences reproduce the Established path using the Learned preferences. 

7.Notice that the collective solution using the random-walk learning rules does not converge to a shorter path. This is because the random selection of nodes does not have any useful information, even a weak signal, that the collective can amplify. The rules with random-walk without backstep have some information, so the collective solution marginally improves the individual average solution. The 1998 rules using better bread-crumbs have the most useful information, and the collective solution converges quickly to a shorter path if the maze is not too complex (see the next item). 

8.Compare larger and larger generated mazes.  Notice how the complexity of the maze affects the final number of steps of the novice, established, and collective. For simple generated mazes (say 3X3), all three provide the shortest Established path. Hence for simple problems, there is no advantage in creating a collective solution.  As the maze becomes more complex, the collective solution is better than the average individual solution. But as the maze becomes highly complex, the collective solution fails to find the shortest path (although it does find a shorter path than the average individual until the maze gets too complex). Hence, there is a sweet spot of problem complexity for the collective advantage over the average individual.  But when the individual solution degenerates into a random solution for some increased complexity of the maze, as will occur for any set of rules, there is no weak signal for the collective to amplify, so the collective solution also becomes a random walk.  

9.Because most mazes have multiple shortest paths (the 1998 maze supplied has 14 unique shortest paths!), the collective solution can randomly flip from one to another while maintaining a shortest path solution, illustrating robustness in the collective solution. Robustness is defined as when a solution is accurate, even if noise is introduced or the problem changes.  For example, in the current simulations, the collective can switch to any of the other 13 paths if one of the shortest paths is blocked. 

REFERENCE: Johnson, N.L., "Applied Science of Collective Intelligence: Solving the grand challenges facing humanity." Spanda Journal 5(2), 97-108 (2014).

## THINGS TO TRY
1.Try creating your own mazes and loading them into the program.  The two files required are: "base-maze-attributes.txt" and "base-maze-links.txt".  See the Code for a description of what is required in the file. What types of mazes have the property of exhibiting collective improvement?  (SEE a published paper that studied this general problem.)

2.Can you reproduce the results from the 1998 paper that used Mathematica for the simulations and analysis?  

3.Use the random maze option to generate a very simple maze.   If most of the individuals solve the maze perfectly, how does that affect the collective solution? What can you conclude about the complexity of the maze and the advantage provided by the collective solution? 

4.Use the random maze option to generate a very difficult maze. How does this type of maze affect the collective improvement?  Will using more individuals in the collective solution improve the solution?   Can you generalize the results from #3 and #4 on how complexity affects the collective solution?  See the following reference for a discussion. 

## EXTENDING THE MODEL

1.Try changing the rules for learning and solving the maze using only local information. For example, can you improve the random walk solution by adding pheromone-like behavior in the preferences? (see #9 below)

2.There are many components in the “1998 rules”; what happens if a part is removed or modified?  

3.In the 1998 paper, the effect of modifying the individual's contribution to the collective preferences was examined. Consider these:

* The individual contributes all their novice preferences (small to large) with the same intensity.  That is, each individual shares their choices with equal magnitude (e.g., secondary choices are weighed the same as first choices). 
* The individual is uncertain of their preferences and randomly contributes one preference to the collective.  Hint, the outcome is disastrous. 
* The collective selects a random individual to lead at each node from all the individuals that make up the collective. (Hint: this requires storing the individual's preferences for each run and then selecting the output preference from one of the individuals in the collective to contribute at each node.)

4.What if slow learners die if they don’t find the end node in a reasonable time?  How does this affect the collective solution?  The 1998 paper found that creating a collective of moderate performers did better than a collective made up of only the highest performers (the ones with the shortest path).  How would eliminating the slowest learners affect the collective outcome? How is the diversity changed?  This is relatively easy to program.  What if this is taken to an extreme and only the top individual performers contribute to the collective? 

5.What if the connections between the nodes are one way - you can leave, but you can’t return by that path?  How does that change the collective performance results? (This requires only a different set-up of the maze so that one directed link between nodes is generated.)

6.What if the individuals learn with a different endpoint than the collective?  Is the information on the connectivity within the maze applicable to different starting and endpoints? The real-life equivalent of this question: Is knowledge attained in one problem helpful to solving a problem in a similar domain?  (See the 1998 paper for this study.)

7.What if a genetic algorithm is used to find optimal rules for the individual? Does the collective solution still exhibit an improvement? (This was the method used in 1998 to originally develop the optimized learning rules.)

8.What if competition between agents were introduced during the learning stage, such that many agents start but only the first few to arrive at the endpoint survive to contribute to the collective?  How does this change collective performance? Is this identical to #4 above? (See the 1998 paper for this study.)

9.Implement and evaluate the diversity measures in the 1998 paper and observe if they correlate with the convergence to the shortest path? 

10.Try weighting the individual preferences in forming the collective aggregate preference to mimic the effect of pheromones evaporating in the ant-foraging problem.  Two options: 1) as the individual takes more steps, decrease the increment of the preferences, causing shorter paths to have higher contributions to the collective, and 2) weight the entire preference contribution by the inverse of the established path length of the individual.  How do these change the convergence to the shortest path with number of individuals in the collective?  How does this weighting change the diversity of the collective? (This is not studied in 1998.)


## NETLOGO AND SIMULATION APPROACH
There are three types of agents used in the simulations
1. nodes that are junctions in the network, 
2. directed links that connect the notes, and 
3. ents (entities) that solve the maze.  

Normally in a simulation of a maze (such as in the Mathematica simulation used in the 1998 paper), the individuals or groups as the ents would carry learned information with them.  But because of the nature of NetLogo to restrict information to agent types, the learned information of the ents is stored in the links connecting the nodes, maybe similar to how pheromones are stored in the domain rather than in the memory of the ants. This approach reduces the transfer of information to the links for the solution of the maze and greatly speeds up the simulation.  Hence, the history of each run is stored on each link in pref-hist - a list of lists, rather than as a global variable. This requires larger memory storage because of the duplication but simplifies the coding and speeds up the simulations. 

## HOW TO CITE

Norman L. Johnson, private communication (2022).  norman@santafe.edu

Johnson, N. L. "Collective Problem Solving: Functionality beyond the individual," Los Alamos National Laboratory Report, Los Alamos, NM LA-UR-98-2227, Nov 1998. Available at: http://collectivescience.com

Download the NetLogo software at: http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## VERSION HISTORY
11 Dec 2022 v2.0 - updated interface, validated with 1998 results using new plots statistics at bottom of interface, cleaned up coding. The Info page was updated to reflect the new simulations and analysis options.  
Nov 2022 - V1.0  - first released version
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

small-arrow-link
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 120 180
Line -7500403 true 150 150 180 180
@#$#@#$#@
0
@#$#@#$#@
