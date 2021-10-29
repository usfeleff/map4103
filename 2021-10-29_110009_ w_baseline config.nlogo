extensions [ gis ]

breed [ households household ]
breed [ seeds seed ]

patches-own [
  tract-id
  population
]

households-own [
  parent-tract
  rental?
  expiration
  renew?
  neighborhood-seeds
  neighborhood-rentals
]

seeds-own [
]

globals [
  stp-roads-dataset
  stp-tracts-dataset
  stp-acommodations-dataset
  stp-landmarks-dataset
  tract-ids

  total-aggregate-households ;; storing this avoids recounting a fixed number each tick

  display? ;; records if the display has been toggled

  events-schedule ;; stores which weeks have events

]



;;;;; setup

to setup

  clear-all

  if-else force-repeatable? = true [
    random-seed 42
  ][
    random-seed new-seed
  ]

  setup-gis

  ;; init random number generator
  ;; random-seed new-seed

  ask patches with [ population >= household-aggregation ] [
    sprout-households population / household-aggregation [

      set parent-tract [ tract-id ] of myself ;; stores the tract-id of the patch the household is on
      set rental? false
      set expiration 0
      set neighborhood-seeds 0
      set neighborhood-rentals 0

      set size 0.75
      set shape "dot"
      set color white
      set color lput 125 extract-rgb color ;; ~50% alpha

      ;; jitter markers
      set heading random 360
      fd 0.1
    ]
  ]

  make-seeds

  set total-aggregate-households ( count households )

  reset-ticks
end


to setup-gis
  set stp-roads-dataset gis:load-dataset "data/roads.shp"
  set stp-tracts-dataset gis:load-dataset "data/tracts.shp"
  set stp-acommodations-dataset gis:load-dataset "data/accomodations.shp"
  set stp-landmarks-dataset gis:load-dataset "data/landmarks.shp"
  set tract-ids []

  gis:set-world-envelope (gis:envelope-union-of (gis:envelope-of stp-roads-dataset) (gis:envelope-of stp-tracts-dataset))
  ;;gis:set-world-envelope gis:envelope-of stp-tracts-dataset

  ;; assign patches to their corresponding census tracts
  foreach gis:feature-list-of stp-tracts-dataset [ feature ->
    ask patches gis:intersecting feature [
      set tract-id gis:property-value feature "GEOID20"

      ;; this is actually debugging code, at one point I needed a count of unique tract-ids
      if not member? tract-id tract-ids [
        set tract-ids lput ( word tract-id ) tract-ids
      ]

    ]
  ]

  ;; load census data into tracts/patches
  load-census-data

  display-tracts
end


to load-census-data
  if-else file-exists? "data/households.txt"
  [ file-open "data/households.txt"
    while [ not file-at-end? ]
    [
      let geo_id file-read
      let geo_code file-read
      let geo_name file-read
      let tract_population file-read

      ask patches with [ tract-id = geo_code ] [
        set population read-from-string tract_population
      ]
    ]

    ;; distribute households evenly across all patches in the tract
    ask patches with [ population > 0 ] [
      set population population / count patches with [ tract-id = [ tract-id ] of myself ]
    ]

  file-close
  ][
  user-message "Missing census data file data/households.txt"
  ]
end


to make-seeds
  create-seeds seed-count [

    ;; create a seed at the position of a random household, then kill that household
    let target one-of households
    set xcor [ xcor ] of target
    set ycor [ ycor ] of target
    ;; ask target [ die ] ;; We no longer do this as the households are not recreated on reset. If you ran the model long enough -- say 376 times (~113000 households / 300 max seeds ) you wouldn't have any households left

    set size 1
    set shape "target"
    set color red

    ;; alert households if they are within influence of the seed
    ;; since seeds do not change during a run, storing this with the
    ;; household is far more efficient then a radial check with each tick

    ask households in-radius seed-radial-influence [
      set neighborhood-seeds ( 1 + neighborhood-seeds )
    ]
  ]
end

;;;;; go

to go
  if ( ticks > ( projection-length * 52 ) - 1 ) [
    output-print "End of Projection Reached"
    debug
    stop
  ]

  ;; reset-timer ;; for debugging

  ;; create events schedule
  if ( ticks = 0 ) [ schedule-events ]
  ;; reset event-multiplier
  let multiplier 1
  ;; check-schedule
  if ( member? ticks events-schedule ) [
   ;; output-print "event week!"
    set multiplier event-multiplier
   ;; output-type "event size: "
   ;; output-print multiplier
  ]


  ask households [

    if-else ( not rental? ) [
      ;; decide if household should randomly start renting, independent of other considerations
      ;; i.e., ignoring all external influence -- the neighborhood, fees, anything I add later
      if-else ( random-float 1.0 < ( randomly-start-renting / 100 ) ) [
        switch-rental-status
      ][
        ;; if we have not switched randomly, check in the normal way (with external influences)
        let probability ( ( willingness-to-rent * multiplier ) - ( subjective-regulations ) ) / 100
        if probability > 0 [
          if random-float 1.0 < probability [ switch-rental-status ]
        ]
      ]
    ][
      ;; check if a rental term is expiring, or if taxes have made us change our mind
      ;; about renting
      if-else expiration = ticks [
        switch-rental-status
        if random-float 1.0 < ( renewal-chance / 100 ) [ switch-rental-status ]
      ][
        if random-float 1.0 < ( taxes / 400 ) [ switch-rental-status ]
      ]
    ]

  ]

  ;; show-time

  tick
end



;;;;; calculate willingness to start/stop renting

to-report willingness-to-rent
  let willingness ( neighborhood-seeds * seed-effect ) + ( neighborhood-rentals * rental-effect )
  report willingness
end


;;;;; start/stop renting

to switch-rental-status
  if-else ( not rental? ) [
    start-renting
  ][
    stop-renting
  ]
end

to start-renting
  set rental? true
  set expiration rental-expiration-week

  ask households in-radius rental-radial-influence [
      set neighborhood-rentals ( 1 + neighborhood-rentals )
    ]

  set color green
  ;; set color lput 125 extract-rgb color ;; ~50% alpha
end

to-report rental-expiration-week
  let rental-duration ( random ( max-rental-life - min-rental-life + 1) )
  report ticks + min-rental-life + rental-duration
end

to stop-renting
  set rental? false
  set expiration 0

  ask households in-radius rental-radial-influence [
      set neighborhood-rentals ( (-1) + neighborhood-rentals )
    ]

  set color white
  set color lput 125 extract-rgb color ;; ~50% alpha
end

;;;;; events

to schedule-events
  let events-to-schedule ( avg-num-events * projection-length )

  set events-schedule []
  let final-week ( projection-length * 52 )

  while [ events-to-schedule > 0 ] [
    let scheduled-event ( ( random final-week ) + 1 )

    if (not member? scheduled-event events-schedule ) [
      set events-schedule lput scheduled-event events-schedule
      set events-to-schedule ( events-to-schedule - 1 )
    ]
  ]
end

to-report event-multiplier
  let event-size-range ( range min-event-multiplier max-event-multiplier 0.5 )
  let event-size ( item ( random ( length ( event-size-range ) ) ) event-size-range )
  report event-size
end



;;;;; reset
to reset
  clear-all-plots
  clear-output

  if-else force-repeatable? = true [
    random-seed 42
  ][
    random-seed new-seed
  ]

  ;; reset households
  ask households [
    set rental? false
    set expiration 0
    set neighborhood-rentals 0

    ;; if we are creating new seeds we will need to 0 out the existing neighborhood-seedss
    if keep-seeds-on-reset? = false [
      set neighborhood-seeds 0
    ]

    set size 0.75
    set shape "dot"
    set color white
    set color lput 125 extract-rgb color
  ]

  ;; create new seeds if desired
  if keep-seeds-on-reset? = false [
    ask seeds [
      die
    ]
    make-seeds
  ]

  ;; reset event weeks
  set events-schedule []

 ;; reseed random number generator
 ;; random-seed new-seed

  reset-ticks
end



;;;;; display
to display-tracts
  set display? true
  gis:set-drawing-color blue
  gis:draw stp-tracts-dataset 3
end

to toggle-view
  if-else ( not display? ) [
    display
    set display? true
    output-print "Turning View Updates On"
  ][
    no-display
    set display? false
    output-print "Turning View Updates Off"
  ]
end


;;;;; debug

to debug
  ;; output-type "at tick " output-print ticks

  output-type count households with [ rental? ]
  output-type " of "
  output-type count households
  output-type " households renting ("
  output-type precision ((count households with [ rental? ] / total-aggregate-households) * 100) 3
  output-print "%)"

 ;; output-type "event schedule "
 ;; output-print events-schedule

end

to show-time
  output-type "Step took "
  output-type timer
  output-print " seconds."
end


;;;;; future ideas
; prefer middle-class-blocks
; proximity to landmarks
; proximity to hotels
; locking
; block cutoff
; annual tax increase
; annual fee increase
; renewal rate








@#$#@#$#@
GRAPHICS-WINDOW
535
10
1898
1524
-1
-1
5.0
1
10
1
1
1
0
0
0
1
0
270
0
300
1
1
1
week
1.0

BUTTON
75
10
275
43
setup
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
280
10
480
43
go
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
280
45
480
78
projection-length
projection-length
0.5
5
5.0
0.5
1
years
HORIZONTAL

SLIDER
75
1320
480
1353
household-aggregation
household-aggregation
1
10
1.0
0.5
1
(requires new setup)
HORIZONTAL

SLIDER
75
80
275
113
seed-count
seed-count
0
300
150.0
1
1
NIL
HORIZONTAL

SLIDER
75
150
275
183
seed-radial-influence
seed-radial-influence
0
5
2.0
0.25
1
NIL
HORIZONTAL

OUTPUT
75
625
480
845
13

BUTTON
75
45
275
78
reset
reset
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
340
850
480
883
debug
debug
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
280
80
480
113
keep-seeds-on-reset?
keep-seeds-on-reset?
1
1
-1000

TEXTBOX
75
1366
480
1542
Household aggregation groups n households reported in the census data together for faster execution/testing.\n\nDue to population density, some tracts wil be completely empty beggining around n  = 1.5, though this is dependent on the number of patches in the view\n\nAdjusting this value requires the model to be re-setup.\n\nforce-repeatable re-inits the random number generator with seed 42 everytime the system is reset, NOT BEFORE. So if you want the system to repeat itself you must turn it on, then reset and run.
10
0.0
1

SLIDER
75
375
480
408
randomly-start-renting
randomly-start-renting
0
1
0.2
0.025
1
% chance / week
HORIZONTAL

SLIDER
75
220
275
253
min-rental-life
min-rental-life
1
52
4.0
1
1
weeks
HORIZONTAL

SLIDER
280
220
480
253
max-rental-life
max-rental-life
1
52
52.0
1
1
weeks
HORIZONTAL

PLOT
75
455
480
620
Percent Aggregate Households Renting
week
%
0.0
10.0
0.0
10.0
true
false
"set-plot-x-range 0 (projection-length * 52)" ""
PENS
"default" 1.0 0 -16777216 true "" "plot ( ( count households with [ rental? ] / total-aggregate-households ) * 100 )"

SLIDER
75
185
275
218
rental-radial-influence
rental-radial-influence
0
5
1.0
0.25
1
NIL
HORIZONTAL

SLIDER
280
150
480
183
seed-effect
seed-effect
0
10
5.0
0.50
1
% increase
HORIZONTAL

SLIDER
280
185
480
218
rental-effect
rental-effect
0
1
0.25
0.025
1
% increase
HORIZONTAL

SLIDER
75
410
275
443
taxes
taxes
0
100
12.0
0.5
1
%
HORIZONTAL

SLIDER
280
410
480
443
subjective-regulations
subjective-regulations
0
10
2.0
1
1
NIL
HORIZONTAL

BUTTON
75
850
267
883
Turn View Updates On/Off
toggle-view
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
85
895
475
1326
RECCOMENDED BASE SETTINGS\n(These should result in ~ 8% of all households renting after a 5 year projection... maybe.)\n\nView: max-pxcor 270, max-pycor 300 (from bottom left)\n\nprojection-length 5.0 years\nseed-count: 150; keep-seeds-on-reset? off\nseed-radial-influence*:   2.00; seed-effect:   5.0%\nrental-radial-influence: 1.00; rental-effect:  0.250%\nmin-rental-life: 4 weeks; max-rental-life: 52 weeks\nrenewal-chance: 50%\navg-num-events: 6/year; \nmin-event-multiplier: 2.0; max-event-multiplier: 4.0\n\nrandomly-start-renting: 0.200% chance/week\n\ntaxes: 12.0%; subjective-regulations: 2\n\nhousehold-aggregation: 1.0\n\n\n* Seed influence is only calculated when seeds are created, adjusting it will require a model reset with keeps-seeds-on-reset? off.\n\nRental influence is calculates when a household begins renting and so can be adjusted without reset.\n\n
12
0.0
1

SLIDER
75
290
275
323
avg-num-events
avg-num-events
0
12
6.0
1
1
/year
HORIZONTAL

SLIDER
280
255
480
288
renewal-chance
renewal-chance
0
100
50.0
1
1
%
HORIZONTAL

SLIDER
75
325
275
358
min-event-multiplier
min-event-multiplier
1
5
2.0
0.5
1
NIL
HORIZONTAL

SLIDER
280
325
480
358
max-event-multiplier
max-event-multiplier
1
5
4.0
0.5
1
NIL
HORIZONTAL

SWITCH
280
115
480
148
force-repeatable?
force-repeatable?
1
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
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
@#$#@#$#@
1
@#$#@#$#@
