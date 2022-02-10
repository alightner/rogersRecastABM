;; ROGERS' PARADOX RECAST AND RESOLVED, Rendell et al (2010) replication model
;; note: this model does not include the demographic effects explored in a variant of Rendell et al's original model
;; UPDATE (12/7) it is unlikely that this model will include demographic effects, as this would complicate the model to a level which is
;; beyond the scope of the current set of research questions which I plan to explore

;; !!! experimental version with critical and conditional learning strategies added into it (11/22)
;; 12/1; not the experimental version anymore, but the default now; critical and conditional strategies work just fine

globals
[
  ;Ns (from before Ns slider was added, mostly for developing environment at fixed Ns = 10). Ns is the number of environmental states (s)
  ;;which each patch can cycle through
 ; mu   ;;;; from testing; mu does not vary (0.0008 in all tests) so this was coded into the model
  W-list    ;; Rendell et al collect the mean fitness values for the last 250 time steps of each simulation, so all W values at time steps
  W-mean     ;; 1750-2000 were compiled into W-list and the mean of this list (W-mean) was updated at each iteration
  s-list    ;; S-list and S-mean are conceptually the same as W-list and W-mean, but correspond to proportion of social learners
  s-mean      ;; cond-list/mean and crit-list/mean conceptually do what s and W lists and means do
  cond-list    ;; collect last 250 ticks of data and take the mean for data analysis
  cond-mean
  crit-list
  crit-mean

]

patches-own [
 s    ;; environmental state of each patch
 b    ;; behavior associated with the individual in a given patch, which aims to correctly match environmental state (s)
 genotype    ;; heritable learning strategies for individual in a given patch, affects which behavior (b) is acquired
 center?     ;; one patch per time step will be center?=TRUE and an environmental perturbation event will occur with this patch at this center
 W     ;; fitness of the individual in a given patch, affects probability of reproduction at each time step
 Pn    ;; crude measure of spatial autocorrelation (as outlined/used by Rendell et al 2010) of environmental state values
 cost  ;; cost associated with each individual for learning with its respective strategy, varies with different genotypes/strategies
]

to setup
  ca  ;; clear-all plots and values
  random-seed seed
  set W-list (list)    ;; setting an empty list for our 250 fitness values to be collected toward the end of the experiment
  set s-list (list)    ;; setting an empty list for our 250 proportions of social learners to be collected toward the end of the experiment
  set cond-list (list)     ;; same as above... conditional learners
  set crit-list (list)     ;; same as above... critical learners
;; First, we start by setting up environmental states in each patch
if environment-type = "temporal"   ;; condition in which the environment varies with time (based on Ps, or
[                                   ;;environmental stability), but does not vary spatially
  let start-s 1 + random Ns         ;; random number from [1, 2, ..., Ns]
  ask patches
  [
  set s start-s               ;; set patches to random environmental state from [1, 2, ..., Ns]
  ]
  ;ask patches
 ; [
   ;;patch-color     ;; used for testing/developing the environmental conditions when Ns=10 (patch-color command commented out near bottom)
 ; ]
 if spatial-corr? = TRUE    ;; user message to alert user that spatial correlation option doesn't make sense when environment doesn't spatially vary
 [user-message "Temporal environment type is already spatially correlated"]
]

if environment-type = "spatiotemporal"  ;; condition in which the environment varies with space and time
[                                         ;;  (still change according to environmental change rates, or Ps value)
  ask patches
  [
    set s 1 + random Ns            ;; random number from [1, 2, ..., Ns]
    set center? FALSE          ;; perturbation events will occur during spatiotemporal conditions as outlined in Rendell et al at each time step
    ;patch-color            ;; used for testing (see above)
  ]
  if learning-type = "global"            ;; spatiotemporally varying environment is a spatially explicit condition like local learning/dispersal
  [user-message "Warning: environment is set to vary spatially for global learning type"]       ;; global is essentially a visual simulation of
  if dispersal-condition = "global"                                                       ;; non-spatially explicit models of Roger's Paradox
  [user-message "Warning: environment is set to vary spatially for global dispersal condition"]   ;; mixing conditions is possible
]                                                                                                   ;; but user warning to avoid mistakenly mixing

;; second, we move to behavioral strategies setup...
ask patches
[
  set b 0        ;; all individuals start as "naive learners" in this model (Rendell et al 2010)
  set cost 0      ;; no costs imposed until a learning strategy is executed;
                   ;;this malleability is necessary for critical and conditional
                 ;; learners to flexibly take on either asocial or social costs (depending on which strategy they use)

 ifelse add-mixed-strategies? = TRUE    ;; in mixed strategies, there are 4 possible heritable strategies; see interface notes for color code
  [ set genotype random 4]        ;; comments in color-update procedure also show colors; (asocial, social, critical, conditional)
  [ set genotype random 2]     ;; if not mixed strategies, then genotypes are binary and asocial/social pure strategies are only relevant
  set center? FALSE
  color-update  ;; coloring patches command during setup based on genotype for visualization of strategy invasions

  ;; setting Pn for plot of (an approximation of) spatial autocorrelation
  ;; not sure that this is actually a proper measure of spatial autocorrelation,
                                  ;; but authors described Pn as "probability of a cell's neighbor being the same as itself"
                                  ;; and "effectively autocorrelation with a 'spatial lag' of one cell"
  let my-s [s] of self   ;; i.e., what is "my" (i.e., patch's) environmental state?
  let n-match neighbors with [s = my-s]    ;; how many neighboring patches have the same environmental state?
  set Pn count n-match / count neighbors ;; calculate the fraction of similar neighbors and set to Pn (plotted, could be collected in BehaviorSpace)
;; Pn is used for plotting the spatial instability in the environment, on plot "spatial-autocorrelation" over time (see interface)
]


reset-ticks

end


to go
tick

;;; TIME STEP PART 1: setting the environmental state

if environment-type = "temporal"
[
if spatial-corr? = TRUE
[user-message "ERROR: Spatial correlation switch can only be used in the spatiotemporal environment type"
  stop]       ;; this is to shortcut around unnecessary BehaviorSpace runs through redundant parameter spaces when spatial var is varied

if random-float 1 < Ps  ;; when a random number falls within the probability of environmental change...
[
  let new-s 1 + random Ns   ;; ...change the environment by assigning s to a new s (between 1 and Ns)
  ask patches
  [
  set s new-s
  ]
]

  ; print s      ;; for testing/developing the environmental states first

]

if environment-type = "spatiotemporal"
[
  ask patches [set center? FALSE] ;; resets centers to false with each time step to avoid compounding perturbation events with each tick
  ask one-of patches [set center? TRUE] ;; only ONE patch will be the center of a perturbation event at each tick
  ifelse spatial-corr? = TRUE
  [
   ask patches [   ;; spatially correlated condition
    if random-float 1 < Ps   ;; when a random number falls within the probability of environmental change...
    [
     let avg-s mean [s] of neighbors   ;; find the mean state of local area and move your env state one step toward that average
     if avg-s > s
     [set s s + 1]
     if avg-s <= s    ;; went back and forth a lot on this one; chose to re-cycle s through Ns ring to avoid
     [set s s - 1]    ;; letting the env "lock in" on an equilibrium state when all s values were equal

     if s > Ns    ;; establishes a "ring structure" for stepping through env states [1, 2, ..., Ns, 1, 2, ...]
     [set s 1]
     if s < 1
     [set s Ns]
    ]
   ]
  ]
  [ask patches
  [                               ;; random spatiotemporal condition
    if random-float 1 < Ps
    [
    ifelse random-float 1 > 0.5      ;; when a random number falls within the probability of environmental change...
    [set s s + 1]                   ;; move your environmental state randomly one step up or one step down
    [set s s - 1]

    if s > Ns   ;; "ring structure" again (as above)
    [set s 1]
    if s < 1
    [set s Ns]
    ]
  ]
 ]


 ask patches [
    if center? = TRUE        ;; all spatiotemporal conditions, one patch calls the perturbation event (of which it is the center)
    [perturbation-event]   ;; this is a homogeneous state disturbance in a cluster of patches, which conforms to a power law
    ;patch-color       ;; <- "patch-color" was for testing while developing environmental conditions (at Ns = 10)
  ]
]

;;; TIME STEP PART 2: setting each organism's response to environmental state (behavior, fitness and reproduction/dispersal)

ask patches
[
  learn-env   ;; calls submodel for learning the environment/executing learning strategy to acquire a behavioral (b) value
  update-fitness  ;; newly acquired behavior is assessed in context of environmental state, fitness is updated accordingly
]
;; separately called patches to reproduce to be certain that all fitnesses (W) were updated first and
;; all reproduction events would occur in lockstep. While it is possibly an unnecessary measure, this precaution was inspired by
;; Wilensky's code (which does the same) in the Conway's Game of Life model
ask patches [
  if random-float 1 < W       ;; if a random number between 0-1 falls within the probability of reproduction (i.e., fitness), then reproduce
                                   ;; asexually and call disperse command...
;  [ if random-float 1 > 0.0008    ;; <- this works for mutations as long as strategies are binary (original pure strategy version ~11/3)
    [disperse]                   ;; but is not necessary in current mixed strategy setup
  color-update       ;; after dispersing and updating the genotype, change the color (if necessary) to reflect the current genotype

  ;;updating Pn for plot of ~spatial autocorrelation
  let my-s [s] of self           ;; each time step, same as the Pn update procedure in setup (see above for explanation)
  let n-match neighbors with [s = my-s]
  set Pn count n-match / count neighbors

]

if ticks >= 1750           ;; basically, in the last 250 time steps, start collecting data in lists and keep track of the means of those data...
[set W-list fput (mean [W] of patches) W-list                   ;; making up for a limitation in BehaviorSpace to replicate authors' method
  set s-list fput (count patches with [genotype = 1] / count patches) s-list     ;; output is a mean value of last 250 ticks, which is the collected
  set cond-list fput (count patches with [genotype = 2] / count patches) cond-list      ;; data of interest for plotting
  set crit-list fput (count patches with [genotype = 3] / count patches) crit-list
  set W-mean mean W-list                              ;; mean fitness of last 250 ticks
  set s-mean mean s-list                      ;; mean proportion of social (s-mean), conditional (cond-mean) and critical (crit-mean) learners
  set cond-mean mean cond-list        ;; is being tracked with each new addition to each list; this allows the mean at the end (i.e., collection)
  set crit-mean mean crit-list        ;; to be up to date for the last 250 ticks
  ]

;if standard-deviation [genotype] of patches = 0    ;; stop condition @fixation was omitted to replicate authors' data collection method
;[stop]                          ;; because sometimes with this condition, last 250 ticks are not reached
if ticks >= 2000       ;; the end of the run is at 2000 ticks
[stop]

;if (all? patches [pcolor = blue]) or (all? patches [pcolor = yellow])
;[stop]          ;; stop condition from earlier visual development stages

end


to perturbation-event  ;; NOTE: perturbation events follow a power law; this results in an occasional (rare) spike in
                        ;; mean Pn in the spatial autocorrelation plot; this was my reason for including that plot (for detecting
                        ;; reasons for otherwise seemingly spontaneous demographic/mean fitness shifts
let R-val random-float 1    ;; perturbation event deviates from square patches method used in Rendell et al (2010), but still
let per-rad (8 * (R-val ^ (-1 / 6)))     ;; follows the same power law equation (applied to radius)
let per-s 1 + random Ns               ;; basically, pick a random state number between 1 and Ns, find the center?=TRUE patch, and
ask patches in-radius per-rad          ;; tell all the patches in its radius to assign their state number to this value
[ set s per-s ]                   ;; results in homogeneous environmental state disturbances that conform to power law
                       ;; Langmead & Sheppard (2004) are cited here by Rendell et al upon describing this method/equation
end

to color-update

if genotype = 0        ;; pure asocial strategy executors are blue
[set pcolor blue]

if genotype = 1       ;; pure social strategy executors are yellow
[set pcolor yellow]

if genotype = 2        ;; mixed conditional (asocial -> social) are orange
[set pcolor orange]

if genotype = 3        ;; mixed critical (social -> asocial) are red
[set pcolor red]

if genotype > 3
[set pcolor black]    ;; precautionary measure to visually detect potential bugs related to genotype variable (though this is unlikely)

end


to learn-env   ;; learning environment/acquiring behavior here...

if learning-type = "local"  ;; in the local learning condition, social learning occurs within Moore neighborhoods
[                    ;;;; asocial procedure...
if genotype = 0             ;; pure strategy asocial learners track environment with success determined by pr-asocial-learning
[
  ifelse random-float 1 < pr-asocial-learning
     [ set b s ]
     [ set b 1 + random Ns]        ;; but if asocial learning fails, individuals end up with a random behavior instead
     set cost C-asocial        ;; and asocial cost is incurred thereafter
]
                   ;;;; social procedure...
if genotype = 1       ;; pure strategy social learners simply copy one of the behaviors in their Moore neighborhood, incur social cost
[
  set b [b] of one-of neighbors
  set cost C-social
]

if genotype = 2       ;; conditional learner uses asocial first, social if asocial fails
  [                     ;; (NOTE: this means NOTHING if pr-asocial is not < 1, so when mixed? is TRUE, pr-asocial should be adjusted to < 1)
   ifelse random-float 1 < pr-asocial-learning
     [ set b s ]            ;; asocial procedure with asocial cost
     [ set b 1 + random Ns]
     set cost C-asocial
   if b != s                          ;; but if that fails...
   [set b [b] of one-of neighbors   ;; use social procedure with social cost instead
     set cost C-social]
  ]

if genotype = 3        ;; critical learner uses social first, asocial if social fails
[ set b [b] of one-of neighbors   ;; social procedure with social cost first
  set cost C-social
  if b != s                       ;; but if that fails...
  [ifelse random-float 1 < pr-asocial-learning    ;; asocial procedure with asocial cost instead
     [ set b s ]
     [ set b 1 + random Ns]
     set cost C-asocial]

]

]

if learning-type = "global"      ;; learning model without spatially explicit component
[                               ;; all procedures are identical to local learning-type (above); differences are commented on
  if genotype = 0
  [
    ifelse random-float 1 < pr-asocial-learning
     [ set b s ]
     [ set b 1 + random Ns]
     set cost C-asocial
  ]
  if genotype = 1
[
  set b [b] of one-of patches   ;; social learners copy a random individual in the torus, NOT necessarily a neighbor
  ;with [genotype = 0]     ;; IMPORTANT: double check this; not sure if this is Rendell et al strategy (it isn't - resolved on 11/4)
  set cost C-social
]

  if genotype = 2       ;; conditional learner uses asocial first, social if asocial fails
  [
   ifelse random-float 1 < pr-asocial-learning
     [ set b s ]
     [ set b 1 + random Ns]
     set cost C-asocial
   if b != s
   [set b [b] of one-of patches    ;; social learning scope is torus, not Moore neighborhood (as above)
     set cost C-social]
  ]

  if genotype = 3        ;; critical learner uses social first, asocial if social fails
[ set b [b] of one-of patches
  set cost C-social
  if b != s
  [ifelse random-float 1 < pr-asocial-learning
     [ set b s ]
     [ set b 1 + random Ns]
     set cost C-asocial]
]

]
end


to disperse     ;; reproduction is asexual in this model, but dispersal of genotype can be local or global, and is subject to mutation

if dispersal-condition = "local"  ;; in local dispersal conditions...
[ ask one-of neighbors [   ;; random Moore neighbor is displaced by offspring
    ifelse random-float 1 > 0.0008   ;; mutation rate is 0.0008
    [set genotype [genotype] of myself
      ; set b [b] of myself           ;; commented this out because I have reservations about this assumption
      ]                                    ;; dispersal event
    [ifelse add-mixed-strategies? = TRUE   ;; mutation event
      [set genotype random 4]     ;; mutation for mixed strategies
      [set genotype random 2]]    ;; mutation for pure strategies ;; mutation event should go here (NOTE FOR LATER; 11/28)
]]

if dispersal-condition = "global"    ;; emulates previous Roger's Paradox models (e.g., ...) without spatially explicit component
[                                         ;; making it essentially a visual representation of previous mathematical models
ask one-of patches [                   ;; in global conditions... as above except for dispersal scope is torus, not Moore neighborhoods
  ifelse random-float 1 > 0.0008   ;; mutation rate is 0.0008
    [set genotype [genotype] of myself
     ; set b [b] of myself      ;; this assumption seems to solve a plot discrepancy I had, but commented out because
                                 ;; it is a questionable/(possibly unjustified?) assumption...
      ]
    [ifelse add-mixed-strategies? = TRUE   ;; mutation event (as above)
      [set genotype random 4]
      [set genotype random 2]]]
]

end


to update-fitness   ;; updating fitness after collecting behavioral and state values

 ; let cost 0   ;; previously, cost was local (not patches owned)
  let Sb 0        ;; start by resetting Sb value

  ifelse abs (s - b) <= (Ns / 2)   ;; measures distance between state (s) and behavior (b) while maintaining Ns ring structure
  [set Sb (abs (s - b))]           ;; and sets Sb to that distance
  [set Sb (Ns - (abs (s - b)))]

  set W ((1 / (harshness ^ (Sb))) - cost)    ;; fitness updating equation compounds error (Sb) by harshness, deducts from 1
                                              ;; before deducting strategy cost

if W < 0
[set W 0]   ;; W is effectively a probability, so must be between 0-1

;print W   ;; for testing

end



;;;;;;;;;;;;;;;;;;; Below this line lie my failed ideas (and some early development things) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;to collect-data

 ; file-open "largerDataset.csv"
  ;file-print ""
 ; file-write "," file-write C-asocial file-write ","
  ;file-write (count patches with [genotype = 1] / count patches) file-write ","
  ;file-write mean [W] of patches
;  file-print ""
 ; file-close

;end





;to patch-color      ;; used for testing during development of env conditions at Ns = 10
                      ;; used diverse colors for easier visual debugging
 ;   if s = 0 [set pcolor yellow]
  ;  if s = 1 [set pcolor lime]
   ; if s = 2 [set pcolor cyan]
;    if s = 3 [set pcolor blue]
 ;   if s = 4 [set pcolor green]
  ;  if s = 5 [set pcolor brown]
   ; if s = 6 [set pcolor red]
    ;if s = 7 [set pcolor white]
 ;   if s = 8 [set pcolor magenta]
  ;  if s = 9 [set pcolor sky]
   ; if s = 10 [set pcolor gray]

;end


;to-report spatial-autocorrelation ;; not sure that this is actually a proper measure of spatial autocorrelation,
                                  ;; but authors described Pn as "probability of a cell's neighbor being the same as itself"

;let avg-pn mean [Pn] of patches
;report avg-pn

;end

;to-report mean-fitness

;let avg-W mean [W] of patches
;report avg-W

;end

;set exec random 2     ;; remaining piece from an old strategy for incorporating mixed strategies

  ;let genotype-prim random-float 1
       ;; setting random strategy; 0 = asocial, 1 = social
  ;ifelse genotype-prim > 0.1              ;; old approach for incorporating mixed strategies which has been abandoned
  ;[ set genotype 0 ]
  ;[ set genotype 1 ]


  ;which-type   ;; added in with critical/conditional learning strategies for initial assignment
  ;; update 11/29: opted not to use the which-type and check-strategy approach with exec
  ;; because code changes to include critical/conditional were less complex than anticipated



  ;to which-type ;; first learning strategy set here        ;; abandoned this strategy (see note above - 11/29)

;if genotype = 0 [set exec 0]
;if genotype = 1 [set exec 1]
;if genotype = 2 [set exec 0]  ;; critical uses asocial first
;if genotype = 3 [set exec 1]  ;; conditional uses social first

;end

;to check-strategy

 ; if genotype = 2
  ;[if b

;end



;ifelse genotype = 0
  ;[set cost C-asocial]
  ;[set cost C-social]

  ;; send to patch-coloring here when recoloring (strategies)
@#$#@#$#@
GRAPHICS-WINDOW
210
10
544
365
40
40
4.0
1
10
1
1
1
0
1
1
1
-40
40
-40
40
0
0
1
ticks
30.0

BUTTON
7
272
73
305
NIL
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
80
272
143
305
NIL
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
7
93
179
126
Ps
Ps
0
1
0.1
0.01
1
NIL
HORIZONTAL

CHOOSER
7
177
149
222
environment-type
environment-type
"temporal" "spatiotemporal"
1

SLIDER
7
135
179
168
harshness
harshness
1
5
2
0.1
1
NIL
HORIZONTAL

SLIDER
7
355
179
388
C-asocial
C-asocial
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
7
397
179
430
C-social
C-social
0
1
0
0.01
1
NIL
HORIZONTAL

CHOOSER
210
373
357
418
dispersal-condition
dispersal-condition
"local" "global"
0

SLIDER
7
314
181
347
pr-asocial-learning
pr-asocial-learning
0
1
1
0.01
1
NIL
HORIZONTAL

CHOOSER
210
427
348
472
learning-type
learning-type
"local" "global"
0

SLIDER
7
10
179
43
seed
seed
0
100
20
1
1
NIL
HORIZONTAL

SLIDER
7
50
179
83
Ns
Ns
0
1000
1000
1
1
NIL
HORIZONTAL

SWITCH
7
230
143
263
spatial-corr?
spatial-corr?
0
1
-1000

PLOT
553
10
753
160
spatial-autocorrelation
time
Pn
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [Pn] of patches"

PLOT
553
169
753
319
mean-fitness
time
avg-W
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [W] of patches"

PLOT
759
10
1039
160
proportion-learners
time
p-strategies
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"social" 1.0 0 -4079321 true "" "plot ((count patches with [genotype = 1]) / count patches)"
"critical" 1.0 0 -5298144 true "" "plot ((count patches with [genotype = 3]) / count patches)"
"asocial" 1.0 0 -14070903 true "" "plot ((count patches with [genotype = 0]) / count patches)"
"conditional" 1.0 0 -817084 true "" "plot ((count patches with [genotype = 2]) / count patches)"

PLOT
760
169
960
319
mean-environment-s
time
mean-s
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [s] of patches"

SWITCH
7
439
188
472
add-mixed-strategies?
add-mixed-strategies?
1
1
-1000

TEXTBOX
372
373
529
457
Learning strategies:\nPure asocial (blue)\nPure social (yellow)\nConditional asocial (orange)\nCritical social (red)
11
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is an agent based model built in Netlogo 5.3.1 designed to replicate the model of evolving  learning strategies in Rendell et al (2010). In particular, this model allows observers to investigate the ecological conditions under which social and asocial learning strategies spread more readily within a population, the impact that these various conditions have on mean fitness of individuals in a population, and how spatially explicit, local interactions affect this dynamic. Each agent in this model is represented by a single, immobile patch following a set of basic rules, and through stochastic interactions with state variables and one another, they simulate emergent, dynamic learning outcomes that affect fitness and reproduction. This model description follows the ODD protocol for describing agent-based models (Railsback & Grimm 2012).

### Purpose

The purpose of this model is to test: (1) the effect of a spatially explicit, dynamic environment on the mean fitness and reproductive success of pure strategy social and asocial learners, and (2) test the effect of introducing mixed strategy social learners into spatially explicit, dynamic environments with varying levels of harshness and spatial variation. These questions are raised in an effort to find conditions that resolve Rogers’ Paradox, the analytic finding that frequency-dependent mean fitness among social learners is only advantageous at low frequencies because they are essentially informational freeriders (i.e., they derive benefit from and contribute no added mean fitness to populations of asocial learners - see Rogers 1988).


## HOW IT WORKS

### Entities, state variables and scales

This model consists of 6400 patches in a toroidal population. Each represents a single individual executing a behavior in response to its local (i.e., patch-owned) environmental state, resulting in a fitness value during each time step that dictates the likelihood of reproduction.

Each time step reflects approximately one generation. During a time step, the environmental state of each individual’s particular patch is represented by a value s, and the behavior of the individual within a given patch is represented by b. These values are specific to their own patches. The s values can take on any number between 1 and Ns (Ns is number of environmental states possible), and b values can take on this same range of values. The number of possible environmental state values (Ns) are organized in a “ring structure,” meaning that there are no high or low values (e.g., if Ns = 10, then counting “up” from 8 would look like: 9, 10, 1, 2, and so on). Ns can be any value between 2 and 1000 in this model.

The error term (Sb) is a result of how close an individual’s behavior (b) comes to matching its environmental state value (s), such that:

For all abs(s - b) <= Ns / 2:
Sb = abs(s - b)

For all abs(s - b) > Ns / 2:
Sb = Ns - abs(s - b)

This allows for the error term to reflect a magnitude of distance between b and s values while maintaining the ring structure of environmental state values. The error term of a behavior in its environmental context is magnified by the environmental harshness (global slider variable called harshness), which can take on values between 1.1 and 5. The error term when considering harshness is:

harshness ^ Sb

The acquisition of b values is contingent on heritable learning strategies (genotype), which can be values [0, 1, 2, 3] and correspond to learner strategies: asocial = 0, social = 1, conditional = 2 and critical = 3 (the latter two are only relevant when mixed strategies are introduced, defined by an on/off switch in the interface set to add-mixed-strategies? = true). The genotype of each entity also influences which learning costs are to be deducted from potential fitness. Costs of learning (cost) is a patch variable that is set at each time step according to the method by which behavior is acquired. For example, if a patch copies its b value from another patch, then its cost is set to C-social (cost of social learning; global slider value). Conversely, if a patch learns s values asocially, then it takes on a cost of C-asocial (cost of asocial learning; global slider value).

An individual with a genotype = 0 (asocial) will always learn about s values asocially and will incur cost = C-asocial at each time step. Individuals with genotype = 1 (social) will always learn about s values socially and will incur cost = C-social at each time step. When add-mixed? = true (i.e., mixed strategies are introduced), genotype = 2 (conditional) will default to asocial learning at cost = C-asocial, but if pr-asocial < 1 (which is typically the case in mixed strategy conditions) and the resulting b value is not equal to s, then it will switch to copying the b value of another patch and incur cost = C-social. Conversely, genotype = 3 (critical) will default to socially copying b from another patch at cost = C-social, but if the result does not equal s it will switch to asocially learning s at cost = C-asocial.

The probability of coming up with a “correct” b value while learning asocially is defined by pr-asocial-learning. This is typically set to 1.0 (i.e., 100% asocial learning accuracy) in pure strategy settings. Because pr-asocial is a probability value, it can only be set (on its global slider) between 0-1. In mixed strategy conditions, it only deviates from pr-asocial = 1 to differentiate the acquisition methods and fitness outcomes of asocial and conditional learners. C-social and C-asocial are also values between 0-1, and are typically set such that C-asocial > C-social (C-social is usually ~0.0; see below for initialization values).

The copying of other patches has a “reach” (i.e., how far away can an individual see while copying another b value?) determined by the learning-type chooser in the interface. Local learning-type means that individuals can only copy b values from neighboring patches, while global learning-type means that individuals can copy b values from any patch in the torus.

The relationship between the harshness-relevant error term, cost and fitness (W) is updated at each time step, defined by the equation:

W = (1 / (harshness ^ Sb)) - cost

A “perfect” potential W value with neither error nor cost would be 1, and all negative W values are coded to reset to a lower limit of zero. The W value takes on only values between 0-1 because it defines the probability of reproduction (which occurs asexually in this model) and dispersal at a given time step.

The dispersal of individual offspring results in the displacement of an individual of another patch. The end result of this is another patch taking on the genotype value of the dispersing patch. In the global condition of the dispersal-type chooser, this displaced/new offspring patch is any patch in the torus. In the local condition, it is a neighboring patch only. The mutation rate is coded in at 0.0008, so with probability 0.0008 at each dispersal event, the offspring genotype value will take on a random number (0 or 1 in pure strategy conditions, 0-3 in mixed strategy conditions) instead of making a copy of its parent.

The dynamics of environmental state in this model have two settings in the environment-type chooser. The temporal environment-type sets the s values of each patch to the same random number between 1 and Ns in lockstep, with probability Ps (probability of state change, a global slider with possible values 0-1). The spatiotemporal environment-type sets the s value of each patch to different numbers between 1 and Ns independently, each with probability Ps at a given time step. The randomness of change in the spatiotemporal condition is determined by the “spatial-corr?” switch. If spatial-corr? = true, then each spatiotemporal patch change will move one step (s = s + 1; or s = s - 1) toward the average of its surrounding state values. If spatial-corr? = false, then each changing patch will pick an s value between 1 and Ns, randomly and independently of other patches. If spatial-corr? = true in the temporal condition, the model will give a user error and stop running. This is because temporal environment types are perfectly spatially correlated already, and is featured in the model to prevent long and unnecessary BehaviorSpace runs through redundant parameter spaces.

Finally, the center? variable is a patch-only boolean variable which is kept false at all patches except for a randomly selected one during each time step. The patch with center? = true is to be the center of a homogeneous perturbation event, the size of which follows a power law equation:

Radius = 8*R^(-1/6), where R is a randomly selected number from a uniform distribution between 0-1.

The perturbation event only occurs once per time step during the spatiotemporal environment-type setting, and does not occur during the temporal environment-type setting. It is worth noting that the Pn value is an approximation used by Rendell et al (2010) to track the homogeneity of the environmental state values across space (or what they call “effectively autocorrelation… with a spatial ‘lag’ of one cell”). It is the probability that a given patch in the torus has a neighbor with the same s value as its own.

### Process overview and scheduling
This model goes through the following order at each time step: environmental updates, learning behavioral responses, updating fitness values, and with probability of the resulting fitness value (W), dispersal may or may not occur.

The environmental state values update concurrently depending on the environment-type setting. If this is temporal, then all s values change with probability Ps, and if it is spatiotemporal then each patch s value changes with probability Ps, in accordance with its spatial-corr? setting. In the spatiotemporal setting, all center? values are reset to false before one random patch is set to center? = true. The perturbation-event command is then called, and a perturbation event occurs with the center? = true patch at the center.
After the environmental state has been established, each patch is asked to learn their environment by calling the learn-env command, where they acquire a b value (socially or asocially, depending on genotype) and get assigned a cost. After this, fitness is assessed by calling the update-fitness command, which runs through the fitness equation described above and assigns an updated W value to each patch.

All patches are then asked to (reproduce and) disperse based on their fitness value by calling the disperse command with probability W. When this has concluded, the color-update command is called to update each patch color according to genotype. Pn values are then updated, and if the time step is within the final 250 ticks of the simulation, then the mean W and mean proportion of each genotype are each added to their own respective lists before the list means are updated (see below under Observations).

## HOW TO USE IT

### Inititialization

For the pure strategy conditions replicating the model of Rendell et al (Figures 1A and 1B), the spatially explicit condition is initialized at Ns=1000, Ps=0.1, harshness=2, environment-type=temporal (meaning that spatial-corr? is not applicable and thus set to false), pr-asocial-learning=1.0, C-social=0.0, add-mixed-strategies?=false, and both dispersal-condition and learning-type are set to local. This condition varies C-asocial from 0.01 to 0.7 in increments of 0.01, with 20 experiments at each C-asocial value. The global condition, which replicates previous mathematical analyses of Rogers’ paradox without a spatially explicit component, is a replication of the spatially explicit condition but with dispersal-condition and learning-type set to global.

In the mixed strategy conditions, the simulation is initialized at Ns=10, Ps=0.1, environment-type=temporal (meaning that spatial-corr? is not applicable and still set to false), pr-asocial-learning=0.5, C-social=0.02, add-mixed-strategies?=true, and both dispersal-condition and learning-type are set to local. This condition varies C-asocial from 0.01 to 0.7 in increments of 0.01 (20 runs at each C-asocial value), under three different harshness conditions (harshness = 1.1, 2 and 5). This simulation was repeated with the same settings, but environment-type was set to spatiotemporal and spatial-corr? was set to true. This was to repeat the first mixed strategy simulation, but after weakening the effectiveness of social learning.

### Submodels

#### perturbation-event
This occurs at each time step among spatially varying environmental states, resulting in homogeneous, circular clusters of environmental states that follow the power law equation:

Radius = 8*R^(-1/6)

After a center? = true patch has been selected already, a random-float number (0-1) is assigned to R, and the radius is calculated thereafter. All patches within that radius of the randomly selected center patch are then asked to change their environmental state to a random s value between 1 and Ns.

#### color-update

This is for visualizing the demographic changes among different strategy types. If genotype = 0 (asocial) then patch color is blue, if genotype = 1 (asocial) then patch color is yellow, if genotype = 2 (conditional) then patch color is orange and if genotype = 3 (critical) then patch color is red. Patch color turns black if genotypes deviate by going above 3, which is unlikely but a visual failsafe nonetheless.

#### learn-env

This is the command where individuals learn their environment. Asocial, social, conditional and critical learners if identified by their genotype first. For asocial learners, behavior is set to state number based on the probability of asocial learner success. If asocial learning does fail (which is only possible when pr-asocial < 1), then behavior is set to a random number. In either case, cost is set to C-asocial. Social learners set their behaviors to that of another patch (in the torus for global learning-type, and in their immediate Moore neighborhood for the local learning-type), and then set their cost to C-social.

If this command is called for a conditional learner, then it will run through the asocial learning procedure as described above. If the resulting behavior does not equal the state value, then it will run through the social procedure as described above. Conversely, if this command is called for a critical learner then it will run through the social learning procedure as described above, and if the resulting behavior does not equal the state value, it will run through the asocial procedure as described above.

#### update-fitness

The error term Sb is set according the accuracy of behavior relative to state, such that:

For all abs(s - b) <= Ns / 2:
Sb = abs(s - b)

For all abs(s - b) > Ns / 2:
Sb = Ns - abs(s - b)

Fitness is then updated by the equation outlined by Rendell et al (2010):

W = (1 / (harshness ^ Sb)) - cost

To maintain the [0, 1] boundaries of W in this model, if W < 0 then W is reset to zero.

#### disperse

If a patch is selected to execute the disperse command (the probability of which was based on its fitness, as discussed), then it will first determine if it is leaving a mutation or a copy of itself. In the rare event (pr < 0.0008) that it is a mutation, it will ask another patch to set its genotype to a random number (0-1 if add-mixed?=false, 0-3 if add-mixed?=true). In most cases, when it is leaving a copy of itself, it will ask another patch to change its genotype to the genotype of itself.

This procedure is spelled out in the disperse command twice, because in the local dispersal condition it is asking a neighboring patch to change its genotype, and in the global dispersal condition it is asking any random patch in the torus to change its genotype (but the procedures are otherwise identical).

## THINGS TO NOTICE

### Design concepts

#### Basic principles

The basic principle addressed by this model is the underlying evolutionary game theoretic concept of asocial and social learning among pure strategy learners in a dynamic environment. This model is conceptually rooted in the fundamental relationship between environment state, organism behavior in the context of that environmental state, and the impact that this behavior has on fitness. A basic assumption of this model, then, is the abstract and uncontroversial notion that organisms “correctly” responding to their environmental state are more likely to have higher fitness outcomes than those who do not. Two learning strategies explored in in depth with this model are asocial and social learning.

Within a population of asocial learners, individuals independently track environmental states while incurring a cost. Assuming a relatively low rate of error, the mean fitness of asocial learners is almost entirely a result of learning cost because their informed behavior is likely to be in accordance with their environmental state, so they are unlikely to be penalized for incorrect behaviors.

Social learners, on the other hand, indiscriminately copy other individuals’ behaviors at little to no cost because they bypass environment tracking efforts. When a small number of social learners is introduced into a population of asocial learners, they have a high probability of cheaply copying high quality, up-to-date information from an asocial learner, resulting in a relatively substantial fitness advantage. The fitness of social learners, however, is frequency-dependent. When the proportion of social learners increases, asocial learners are displaced and fewer environment-tracking individuals are feeding relevant information to social learners. In an extreme case, a population of only social learners would effectively have zero fitness because they would have no means of tracking their dynamic environmental conditions. Thus, as an invasion of social learners spreads, the probability increases that a given social learner will copy lower quality, potentially outdated information from another social learner.

#### Emergence

The emergent result of the frequency-dependent payoff structure between social and asocial learners is the basis for Rogers’ Paradox. The fitness advantage among social learners leads to their spread in a population, and this spread leads to a decrease in their fitness until a polymorphic equilibrium is reached. At this equilibrium, the mean fitness among the population is equivalent to the original mean fitness among only asocial learners (assuming the same costs and environmental conditions in each scenario). This is considered a paradox because of the commonly held assertion that culture enhances fitness.

This model demonstrates the emergence of polymorphic equilibria reached among learning strategies in various environmental conditions. Moreover, it also demonstrates the emergence of mean fitness effects associated with these equilibria across ecological conditions, which may result above or below the Rogers’ paradox “threshold” (mean W = 1 - C-asocial). Mean fitnesses above the threshold indicate a resolution of Rogers’ paradox. The basic rules underlying these emergent properties are simple heritable learning rules associated with different costs. The effectiveness of asocial learning rules is fixed (pr-asocial), but the effectiveness of social learning rules is inversely related to the emergent increase of social learners. This feedback essentially drives mean fitness downward.

This is particularly distinct in the local learning and dispersal condition, in which many small clusters of social learners form as a result of higher social learner fitness in contact zones with asocial learners (i.e., cluster edges) and lower social learner fitness inside the clusters. This local condition phenomenon accommodates a more widespread invasion of social learners, with an overall lower mean fitness (see Collectives for brief discussion on this phenomenon).

In mixed strategy simulations, the expected utilities of conditional and critical learners are equivalent when social and asocial learning are equally effective, harshness is medium to low, and asocial costs are low. By spatially varying the environment in these conditions, social learning becomes less effective and conditional learners emerge as the dominant portion of the population until a pivotal asocial cost is reached. The increase of asocial costs in low harshness settings results in the prevalence of random social learning, which is a robust result because penalties for error are low and asocial costs are avoided. This scenario raises mean fitness, resolving Rogers’ paradox. In most other harshness and environmental stability settings, critical social learners prevail and similarly, they resolve Rogers’ paradox because they only incur asocial costs when necessary, and raise mean fitness across a range of harshness and asocial cost parameter spaces. These emergent trends in demography and mean fitness are informative results of simple learning strategies with different costs, environmental harshness, and spatial stability levels in environmental state.

#### Adaptation

Individuals adapt to state values by unthinkingly employing different heritable strategies. Because these strategies are inherited and influence individual fitness, the adaptation of a given learning strategy type is the result of most individuals’ reproductive success with that strategy. In other words, in a given set of environmental conditions, individuals are actively adapting to the environment by learning to behave correctly, in the most fitness-enhancing way possible. This results in the emergence of an evolutionarily adaptive learning strategy, particular to the given set of environmental conditions, as a result of continued existence for some learning types (and extinction for others).

#### Objectives

The individuals in patches have an objective to correctly assess (i.e., learn about) and respond to their environmental state. Those doing so successfully are more likely to reproduce, meaning that the goal-directed nature of individuals “solving” or successfully learning their environmental state is an effect of the underlying rules. The objective, then is for an individual to maximize its fitness and reproduce at a higher rate than other individuals.

#### Learning

Learning is the central phenomenon investigated in this model. Individuals in this simulation are learning the environmental states of their patches with different strategies, which results in different fitness outcomes. Strategies which are better at acquiring the correct state information in a given environment, and responding appropriately, are more likely to reproduce and invade the population. Strategies which incur too many costs and incorrect responses are more likely to be driven to extinction. The success of learning, then, is contingent on expected utility for this evolutionary game theoretic model, where utility is reproductive success in a finite environment.

#### Prediction

Individuals learn the environment in order to predict the correct behavior to execute, though a simplifying assumption of this model is that individuals are predicting the correct behaviors based on the state that they are learning. Because behavioral responses and learned state are inextricably linked in this model, prediction is an assumption (but not an ability with varying competences). In other words, prediction is tacitly suggested by adaptive traits such as behavioral responses to state.

#### Sensing

Individuals are assumed to sense the environmental state when they have asocial learning capacities. Those with social learning capacities are assumed to sense the behavioral responses to environmental state of other individuals, either globally or locally (depending on the learning type condition). If this condition is global, then social learners can sense the behaviors of others across the entire World. Social learners in the local condition can sense only the behaviors of their neighbors (in a Moore neighborhood). Individuals neither “know” nor have to know the fitness consequences of their behaviors, because this is selected for based on differential fitness outcomes.

#### Interaction

In an instance of social learning, the model’s agents interact by copying the behavior of other agents. In instances of asocial learning, there is no active interaction on the asocial learners’ part with other agents, but another socially learning agent may be copying the behavior of the asocial learner. During the reproductive and dispersal stage of each time step, the most significant interaction between agents takes place, because the result of dispersing offspring into a finite number of patches necessarily results in the displacement of many individuals of the previous generation by incoming offspring. The competition between agents, then, is for continued existence in this model.

#### Stochasticity

The genotypes of individuals at initialization, along with their state values, are assigned randomly. The simulation also relies on stochasticity for resetting environmental states (including perturbation events, both in size and location), along with the existence of an environmental change in the first place. It is also used for copying the behavior of social learners, because an individual learning socially selects a random patch in its learning scope (based on condition). While fitness updates are a non-random result of random states and behavioral outcomes, the value of fitness itself also serves as a probability against which randomly drawn numbers between 0-1 are compared at each time step. This determines the dispersal of offspring, which results in the displacement of a random neighbor (or patch, depending on dispersal condition). This displacement is truly stochastic in that it does not take the displaced individual’s fitness, age, learning strategy, etc. into account. The mutation rate at mu = 0.0008 also allows for some stochasticity, in that transmission of learning strategy may sometimes result in errors.

#### Collectives

Strategies in this model are distinguished by how behaviors are learned and dictated by the heritable genotypes in this model. There is no other overt top-down force or cooperation rules that establish collectives in this model; they are always a bottom-up, emergent property. In the spatially explicit conditions (i.e., local learning and dispersal), the clustering of social learners is perhaps the most readily apparent instance of collective formation in this model. This is the simple result of environmental changes, because in the event of a state change, social learners in close contact with asocial learners are more likely to copy up-to-date information. On the other hand, social learners inside the middle of the collective are receiving this information by means of a copying cascade, but individuals further from the cluster edges are receiving increasingly outdated information. This is damaging to the fitness of individuals inside large clusters of social learners, and seems to favor smaller clusters of social learners maximizing their “surface area” (i.e., collective contact with asocial learners). This emergent process alone seems to be responsible for the social learner clustering in spatially explicit conditions.

#### Observation

The data collected were mean fitnesses in the population, proportion of social learners, and (if applicable) proportions of conditional and critical learners (W-mean, s-mean, cond-mean and crit-mean, respectively). These were all taken from the mean values of the last 250 time steps of a 2000 time step simulation, which was done by compiling lists for each of these values during time steps 1750-2000, updating the mean of these lists in each time step, and collecting the final value at the last time step. The plots coded on the interface included mean fitness across time and mean proportion of learning types across time, which allows the observer to track these relevant data visually during a simulation. Furthermore, informative plots in the interface include spatial autocorrelation (i.e., homogeneity of environmental state), which plots the Pn value across time, and mean environmental state across time. The former is for observing how conducive to social learning the environment is at a given time (low stability undermines the effectiveness of social learning), and the latter is for detecting sudden shifts in environmental states. This often can coincide with and account for either sudden demographic shifts in the proportion of learners plot, sudden changes in mean fitness, or both.

## CREDITS AND REFERENCES

Rendell, L, Fogarty, L, Laland, KN, 2010. Rogers' paradox recast and resolved: Population structure and the evolution of learning strategies. Evolution 64(2):534-48

Rogers, A, 1988. Does biology constrain culture? American Anthropologist 90:819-831
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
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="local" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 100</exitCondition>
    <metric>mean [W] of patches</metric>
    <metric>(count patches with [genotype = 1] + count patches with [genotype = 3]) / count patches</metric>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harshness">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="global" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 100</exitCondition>
    <metric>mean [W] of patches</metric>
    <metric>(count patches with [genotype = 1] + count patches with [genotype = 3]) / count patches</metric>
    <enumeratedValueSet variable="Ns">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harshness">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="local-harshness" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 100</exitCondition>
    <metric>mean [W] of patches</metric>
    <metric>(count patches with [genotype = 1] + count patches with [genotype = 3]) / count patches</metric>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harshness">
      <value value="1.1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="global-harshness" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 100</exitCondition>
    <metric>mean [W] of patches</metric>
    <metric>(count patches with [genotype = 1] + count patches with [genotype = 3]) / count patches</metric>
    <enumeratedValueSet variable="Ns">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harshness">
      <value value="1.1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPERIMENT 1 LOCAL" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [W] of patches</metric>
    <metric>W-mean</metric>
    <metric>s-mean</metric>
    <metric>count patches with [genotype = 1] / count patches</metric>
    <enumeratedValueSet variable="harshness">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.11"/>
      <value value="0.12"/>
      <value value="0.13"/>
      <value value="0.14"/>
      <value value="0.15"/>
      <value value="0.16"/>
      <value value="0.17"/>
      <value value="0.18"/>
      <value value="0.19"/>
      <value value="0.2"/>
      <value value="0.21"/>
      <value value="0.22"/>
      <value value="0.23"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.26"/>
      <value value="0.27"/>
      <value value="0.28"/>
      <value value="0.29"/>
      <value value="0.3"/>
      <value value="0.31"/>
      <value value="0.32"/>
      <value value="0.33"/>
      <value value="0.34"/>
      <value value="0.35"/>
      <value value="0.36"/>
      <value value="0.37"/>
      <value value="0.38"/>
      <value value="0.39"/>
      <value value="0.4"/>
      <value value="0.41"/>
      <value value="0.42"/>
      <value value="0.43"/>
      <value value="0.44"/>
      <value value="0.45"/>
      <value value="0.46"/>
      <value value="0.47"/>
      <value value="0.48"/>
      <value value="0.49"/>
      <value value="0.5"/>
      <value value="0.51"/>
      <value value="0.52"/>
      <value value="0.53"/>
      <value value="0.54"/>
      <value value="0.55"/>
      <value value="0.56"/>
      <value value="0.57"/>
      <value value="0.58"/>
      <value value="0.59"/>
      <value value="0.6"/>
      <value value="0.61"/>
      <value value="0.62"/>
      <value value="0.63"/>
      <value value="0.64"/>
      <value value="0.65"/>
      <value value="0.66"/>
      <value value="0.67"/>
      <value value="0.68"/>
      <value value="0.69"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPERIMENT 1 GLOBAL" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [W] of patches</metric>
    <metric>W-mean</metric>
    <metric>s-mean</metric>
    <metric>count patches with [genotype = 1] / count patches</metric>
    <enumeratedValueSet variable="harshness">
      <value value="2"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;global&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.11"/>
      <value value="0.12"/>
      <value value="0.13"/>
      <value value="0.14"/>
      <value value="0.15"/>
      <value value="0.16"/>
      <value value="0.17"/>
      <value value="0.18"/>
      <value value="0.19"/>
      <value value="0.2"/>
      <value value="0.21"/>
      <value value="0.22"/>
      <value value="0.23"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.26"/>
      <value value="0.27"/>
      <value value="0.28"/>
      <value value="0.29"/>
      <value value="0.3"/>
      <value value="0.31"/>
      <value value="0.32"/>
      <value value="0.33"/>
      <value value="0.34"/>
      <value value="0.35"/>
      <value value="0.36"/>
      <value value="0.37"/>
      <value value="0.38"/>
      <value value="0.39"/>
      <value value="0.4"/>
      <value value="0.41"/>
      <value value="0.42"/>
      <value value="0.43"/>
      <value value="0.44"/>
      <value value="0.45"/>
      <value value="0.46"/>
      <value value="0.47"/>
      <value value="0.48"/>
      <value value="0.49"/>
      <value value="0.5"/>
      <value value="0.51"/>
      <value value="0.52"/>
      <value value="0.53"/>
      <value value="0.54"/>
      <value value="0.55"/>
      <value value="0.56"/>
      <value value="0.57"/>
      <value value="0.58"/>
      <value value="0.59"/>
      <value value="0.6"/>
      <value value="0.61"/>
      <value value="0.62"/>
      <value value="0.63"/>
      <value value="0.64"/>
      <value value="0.65"/>
      <value value="0.66"/>
      <value value="0.67"/>
      <value value="0.68"/>
      <value value="0.69"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPERIMENT 2B (harshness)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [W] of patches</metric>
    <metric>W-mean</metric>
    <metric>s-mean</metric>
    <metric>count patches with [genotype = 1] / count patches</metric>
    <enumeratedValueSet variable="harshness">
      <value value="1.1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.01"/>
      <value value="0.02"/>
      <value value="0.03"/>
      <value value="0.04"/>
      <value value="0.05"/>
      <value value="0.06"/>
      <value value="0.07"/>
      <value value="0.08"/>
      <value value="0.09"/>
      <value value="0.1"/>
      <value value="0.11"/>
      <value value="0.12"/>
      <value value="0.13"/>
      <value value="0.14"/>
      <value value="0.15"/>
      <value value="0.16"/>
      <value value="0.17"/>
      <value value="0.18"/>
      <value value="0.19"/>
      <value value="0.2"/>
      <value value="0.21"/>
      <value value="0.22"/>
      <value value="0.23"/>
      <value value="0.24"/>
      <value value="0.25"/>
      <value value="0.26"/>
      <value value="0.27"/>
      <value value="0.28"/>
      <value value="0.29"/>
      <value value="0.3"/>
      <value value="0.31"/>
      <value value="0.32"/>
      <value value="0.33"/>
      <value value="0.34"/>
      <value value="0.35"/>
      <value value="0.36"/>
      <value value="0.37"/>
      <value value="0.38"/>
      <value value="0.39"/>
      <value value="0.4"/>
      <value value="0.41"/>
      <value value="0.42"/>
      <value value="0.43"/>
      <value value="0.44"/>
      <value value="0.45"/>
      <value value="0.46"/>
      <value value="0.47"/>
      <value value="0.48"/>
      <value value="0.49"/>
      <value value="0.5"/>
      <value value="0.51"/>
      <value value="0.52"/>
      <value value="0.53"/>
      <value value="0.54"/>
      <value value="0.55"/>
      <value value="0.56"/>
      <value value="0.57"/>
      <value value="0.58"/>
      <value value="0.59"/>
      <value value="0.6"/>
      <value value="0.61"/>
      <value value="0.62"/>
      <value value="0.63"/>
      <value value="0.64"/>
      <value value="0.65"/>
      <value value="0.66"/>
      <value value="0.67"/>
      <value value="0.68"/>
      <value value="0.69"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;spatiotemporal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPERIMENT 3 (mixed harshness)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [W] of patches</metric>
    <metric>W-mean</metric>
    <metric>s-mean</metric>
    <metric>cond-mean</metric>
    <metric>crit-mean</metric>
    <metric>count patches with [genotype = 1] / count patches</metric>
    <metric>count patches with [genotype = 2] / count patches</metric>
    <metric>count patches with [genotype = 3] / count patches</metric>
    <enumeratedValueSet variable="harshness">
      <value value="1.1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;spatiotemporal&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="EXPERIMENT 3 (no spatial var-mixed harshness)" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [W] of patches</metric>
    <metric>W-mean</metric>
    <metric>s-mean</metric>
    <metric>cond-mean</metric>
    <metric>crit-mean</metric>
    <metric>count patches with [genotype = 1] / count patches</metric>
    <metric>count patches with [genotype = 2] / count patches</metric>
    <metric>count patches with [genotype = 3] / count patches</metric>
    <enumeratedValueSet variable="harshness">
      <value value="1.1"/>
      <value value="2"/>
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="seed" first="1" step="1" last="20"/>
    <enumeratedValueSet variable="Ps">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="spatial-corr?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pr-asocial-learning">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="learning-type">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="add-mixed-strategies?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-social">
      <value value="0.02"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dispersal-condition">
      <value value="&quot;local&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="C-asocial">
      <value value="0.01"/>
      <value value="0.05"/>
      <value value="0.1"/>
      <value value="0.15"/>
      <value value="0.2"/>
      <value value="0.25"/>
      <value value="0.3"/>
      <value value="0.35"/>
      <value value="0.4"/>
      <value value="0.45"/>
      <value value="0.5"/>
      <value value="0.55"/>
      <value value="0.6"/>
      <value value="0.65"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ns">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="environment-type">
      <value value="&quot;temporal&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
0
@#$#@#$#@
