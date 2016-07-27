# swale
Original code by Reasbeck, et al. in "INSIDE CASE-BASED EXPLANATION"

## Loading with Quicklisp

```
$ cd ~/quicklisp/local-projects
$ git clone https://github.com/SeijiKoide/swale.git
```
and

```cl
(asdf:initialize-source-registry)
(ql:quickload :icbe-swale)
```

## Running the demos

```cl
(in-package :cbr)


(reset-swale-memory)
→  ("swale-mem")


(load-swale-memory)
→  ("swale-mem")


(explain-swale)
⊳  
⊳  
⊳  Micro Explainer
⊳  ---------------
⊳  Trying to explain m-swale-story
⊳  which has the anomaly:
⊳    :instance isa m-event-sequence-conflict
⊳    :event-sequence: m-racehorse-life
⊳    :expectation: m-expectation-1
⊳    :surprising-fact: m-swale-dead-state
⊳  
⊳  
⊳  Micro XP Retriever
⊳  ------------------
⊳  Searching for XP's.
⊳  ... found m-die-from-illness-xp, m-die-from-old-age-xp.
⊳  Applying m-die-from-illness-xp.
⊳  ... generated m-expl138.
⊳  Applying m-die-from-old-age-xp.
⊳  ... generated m-expl139.
⊳  
⊳  
⊳  Micro-Accepter
⊳  --------------
⊳  Evaluating m-expl138 for the purpose prediction.
⊳  The underling XP is
⊳    :mop isa m-xp
⊳    :events: ((:illness m-bad-health-state) (:outcome m-dead-state))
⊳    :causals: ((:illness => :outcome))
⊳    :constraints: (((:events :illness :object) (:events :outcome :object)))
⊳  Checking if explanation accounts for m-swale-dead-state.
⊳  ... appears relevant.
⊳  Checking believability.
⊳  Problem found!
⊳    :instance isa m-non-normative-filler-problem
⊳    :explanation: m-expl138
⊳    :path: (:events :illness :object)
⊳    :expected-value: m-unhealthy-animal
⊳    :observed-value: m-swale
⊳  ... explanation is not believable.
⊳  Checking if explanation is useful for the purpose prediction.
⊳  Is the explanation predictive?
⊳  ... Explanation is adequate for prediction.
⊳  Checking seriousness of problems.
⊳  ... m-expl138 is tweakable.
⊳  
⊳  
⊳  Micro Tweaker
⊳  -------------
⊳  Looking for candidate tweaks in memory for m-prob140.
⊳  ... found m-replace-action-use-causals-tweak, m-replace-actor-use-stereotypes-tweak, m-replace-action-use-stereotypes-tweak.
⊳  Trying tweak replace-action-search-through-causal-rules on m-expl138.
⊳  ... tweak failed.
⊳  Trying tweak replace-actor-search-through-stereotypes on m-expl138.
⊳  ... tweak failed.
⊳  Trying tweak replace-action-search-through-stereotypes on m-expl138.
⊳  Looking for effects to be accounted for.
⊳  ... found m-dead-state.
⊳  Looking for scripts for m-swale that include m-dead-state.
⊳  ... no scripts found.
⊳  ... tweak failed.
⊳  
⊳  
⊳  Micro XP Retriever
⊳  ------------------
⊳  Searching for XP's.
⊳  Looking for retrieval cues in m-prob140.
⊳  ... Cue: observed m-swale, rather than m-unhealthy-animal.
⊳  ... found m-fixx-xp.
⊳  Applying m-fixx-xp.
⊳  ... generated m-expl141.
⊳  
⊳  
⊳  Micro-Accepter
⊳  --------------
⊳  Evaluating m-expl139 for the purpose prediction.
⊳  The underling XP is
⊳    :mop isa m-xp
⊳    :events: ((:old-age m-old-age-state) (:outcome m-dead-state))
⊳    :causals: ((:old-age => :outcome))
⊳    :constraints: (((:events :old-age :object) (:events :outcome :object)))
⊳  Checking if explanation accounts for m-swale-dead-state.
⊳  ... appears relevant.
⊳  Checking believability.
⊳  Problem found!
⊳    :instance isa m-non-normative-filler-problem
⊳    :explanation: m-expl139
⊳    :path: (:events :old-age :object)
⊳    :expected-value: m-old-animal
⊳    :observed-value: m-swale
⊳  ... explanation is not believable.
⊳  Checking if explanation is useful for the purpose prediction.
⊳  Is the explanation predictive?
⊳  ... Explanation is adequate for prediction.
⊳  Checking seriousness of problems.
⊳  ... m-expl139 is tweakable.
⊳  
⊳  
⊳  Micro Tweaker
⊳  -------------
⊳  Looking for candidate tweaks in memory for m-prob142.
⊳  ... found m-replace-action-use-causals-tweak, m-replace-actor-use-stereotypes-tweak, m-replace-action-use-stereotypes-tweak.
⊳  Trying tweak replace-action-search-through-causal-rules on m-expl139.
⊳  ... tweak failed.
⊳  Trying tweak replace-actor-search-through-stereotypes on m-expl139.
⊳  ... tweak failed.
⊳  Trying tweak replace-action-search-through-stereotypes on m-expl139.
⊳  Looking for effects to be accounted for.
⊳  ... found m-dead-state.
⊳  Looking for scripts for m-swale that include m-dead-state.
⊳  ... no scripts found.
⊳  ... tweak failed.
⊳  
⊳  
⊳  Micro XP Retriever
⊳  ------------------
⊳  Searching for XP's.
⊳  Looking for retrieval cues in m-prob142.
⊳  ... Cue: observed m-swale, rather than m-old-animal.
⊳  ... no XP's found.
⊳  
⊳  
⊳  Micro-Accepter
⊳  --------------
⊳  Evaluating m-expl141 for the purpose prediction.
⊳  The underling XP is
⊳    :mop isa m-xp
⊳    :observed: m-healthy-animal
⊳    :expected: m-unhealthy-animal
⊳    :events: ((:jog m-jog-event) (:run m-run-event) (:exert m-exert-event)
⊳              (:defect m-heart-defect-state) (:outcome m-dead-state))
⊳    :causals: ((:jog => :run) (:run => :exert) (:defect :exert => :outcome))
⊳    :constraints: (((:events :jog :actor) (:events :run :actor)
⊳                    (:events :exert :actor) (:events :defect :owner)
⊳                    (:events :outcome :object)))
⊳  Checking if explanation accounts for m-swale-dead-state.
⊳  ... appears relevant.
⊳  Checking believability.
⊳  Problem found!
⊳    :instance isa m-non-normative-filler-problem
⊳    :explanation: m-expl141
⊳    :path: (:events :jog :actor)
⊳    :expected-value: m-human
⊳    :observed-value: m-swale
⊳  ... explanation is not believable.
⊳  Checking if explanation is useful for the purpose prediction.
⊳  Is the explanation predictive?
⊳  Problem found!
⊳    :instance isa m-non-knowable-antecedent-problem
⊳    :explanation: m-expl141
⊳    :fact: m-heart-defect-state
⊳    :event-label: :defect
⊳  ... Explanation is not adequate for prediction.
⊳  Checking seriousness of problems.
⊳  ... m-expl141 is tweakable.
⊳  
⊳  
⊳  Micro Tweaker
⊳  -------------
⊳  Looking for candidate tweaks in memory for m-prob143.
⊳  ... found m-replace-action-use-causals-tweak, m-replace-actor-use-stereotypes-tweak, m-replace-action-use-stereotypes-tweak.
⊳  Trying tweak replace-action-search-through-causal-rules on m-expl141.
⊳  ... tweak failed.
⊳  Trying tweak replace-actor-search-through-stereotypes on m-expl141.
⊳  ... tweak failed.
⊳  Trying tweak replace-action-search-through-stereotypes on m-expl141.
⊳  Looking for effects to be accounted for.
⊳  ... found m-run-event.
⊳  Looking for scripts for m-swale that include m-run-event.
⊳  ... found m-horserace-event.
⊳  Creating a new xp m-xp145 with m-horserace-event instead of m-jog-event
⊳  Applying m-xp145.
⊳  ... generated m-expl146.
⊳  ... tweak generated m-expl146.
⊳  Looking for candidate tweaks in memory for m-prob144.
⊳  ... no tweaks found.
⊳  
⊳  
⊳  Micro XP Retriever
⊳  ------------------
⊳  Searching for XP's.
⊳  Looking for retrieval cues in m-prob143.
⊳  ... Cue: observed m-swale, rather than m-human.
⊳  Looking for retrieval cues in m-prob144.
⊳  ... no XP's found.
⊳  
⊳  
⊳  Micro-Accepter
⊳  --------------
⊳  Evaluating m-expl146 for the purpose prediction.
⊳  The underling XP is
⊳    :mop isa m-xp
⊳    :events: ((:jog m-horserace-event) (:run m-run-event) (:exert m-exert-event)
⊳              (:defect m-heart-defect-state) (:outcome m-dead-state))
⊳    :causals: ((:jog => :run) (:run => :exert) (:defect :exert => :outcome))
⊳    :observed: m-healthy-animal
⊳    :expected: m-unhealthy-animal
⊳  Checking if explanation accounts for m-swale-dead-state.
⊳  ... appears relevant.
⊳  Checking believability.
⊳  ... no problems.
⊳  Checking if explanation is useful for the purpose prediction.
⊳  Is the explanation predictive?
⊳  Problem found!
⊳    :instance isa m-non-knowable-antecedent-problem
⊳    :explanation: m-expl146
⊳    :fact: m-heart-defect-state
⊳    :event-label: :defect
⊳  ... Explanation is not adequate for prediction.
⊳  Checking seriousness of problems.
⊳  ... m-expl146 is acceptable.
⊳  
→  m-expl146
```
