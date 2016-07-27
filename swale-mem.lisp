(defpackage :cbr (:use :cl))
(in-package :cbr)

;;; ----------------------------------------------------------
;;; MOPs for SWALE Demo
;;; ----------------------------------------------------------
;;; Programmer: Chris Riesbeck

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "mops")
  )

#|
;;; Time and Race
;;; ----------------------------------------------------------

(defmop m-time)
(defmop m-race)
|#
;;; Abstract values
;;; ----------------------------------------------------------

(defmop m-value)
(defmop m-sick (m-value))
(defmop m-healthy (m-value))


;;; Things
;;; ----------------------------------------------------------

(defmop m-thing)
(defmop m-animate (m-thing))
(defmop m-animal (m-animate))
(defmop m-human (m-animal))
(defmop m-horse (m-animal))
(defmop m-racehorse (m-horse))

(defmop m-healthy-animal (m-animal)
  :health m-healthy)

(defmop m-unhealthy-animal (m-animal)
  :health m-sick)

(defmop m-alive-animal (m-animal)
  :health m-alive)

(defmop m-dead-animal (m-animal)
  :health m-dead)

(defmop m-young-animal (m-animal)
  :age m-young)

(defmop m-old-animal (m-animal)
  :age m-old)

(defmop m-body-part)
(defmop m-heart (m-body-part))
(defmop m-leg (m-body-part))
(defmop m-bad-heart (m-heart)
  :health m-sick)

(defmop m-bad-hert-animal (m-animal)
  :heart m-bad-heart)

;;; Events
;;; ----------------------------------------------------------

(defmop m-fact)

(defmop m-event (m-fact))

(defmop m-run-event (m-event)
  :actor m-animal
  :action m-move-body-part
  :object m-leg
  :speed m-fast)

(defmop m-exert-event (m-event)
  :actor m-animal)

(defmop m-warm-up-event (m-event)
  :actor m-animal)

(defmop m-cool-off-event (m-event)
  :actor m-animal)

(defmop m-enter-race-event (m-event)
  :actor m-human
  :object m-race
  :entry m-horse)

(defmop m-mount-event (m-event)
  :actor m-human
  :object m-animal)

(defmop m-win-event (m-event)
  :actor m-animal
  :object m-race)

;;; States
;;; ----------------------------------------------------------

(defmop m-state (m-fact)
  :object m-thing :value m-value :time m-time)

(defmop m-hidden-state (m-state))

(defmop m-present (m-time))
(defmop m-past (m-time))
(defmop m-future (m-time))

(defmop m-alive-state (m-state)
  :object m-alive-animal)

(defmop m-dead-state (m-state)
  :object m-dead-animal)

(defmop m-bad-health-state (m-state)
  :object m-unhealthy-animal)

(defmop m-old-age-state (m-state)
  :object m-old-animal)

(defmop m-heart-defect-state (m-state m-hidden-state)
  :object m-bad-heart-animal)

;;; Abstract explanation MOPs
;;; ----------------------------------------------------------

(defmop m-anomaly)
(defmop m-event-sequence-conflict (m-anomaly))

(defmop m-story ()
  :anomaly m-anomaly
  :background (m-fact))

(defmop m-explanation ()
  :story m-story
  :xp m-xp
  :indices ((:story :xp)))

(defmop m-explanation-problem)

(defmop m-relevance-problem (m-explanation-problem)
  :status hopeless)

(defmop m-believability-problem (m-explanation-problem)
  :status tweakable)

(defmop m-detail-problem (m-explanation-problem)
  :status acceptable)

(defmop m-irrelevant-to-surprising-feature-problem
                         (m-relevance-problem))

(defmop m-non-normative-filler-problem
                         (m-believability-problem))

(defmop m-contradictory-fille-problem
                         (m-believability-problem))

(defmop m-non-knowable-antecedent-problem (m-detail-problem))

(defmop m-explanation-evaluation ()
  :explanation m-explanation
  :problems (m-problem))


;;; The SWALE story
;;; ----------------------------------------------------------

;;; The anomaly of SWALE's death
;;; ----------------------------------------------------------

(definstance m-swale
  (m-racehorse m-healthy-animal m-young-animal))

(definstance m-expectation-1 (m-dead-state)
  :object m-swale
  :time m-future)

(definstance m-swale-dead-state (m-dead-state)
  :object m-swale
  :time m-present)

(definstance m-swale-anomaly (m-event-sequence-conflict)
  :event-sequence m-racehorse-life
  :expectation m-expectation-1
  :surprising-fact m-swale-dead-state)

(definstance m-belmont-race (m-race))

(definstance m-swale-won-belmont (m-win-event)
  :actor m-swale
  :action m-win
  :object m-belmont-race)

(definstance m-swale-story (m-story)
  :anomaly m-swale-anomaly
  :background m-swale-won-belmont)

;;; Scripts
;;; ----------------------------------------------------------

(defmop m-script-event (m-event)
  :indices ((:actor)))

(defmop m-horserace-event (m-script-event)
  :owner m-human
  :jockey m-human
  :actor m-racehorse
  :events ((:enter m-enter-race-event)
           (:mount m-mount-event)
           (:run m-run-event))
  :constraints (((:owner)
                 (:events :enter :actor))
                ((:jockey)
                 (:events :mount :actor))
                ((:actor)
                 (:events :enter :entry)
                 (:events :mount :object)
                 (:events :run :actor))))

(defmop m-jog-event (m-script-event)
  :actor m-human
  :events ((:warm-up m-warm-up-event)
           (:run m-run-event)
           (:cool-off m-cool-off-event))
  :constraints (((:actor)
                 (:events :warm-up :actor)
                 (:events :run :actor)
                 (:events :cool-off :actor))))

;;; XPs and explanations
;;; ----------------------------------------------------------

(defmop m-xp ()
  :indices ((:observed (:events :outcome))))

(defmop m-die-from-old-age-xp (m-xp)
  :events ((:old-age m-old-age-state)
           (:outcome m-dead-state))
  :causals ((:old-age => :outcome))
  :constraints (((:events :old-age :object)
                 (:events :outcome :object))))

(defmop m-die-from-illness-xp (m-xp)
  :events ((:illness m-bad-health-state)
           (:outcome m-dead-state))
  :causals ((:illness => :outcome))
  :constraints (((:events :illness :object)
                 (:events :outcome :object))))

(defmop m-fixx-xp (m-xp)
  :observed m-healthy-animal
  :expected m-unhealthy-animal
  :events ((:jog m-jog-event)
           (:run m-run-event)
           (:exert m-exert-event)
           (:defect m-heart-defect-state)
           (:outcome m-dead-state))
  :causals ((:jog => :run)
            (:run => :exert)
            (:defect :exert => :outcome))
  :constraints (((:events :jog :actor)
                 (:events :run :actor)
                 (:events :exert :actor)
                 (:events :defect :owner)
                 (:events :outcome :object))))

;;; Tweaks
;;; ----------------------------------------------------------

(defmop m-tweak ()
  :problem m-problem
  :indices ((:problem)))

(defmop m-replace-action-use-stereotypes-tweak (m-tweak)
  :function replace-action-search-through-stereotypes
  :problem m-non-normative-filler-problem)

(defmop m-replace-actor-use-stereotypes-tweak (m-tweak)
  :function replace-actor-search-through-stereotypes
  :problem m-non-normative-filler-problem)

(defmop m-replace-action-use-causals-tweak (m-tweak)
  :function replace-action-search-through-causal-rules
  :problem m-non-normative-filler-problem)

;;; End of module
;;; ----------------------------------------------------------

(provide "swale-mem")
