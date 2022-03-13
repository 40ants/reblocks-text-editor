(uiop:define-package #:reblocks-text-editor/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow)
  (:import-from #:40ants-ci/jobs/linter)
  (:import-from #:40ants-ci/jobs/critic)
  (:import-from #:40ants-ci/jobs/run-tests))
(in-package #:reblocks-text-editor/ci)


(defworkflow linter
  :on-push-to "master"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/linter:linter)))


(defworkflow critic
  :on-push-to "master"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/critic:critic
          :ignore-critiques  ("function-too-long"))))


(defworkflow tests
  :on-push-to "master"
  :by-cron "0 10 * * 1"
  :on-pull-request t
  :cache t
  :jobs ((40ants-ci/jobs/run-tests:run-tests
          :coverage t)))
