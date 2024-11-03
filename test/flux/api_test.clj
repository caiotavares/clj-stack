(ns flux.api-test
  (:require [clojure.test :refer :all]
            [flux.api :as api]))

(defn fn-with-no-args [])
(defn fn-with-args [arg1] "result")
(defn fn-with-children [arg1] (fn-with-no-args))
(defn fn-that-throws [arg1] (throw (ex-info "some went bad" {:data "exception"})))

(deftest expand-stack-test
  (testing "Expanding stack of a fn of no args with no children"
    (is (= {:name     :flux.api-test/fn-with-no-args
            :children []}
           (api/expand-stack #'fn-with-no-args))))

  (testing "Expanding stack of a fn of 1 arg with no children"
    (is (= {:name     :flux.api-test/fn-with-args
            :children []}
           (api/expand-stack #'fn-with-args))))

  (testing "Expanding stack of a fn of 1 arg with 1 children"
    (is (= {:name     :flux.api-test/fn-with-children
            :children [{:name     :flux.api-test/fn-with-no-args
                        :children []}]}
           (api/expand-stack #'fn-with-children)))))

(deftest tracing-test
  (testing "Tracing a fn of 1 arg"
    (is (nil? (api/trace-stack #'fn-with-args)))

    (fn-with-args "argument")

    (is (= {:name     :flux.api-test/fn-with-args
            :input    ["argument"]
            :output   "result"
            :throw    nil
            :children []}
           (api/get-stack))))

  (testing "Tracing a fn of 1 arg and children"
    (is (nil? (api/trace-stack #'fn-with-children)))

    (fn-with-children "argument")

    (is (= {:name     :flux.api-test/fn-with-children
            :input    ["argument"]
            :output   nil
            :throw    nil
            :children [{:name     :flux.api-test/fn-with-no-args
                        :children []
                        :input    nil
                        :output   nil
                        :throw    nil}]}
           (api/get-stack))))

  (testing "Tracing a fn that throws an exception"
    (is (nil? (api/trace-stack #'fn-that-throws)))

    (try (fn-that-throws "argument") (catch Exception _e nil))

    (is (= {:name     :flux.api-test/fn-that-throws
            :input    ["argument"]
            :output   nil
            :throw    {:data "exception"}
            :children []}
           (api/get-stack)))))
