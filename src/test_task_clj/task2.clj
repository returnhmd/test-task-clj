(ns test-task-clj.task2)

;; сделать макрос, расширенную версию threading marco ->>
;; с возможность давать alias промежуточному результату и вставлять его в произвольные места форм
;; Если одна из промежуточных форм возвращает nil - цепочка должна обрываться

;; примерный синтаксис:
;; (extended->> owners ! (nth ! 0) (:pets !) (deref !) (! 1) (! :type))
;; где owners - начало цепочки, ! - алиас, все остальное формы

(def owners [{:owner "Jimmy"
              :pets (ref [{:name "Rex"
                           :type :dog}
                          {:name "Sniffles"
                           :type :hamster}])}
             {:owner "Jacky"
              :pets (ref [{:name "Spot"
                           :type :mink}
                          {:name "Puff"
                           :type :magic-dragon}])}])

(defmacro extended->>  [x result-alias & forms]
  (loop [x x forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (map #(if (= % result-alias) x %) form)
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(extended->> owners
             !
             (nth ! 0)
             (:pets !)
             (deref !)
             (! 1)
             (! :type)
             name)
