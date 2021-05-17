(ns test-task-clj.task1)

(def all-patients
  '({:firstname "Adam"
     :lastname  "Smith"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Joseph"
     :lastname  "Goodman"
     :diagnosis "COVID-19"
     :treated   true}
    {:firstname "Werner"
     :lastname  "Ziegler"
     :diagnosis "COVID-19"
     :treated   false}
    {:firstname "Boris"
     :lastname  "Henry"
     :diagnosis "Healthy"
     :treated   false}
    {:firstname "Johnny"
     :lastname  "Grayhand"
     :diagnosis "COVID-76"
     :treated   false}))

(defn group-patients [patients ks]
  (->> patients
       (group-by (apply juxt ks))
       (map second)))

(defn extract-keys [sym-n-key-bindings]
  (->> sym-n-key-bindings
       (partition 2)
       (map second)
       vec))

(defn format-bindings [bindings first-group]
  (->> bindings
       (partition 2)
       (map #(vector (first %) `(~(second %) (first ~first-group))))
       (apply concat)
       vec))

(defmacro factor-group [patients group-binding sym-n-key-bindings & body]
  `(let [keys-to-group# ~(extract-keys sym-n-key-bindings)
         grouped-patients# (group-patients ~patients keys-to-group#)]
     (loop [patients-rest# grouped-patients# result# []]
       (if (empty? patients-rest#)
         (apply list result#)
         (let [~group-binding (first patients-rest#)]
           (let ~(format-bindings sym-n-key-bindings group-binding)
             (recur (rest patients-rest#) (conj result# (do ~@body)))))))))

(defn main []
  (factor-group
   all-patients ; передаем список всех пациентов
   patients-group ; определяем имя символа, на который будет завязан список пациентов каждой группы

 ; определяем ключи группировки и соответствующие символы.
 ; эти символы (treated?, disease-name) будем использовать для доступа к значениям соответствующих ключей
 ; (в пределах группы значения не меняются)
   [treated?     :treated
    disease-name :diagnosis]

 ; тело макроса (побочные эффекты допускаются)
 ; TODO: research how this should work
   (println " начало обработки группы пациентов с диагнозом " disease-name
            (if treated? ", подвергавшихся лечению"
                ", НЕ подвергавшихся лечению"))

   (println " количество пациентов в группе - " (count patients-group))
   (println " фамилии пациентов - " (clojure.string/join ", " (map :lastname patients-group)))

 ; в результате для каждой группы вернем количество пациентов; макрос должен сформировать список из этих значений
   (count patients-group)))