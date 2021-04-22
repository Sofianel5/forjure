(ns forjure.core)

(defmacro assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defmacro if-let-all
  "💈
   if-let but allows multiple items and checks if any are nil"
  ([bindings then]
   `(if-let-all ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
    (vector? bindings) "a vector for its binding"
    (nil? oldform) "1 or 2 forms after binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro if-let-last
  "✅
   if-let but allows multiple items and checks if the last item is nil"
  ([bindings then]
   `(if-let-last ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
    (vector? bindings) "a vector for its binding"
    (nil? oldform) "1 or 2 forms after binding vector")
   (let [form (-> bindings pop last) tst (last bindings)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ; need to apply let anyway or will cause errors
          (let bindings 
            ~else))))))

(if-let-last [a 0 b 1 c nil] b)

