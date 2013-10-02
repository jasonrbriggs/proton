(ns proton.proton)

(import 'proton.Template)


(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
  
(defn foo2
    "I don't do much either"
    []
    "test")

(defn default-value [value defvalue]
    (if value
        value
        defvalue))

(defn templates [dir]
    (new proton.Templates dir))
    
(defn get-template [tmps tmpname]
    (. tmps get tmpname))
    
(defn set-value
    ([tmp eid value]
        (let [ignored (. tmp setElement eid value -1)]
            tmp))
    ([tmp eid value pos]
        (let [ignored (. tmp setElement eid value pos)]
            tmp)))

(defn set-attribute
    ([tmp aid name value]
        (let [ignored (. tmp setAttribute aid name value -1)]
            tmp))
    ([tmp aid name value pos]
        (let [ignored (. tmp setAttribute aid name value pos)]
            tmp)))
