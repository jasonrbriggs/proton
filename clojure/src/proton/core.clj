(ns proton.core)

(import 'proton.Template)


(defn default-value [value defvalue]
    (if value
        value
        defvalue))


(defn templates [dir]
    (new proton.Templates dir))

    
(defn get-template [tmps tmp-name]
    (. tmps get tmp-name))

    
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


(defn hide-element
    ([tmp eid]
        (let [ignored (. tmp hideElement eid -1)]
            tmp)
        )
    ([tmp eid pos]
        (let [ignored (. tmp hideElement eid pos)]
            tmp)
        ))


(defn repeat-element
    ([tmp eid count]
        (let [ignored (. tmp repeatElement eid count -1)]
            tmp)
        )
    ([tmp eid count pos]
        (let [ignored (. tmp repeatElement eid count pos)])))

        
(defn include
    ([tmp eid template-path]
        (let [ignored (. tmp include eid template-path)]
            tmp)))
