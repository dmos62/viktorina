(ns viktorina.core
  (:require
    [clojure.string]
    [hiccup.core :as hic]
    [markdown.core :as md]
            ))

(defprotocol IContent
  (render [content])
  (save [content])
  )

(defrecord Content [about raw]
  IContent
  (render [content]
    (let [raw (:raw content)]
      (cond
        (vector? raw) ; clojure structure
        ; https://github.com/weavejester/hiccup
        (hic/html raw)
        (string? raw) ; markdown string
        ; https://github.com/yogthos/markdown-clj
        (md/md-to-html-string raw)
        )))
  (save [content]
    (let [href (-> content :about :href)]
      (->>
        (render content)
        (spit href) ; http://stackoverflow.com/a/7757674/1714997
        )))
  )

(defprotocol IExtendedFile
  (split-ext [file])
  (neim [file])
  (extension [file]) 
  (content-file? [file])
  (extract-about [file])
  (extract-raw [file])
  (to-content [file])
  )

(defn split-sections [^java.io.File file]
  ; reikejo sita iskelti is extend-type del neaiskaus bug'o
  (let [whole (slurp file)]
    (clojure.string/split whole #"\n*\s*<=>\s*\n*" 2)
    ))

(extend-type java.io.File
  IExtendedFile
  (split-ext [file] 
    ; https://github.com/Raynes/fs/blob/567dc11ef9021c75b42ff9ee58793f4032d8ecf3/src/me/raynes/fs.clj#L233
    (let [neim (.getName file)
          i (.lastIndexOf neim ".")]
      (cond
        (pos? i) [(subs neim 0 i) (subs neim (inc i))]
        true [neim nil]
        )))
  (neim [file] (first (split-ext file)))
  (extension [file] (second (split-ext file)))
  (content-file? [file]
    (and
      (.isFile file)
      (-> (extension file) (= "content"))
      ))
  (extract-about [file] (first (split-sections file)))
  (extract-raw [file] (second (split-sections file)))
  (to-content [file]
    (map->Content
      {:about (merge 
                (extract-about file)
                {:href file}) ; TODO
       :raw (extract-raw file)
       }))
  )

(defn files-in [path] (-> (clojure.java.io/file path) file-seq))

(defn content-in [path-to-dir]
  (->>
    (files-in path-to-dir)
    (filter content-file?)
    (map to-content)
    ))

(def projects (content-in "projects"))

(defn render-project-summary [project-content]
  (let [{:keys [main-image summary href]} (project-content :about)]
    [:a {:href href}
     [:img {:src main-image}]
     [:p summary]
     ]))

(defn wrap-with [wrapper wrappee] (vector wrapper wrappee))

(def index
  (map->Content
    {:about {}
     :raw
     [:body
      [:header "Projects"]
      [:ul
       (->> (map render-project-summary projects)
            (map (partial wrap-with :li))
            )]
      [:footer]
      ]}))

; (save index)
; (save contacts)
; (->>
;   projects
;   (map save)
;   doall)


;/* = */

; turinio failas gali buti padalintas i aprosamaji edn hashmap'a ir raw'ui parasyta turini

#_{:main-image "main.png"
 :summary "Very good nice project. Very good."
 :title "Nice"
 :type :portfolio-item
 }

; zemiau galetu buti markup'u parasytas tekstas. tai butu nepatogu kaip clojure kodas.

#_[:presentation
 [:p "this here is a nice facade"]
 [:img {:src (asset-file "side-view.jpg")}]
 [:p "look at that nice side view"]
 [:img {:src (asset-file "side-view.jpg")}]
 [:p "bye bye"]]

;/* = */
