(require 'package)


(defconst tlc:config-directory (file-name-directory load-file-name))
(add-to-list 'load-path tlc:config-directory)

(require 'config)


(defconst tlc:devtools-server-dependency
  (make-tlc:fragile-dependency
   :project-name "devtools-server"
   :branch 'development))


(defconst tlc:devtools-client-dependency
  (make-tlc:fragile-dependency
   :project-name "devtools-client"
   :branch 'development))

(defconst tlc:ss-rpc-dependency
  (make-tlc:fragile-dependency
   :project-name "ss-rpc-client"
   :branch 'stable))




(defconst tlc:elisp-fragile-dependencies
  (list
   tlc:ss-rpc-dependency
   tlc:devtools-client-dependency))




(tlc:load-fragile-dependencies
 tlc:elisp-fragile-dependencies)
