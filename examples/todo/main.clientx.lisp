;;;; examples/todo/main.clientx.lisp -- browser entry point: mounts <App /> into #root

(import :from "react-dom/client" :targets (createRoot))
(import :from "app" :targets App)

(.. (createRoot (the |HTMLElement| (doc-get-element-by-id "root")))
    (render (as-html-string (:App))))
