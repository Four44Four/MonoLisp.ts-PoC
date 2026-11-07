;;;; examples/dashboard/main.clientx.lisp -- browser entry point: mounts <Dashboard /> into #root

(import :from "react-dom/client" :targets (createRoot))
(import :from "app" :targets Dashboard)                 ; default import of a sibling module

(.. (createRoot (the |HTMLElement| (doc-get-element-by-id "root")))
    (render (as-html-string (:Dashboard))))
