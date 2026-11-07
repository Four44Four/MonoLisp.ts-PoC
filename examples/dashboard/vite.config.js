// Dev server for the generated frontend: Vite serves index.html and build/*.tsx, and forwards
// /rpc to the Lisp backend (npm run serve:dashboard), so the browser sees a single origin.
import { defineConfig } from "vite";

export default defineConfig({
  server: {
    port: 5174,
    proxy: { "/rpc": "http://127.0.0.1:8080" },
  },
});
