import deno from "@deno/vite-plugin";
import { defineConfig } from "vite";

// https://vite.dev/config/
export default defineConfig({
  plugins: [deno()],
});
