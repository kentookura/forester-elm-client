import { defineConfig } from "vite";
import path from "path";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  plugins: [elmPlugin()],
  server: { watch: { ignored: [`${__dirname}/.direnv/**`] } },
});
