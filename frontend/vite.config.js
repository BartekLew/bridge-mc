import { defineConfig } from 'vite'
import vue from '@vitejs/plugin-vue'

// https://vite.dev/config/
export default defineConfig({
  plugins: [vue()],
  // Pages: the simulator and play training
  build: {
    rollupOptions: {
      input: ['index.html', 'training.html']
    }
  },
  server: {
    proxy: {
        '/api': {
            target: "http://localhost:5000",
            changeOrigin: true
        }
    }
  }
})
