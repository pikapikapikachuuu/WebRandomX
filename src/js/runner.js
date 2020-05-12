import AsyncClass from './async-class'

// template string didnt work...dont know why, seems like a webpack bug
// const binRoot = '../bin/'

class Runner extends AsyncClass {
  constructor (moduleName) {
    super(async () => {
      const Module = (await import('../bin/' + moduleName + '.js')).default
      const wasmBin = (await import('../bin/' + moduleName + '.wasm')).default
      const module = await Module({
        locateFile (path) {
          if (path.endsWith('.wasm')) {
            return wasmBin
          }
          return path
        }
      }).then(m => {
        // emcc's thenable implementation cause infinite loop
        // https://github.com/emscripten-core/emscripten/issues/5820
        delete m['then']
      })
      this.module = module
    })
  }
}

export {
  Runner as default
}
