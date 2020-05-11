import AsyncClass from './async-class'
import * as path from 'path'

// template string didnt work...dont know why, seems like a webpack bug
// const binRoot = '../bin/'

class Runner extends AsyncClass {
  constructor (moduleName) {
    super(async () => {
      const Module = (await import('../bin/' + moduleName + '.js')).default
      const wasmFile = (await import('../bin/' + moduleName + '.wasm')).default
      const module = Module({
        locateFile (path) {
          if (path.endsWith('.wasm')) {
            return wasmFile
          }
          return path
        }
      })
      // this.module = module
      module.onRuntimeInitialized = () => {
        console.log(module._fib(4))
      }
    })
  }
}

export {
  Runner as default
}
