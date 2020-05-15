import AsyncClass from './async-class'

// template string didnt work...dont know why, seems like a webpack bug
// const binRoot = '../bin/'

class WASMDriver extends AsyncClass {
  constructor (moduleName) {
    super(async () => {
      try {
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
        // emcc's `thenable` implementation may cause infinite loop
        // https://github.com/emscripten-core/emscripten/issues/5820
          delete m['then']
        })

        this._module = module
        this._name = moduleName

        this._wrapApi()
      } catch (error) {
        // XXX
        throw error
      }
    })
  }

  get module () {
    return this._name
  }

  _wrapApi () {
    this._api = []
    for (const key in this._module) {
      const apiPattern = /^_([a-z][A-Za-z0-9]*)/
      const match = apiPattern.exec(key)
      if (match) {
        // XXX
        this[match[1]] = (...args) => this._module[match[0]](...args)
        this._api.push(match[1])
      }
    }
  }

  get api () {
    return this._api
  }

  get heapChar () {
    return this._module.HEAP8
  }

  get heapInt () {
    return this._module.HEAP32
  }
}

export {
  WASMDriver as default
}
