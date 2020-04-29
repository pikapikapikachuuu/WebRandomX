/* eslint-disable camelcase */
const binRoot = '../bin/'

import randomx_api_example2 from '../bin/randomx-api-example2'

class Runner {
  constructor (moduleName) {
    const init = (async () => {
      // const randomx_api_example2 = await import(`${binRoot}${moduleName}.js`).default
      const module = await randomx_api_example2({ wasmBinaryFile: `${binRoot}${moduleName}.wasm` })
      module.calledRun = true

      this.module = randomx_api_example2
      window.module = this.module

      delete this.then
      return this
    })()
    this.then = init.then.bind(init)
  }
}

// class Runner {
//   constructor (moduleName) {
//     import(`${binRoot}${moduleName}.js`).then((m) => {
//       m({ wasmBinaryFile: `${binRoot}${moduleName}.wasm` })
//       m.calledRun = true
//       this.module = m
//       window.module = this.module
//     })
//   }
// }

export {
  Runner as default
}
