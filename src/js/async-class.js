class AsyncClass {
  constructor (asyncConstructor) {
    const init = (async () => {
      await asyncConstructor()
      delete this.then
      return this
    })()
    this.then = init.then.bind(init)
  }
}

export {
  AsyncClass as default
}
